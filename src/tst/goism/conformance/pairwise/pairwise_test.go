package pairwise

import (
	"assert"
	"bytes"
	"fmt"
	"io/ioutil"
	"os/exec"
	"regexp"
	"strings"
	"testing"
	"text/template"
	"tst"
	"tst/goism"
)

type testInfo struct {
	Filename string
	Funcs    []string
}

func getTestFileContents(name string) []byte {
	path := fmt.Sprintf(
		"%s/src/emacs/conformance/pairwise/%s",
		goism.Home, name,
	)
	testFileContents, err := ioutil.ReadFile(path)
	assert.Nil(err)
	return testFileContents[len("package pairwise"):]
}

func setTestInfoDefaults(info *testInfo, testFileContents []byte) {
	// If funcs are not enumerated explicitly, build up a
	// list by examining test file contents.
	if info.Funcs == nil {
		funcs := make([]string, 0, 64)
		rx := regexp.MustCompile(`func (test\w+)`)
		for _, match := range rx.FindAllSubmatch(testFileContents, -1) {
			funcs = append(funcs, string(match[1]))
		}
		info.Funcs = funcs
	}
	fmt.Printf("%d funcs to test\n", len(info.Funcs))
}

func prepareGcgoProgram(info *testInfo, testFileContents []byte) string {
	tmpl := template.Must(template.New("go").Parse(`
		package main
		import "fmt"
		// {{.TestFilename}} contents:
		{{.TestFileContents}}
		func main() {
			{{ range .TestFuncs }}
			fmt.Println({{.}}())
			{{ end }}
		}
	`))
	var program bytes.Buffer
	tmpl.Execute(&program, struct {
		TestFilename     string
		TestFileContents string
		TestFuncs        []string
	}{
		TestFilename:     info.Filename,
		TestFileContents: string(testFileContents),
		TestFuncs:        info.Funcs,
	})

	path := fmt.Sprintf("%s/build/pairwise.go", goism.Home)
	assert.Nil(ioutil.WriteFile(path, program.Bytes(), 0644))
	return path
}

func testGcgo(info *testInfo, programPath string) []string {
	output, err := exec.Command("go", "run", programPath).Output()
	assert.Nil(err)
	results := strings.Split(string(output), "\n")
	return results[:len(results)-1]
}

func testGoism(info *testInfo) []string {
	goism.LoadPackage("conformance/pairwise")
	goism.LoadPackage("rt")
	results := make([]string, len(info.Funcs))
	for i, fn := range info.Funcs {
		results[i] = goism.EvalCall("conformance/pairwise", fn)
	}
	return results
}

func testPairwise(t *testing.T, info testInfo) {
	testFileContents := getTestFileContents(info.Filename)
	setTestInfoDefaults(&info, testFileContents)
	gcgoProgramPath := prepareGcgoProgram(&info, testFileContents)
	results := testGoism(&info)
	correctResults := testGcgo(&info, gcgoProgramPath)
	for i, res := range results {
		tst.CheckError(t, info.Funcs[i], res, correctResults[i])
	}
}

func TestPairwise(t *testing.T) {
	testPairwise(t, testInfo{
		Filename: "structs.go",
	})
}
