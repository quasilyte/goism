package goism

import (
	"os"
)

// Home is a path to directory that
// contains goism repository (sources, scripts, etc).
var Home = func() string {
	res := os.Getenv("GOISM_HOME")
	if res == "" {
		panic("GOISM_HOME environment variable is not set")
	}
	return res
}()
