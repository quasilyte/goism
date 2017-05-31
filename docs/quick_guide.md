# Quick guide

This guide presents a shortest way to learn `Go.el`.
Details described elsewhere.

## 1. Preparations

### 1.1 Prerequisites

* `go` 1.6 or above
* `emacs` 24.1 or above (lexical scoping)
* Fundamental tools like `make` and `git`
* `GOPATH` is set (see **1.3**)

### 1.2 Install

```
# Download repository.
git clone https://github.com/Quasilyte/Go.el.git
cd 'Go.el'
# Build everything.
make all
# Install bundled emacs packages and compiled binaries.
sudo make install
```

> `<...>` inside paths means "directory which contains cloned Go.el"

Note that you can avoid `make install`.
This guide is written for the case when you do install it.
Check `Makefile` for clues about what you will miss without `install`.

Because `Go.el` is not a proper Emacs package yet,
you have to `load` it manually. 

Options:
* `(load "<...>/Go.el/build/Go.elc")`
* Visit `build/Go.elc` buffer and run `M-x RET eval-buffer`

There is `build/Go.el` file if you want to inspect package sources
before loading them.

### 1.3 Setup environment

You need to choose a directory which will be 
set as a `GOPATH` for `Go.el` code.
`~/emacs.d/Go.el/` can be suitable.

Add `export GOPATH=~/emacs.d/Go.el/` to your `~/.bashrc`.
If you choose different location, update `Go-emacs-gopath`
variable (can be done through `M-x customize`, 
group `development`->`Go.el`).

It is mandatory to store your code below Go workspace.
If you already have `GOPATH` set, then switch it temporary
or use current workspace for the rest of the guide.

### 1.4 Check installation

`<...>/Go.el/src/emacs` contains useful packages that
are copied into `~/.emacs.d/Go.el/src/emacs` during `make install`.

Run `M-x Go-translate-by-name` and enter `example` package name.
You should see `*Go.el compile*` temporary buffer which contain
compiled Go package. 

Switch to that buffer and do `M-x eval-buffer`.

> As an alternative, you may use `Go-load-by-name`.
> Use `C-h f Go-load-by-name` for more information.

```clojure
;; You can paste this code in *Scratch* buffer and evaluate it.
(Go-example.PrintFiveLetters [?a ?b ?c ?d ?e])
(Go-example.PrintMessage "Hello, Emacs!") 
```

Check the `*Messages*` buffer. It must contain output
that is done by invocations of `Go-example.Print*`.

## 2. Basic usage

### 2.1 Run simple Go code

Each package that is intended to be executed inside Emacs
should have `emacs` path prefix.
This means that the package `guide` we are about to create
will have `emacs/guide` path.

```shell
sudo mkdir -p $GOPATH/src/emacs/guide
emacs $GOPATH/src/emacs/guide/guide.go
```

Fill opened file with code from snippet below:

```go
// Package guide is a part of quick guide document.
package guide

var (
	fact3, fact4 = Factorial(3), Factorial(4)
)

// Factorial computes factorial of X.
func Factorial(x int) int {
	if x <= 1 {
		return x
	}
	return x * Factorial(x-1)
}
```

We have **package comment** and a function with **documentation comment**.
Generated code will preserves documentation.

Run `M-x Go-load-by-name RET guide`.

```clojure
(Go-guide.Factorial 4)
;; => 120
Go-guide.fact3
;; => 6
Go-guide.fact4
;; => 24
```

If you want to save generated package, run `Go-translate-by-name`
and save buffer contents to a file.

Your package may consist of multiple files.
Multiple package comments are permitted, they are joined together.

> You can bind compilation to a hotkey for convenience:
> `(global-set-key (kbd "C-x g") 'Go-translate-by-name)`

### 2.2 Import existing Go package

```shell
# Create another package.

mkdir -p $(go env GOROOT)/src/emacs/mylib

cat << EOF > ~$(go env GOROOT)/src/emacs/mylib/mylib.go
package mylib

const FavNumber = 256

func GreetMsg(name string) string {
    return "Hello, " + name
}
EOF
```

Modify `guide.go` file by adding `import "emacs/mylib"`
and a new function that uses it.

```go
import "emacs/mylib"

func Foo() {
    println("FavNumber=", mylib.FavNumber)
    println(mylib.GreetMsg("Lisp hacker"))
}
```

### 2.2 EmacsX utility packages

> Mockup

### 2.3 Running Go code from Emacs

> Mockup


## 3. Conventions, best practices and advices

### 3.1 Public API design

> Mockup
