# Quick guide

This guide presents a shortest way to learn `goism`.
Details described elsewhere.

**Note**: this guide is written for Linux systems.

## 0. Script install

If you wish to skip most of the text, use 
[quick install script](../script/quick_install)
to prepare you environment with default settings.

## 1. Preparations

### 1.1 Prerequisites

* `go` 1.6 or above
* `emacs` 24.1 or above (lexical scoping)
* Fundamental tools like `make` and `git`
* `GOPATH` is properly set (see **1.3**)

### 1.2 Install

```
mkdir -p ~/.emacs.d/
cd ~/.emacs.d/
# Download the repository.
git clone https://github.com/Quasilyte/goism.git
cd 'goism'
# Build everything.
make all
# Install bundled emacs packages and compiled binaries.
sudo make install
```

> `<...>` inside paths means "directory which contains cloned goism"
> (should be the same as `GOPATH` is you follow default installation scheme)

Note that you can avoid `make install`.
This guide is written for the case when you do install it.
Check `Makefile` for clues about what you will miss without `install`.

Because `goism` is not a proper Emacs package yet,
you have to `load` it manually. 

Options:
* Use `script/quick_run` (requires `GOPATH` to be set, see **1.3**)
* `(load "<...>/goism/build/goism.elc")`
* Visit `build/goism.elc` buffer and run `M-x RET eval-buffer`

There is `build/goism.el` file if you want to inspect package sources
before loading them.

### 1.3 Setup environment

You need to choose a directory which will be 
set as a `GOPATH` for `goism` code.
Choose `~/.emacs.d/goism/` if you want more defaults to
work out-of-the-box.

Add `export GOPATH=~/.emacs.d/goism/` to your `~/.bashrc`.
If you choose different location, update `goism-emacs-gopath`
variable (can be done through `M-x customize`, 
group `development`->`goism`).

It is mandatory to store your code under the  
[Go workspace](https://golang.org/doc/code.html#Workspaces).
If you already have `GOPATH` set, then switch it temporary
or use current workspace for the rest of the guide.

### 1.4 Check installation

At this point you need to have `goism` loaded into Emacs (see **1.2**).
If it is not, run emacs via `script/quick_run`.

`<...>/goism/src/emacs` contains special packages that
are copied into `~/.emacs.d/goism/src/emacs` during `make install`.
`emacs/rt` and `emacs/lisp` packages are mandatory.

Run `M-x goism-translate` and enter `rt` package name.
You should see `*goism compile*` temporary buffer which contain
compiled Go package. 

Switch to that buffer and do `M-x eval-buffer`.

> As an alternative, you may use `goism-load`.
> Use `C-h f gosim-load` for more information.

```clojure
;; You can paste this code in *Scratch* buffer and evaluate it.
(goism-rt.arrayToStr [?a ?b]) ;; => "ab"
(goism-rt.Println '("hello" "world")) ;; Check *Messages* buffer
```

You have just loaded `goism` runtime (rt) package.
In order to execute code that uses slices, maps and other 
fancy stuff, runtime must be loaded into Emacs.

It is convenient to save `*goism compile*` contents and
load it inside your `.emacs`. This way runtime will always
be available.

## 2. Basic usage

### 2.1 Run simple Go code

Each package that is intended to be executed inside Emacs
should have `emacs` path prefix.
This means that the package `guide` we are about to create
will have `emacs/guide` path (the absolute path is, therefore,
`$GOPATH/src/emacs/guide`).

1. Create new package directory: `mkdir -p $GOPATH/src/emacs/guide`
2. Open new with `M-x find-file $GOPATH/src/emacs/guide/guide.go`
3. Fill opened file with snipped presented below

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

Run `M-x goism-load RET guide`.

```clojure
;; Test functions:
(goism-guide.Factorial 4) ;; => 120

;; Test variables:
goism-guide.fact3 ;; => 6
goism-guide.fact4 ;; => 24
```

Your package may consist of multiple files.
Multiple package comments are permitted, they are joined together.

> You can bind compilation to a hotkey for convenience:
> `(global-set-key (kbd "C-x g") 'goism-translate)`

### 2.2 Import existing Go package

```shell
# Create another package.

mkdir -p $GOPATH/src/emacs/guide/mylib

cat << EOF > $GOPATH/src/emacs/guide/mylib/mylib.go
package mylib

const FavNumber = 256

func GreetMsg(name string) string {
	if name == "" {
		name = "Emacs user"
	}
	return "Hello, " + name + "!"
}
EOF
```

Visit a new file `M-x find-file $GOPATH/src/emacs/guide/foo.go`
Import `mylib` package:

```go
package guide 

import (
	"emacs/guide/mylib"
)

func Foo() {
    println("FavNumber=", mylib.FavNumber)
    println(mylib.GreetMsg("Lisp hacker"))
}
```

Now try to evaluate `(goism-guide.Foo)`.

You should get this:
```
Debugger entered--Lisp error: (void-function goism-guide/mylib.GreetMsg)
  goism-guide/mylib.GreetMsg("Lisp hacker")
  goism-guide.Foo()
```

`goism-guide/mylib.GreetMsg` is undefined.

That is correct, you have not loaded `mylib` yet.
Execute `M-x goism-load RET guide/mylib` and run `Foo` again.

Expected output is:
```
"FavNumber=" 256
"Hello, Lisp hacker!"
```

### 2.3 Type mapping overview

* Integers, floats and strings map in intuitive way
* Some special Elisp types are available via `emacs/lisp` package
* Go maps are Elisp hash tables
* Go arrays implemented via Elisp vectors
* Go slices are emulated by vectors with {offset, len, cap}.
* Go structures represented as lists and vectors (depending on the field count)
* Go interfaces use `(type-info . data)` pairs

Right now only single level of indirection is possible; and only
for struct types. 
Unsigned integers of different fixed sizes are emulated, 
but in a tricky way; to get more details, 
see [translation spec](translation_spec.md).

### 2.4 User defined types

Like any gopher you may want to define your own types.

This section describes some details about how your Go types are
mapped into Emacs Lisp.

For simple `type T1 T2` types you get overhead-free, unboxed values.
This means that `Foo` from `type Foo int` is expressed as an 
ordinary integer. No allocations are performed during such objects
instantiation (unless the `T2` is composite object itself).

Struct types yield either (improper) lists or vestors.
When field count is less than 5, list is used,
othervise vector is chosen.
```
struct {x1 int}                     | (cons x1 nil)
struct {x1, x2 int}                 | (cons x1 x2)
struct {x1, x2, x3 int}             | (cons x1 (cons x2 x3))
struct {x1, x2, x3, x4 int}         | (cons x1 (cons x2 (cons x3 x4)))
struct {x1, x2, x3, x4, x5 int}     | (vector x1 x2 x3 x4 x5)
struct {x1, x2, x3, x4, x5, x6 int} | (vector x1 x2 x3 x4 x5 x6)
```

This has several consequences:
* Taking address `&S` is free
* Dereferencing `*S` is free
* Passing `S` by value creates a copy, but passes a refential type
* Objects are untagged (no type ID member)

Using `*S` is more efficient than `S` in **all** cases.
Pass `S` only when you really need an object copy.

The `N=4` threshold may be changed in future.
The main point is that you should not rely on the Go struct objects
data layout. Do not pass them to your Elisp code.

Methods converted to Elisp functions where first argument is
methods receiver. 
```go
type MyInt int

// Both definitions produce same executable code.
func (v MyInt) Sqr() int { return int(v * v) }
func Sqr(v MyInt) int { return int(v * v) }
```

### 2.5 emacs/lisp package

You can call any Emacs Lisp function with `lisp.Call`:
`lisp.Call("insert", "Text to be inserted")`.

`lisp.Call` returns `lisp.Object` which is an interface type.  
`lisp.Object` can be queried for specific type value in type-assert style:
`x := lisp.Call("+", 1, 2).Int()`.

Functions that have `FFI` wrapper can be called in more
convenient and type safe way:
`lisp.Insert("Text to be inserted")` 
More on `FFI` in **2.6**.

```go
package example

import "emacs/lisp"

func usingLispPackage(a, b int) int {
	// Create Lisp cons pair.
	pair := lisp.Call("cons", 1, 2)
	// Access cons pair members.
	a := lisp.Call("car", pair)
	b := lisp.Call("cdr", pair)
	// Coerce lisp.Object to ints.
	return a.Int() + b.Int()
}
```

Look into `emacs/lisp` package sources to see full API.
If you want *real examples*, `emacs/rt` package is what you
are looking for.

### 2.6 emacs/lisp FFI

`src/emacs/lisp/ffi.go` contains automatically generated 
FFI signatures. 

You can edit that file, for example, remove some functions
from it, but if you wish to add new entries, using **FFI generator**
is strongly advised.

Suppose you want to add `identity` Lisp function that takes
any argument and returns it:

1. First, load `lisp/ffi/ffi.el`.
2. Now open `lisp/ffi/default-ffi.el`
3. Look for the "Other functions" comment
4. Insert `(identity Identity (:any x) :object)` below that comment
5. Evaluate the whole form `(goism-declare ... (identity ...))`
6. Copy `*ffi.go*` buffer contents to `$GOPATH/src/emacs/lisp/ffi.go`

Now it is possible to call `identity` as `lisp.Identity` inside 
you Go code.

```
;; FFI entry format.
(identity Identity (:any x) :object)
 ^        ^        ^        ^
 |        |        |        |
 |        |        |        Output type (can not be :any)
 |        |        List of input params. {type, name} pairs
 |        Symbol that is visible inside Go. Should be a valid Go identifier
 lisp symbol; function to be called via FFI
```

## 3. Conventions, best practices and advices

### 3.1 Public API design

Two kinds of packages should be distinguished.
The first kind is libraries that are intended to be used
from Go code (written for Emacs).
Nothing special about them.
`emacs/guide/mylib` is such package.

The second kind is a packages that expose functions
to be called from Emacs Lisp directly. 

Important points:
* Do not return types that are specific to Go runtime (e.g. slices)
* Do not use multiple return values

```go
// BAD
func f2() (int, int) { return 1, 2 }
// GOOD
func f2() lisp.Object { 
	return lisp.Call("cons", 1, 2) 
}
```

### 3.2 Monitoring implementation status

Features that are not implemented and are not planned to
be implemented in near future can be found in 
[unimplemented.md](unimplemented.md).

Github [Projects tab](https://github.com/Quasilyte/goism/projects)
contains project roadmaps.

[Issues](https://github.com/Quasilyte/goism/issues) are a good 
source of known bugs and limitations. 
By the way, you are welcome to open new issues.
