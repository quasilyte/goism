# Quick guide

This guide aims to be as short and informative, as possible. 

## 1. Preparations

### 1.1 Install

```
# Assuming your current directory is <...>.
# Download repository.
git clone https://github.com/Quasilyte/Go.el.git
cd 'Go.el'
# Build everything.
make all
# Install emacs packages and compiled binaries
sudo make install
```

Note that you can avoid `make install`.
This guide is written for the case when you do install it.
Look at the `Makefile` to get clues what you will miss without `install`.

Because `Go.el` is not a proper Emacs package yet,
you have to `load` it manually. 
Options:
a) `(load "<...>/Go.el/build/Go.elc")`
b) Visit `build/Go.elc` buffer and run `M-x RET eval-buffer`
There is `build/Go.el` file if you want to inspect package sources
before loading them.

### 1.2 Setup environment

If you did `make install`, skip this section.

Set `Go-utils-path` Emacs variable: it must point to `bin`
directory which contains `goel_translate_package`.
It can be done via `M-x customize` (`development/Go.el` group).
Value `<...>/Go.el/bin` will work.

By default, `Go.el` searches `Go` packages that are meant to
be run inside Emacs at `~/go/src/emacs/`.
For example, `(Go-translate-by-name "foo")`
will look for `~/go/src/emacs/foo`.
This can be customized with `Go-emacs-package-path` variable.

### 1.3 Check installation

Run `M-x Go-translate-by-name` and enter `example` package name.
You should see `*IR compile*` temporary buffer which contain
compiled Go package. 

Switch to that buffer and do `M-x eval-buffer`.

```
(Go-example.PrintFiveLetters [?a ?b ?c ?d ?e])
(Go-example.PrintMessage "Hello, Emacs!") 
;; Check *Messages* buffer.
```

You have just executed Go code inside Emacs.

## 2. Basic usage

### 2.1 Writing Go packages for Emacs

> Mockup

### 2.2 EmacsX utility packages

> Mockup

### 2.3 Running Go code from Emacs

> Mockup


## 3. Conventions, best practices and advices

### 3.1 Public API design

> Mockup
