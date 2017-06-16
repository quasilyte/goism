VERSION=0.1.0
PREFIX=/usr/local
DESTDIR=
DST=$(DESTDIR)$(PREFIX)
EMACS_GOPATH=~/.emacs.d/goism
GOPATH=$(shell pwd)

all: lisp translate_package

lisp:
	mkdir -p build
	go build -o build/build_lisp \
		-ldflags "-X main.Version=${VERSION}" \
		main/build_lisp
	build/build_lisp > build/goism.el
	emacs -Q --batch -f batch-byte-compile build/goism.el

translate_package:
	go build -o bin/goism_translate_package main/translate_package

clean:
	rm -rf build/* bin/*

install:
	mkdir -p $(EMACS_GOPATH)/src/emacs
	cp bin/goism_translate_package $(DST)/bin/
	chmod 755 $(DST)/bin/goism_translate_package

# Needed only if GOPATH not point to goism source dir.
install_lisp:
	cp -R src/emacs/lisp $(EMACS_GOPATH)/src/emacs/
	cp -R src/emacs/rt $(EMACS_GOPATH)/src/emacs/
	cp -R src/emacs/example $(EMACS_GOPATH)/src/emacs/

uninstall:
	rm $(DST)/bin/goism_translate_package

.PHONY: all lisp translate_package clean install install_lisp uninstall
