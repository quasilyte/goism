VERSION=0.1.0

all: lisp translate_package

lisp:
	go build -o build/build_lisp \
		-ldflags "-X main.Version=${VERSION}" \
		main/build_lisp
	build/build_lisp > build/Go.el
	emacs -Q --batch -f batch-byte-compile build/Go.el

translate_package:
	go build -o bin/goel_translate_package main/translate_package

clean:
	rm -rf build/* bin/*

.PHONY: translate_package clean lisp
