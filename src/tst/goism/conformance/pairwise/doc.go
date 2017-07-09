// Package pairwise is a second layer of conformance tests.
//
// Specially written Go package is compiled with two separate
// compilers: goism compiler and gcgo.
//
// After compiled, test functions are executed for both
// generated programs. Gcgo produces native binary, so that is
// executed. Goism program is run by Emacs daemon.
//
// The result of those invocations is then compared pairwise,
// hence the name of this test suit.
package pairwise
