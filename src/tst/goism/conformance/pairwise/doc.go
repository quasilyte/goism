// Package pairwise is a second layer of conformance tests.
//
// Specially written Go package is compiled with two separate
// compilers: goism compiler and gcgo.
//
// After compiled, {test functions}[1] are executed for both
// generated programs. Gcgo produces native binary, so that is
// executed. Goism program is run by Emacs daemon.
//
// The result of those invocations is then compared pairwise,
// hence the name of this test suit.
//
// [1] Test function starts with "test" prefix.
// It may safely return:
// - any integer type, int/int32 are preffered;
// - string;
// - float64;
// - array (not slice!) of any type listed above;
package pairwise
