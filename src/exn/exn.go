package exn

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/printer"
	"go/token"
)

type Kind int

const (
	ErrLogic Kind = iota
	ErrNoImpl
	ErrUser
	ErrConv
)

type Error struct {
	Kind Kind
	Msg  string
}

func (e Error) Error() string {
	switch e.Kind {
	case ErrLogic:
		return "logical error: " + e.Msg
	case ErrNoImpl:
		return "unimplemented: " + e.Msg
	case ErrUser:
		return "error: " + e.Msg
	case ErrConv:
		return "translation error: " + e.Msg
	default:
		return "unknown error: " + e.Msg
	}
}

// Logic returns LogicError error.
func Logic(format string, args ...interface{}) Error {
	return errorf(ErrLogic, format, args...)
}

// NoImpl returns NoImplError error.
func NoImpl(format string, args ...interface{}) Error {
	return errorf(ErrNoImpl, format, args...)
}

// User returns ErrUser error.
func User(format string, args ...interface{}) Error {
	return errorf(ErrUser, format, args...)
}

// Conv returns rich ErrConv error.
func Conv(fileSet *token.FileSet, label string, node ast.Node) Error {
	var buf bytes.Buffer
	printer.Fprint(&buf, fileSet, node)
	pos := fileSet.Position(node.Pos())
	return errorf(
		ErrConv,
		"%s:%d: %s: `%s' (%T)",
		pos.Filename, pos.Line, label, buf.String(), node,
	)
}

// Catch handles "recover()" return value.
func Catch(x interface{}) error {
	switch panicArg := x.(type) {
	case nil:
		return nil // No panic happened
	case Error:
		return panicArg
	default:
		panic(panicArg) // Unexpected panic
	}
}

func errorf(k Kind, format string, args ...interface{}) Error {
	return Error{
		Kind: k,
		Msg:  fmt.Sprintf(format, args...),
	}
}
