package sexpconv

import "ir/instr"
import "sexp"
import "go/ast"

var nameToInstr = map[string]instr.Instr{
	"cons":      instr.Cons,
	"car":       instr.Car,
	"cdr":       instr.Car,
	"aref":      instr.ArrayRef,
	"aset":      instr.ArraySet,
	"=":         instr.NumEq,
	">":         instr.NumGt,
	"<":         instr.NumLt,
	"<=":        instr.NumLte,
	">=":        instr.NumGte,
	"+":         instr.NumAdd,
	"-":         instr.NumSub,
	"*":         instr.NumMul,
	"/":         instr.NumQuo,
	"string=":   instr.StrEq,
	"string<":   instr.StrLt,
	"substring": instr.Substr,
}

func (conv *Converter) instrCall(name string, args []ast.Expr) *sexp.InstrCall {
	if name == "concat" {
		return &sexp.InstrCall{
			Instr: instr.Concat(len(args)),
			Args:  conv.valueCopyList(conv.exprList(args)),
		}
	}
	ins, ok := nameToInstr[name]
	if !ok {
		return nil
	}
	if len(args) > 3 || len(args) != int(ins.Input) {
		return nil
	}
	return &sexp.InstrCall{
		Instr: ins,
		Args:  conv.valueCopyList(conv.exprList(args)),
	}
}
