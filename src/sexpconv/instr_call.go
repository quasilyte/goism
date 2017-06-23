package sexpconv

import (
	"elapc/instr"
	"sexp"
)

var nameToInstr = map[string]instr.Instr{
	"cons":     instr.Cons,
	"car":      instr.Car,
	"cdr":      instr.Car,
	"aref":     instr.ArrayRef,
	"aset":     instr.ArraySet,
	"=":        instr.NumEq,
	">":        instr.NumGt,
	"<":        instr.NumLt,
	"<=":       instr.NumLte,
	">=":       instr.NumGte,
	"+":        instr.NumAdd,
	"-":        instr.NumSub,
	"*":        instr.NumMul,
	"/":        instr.NumQuo,
	"min":      instr.NumMin,
	"string=":  instr.StrEq,
	"string<":  instr.StrLt,
	"length":   instr.Length,
	"not":      instr.Not,
	"memq":     instr.Memq,
	"member":   instr.Member,
	"integerp": instr.IsInt,
	"stringp":  instr.IsStr,
	"symbolp":  instr.IsSymbol,
	"eq":       instr.Eq,
	"equal":    instr.Equal,
}

func (conv *converter) instrCall(name string, args []sexp.Form) sexp.Form {
	if name == "concat" {
		return &sexp.InstrCall{Instr: instr.Concat(len(args)), Args: args}
	}
	if name == "list" {
		return &sexp.InstrCall{Instr: instr.List(len(args)), Args: args}
	}
	if name == "substring" && len(args) <= 3 {
		for len(args) < 3 {
			args = append(args, sexp.Symbol{Val: "nil"})
		}
		return &sexp.InstrCall{Instr: instr.Substr, Args: args}
	}

	ins, ok := nameToInstr[name]
	if !ok {
		return nil
	}
	if len(args) > 3 || len(args) != int(ins.Input) {
		return nil
	}
	return &sexp.InstrCall{Instr: ins, Args: args}
}
