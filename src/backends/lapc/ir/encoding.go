package ir

type Encoding struct {
	Name   []byte
	HasArg bool
	Input  inputMode
	Output outputMode
}

func EncodingOf(kind InstrKind) *Encoding {
	return &kindToEncoding[kind]
}

type inputMode int

const (
	AttrTakeNothing inputMode = iota
	AttrTake1
	AttrTake2
	AttrTake3
	AttrTakeN
	AttrTakeNplus1
	AttrReplaceNth
)

type outputMode int

const (
	AttrPushNothing outputMode = iota
	AttrDupNth
	AttrPushTmp
	AttrPushConst
	AttrPushAndDiscard
)

var kindToEncoding = [...]Encoding{
	Empty:       Encoding{Name: []byte("%empty")},
	XvarRef:     Encoding{Name: []byte("%xvar-ref")},
	XvarSet:     Encoding{Name: []byte("%xvar-set")},
	XlocalRef:   Encoding{Name: []byte("%xlocal-ref")},
	XlocalSet:   Encoding{Name: []byte("%xlocal-set")},
	Xbind:       Encoding{Name: []byte("%xbind")},
	XscopeEnter: Encoding{Name: []byte("%scope-enter")},
	XscopeLeave: Encoding{Name: []byte("%scope-leave")},

	Xgoto:           Encoding{Name: []byte("%xgoto")},
	XlambdaRet:      Encoding{Name: []byte("%xinline-ret")},
	XlambdaEnter:    Encoding{Name: []byte("%xinline-enter")},
	XlambdaRetLabel: Encoding{Name: []byte("%xinline-ret-label")},

	Label:            Encoding{Name: []byte("label")},
	Jmp:              Encoding{Name: []byte("goto")},
	JmpNil:           jump("goto-if-nil"),
	JmpNotNil:        jump("goto-if-not-nil"),
	JmpNilElsePop:    jump("goto-if-nil-else-pop"),
	JmpNotNilElsePop: jump("goto-if-not-nil-else-pop"),

	Return: returnEnc,
	Call:   callEnc,

	Eq:        op2("eq"),
	Equal:     op2("equal"),
	Substring: op3("substr"),
	Length:    op1("length"),

	NumEq:  op2("num="),
	NumGt:  op2("num>"),
	NumLt:  op2("num<"),
	NumLte: op2("num<="),
	NumGte: op2("num>="),
	Add:    op2("add"),
	Add1:   op1("add1"),
	Sub:    op2("sub"),
	Sub1:   op1("sub1"),
	Mul:    op2("mul"),
	Quo:    op2("quo"),
	Min:    op2("min"),
	Neg:    op1("neg"),

	StrEq: op2("str="),
	StrLt: op2("str<"),

	Aref: op2("array-ref"),
	Aset: asetEnc,

	Car:    op1("car"),
	Cdr:    op1("cdr"),
	SetCar: setCarEnc,
	SetCdr: setCdrEnc,
	Cons:   op2("cons"),
	List:   listEnc,
	Memq:   op2("memq"),
	Member: op2("member"),

	Concat: concatEnc,

	Stringp:  op1("str?"),
	Integerp: op1("int?"),
	Symbolp:  op1("symbol?"),
	Not:      op1("not"),

	ConstRef: constRefEnc,
	StackRef: stackRefEnc,
	StackSet: stackSetEnc,
	Discard:  discardEnc,
	VarRef:   varRefEnc,
	VarSet:   varSetEnc,
}

var (
	callEnc = Encoding{
		Name:   []byte("call"),
		HasArg: true,
		Input:  AttrTakeNplus1,
		Output: AttrPushTmp,
	}

	returnEnc = Encoding{
		Name:  []byte("return"),
		Input: AttrTake1,
	}

	asetEnc = Encoding{
		Name:   []byte("array-set"),
		HasArg: false,
		Input:  AttrTake3,
		Output: AttrPushTmp,
	}

	substringEnc = Encoding{
		Name:   []byte("substr"),
		Input:  AttrTake1,
		Output: AttrPushTmp,
	}

	setCarEnc = Encoding{
		Name:   []byte("setcar"),
		Input:  AttrTake2,
		Output: AttrPushAndDiscard,
	}

	setCdrEnc = Encoding{
		Name:   []byte("setcdr"),
		Input:  AttrTake2,
		Output: AttrPushAndDiscard,
	}

	listEnc = Encoding{
		Name:   []byte("list"),
		HasArg: true,
		Input:  AttrTakeN,
		Output: AttrPushTmp,
	}

	concatEnc = Encoding{
		Name:   []byte("concat"),
		HasArg: true,
		Input:  AttrTakeN,
		Output: AttrPushTmp,
	}

	constRefEnc = Encoding{
		Name:   []byte("constant"),
		HasArg: true,
		Output: AttrPushConst,
	}

	stackRefEnc = Encoding{
		Name:   []byte("stack-ref"),
		HasArg: true,
		Output: AttrDupNth,
	}

	stackSetEnc = Encoding{
		Name:   []byte("stack-set"),
		HasArg: true,
		Input:  AttrTake1,
	}

	discardEnc = Encoding{
		Name:   []byte("discard"),
		HasArg: true,
		Input:  AttrTakeN,
	}

	varRefEnc = Encoding{
		Name:   []byte("var-ref"),
		HasArg: true,
		Output: AttrPushTmp,
	}

	varSetEnc = Encoding{
		Name:   []byte("var-set"),
		HasArg: true,
		Input:  AttrTake1,
	}
)

func op1(name string) Encoding {
	return Encoding{
		Name:   []byte(name),
		HasArg: false,
		Input:  AttrTake1,
		Output: AttrPushTmp,
	}
}

func op2(name string) Encoding {
	return Encoding{
		Name:   []byte(name),
		HasArg: false,
		Input:  AttrTake2,
		Output: AttrPushTmp,
	}
}

func op3(name string) Encoding {
	return Encoding{
		Name:   []byte(name),
		HasArg: false,
		Input:  AttrTake3,
		Output: AttrPushTmp,
	}
}

func jump(name string) Encoding {
	return Encoding{
		Name:  []byte(name),
		Input: AttrTake1,
	}
}
