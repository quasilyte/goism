package function

var (
	Print = Type{
		name:       "Go--print",
		resultKind: ResultVoid,
	}

	Println = Type{
		name:       "Go--println",
		resultKind: ResultVoid,
	}

	Panic = Type{
		name:       "Go--panic",
		resultKind: ResultUndefined,
	}

	MakeHashTable = Type{name: "make-hash-table"}
	Puthash       = Type{name: "puthash", resultKind: ResultVoid}
	Gethash       = Type{name: "gethash"}

	Intern = Type{name: "intern"}
)
