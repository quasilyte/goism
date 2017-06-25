package sexp

func (atom Bool) Copy() Form   { return Bool(atom) }
func (atom Int) Copy() Form    { return Int(atom) }
func (atom Float) Copy() Form  { return Float(atom) }
func (atom Str) Copy() Form    { return Str(atom) }
func (atom Symbol) Copy() Form { return Symbol(atom) }
func (atom Var) Copy() Form {
	return Var{Name: atom.Name, Typ: atom.Typ}
}

func (lit *ArrayLit) Copy() Form {
	return &ArrayLit{Vals: CopyList(lit.Vals), Typ: lit.Typ}
}
func (lit *SparseArrayLit) Copy() Form {
	vals := make(map[int]Form, len(lit.Vals))
	for i, val := range lit.Vals {
		vals[i] = val.Copy()
	}
	return &SparseArrayLit{
		Ctor: lit.Ctor.Copy(),
		Vals: vals,
		Typ:  lit.Typ,
	}
}
func (lit *SliceLit) Copy() Form {
	return &SliceLit{Vals: CopyList(lit.Vals), Typ: lit.Typ}
}
func (lit *StructLit) Copy() Form {
	return &StructLit{Vals: CopyList(lit.Vals), Typ: lit.Typ}
}

func (form *ArrayUpdate) Copy() Form {
	return &ArrayUpdate{
		Array: form.Array.Copy(),
		Index: form.Index.Copy(),
		Expr:  form.Expr.Copy(),
	}
}
func (form *SliceUpdate) Copy() Form {
	return &SliceUpdate{
		Slice: form.Slice.Copy(),
		Index: form.Index.Copy(),
		Expr:  form.Expr.Copy(),
	}
}
func (form *StructUpdate) Copy() Form {
	return &StructUpdate{
		Struct: form.Struct.Copy(),
		Index:  form.Index,
		Expr:   form.Expr.Copy(),
		Typ:    form.Typ,
	}
}
func (form *Bind) Copy() Form {
	return &Bind{Init: form.Init.Copy()}
}
func (form *Rebind) Copy() Form {
	return &Rebind{Expr: form.Expr.Copy()}
}
func (form *VarUpdate) Copy() Form {
	return &VarUpdate{Expr: form.Expr.Copy()}
}
func (form *FormList) Copy() Form {
	return &FormList{Forms: CopyList(form.Forms)}
}
func (form *Block) Copy() Form {
	return &Block{Forms: CopyList(form.Forms)}
}
func (form *If) Copy() Form {
	return &If{
		Cond: form.Cond.Copy(),
		Then: &Block{Forms: CopyList(form.Then.Forms)},
		Else: form.Else.Copy(),
	}
}
func (form *Switch) Copy() Form {
	return &Switch{
		Expr:       form.Expr.Copy(),
		SwitchBody: copySwitchBody(form.SwitchBody),
	}
}
func (form *SwitchTrue) Copy() Form {
	return &SwitchTrue{SwitchBody: copySwitchBody(form.SwitchBody)}
}
func (form *Return) Copy() Form {
	return &Return{Results: CopyList(form.Results)}
}
func (form *ExprStmt) Copy() Form {
	return &ExprStmt{Expr: form.Expr.Copy()}
}
func (form *Goto) Copy() Form  { return &Goto{LabelName: form.LabelName} }
func (form *Label) Copy() Form { return &Label{Name: form.Name} }

func (form *Repeat) Copy() Form {
	return &Repeat{
		N:    form.N,
		Body: &Block{Forms: CopyList(form.Body.Forms)},
	}
}
func (form *DoTimes) Copy() Form {
	return &DoTimes{
		N:     form.N.Copy(),
		Iter:  form.Iter,
		Step:  form.Step.Copy(),
		Body:  &Block{Forms: CopyList(form.Body.Forms)},
		Scope: form.Scope,
	}
}
func (form *Loop) Copy() Form {
	return &Loop{Body: &Block{Forms: CopyList(form.Body.Forms)}}
}
func (form *While) Copy() Form {
	return &While{
		Cond: form.Cond.Copy(),
		Post: form.Post.Copy(),
		Body: &Block{Forms: CopyList(form.Body.Forms)},
	}
}

func (form *ArrayIndex) Copy() Form {
	return &ArrayIndex{
		Array: form.Array.Copy(),
		Index: form.Index.Copy(),
	}
}
func (form *SliceIndex) Copy() Form {
	return &SliceIndex{
		Slice: form.Slice.Copy(),
		Index: form.Index.Copy(),
	}
}
func (form *StructIndex) Copy() Form {
	return &StructIndex{
		Struct: form.Struct.Copy(),
		Index:  form.Index,
		Typ:    form.Typ,
	}
}

func (form *ArraySlice) Copy() Form {
	return &ArraySlice{
		Array: form.Array.Copy(),
		Typ:   form.Typ,
		Span:  copySpan(form.Span),
	}
}
func (form *SliceSlice) Copy() Form {
	return &SliceSlice{
		Slice: form.Slice.Copy(),
		Span:  copySpan(form.Span),
	}
}

func (form *TypeAssert) Copy() Form {
	return &TypeAssert{Expr: form.Expr.Copy(), Typ: form.Typ}
}

func (call *LispCall) Copy() Form {
	return &LispCall{Fn: call.Fn, Args: CopyList(call.Args)}
}
func (call *Call) Copy() Form {
	return &Call{Fn: call.Fn, Args: CopyList(call.Args)}
}
func (form *Let) Copy() Form {
	binds := make([]*Bind, len(form.Bindings))
	for i, bind := range form.Bindings {
		binds[i] = &Bind{Init: bind.Init.Copy()}
	}
	if form.Expr != nil {
		return &Let{Bindings: binds, Expr: form.Expr.Copy()}
	}
	return &Let{Bindings: binds, Stmt: form.Stmt.Copy()}
}
func (form *TypeCast) Copy() Form {
	return &TypeCast{Form: form.Form.Copy(), Typ: form.Typ}
}

func (form *And) Copy() Form {
	return &And{X: form.X.Copy(), Y: form.Y.Copy()}
}
func (form *Or) Copy() Form {
	return &Or{X: form.X.Copy(), Y: form.Y.Copy()}
}

func CopyList(forms []Form) []Form {
	if forms == nil {
		return nil
	}
	res := make([]Form, len(forms))
	for i, form := range forms {
		res[i] = form.Copy()
	}
	return res
}

func copySpan(span Span) Span {
	return Span{
		Low:  span.Low.Copy(),
		High: span.High.Copy(),
	}
}

func copySwitchBody(b SwitchBody) SwitchBody {
	var clauses []CaseClause
	if b.Clauses != nil {
		clauses = make([]CaseClause, len(b.Clauses))
		for i, cc := range b.Clauses {
			clauses[i] = CaseClause{
				Expr: cc.Expr.Copy(),
				Body: cc.Body.Copy().(*Block),
			}
		}
	}
	return SwitchBody{
		Clauses:     clauses,
		DefaultBody: b.DefaultBody.Copy().(*Block),
	}
}

func copyCaseClauseList(clauses []CaseClause) []CaseClause {
	if clauses == nil {
		return nil
	}
	res := make([]CaseClause, len(clauses))
	for i, cc := range clauses {
		res[i] = CaseClause{
			Expr: cc.Expr.Copy(),
			Body: cc.Body.Copy().(*Block),
		}
	}
	return res
}
