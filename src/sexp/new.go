package sexp

// Constructors for simple forms.

func NewNot(arg Form) *Not   { return &Not{Arg: arg} }
func NewNeg(arg Form) *Neg   { return &Neg{Arg: arg} }
func NewAdd1(arg Form) *AddX { return &AddX{Arg: arg, X: 1} }
func NewSub1(arg Form) *SubX { return &SubX{Arg: arg, X: 1} }

func NewShl(a, b Form) *Shl           { return &Shl{Args: [2]Form{a, b}} }
func NewShr(a, b Form) *Shr           { return &Shr{Args: [2]Form{a, b}} }
func NewBitOr(a, b Form) *BitOr       { return &BitOr{Args: [2]Form{a, b}} }
func NewBitAnd(a, b Form) *BitAnd     { return &BitAnd{Args: [2]Form{a, b}} }
func NewBitXor(a, b Form) *BitXor     { return &BitXor{Args: [2]Form{a, b}} }
func NewAdd(a, b Form) *Add           { return &Add{Args: [2]Form{a, b}} }
func NewSub(a, b Form) *Sub           { return &Sub{Args: [2]Form{a, b}} }
func NewMul(a, b Form) *Mul           { return &Mul{Args: [2]Form{a, b}} }
func NewQuo(a, b Form) *Quo           { return &Quo{Args: [2]Form{a, b}} }
func NewNumEq(a, b Form) *NumEq       { return &NumEq{Args: [2]Form{a, b}} }
func NewNumNotEq(a, b Form) *NumNotEq { return &NumNotEq{Args: [2]Form{a, b}} }
func NewNumLt(a, b Form) *NumLt       { return &NumLt{Args: [2]Form{a, b}} }
func NewNumLte(a, b Form) *NumLte     { return &NumLte{Args: [2]Form{a, b}} }
func NewNumGt(a, b Form) *NumGt       { return &NumGt{Args: [2]Form{a, b}} }
func NewNumGte(a, b Form) *NumGte     { return &NumGte{Args: [2]Form{a, b}} }
func NewStrEq(a, b Form) *StrEq       { return &StrEq{Args: [2]Form{a, b}} }
func NewStrNotEq(a, b Form) *StrNotEq { return &StrNotEq{Args: [2]Form{a, b}} }
func NewStrLt(a, b Form) *StrLt       { return &StrLt{Args: [2]Form{a, b}} }
func NewStrLte(a, b Form) *StrLte     { return &StrLte{Args: [2]Form{a, b}} }
func NewStrGt(a, b Form) *StrGt       { return &StrGt{Args: [2]Form{a, b}} }
func NewStrGte(a, b Form) *StrGte     { return &StrGte{Args: [2]Form{a, b}} }

func NewConcat(args ...Form) *Concat {
	return &Concat{Args: args}
}
