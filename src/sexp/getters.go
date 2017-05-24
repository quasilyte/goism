package sexp

func GetBool(form Form) (bool, bool) {
	if form, ok := form.(Bool); ok {
		return bool(form), true
	}
	return false, false
}

func GetInt(form Form) (int64, bool) {
	if form, ok := form.(Int); ok {
		return int64(form), true
	}
	return 0, false
}

func GetFloat(form Form) (float64, bool) {
	if form, ok := form.(Float); ok {
		return float64(form), true
	}
	return 0.0, false
}

func GetString(form Form) (string, bool) {
	if form, ok := form.(String); ok {
		return string(form), true
	}
	return "", false
}

func (form *Shl) Arg() Form { return form.Args[0] }
func (form *Shl) N() Form   { return form.Args[1] }
func (form *Shr) Arg() Form { return form.Args[0] }
func (form *Shr) N() Form   { return form.Args[1] }
