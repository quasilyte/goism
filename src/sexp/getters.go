package sexp

func GetBool(form Node) (bool, bool) {
	if form, ok := form.(Bool); ok {
		return form.Val, true
	}
	return false, false
}

func GetChar(form Node) (rune, bool) {
	if form, ok := form.(Char); ok {
		return form.Val, true
	}
	return rune(0), false
}

func GetInt(form Node) (int64, bool) {
	if form, ok := form.(Int); ok {
		return form.Val, true
	}
	return 0, false
}

func GetFloat(form Node) (float64, bool) {
	if form, ok := form.(Float); ok {
		return form.Val, true
	}
	return 0.0, false
}

func GetString(form Node) (string, bool) {
	if form, ok := form.(String); ok {
		return form.Val, true
	}
	return "", false
}
