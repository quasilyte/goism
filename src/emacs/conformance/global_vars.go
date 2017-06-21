package conformance

const (
	c0 = iota
	c1
	c2
)

var (
	var4       = var2 // To check init order
	var1       = 1
	var2, var3 = 2, 3
	var5       = c1 + c2 + c2
	var6       = c1 + var5
)
