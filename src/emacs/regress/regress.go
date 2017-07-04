package regress

// #REFS: 78.
func selfAssign1(n int) int {
	x := n
	x = x
	return x
}

// #REFS: 78.
func selfAssign2(n int) int {
	{
		x := n
		{
			x = x
		}
		x = x
		return x
	}
}
