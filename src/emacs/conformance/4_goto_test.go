package conformance

func testGoto(n int) int {
	x := n
	goto exit
	x++
exit:
	return x
}

func testGotoOutBlock(n int) int {
	x := n
	{
		goto exit
	}
	x++
exit:
	return x
}

func testGotoTwice(n int) int {
	x := n
	goto label1
	x++
label1:
	goto label2
	x++
label2:
	return x
}

func testGotoChain(n int) int {
	x := n
	goto label1
label1:
	goto label2
label2:
	return x
}

func testGotoBack(n int) int {
	x := 0
	jumped := false
	goto label2
label1:
	x = n
label2:
	if !jumped {
		jumped = true
		goto label1
	}
	return x
}

func testGotoScopes1(n int) int {
	x := n
	{
		y := x
		{
			z := y
			x = z
			goto exit
		}
	}
exit:
	return x
}

func testGotoScopes2(n int) int {
	var x1, x2 int
	{
		y := n
		{
			x1 = y
			{
				z := n
				y = z
				goto label1
			}
		label1:
			x2 = y
			goto exit
		}
	}
exit:
	return x1 + x2 - n
}

func testGotoScopes3(n int) int {
	var a, b int
	x := n
	{
		y := 1
		{
			z := 2
			b = z
			goto label1
		}
	label1:
		a = y
		goto exit
	}
exit:
	if a == 1 && b == 2 && x == n {
		return n
	}
	return -1
}
