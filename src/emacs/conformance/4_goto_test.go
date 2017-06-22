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
