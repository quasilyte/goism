package conformance

func stringGet(s string, index int) byte {
	return s[index]
}

func stringLen(s string) int {
	return len(s)
}

func substring(s string, low, high int) string {
	switch {
	case low >= 0 && high >= 0:
		return s[low:high]
	case low >= 0:
		return s[low:]
	case high >= 0:
		return s[:high]
	default:
		return s
	}
}
