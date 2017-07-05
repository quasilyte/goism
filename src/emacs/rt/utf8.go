package rt

// Return encoded char CH size.
// Returned values are in range of [1,4].
func utf8CharWidth(ch rune) int {
	switch {
	case ch <= 127:
		return 1
	case ch <= 2048:
		return 2
	case ch <= 65536:
		return 3
	default:
		return 4
	}
}

// Returns integer (rune) value of n-th byte of
// utf-8 encoded char.
// Size should be equal to Utf8CharWidth(ch).
func utf8DecodeByte(ch rune, n, size int) rune {
	switch size {
	default: // Size of 1 implied
		return ch

	case 2:
		if n == 0 {
			return 0xC0 | ch>>6
		}
		return 0x80 | 0x3F&ch

	case 3:
		switch n {
		case 0:
			return 0xE0 | ch>>12
		case 1:
			return 0x80 | 0x3F&(ch>>6)
		default:
			return 0x80 | 0x3F&ch
		}

	case 4:
		switch n {
		case 0:
			return 0xF0 | ch>>18
		case 1:
			return 0x80 | 0x3F&(ch>>12)
		case 2:
			return 0x80 | 0x3F&(ch>>6)
		default:
			return 0x80 | 0x3F&ch
		}
	}
}
