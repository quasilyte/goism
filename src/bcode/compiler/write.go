package compiler

import "bytes"

func writeOp(dst *bytes.Buffer, opcode byte) {
	dst.WriteByte(opcode)
}

func writeOp1(dst *bytes.Buffer, opcode byte, data uint16) {
	dst.Write([]byte{opcode, byte(data)})
}

func writeOp2(dst *bytes.Buffer, opcode byte, data uint16) {
	low, high := imm16Bytes(data)
	dst.Write([]byte{opcode, low, high})
}

// Write an operation that has three encodings
// that depend on the argument value (instruction data).
func writeTriOp(dst *bytes.Buffer, opcode byte, data uint16) {
	// Possible encodings:
	const (
		enc0 = 5      // Arg encoded inside opcode.
		enc1 = 0xFF   // Arg encoded as imm8.
		enc2 = 0xFFFF // Arg encoded as imm16.
	)

	if data <= enc0 {
		writeOp(dst, opcode+byte(data))
	} else if data <= enc1 {
		writeOp1(dst, opcode+6, data)
	} else {
		writeOp2(dst, opcode+7, data)
	}
}

func imm16Bytes(x uint16) (byte, byte) {
	low := x & 0x00FF
	high := (x & 0xFF00) >> 8
	return byte(low), byte(high)
}
