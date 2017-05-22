package ir

type InstrInputMode int

const (
	InstrTakeNothing InstrInputMode = iota
	InstrTake1
	InstrTake2
	InstrTake3
	InstrTakeN
	InstrTakeNplus1
)

type InstrOutputMode int

const (
	InstrPushNothing InstrOutputMode = iota
	InstrDupNth
	InstrPushTmp
	InstrPushConst
	InstrPushAndDiscard
)

type InstrEncoding int

const (
	InstrEnc0 InstrEncoding = iota
	InstrEnc1
)

// Instr is a single IR instruction.
type Instr struct {
	Name     []byte
	Data     uint16
	Input    InstrInputMode
	Encoding InstrEncoding
	Output   InstrOutputMode
}
