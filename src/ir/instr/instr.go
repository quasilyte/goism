package instr

type inputMode int

const (
	AttrTakeNothing inputMode = iota
	AttrTake1
	AttrTake2
	AttrTake3
	AttrTakeN
	AttrTakeNplus1
)

type outputMode int

const (
	AttrPushNothing outputMode = iota
	AttrDupNth
	AttrPushTmp
	AttrPushConst
	AttrPushAndDiscard
)

type encoding int

const (
	AttrEnc0 encoding = iota
	AttrEnc1
)

// Instr is a single IR instruction.
type Instr struct {
	Name     []byte
	Encoding encoding
	Input    inputMode
	Output   outputMode
	Data     uint16
}
