package sexp

type SpanKind int

const (
	SpanLowOnly SpanKind = iota
	SpanHighOnly
	SpanBoth
	SpanWhole
)

type Span struct {
	Low  Form // [!] Can be nil
	High Form // [!] Can be nil
}

func (span *Span) Kind() SpanKind {
	if span.Low == nil {
		if span.High == nil {
			return SpanWhole
		}
		return SpanHighOnly
	}
	if span.High == nil {
		return SpanLowOnly
	}
	return SpanBoth
}
