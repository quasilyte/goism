package dt_test

import (
	"bytes"
	"dt"
	"reflect"
	"testing"
)

func TestConstPoolInsertDuplicates(t *testing.T) {
	cvec := dt.ConstPool{}
	if cvec.InsertInt(1) != cvec.InsertInt(1) {
		t.Error("Duplicates when inserting ints")
	}
	if cvec.InsertFloat(1.0) != cvec.InsertFloat(1.0) {
		t.Error("Duplicates when inserting floats")
	}
	if cvec.InsertString("1") != cvec.InsertString("1") {
		t.Error("Duplicates when inserting strings")
	}
	if cvec.InsertSym("nil") != cvec.InsertSym("nil") {
		t.Error("Duplicates when inserting symbols")
	}
}

func TestConstPoolIndexes(t *testing.T) {
	cvec := dt.ConstPool{}
	indexes := []int{
		cvec.InsertInt(1),
		cvec.InsertFloat(1.0),
		cvec.InsertString("1"),
		cvec.InsertSym("nil"),
	}
	if !reflect.DeepEqual(indexes, []int{0, 1, 2, 3}) {
		t.Errorf("Indexes are not consecutive: %v", indexes)
	}
}

func TestConstPoolGet(t *testing.T) {
	cvec := dt.ConstPool{}
	index := cvec.InsertInt(10)
	if cvec.GetInt(uint16(index)) != 10 {
		t.Error("Lookup failed")
	}
}

func TestConstPoolBytes(t *testing.T) {
	cvec := dt.ConstPool{}
	cvec.InsertInt(1)
	cvec.InsertFloat(1.5)
	cvec.InsertString("nil")
	cvec.InsertSym("nil")

	result := cvec.Bytes()
	expected := []byte(`[1 1.5 "nil" nil ]`)
	if !bytes.Equal(result, expected) {
		t.Errorf("%s != %s", string(result), string(expected))
	}

	cvec.Clear()

	result = cvec.Bytes()
	expected = []byte(`[]`)
	if !bytes.Equal(result, expected) {
		t.Errorf("%s != %s", string(result), string(expected))
	}
}
