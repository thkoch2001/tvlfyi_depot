package main

import (
	"testing"
)

func TestChangeShouldBeSkipped(t *testing.T) {
	dontSkipAny := ""
	if changeShouldBeSkipped(dontSkipAny, "mysubject") {
		t.Fatal("dontSkipAny should not not be skip any")
	}

	showThese := "A,B"
	if changeShouldBeSkipped(showThese, "A") {
		t.Fatal("A should be shown")
	}
	if changeShouldBeSkipped(showThese, "B") {
		t.Fatal("B should be shown")
	}
	if !changeShouldBeSkipped(showThese, "C") {
		t.Fatal("C should not be shown")
	}

}
