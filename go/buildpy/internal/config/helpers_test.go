package config

import (
	"slices"
	"testing"
)

func TestAppend(t *testing.T) {
	xs := []string{"a", "b", "joe"}
	x := "sam"
	want := []string{"a", "b", "joe", "sam"}
	result := Append(xs, x)
	if slices.Compare(want, result) != 0 {
		t.Errorf("got: %s, want: %s", result, want)
	}
}

func TestRemoveName(t *testing.T) {
	xs := []string{"a", "b", "joe"}
	x := "joe"
	want := []string{"a", "b"}
	result := RemoveName("", xs, x)
	if slices.Compare(want, result) != 0 {
		t.Errorf("got: %s, want: %s", result, want)
	}
}

func TestRemoveNames(t *testing.T) {
	xs := []string{"a", "b", "joe"}
	x := "joe"
	y := "b"
	want := []string{"a"}
	result := RemoveNames("", xs, x, y)
	if slices.Compare(want, result) != 0 {
		t.Errorf("got: %s, want: %s", result, want)
	}
}

func TestAddNames(t *testing.T) {
	xs := []string{"a", "b", "joe"}
	x := "sam"
	y := "bo"
	want := []string{"a", "b", "joe", "sam", "bo"}
	result := AddNames("", xs, x, y)
	if slices.Compare(want, result) != 0 {
		t.Errorf("got: %s, want: %s", result, want)
	}
}

func TestToVer(t *testing.T) {
	x := "3.12.2"
	want := "3.12"
	result := ToVer(x)
	if result != want {
		t.Errorf("got: %s, want: %s", result, want)
	}
}
