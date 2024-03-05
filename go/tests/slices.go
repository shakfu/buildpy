package xslices

// bunch of string slice functions pulled from various sources
// mostly supplanted by generic slices module in standard lib

import (
	"sort"
)

// Package slices defines various functions useful with slices of string type.
// The goal is to be as close as possible to
// https://github.com/golang/go/issues/45955. Ideal would be if we can just
// replace "stringslices" if the "slices" package becomes standard.
// package slice

// Equal reports whether two slices are equal: the same length and all
// elements equal. If the lengths are different, Equal returns false.
// Otherwise, the elements are compared in index order, and the
// comparison stops at the first unequal pair.
func Equal(s1, s2 []string) bool {
	if len(s1) != len(s2) {
		return false
	}
	for i, n := range s1 {
		if n != s2[i] {
			return false
		}
	}
	return true
}

// Filter appends to d each element e of s for which keep(e) returns true.
// It returns the modified d. d may be s[:0], in which case the kept
// elements will be stored in the same slice.
// if the slices overlap in some other way, the results are unspecified.
// To create a new slice with the filtered results, pass nil for d.
func Filter(d, s []string, keep func(string) bool) []string {
	for _, n := range s {
		if keep(n) {
			d = append(d, n)
		}
	}
	return d
}

// Contains reports whether v is present in s.
func Contains(s []string, v string) bool {
	return Index(s, v) >= 0
}

// Index returns the index of the first occurrence of v in s, or -1 if
// not present.
func Index(s []string, v string) int {
	// "Contains" may be replaced with "Index(s, v) >= 0":
	// https://github.com/golang/go/issues/45955#issuecomment-873377947
	for i, n := range s {
		if n == v {
			return i
		}
	}
	return -1
}

// Functions below are not in https://github.com/golang/go/issues/45955

// Clone returns a new clone of s.
func Clone(s []string) []string {
	// https://github.com/go101/go101/wiki/There-is-not-a-perfect-way-to-clone-slices-in-Go
	if s == nil {
		return nil
	}
	c := make([]string, len(s))
	copy(c, s)
	return c
}

// SortStrings sorts the specified string slice in place. It returns the same
// slice that was provided in order to facilitate method chaining.
func SortStrings(s []string) []string {
	sort.Strings(s)
	return s
}

// ContainsString checks if a given slice of strings contains the provided string.
// If a modifier func is provided, it is called with the slice item before the comparation.
func ContainsString(slice []string, s string, modifier func(s string) string) bool {
	for _, item := range slice {
		if item == s {
			return true
		}
		if modifier != nil && modifier(item) == s {
			return true
		}
	}
	return false
}

// RemoveString returns a newly created []string that contains all items from slice that
// are not equal to s and modifier(s) in case modifier func is provided.
func RemoveString(slice []string, s string, modifier func(s string) string) []string {
	newSlice := make([]string, 0)
	for _, item := range slice {
		if item == s {
			continue
		}
		if modifier != nil && modifier(item) == s {
			continue
		}
		newSlice = append(newSlice, item)
	}
	if len(newSlice) == 0 {
		// Sanitize for unit tests so we don't need to distinguish empty array
		// and nil.
		newSlice = nil
	}
	return newSlice
}

func ContainsString2(slice []string, item string) bool {
	for _, j := range slice {
		if j == item {
			return true
		}
	}
	return false
}

func StringsEqual(left, right []string) bool {
	if len(left) != len(right) {
		return false
	}
	for i := 0; i < len(left); i++ {
		if left[i] != right[i] {
			return false
		}
	}
	return true
}
