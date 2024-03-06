package config

import (
	"fmt"
	"slices"
	"strings"

	"github.com/charmbracelet/log"
)

func Assert(test bool, format string, elems ...any) {
	if !test {
		log.Error(fmt.Sprintf(format, elems...))
	}
}

func Append(s []string, name string) []string {
	if slices.Contains(s, name) {
		log.Debug("config.Append", "duplicate", name)
		return s
	}
	return append(s, name)
}

func RemoveName(from string, s []string, name string) []string {
	for i, v := range s {
		if v == name {
			return append(s[:i], s[i+1:]...)
		}
	}
	return s
}

func RemoveNames(from string, s []string, names ...string) []string {
	log.Debug("config.RemoveNames", "from", from, "names", names)
	for _, name := range names {
		s = RemoveName(from, s, name)
	}
	return s
}

func CheckRemoval(from string, s []string, names ...string) {
	for _, name := range names {
		Assert(!slices.Contains(s, name),
			"failed removal: %s from %s", name, from)
	}
}

func AddNames(to string, s []string, names ...string) []string {
	log.Debug("config.AddNames", "to", to, "names", names)
	return append(s, names...)
}

func CheckAddition(to string, s []string, names ...string) {
	for _, name := range names {
		Assert(slices.Contains(s, name),
			"failed add: %s to %s", name, to)
	}
}

func ToVer(version string) string {
	return strings.Join(strings.Split(version, ".")[:2], ".")
}

func GetKeys(m map[string]bool) []string {
	keys := make([]string, len(m))
	i := 0
	for k := range m {
	    keys[i] = k
	    i++
	}
	return keys
}

func SliceToMap(s []string) map[string]bool {
	m := make(map[string]bool, len(s))
	for _, k := range s {
		m[k] = true
	}
	return m
}

