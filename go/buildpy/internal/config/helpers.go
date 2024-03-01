package config

import ()

func RemoveName(s []string, name string) []string {
	for i, v := range s {
		if v == name {
			return append(s[:i], s[i+1:]...)
		}
	}
	return s
}

func RemoveNames(s []string, names ...string) []string {
	for _, name := range names {
		s = RemoveName(s, name)
	}
	return s
}

func AddNames(s []string, names ...string) []string {
	return append(s, names...)
}
