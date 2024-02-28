/*
Copyright © 2024 NAME HERE <EMAIL ADDRESS>
*/
package builder

import (
	"fmt"
)

type Builder interface {
	PreProcess()
	Setup()
	Configure()
	Build()
	Install()
	Clean()
	PostProcess()
	Process()
}

type PythonBuilder struct {
	Name           string
	Version        string
	UrlTemplate    string
	ConfigOptions  []string
	Packages       []string
	RemovePatterns []string
	DependsOn      []Builder
}

func NewPythonBuilder(version string) *PythonBuilder {
	return &PythonBuilder{
		"Python",
		version,
		"https://www.python.org/ftp/python/{ver}/Python-{ver}.tar.xz",
		[]string{
			"--disable-test-modules",
			"--without-ensurepip",
		},
		[]string{},
		[]string{
			"*.exe",
			"*config-3*",
			"*tcl*",
			"*tdbc*",
			"*tk*",
			"__phello__",
			"__pycache__",
			"_codecs_*.so",
			"_test*",
			"_tk*",
			"_xx*.so",
			"distutils",
			"ensurepip",
			"idlelib",
			"lib2to3",
			"libpython*",
			"LICENSE.txt",
			"pkgconfig",
			"pydoc_data",
			"site-packages",
			"test",
			"Tk*",
			"turtle*",
			"venv",
			"xx*.so",
		},
		[]Builder{},
	}
}

func (b *PythonBuilder) DumpConfigOptions() {
	for _, opt := range b.ConfigOptions {
		fmt.Println(opt)
	}
}

func (b *PythonBuilder) DumpRemovePatterns() {
	for _, pat := range b.RemovePatterns {
		fmt.Println(pat)
	}
}

func Demo() {
	builder := NewPythonBuilder("3.11.7")
	builder.DumpConfigOptions()
}
