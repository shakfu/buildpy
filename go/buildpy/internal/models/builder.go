/*
Copyright © 2024 NAME HERE <EMAIL ADDRESS>
*/
package models

import (
	"fmt"
	"github.com/charmbracelet/log"
	"sync"
	// "os"
	// "path/filepath"
	// "github.com/shakfu/buildpy/internal/shell"
)

type Builder interface {
	Url() string
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
}

func NewPythonBuilder(version string) *PythonBuilder {
	return &PythonBuilder{
		"Python",
		version,
		"https://www.python.org/ftp/python/%s/Python-%s.tar.xz",
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
	}
}

func (b *PythonBuilder) InstallPython() {
	log.Info("installing python...")
}

func (b *PythonBuilder) InstallDeps() {
	var wg sync.WaitGroup

	wg.Add(3)

	go InstallOpenssl(&wg)
	go InstallBzip2(&wg)
	go InstallXz(&wg)

	log.Info("waiting for goroutines to finish...")
	wg.Wait()
	log.Info("DONE")
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
