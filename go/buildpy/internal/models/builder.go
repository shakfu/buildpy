/*
Copyright © 2024 NAME HERE <EMAIL ADDRESS>
*/
package models

import (
	"fmt"
	"sync"

	"github.com/charmbracelet/log"

	// "os"
	"path/filepath"

	"github.com/shakfu/buildpy/internal/shell"
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
	DownloadUrl    string
	RepoUrl        string
	ConfigOptions  []string
	Packages       []string
	RemovePatterns []string
	Project        *Project
}

func NewPythonBuilder(version string) *PythonBuilder {
	return &PythonBuilder{
		"Python",
		version,
		"https://www.python.org/ftp/python/%s/Python-%s.tar.xz",
		"https://github.com/python/cpython.git",
		[]string{
			"--enable-shared",
			"--disable-test-modules",
			"--without-ensurepip",
			"--without-static-libpython",
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
		NewProject(),
	}
}

func (b *PythonBuilder) Url() string {
	return fmt.Sprintf(b.DownloadUrl, b.Version, b.Version)
}

func (b *PythonBuilder) RepoBranch() string {
	return fmt.Sprintf("v%s", b.Version)
}

func (b *PythonBuilder) Prefix() string {
	return filepath.Join(b.Project.Install, "python")
}

// func (b *PythonBuilder) SrcDir() string {
// 	var name = filepath.Base(b.Url())
// 	var stem = name[0:len(name)-len(".tar.xz")]
// 	return filepath.Join(b.Project.Src, stem)
// }

func (b *PythonBuilder) SrcDir() string {
	return filepath.Join(b.Project.Src, "cpython")
}

func (b *PythonBuilder) InstallPython() {
	log.Info("installing python", "version", b.Version)
	b.Project.Setup()
	// shell.DownloadTo(b.Url(), b.Project.Downloads, b.Project.Src)
	shell.GitClone(b.RepoUrl, b.RepoBranch(), b.Project.Src, false)
	var prefix = fmt.Sprintf("--prefix=%s", b.Prefix())
	var args = []string{"./configure", prefix}
	args = append(args, b.ConfigOptions...)
	shell.ShellCmd(b.SrcDir(), args...)
	shell.Make(b.SrcDir(), "install")
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

func (b *PythonBuilder) CleanPython() {
	shell.RecursiveRemove(b.Prefix())
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

func (b *PythonBuilder) Process() {
	b.InstallDeps()
	b.InstallPython()
	b.CleanPython()
}
