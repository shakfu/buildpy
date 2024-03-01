/*
Copyright © 2024 NAME HERE <EMAIL ADDRESS>
*/
package models

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"sync"

	"github.com/charmbracelet/log"
	"github.com/shakfu/buildpy/internal/shell"
)

type Builder interface {
	Url() string
	Prefix() string
	SrcDir() string
	BuildDir() string
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
	Config         string
	DownloadUrl    string
	RepoUrl        string
	ConfigOptions  []string
	Packages       []string
	RemovePatterns []string
	Project        *Project
	Optimize       bool
}

func NewPythonBuilder(version string, config string) *PythonBuilder {
	return &PythonBuilder{
		"Python",
		version,
		config,
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
			"idlelib",
			"lib2to3",
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
		false,
	}
}

func (b *PythonBuilder) Url() string {
	return fmt.Sprintf(b.DownloadUrl, b.Version, b.Version)
}

func (b *PythonBuilder) BuildType() string {
	return strings.Split(b.Config, "-")[0]
}

func (b *PythonBuilder) SizeType() string {
	return strings.Split(b.Config, "-")[1]
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

func (b *PythonBuilder) BuildDir() string {
	return filepath.Join(b.SrcDir(), "build")
}

func (b *PythonBuilder) Ver() string {
	return strings.Join(strings.Split(b.Version, ".")[:2], ".")
}

func (b *PythonBuilder) VerMajor() string {
	return strings.Split(b.Version, ".")[0]
}
func (b *PythonBuilder) VerMinor() string {
	return strings.Split(b.Version, ".")[1]
}
func (b *PythonBuilder) VerPatch() string {
	return strings.Split(b.Version, ".")[2]
}

func (b *PythonBuilder) VerNoDot() string {
	return strings.ReplaceAll(b.Ver(), ".", "")
}

func (b *PythonBuilder) NameVersion() string {
	return fmt.Sprintf("%s-%s", b.Name, b.Version)
}

func (b *PythonBuilder) NameVer() string {
	return fmt.Sprintf("%s%s", strings.ToLower(b.Name), b.Ver())
}

func (b *PythonBuilder) PythonExe() string {
	return filepath.Join(b.Prefix(), "bin", "python3")
}

func (b *PythonBuilder) PipExe() string {
	return filepath.Join(b.Prefix(), "bin", "pip3")
}

// -----------------------------------------------------------------
// methods

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

func (b *PythonBuilder) PreProcess() {
}

func (b *PythonBuilder) Setup() {
	log.Info("installing python", "version", b.Version)
	b.Project.Setup()
	// shell.DownloadTo(b.Url(), b.Project.Downloads, b.Project.Src)
	shell.GitClone(b.RepoUrl, b.RepoBranch(), b.Project.Src, false)
}

func (b *PythonBuilder) Configure() {
	if b.BuildType() == "shared" {
		b.ConfigOptions = append(b.ConfigOptions,
			"--enable-shared", "--without-static-libpython")
	} else if b.BuildType() == "framework" {
		b.ConfigOptions = append(b.ConfigOptions,
			fmt.Sprintf("--enable-framework=%s", b.Prefix()))
	}
	if b.Optimize {
		b.ConfigOptions = append(b.ConfigOptions,
			"--enable-optimizations")
	}
	if len(b.Packages) == 0 {
		b.ConfigOptions = append(b.ConfigOptions,
			"--without-ensurepip")
		b.RemovePatterns = append(b.RemovePatterns, "ensurepip")
	}
	var prefix = fmt.Sprintf("--prefix=%s", b.Prefix())
	var args = []string{"./configure", prefix}
	args = append(args, b.ConfigOptions...)
	shell.ShellCmd(b.SrcDir(), args...)
}

func (b *PythonBuilder) Build() {
	shell.Make(b.SrcDir())
}

func (b *PythonBuilder) Install() {
	shell.Make(b.SrcDir(), "install")
}

func (b *PythonBuilder) Clean() {
	shell.RecursiveRemove(b.Prefix(), b.RemovePatterns)
}

func (b *PythonBuilder) ZipLib() {

	var src = filepath.Join(b.Prefix(), "lib", b.NameVer())

	shell.Move(
		filepath.Join(src, "lib-dynload"),
		filepath.Join(b.Project.Build, "lib-dynload"),
	)

	shell.Move(
		filepath.Join(src, "os.py"),
		filepath.Join(b.Project.Build, "os.py"),
	)

	var zippath = filepath.Join(
		b.Prefix(), "lib", fmt.Sprintf("python%s.zip", b.VerNoDot()))

	if err := shell.ZipFileOrFolder(zippath, src, filepath.Dir(src)); err != nil {
		log.Fatal(err)
	}

	os.RemoveAll(src)

	var site_packages = filepath.Join(src, "site-packages")

	os.RemoveAll(filepath.Join(b.Prefix(), "lib", "pkgconfig"))

	os.MkdirAll(src, 0750)
	os.MkdirAll(site_packages, 0750)
	shell.Move(
		filepath.Join(b.Project.Build, "lib-dynload"),
		filepath.Join(src, "lib-dynload"),
	)
	shell.Move(
		filepath.Join(b.Project.Build, "os.py"),
		filepath.Join(src, "os.py"),
	)
}

func (b *PythonBuilder) InstallPackages() {

	shell.Cmd(".", b.PythonExe(), "-m", "ensurepip")
	var args = []string{"install"}
	args = append(args, b.Packages...)
	shell.Cmd(".", b.PipExe(), args...)
}

func (b *PythonBuilder) PostProcess() {
}

func (b *PythonBuilder) Process() {
	b.InstallDeps()
	b.PreProcess()
	b.Setup()
	b.Configure()
	b.Build()
	b.Install()
	b.Clean()
	b.ZipLib()
	b.PostProcess()
}

func (b *PythonBuilder) SetConfigOptions(opts []string) {
	if len(opts) > 0 {
		b.ConfigOptions = opts
	}
}

func (b *PythonBuilder) ListConfigOptions() {
	for _, opt := range b.ConfigOptions {
		fmt.Println(opt)
	}
}

func (b *PythonBuilder) ListRemovePatterns() {
	for _, pat := range b.RemovePatterns {
		fmt.Println(pat)
	}
}

func (b *PythonBuilder) SetRemovePatterns(patterns []string) {
	if len(patterns) > 0 {
		b.RemovePatterns = patterns
	}
}

func (b *PythonBuilder) SetPackages(pkgs []string) {
	if len(pkgs) > 0 {
		b.Packages = pkgs
	}
}
