/*
Copyright © 2024 NAME HERE <EMAIL ADDRESS>
*/
package builder

import (
	"fmt"
	"log"
	"os"
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

type Dependency struct {
	Name        string
	Version     string
	DownloadUrl string
	RepoUrl     string
	RepoBranch  string
}

type Project struct {
	Cwd       string
	Build     string
	Downloads string
	Src       string
	Install   string
}

func NewProject() *Project {
	cwd, err := os.Getwd()
	if err != nil {
		log.Println(err)
	}
	var build = filepath.Join(cwd, "build")
	var downloads = filepath.Join(build, "downloads")
	var src = filepath.Join(build, "src")
	var install = filepath.Join(build, "install")
	return &Project{cwd, build, downloads, src, install}
}

func (p *Project) Setup() {
	shell.Makedirs(p.Build, p.Downloads, p.Src, p.Install)
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

func (b *PythonBuilder) InstallOpenssl() {
	ssl := Dependency{
		Name:        "openssl",
		Version:     "1.1.1w",
		DownloadUrl: "https://www.openssl.org/source/old/1.1.1/openssl-1.1.1w.tar.gz",
		RepoUrl:     "https://github.com/openssl/openssl.git",
		RepoBranch:  "OpenSSL_1_1_1w",
	}

	// shell.DownloadTo(ssl.DownloadUrl, "./downloads")
	shell.GitClone(ssl.RepoUrl, ssl.RepoBranch, "./repos", false)
	shell.ShellCmd("./repos/openssl", "./config", "no-shared", "no-tests",
		"--prefix=/home/sa/projects/pybuild/go/buildpy/install")
	shell.Make("./repos/openssl", "install_sw")
}

func (b *PythonBuilder) InstallBzip2() {
	bz2 := Dependency{
		Name:        "bzip2",
		Version:     "1.0.8",
		DownloadUrl: "https://sourceware.org/pub/bzip2/bzip2-1.0.8.tar.gz",
		RepoUrl:     "https://github.com/libarchive/bzip2.git",
		RepoBranch:  "bzip2-1.0.8",
	}
	// shell.DownloadTo(bz2.DownloadUrl, "./downloads")
	shell.GitClone(bz2.RepoUrl, bz2.RepoBranch, "./repos", false)
	shell.Make("./repos/bzip2", "install",
		"PREFIX=/home/sa/projects/pybuild/go/buildpy/install",
		"CFLAGS='-fPIC'",
	)
}

func (b *PythonBuilder) InstallLzma() {
	xz := Dependency{
		Name:        "xz",
		Version:     "5.6.0",
		DownloadUrl: "https://github.com/tukaani-project/xz/releases/download/v5.6.0/xz-5.6.0.tar.gz",
		RepoUrl:     "https://github.com/tukaani-project/xz.git",
		RepoBranch:  "v5.6.0",
	}
	// shell.DownloadTo(xz.DownloadUrl, "./downloads")
	shell.GitClone(xz.RepoUrl, xz.RepoBranch, "./repos", false)
	shell.ShellCmd("./repos/xz", "./configure", "--disable-shared", "--enable-static",
		"PREFIX=/home/sa/projects/pybuild/go/buildpy/install",
	)
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
	// builder.DumpConfigOptions()
	builder.InstallOpenssl()
	// builder.InstallBzip2()
	// builder.InstallLzma()
}
