/*
Copyright © 2024 NAME HERE <EMAIL ADDRESS>
*/
package builder

import (
	// "os"
	"fmt"
	// "github.com/imroc/req/v3"
	"github.com/shakfu/buildpy/internal/shell"
	"log"
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
		"https://www.python.org/ftp/python/{}/Python-{}.tar.xz",
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
	const url string = "https://www.openssl.org/source/old/1.1.1/openssl-1.1.1w.tar.gz"
	const target string = "./downloads/openssl-1.1.1w.tar.gz"
	err := shell.DownloadFile(target, url)
	if err != nil {
		log.Fatal(err)
	} else {
		log.Printf("downloaded: %s", url)		
	}
    // r, err := os.Open(target)
    // if err != nil {
    //     fmt.Println("error")
    // }
    // shell.ExtractTarGz(r, "./downloads")
}

func (b *PythonBuilder) InstallBzip2() {
	const url string = "https://sourceware.org/pub/bzip2/bzip2-1.0.8.tar.gz"
	const target string = "./downloads/bzip2-1.0.8.tar.gz"
	err := shell.DownloadFile(target, url)
	if err != nil {
		log.Fatal(err)
	} else {
		log.Printf("downloaded: %s", url)
	}
    // r, err := os.Open(target)
    // if err != nil {
    //     fmt.Println("error")
    // }
    // shell.ExtractTarGz(r, "./downloads")
}

func (b *PythonBuilder) InstallLzma() {
	const url string = "http://tukaani.org/xz/xz-5.2.5.tar.gz"
	const target string = "./downloads/xz-5.2.5.tar.gz"
	err := shell.DownloadFile(target, url)
	if err != nil {
		log.Fatal(err)
	} else {
		log.Printf("downloaded: %s", url)		
	}
    // r, err := os.Open(target)
    // if err != nil {
    //     fmt.Println("error")
    // }
    // shell.ExtractTarGz(r, "./downloads")
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
	builder.InstallOpenssl()
	builder.InstallBzip2()
	builder.InstallLzma()
}
