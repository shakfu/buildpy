/*
Copyright © 2024 NAME HERE <EMAIL ADDRESS>
*/
package models

import (
	"errors"
	"fmt"
	"github.com/shakfu/buildpy/internal/shell"
	// "log"
	"github.com/charmbracelet/log"
	"os"
	"path/filepath"
)

type Dependency struct {
	Name        string
	Version     string
	DownloadUrl string
	RepoUrl     string
	RepoBranch  string
	StaticLibs  []string
	Project     *Project
}

func (d *Dependency) Prefix() string {
	return filepath.Join(d.Project.Install, d.Name)
}

func (d *Dependency) SrcDir() string {
	return filepath.Join(d.Project.Src, d.Name)
}

func (d *Dependency) BuildDir() string {
	return filepath.Join(d.SrcDir(), "build")
}

func (d *Dependency) StaticLibsExist() bool {
	for _, path := range d.StaticLibs {
		var libpath = filepath.Join(d.Prefix(), "lib", path)
		if _, err := os.Stat(libpath); errors.Is(err, os.ErrNotExist) {
			return false
		}
	}
	return true
}

func InstallOpenssl() {
	ssl := Dependency{
		Name:        "openssl",
		Version:     "1.1.1w",
		DownloadUrl: "https://www.openssl.org/source/old/1.1.1/openssl-1.1.1w.tar.gz",
		RepoUrl:     "https://github.com/openssl/openssl.git",
		RepoBranch:  "OpenSSL_1_1_1w",
		StaticLibs:  []string{"libssl.a", "libcrypto.a"},
		Project:     NewProject(),
	}
	if !ssl.StaticLibsExist() {
		ssl.Project.Setup()
		prefixOpt := fmt.Sprintf("--prefix=%s", ssl.Prefix())
		shell.GitClone(ssl.RepoUrl, ssl.RepoBranch, ssl.Project.Src, false)
		shell.ShellCmd(ssl.SrcDir(), "./config", "no-shared", "no-tests", prefixOpt)
		shell.Make(ssl.SrcDir(), "install_sw")
		if !ssl.StaticLibsExist() {
			log.Fatal("could not build openssl")
		}
	}

}

func InstallBzip2() {
	bz2 := Dependency{
		Name:        "bzip2",
		Version:     "1.0.8",
		DownloadUrl: "https://sourceware.org/pub/bzip2/bzip2-1.0.8.tar.gz",
		RepoUrl:     "https://github.com/libarchive/bzip2.git",
		RepoBranch:  "bzip2-1.0.8",
		StaticLibs:  []string{"libbz2.a"},
		Project:     NewProject(),
	}
	if !bz2.StaticLibsExist() {
		bz2.Project.Setup()
		prefixOpt := fmt.Sprintf("PREFIX=%s", bz2.Prefix())
		shell.GitClone(bz2.RepoUrl, bz2.RepoBranch, bz2.Project.Src, false)
		shell.Make(bz2.SrcDir(), "install", prefixOpt, "CFLAGS='-fPIC'")
		if !bz2.StaticLibsExist() {
			log.Fatal("could not build bzip2")
		}
	}
}

func InstallXz() {
	xz := Dependency{
		Name:        "xz",
		Version:     "5.6.0",
		DownloadUrl: "https://github.com/tukaani-project/xz/releases/download/v5.6.0/xz-5.6.0.tar.gz",
		RepoUrl:     "https://github.com/tukaani-project/xz.git",
		RepoBranch:  "v5.6.0",
		StaticLibs:  []string{"liblzma.a"},
		Project:     NewProject(),
	}
	if !xz.StaticLibsExist() {
		xz.Project.Setup()
		shell.GitClone(xz.RepoUrl, xz.RepoBranch, xz.Project.Src, false)
		shell.CmakeConfigure(xz.SrcDir(), xz.BuildDir(),
			"-DBUILD_SHARED_LIBS=OFF", "-DENABLE_NLS=OFF", "-DENABLE_SMALL=ON",
			"-DCMAKE_BUILD_TYPE=MinSizeRel",
		)
		shell.CmakeBuild(xz.BuildDir(), true)
		shell.CmakeInstall(xz.BuildDir(), xz.Prefix())
		if !xz.StaticLibsExist() {
			log.Fatal("could not build lzma")
		}
	}
}
