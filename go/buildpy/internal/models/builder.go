package models

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"sync"

	"github.com/charmbracelet/log"
	"github.com/shakfu/buildpy/internal/shell"
	"github.com/shakfu/buildpy/internal/config"
)

var PLATFORM = shell.GetPlatform()
var ARCH = shell.GetArch()

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
	UseGit 		   bool
	Jobs 		   int

}

func NewPythonBuilder(version string, config string) *PythonBuilder {
	return &PythonBuilder{
		Name: "Python",
		Version: version,
		Config: config,
		DownloadUrl: "https://www.python.org/ftp/python/%s/Python-%s.tar.xz",
		RepoUrl: "https://github.com/python/cpython.git",
		ConfigOptions: []string{"--disable-test-modules"},
		Packages: []string{},
		RemovePatterns: []string{
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
		Project: NewProject(),
		Optimize: false,
		UseGit: false,
		Jobs: 1,
	}
}

// -----------------------------------------------------------------
// get properties

func (b *PythonBuilder) Url() string {
	return fmt.Sprintf(b.DownloadUrl, b.Version, b.Version)
}

func (b *PythonBuilder) PlatformArch() string {
	return fmt.Sprintf("%s/%s", PLATFORM, ARCH)
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

func (b *PythonBuilder) SrcDir() string {
	return filepath.Join(b.Project.Src, "python")
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

func (b *PythonBuilder) DylibName() string {
	var name = fmt.Sprintf("lib%s", b.NameVer())
	if PLATFORM == "darwin" {
		return fmt.Sprintf("%s.dylib", name)
	} else if PLATFORM == "linux" {
		return fmt.Sprintf("%s.so", name)
	} else if PLATFORM == "windows" {
		return fmt.Sprintf("%s.dll", name)
	} else {
		log.Fatal("platform not supported")
		return ""
	}
}

func (b *PythonBuilder) PythonExe() string {
	return filepath.Join(b.Prefix(), "bin", "python3")
}

func (b *PythonBuilder) PipExe() string {
	return filepath.Join(b.Prefix(), "bin", "pip3")
}

// -----------------------------------------------------------------
// methods

func (b *PythonBuilder) CheckDeps() {
	log.Info("PythonBuilder.CheckDeps")
	// core dependencies
	shell.CheckDeps("git", "zip", "cmake", "make", "bash")
}

func (b *PythonBuilder) InstallDeps() {
	var wg sync.WaitGroup

	wg.Add(3)

	go InstallOpenssl(&wg)
	go InstallBzip2(&wg)
	go InstallXz(&wg)

	log.Info("PythonBuilder.InstallDeps", "msg", "building openssl, bzip2, lzma deps")
	wg.Wait()
	log.Info("PythonBuilder.InstallDeps", "msg", "waiting for goroutines to finish...")

}

func (b *PythonBuilder) PreProcess() {
	log.Info("PythonBuilder.PreProcess")
}

func (b *PythonBuilder) Setup() {
	log.Info("PythonBuilder.Setup", "pyver", b.Version)
	b.Project.Setup()
	if b.UseGit {
		shell.GitClone(b.RepoUrl, b.RepoBranch(), b.SrcDir(), false)
	} else {
		shell.DownloadTo(b.Url(), b.Project.Downloads, b.Project.Src)
	}
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
	log.Info("PythonBuilder.Configure", "pyver", b.Version, "opts", args)
	config.ConfigWrite(b.Version, b.Config,
		filepath.Join(b.SrcDir(), "Modules", "Setup.local"))
	// var cfg = config.GetConfig(b.Version, b.Config)
    // cfg.Write(filepath.Join(b.SrcDir(), "Modules", "Setup.local"))
	shell.ShellCmd(b.SrcDir(), args...)
}

func (b *PythonBuilder) Build() {
	log.Info("PythonBuilder.Build")
	shell.Make(b.SrcDir(), "-j", fmt.Sprintf("%d", b.Jobs))
}

func (b *PythonBuilder) Install() {
	log.Info("PythonBuilder.Install")
	shell.Make(b.SrcDir(), "install")
}

func (b *PythonBuilder) Clean() {
	log.Info("PythonBuilder.Clean")
	shell.RecursiveRemove(b.Prefix(), b.RemovePatterns)
}

func (b *PythonBuilder) ZipLib() {
	log.Info("PythonBuilder.ZipLib")

	var tmp_libdynload = filepath.Join(b.Project.Build, "lib-dynload")
	var tmp_os_py = filepath.Join(b.Project.Build, "os.py")

	// pre-cleanup
	os.RemoveAll(tmp_libdynload)
	os.RemoveAll(tmp_os_py)

	var src = filepath.Join(b.Prefix(), "lib", b.NameVer())

	shell.Move(
		filepath.Join(src, "lib-dynload"),
		tmp_libdynload,
	)

	shell.Move(
		filepath.Join(src, "os.py"),
		tmp_os_py,
	)

	var zippath = filepath.Join(
		b.Prefix(), "lib", fmt.Sprintf("python%s.zip", b.VerNoDot()))

	shell.ZipLib(zippath, src)

	os.RemoveAll(src)

	var site_packages = filepath.Join(src, "site-packages")

	os.RemoveAll(filepath.Join(b.Prefix(), "lib", "pkgconfig"))

	os.MkdirAll(src, 0750)
	os.MkdirAll(site_packages, 0750)
	shell.Move(
		tmp_libdynload,
		filepath.Join(src, "lib-dynload"),
	)
	shell.Move(
		tmp_os_py,
		filepath.Join(src, "os.py"),
	)
}

func (b *PythonBuilder) InstallPackages() {
	log.Info("PythonBuilder.InstallPackages", "pkgs", b.Packages)
	shell.Cmd(".", b.PythonExe(), "-m", "ensurepip")
	var args = []string{"install"}
	args = append(args, b.Packages...)
	shell.Cmd(".", b.PipExe(), args...)
}

func (b *PythonBuilder) MakeRelocatable() {
	log.Info("PythonBuilder.PostProcess")
	if PLATFORM == "darwin" {
		if b.BuildType() == "shared" {
			var dylib = filepath.Join(b.Prefix(), "lib", b.DylibName())
			var rpath = fmt.Sprintf("@rpath", b.DylibName())
			var to = fmt.Sprintf("@executable_path/../lib/%s", b.DylibName())
			var exe = filepath.Join(b.Prefix(), "bin", b.NameVer())
			os.Chmod(dylib, 750)
			shell.Cmd(".", "install_name_tool)", "-id", rpath, dylib)
			shell.Cmd(".", "install_name_tool", "-change", dylib, to, exe)
		} else if b.BuildType() == "framework" {
			var dylib = filepath.Join(b.Prefix(), b.Name)
			var rpath = fmt.Sprintf("@rpath", b.Name)
			var to = fmt.Sprintf("@executable_path/../%s", b.Name)
			var exe = filepath.Join(b.Prefix(), "bin", b.NameVer())
			os.Chmod(dylib, 750)
			shell.Cmd(".", "install_name_tool)", "-id", rpath, dylib)
			shell.Cmd(".", "install_name_tool", "-change", dylib, to, exe)
		}
	} else if PLATFORM == "linux" {
		if b.BuildType() == "shared" {
			var exe = filepath.Join(b.Prefix(), "bin", b.NameVer())
			shell.Cmd(".", "patchelf", "--set-rpath", "'$ORIGIN'/../lib", exe)
		}
	}
}

func (b *PythonBuilder) PostProcess() {
	log.Info("PythonBuilder.PostProcess")
	if b.BuildType() == "shared" || b.BuildType() == "framework" {
		b.MakeRelocatable()
	}
}

func (b *PythonBuilder) Process() {
	log.Info("PythonBuilder.Process", "ver", b.Version, "cfg", b.Config)
	b.CheckDeps()
	b.InstallDeps()
	b.PreProcess()
	b.Setup()
	b.Configure()
	b.Build()
	b.Install()
	b.Clean()
	b.ZipLib()
	b.PostProcess()
	log.Info("PythonBuilder.Process", "status", "DONE")
}

func (b *PythonBuilder) SetConfigOptions(opts []string) {
	log.Info("PythonBuilder.SetConfigOptions", "opts", opts)

	if len(opts) > 0 {
		b.ConfigOptions = opts
	}
}

func (b *PythonBuilder) ListConfigOptions() {
	log.Info("PythonBuilder.ListConfigOptions")
	for _, opt := range b.ConfigOptions {
		fmt.Println(opt)
	}
}

func (b *PythonBuilder) ListRemovePatterns() {
	log.Info("PythonBuilder.ListRemovePatterns")
	for _, pat := range b.RemovePatterns {
		fmt.Println(pat)
	}
}

func (b *PythonBuilder) SetRemovePatterns(patterns []string) {
	log.Info("PythonBuilder.SetRemovePatterns", "patterns", patterns)
	if len(patterns) > 0 {
		b.RemovePatterns = patterns
	}
}

func (b *PythonBuilder) SetPackages(pkgs []string) {
	log.Info("PythonBuilder.SetPackages", "pkgs", pkgs)
	if len(pkgs) > 0 {
		b.Packages = pkgs
	}
}
