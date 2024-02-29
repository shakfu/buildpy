package shell

import (
	// "log"
	"github.com/charmbracelet/log"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

func filepath_stem(fileName string) string {
	return strings.TrimSuffix(fileName, filepath.Ext(fileName))
}

func Makedirs(paths ...string) {
	for _, path := range paths {
		err := os.Mkdir(path, 0750)
		if err != nil && !os.IsExist(err) {
			log.Fatal(err)
		}
	}
}

func Make(cwd string, args ...string) {
	cmd := exec.Command("make", args...)
	cmd.Dir = cwd
	log.Info("make", "args", args)
	if err := cmd.Run(); err != nil {
		log.Fatal(err)
	}
	log.Info("make DONE")
}

func DownloadTo(url string, downDir string, extractDir string) {
	download := exec.Command("wget", "-P", downDir, url)
	if err := download.Run(); err != nil {
		log.Fatal(err)
	}
	log.Info("downloaded", url)
	var name = filepath.Base(url)
	extract := exec.Command("tar", "xvf", "-C", extractDir, name)
	extract.Dir = extractDir
	if err := extract.Run(); err != nil {
		log.Fatal(err)
	}
	log.Info("extracted:", name)
}

func GitClone(url string, branch string, directory string, recurse bool) {
	var target = filepath.Join(directory, filepath_stem(filepath.Base(url)))
	var args = []string{"clone", "--depth=1"}
	if recurse {
		args = append(args, "--recurse-submodules", "--shallow-submodules")
	}
	args = append(args, "--branch", branch, url, target)
	clone := exec.Command("git", args...)
	log.Info("git", "args", args)
	if err := clone.Run(); err != nil {
		log.Fatal(err)
	}
	log.Info("git clone DONE")
}

func ShellCmd(cwd string, args ...string) {
	scmd := exec.Command("bash", args...)
	scmd.Dir = cwd
	log.Info("bash", "args", args)
	if err := scmd.Run(); err != nil {
		log.Fatal(err)
	}
	log.Info("bash DONE")
}

func CmakeConfigure(srcdir string, builddir string, options ...string) {
	var args = []string{"-S", srcdir, "-B", builddir}
	args = append(args, options...)
	log.Info("cmake", "args", args)
	cmake := exec.Command("cmake", args...)
	if err := cmake.Run(); err != nil {
		log.Fatal(err)
	}
	log.Info("cmake configure: DONE")
}

func CmakeBuild(builddir string, release bool) {
	var args = []string{"--build", builddir}
	if release {
		args = append(args, "--config", "Release")
	}
	log.Info("cmake", "args", args)
	cmake := exec.Command("cmake", args...)
	if err := cmake.Run(); err != nil {
		log.Fatal(err)
	}
	log.Info("cmake build: DONE")
}

func CmakeInstall(builddir string, prefix string) {
	var args = []string{"--install", builddir, "--prefix", prefix}
	log.Info("cmake", "args", args)
	cmake := exec.Command("cmake", args...)
	if err := cmake.Run(); err != nil {
		log.Fatal(err)
	}
	log.Info("cmake install: DONE")
}
