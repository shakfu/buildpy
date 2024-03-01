package shell

import (
	"bytes"
	"io/fs"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"

	"github.com/charmbracelet/log"
)

func GetOs() string {
	return runtime.GOOS
}

func GetArch() string {
	return runtime.GOARCH
}

func Cmd(cwd string, exe string, args ...string) {
	cmd := exec.Command(exe, args...)
	cmd.Dir = cwd
	log.Info("Cmd", "exe", exe, "args", args)
	// store all output in `out` for log in case of error
	var out bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = cmd.Stdout

	if err := cmd.Run(); err != nil {
		log.Fatal("error executing", "exe", exe,
			"err", err, "oug", out.String())
	}
}

func ShellCmd(cwd string, args ...string) {
	Cmd(cwd, "bash", args...)
}

func filepathStem(fileName string) string {
	return strings.TrimSuffix(fileName, filepath.Ext(fileName))
}

func Makedirs(paths ...string) {
	log.Info("Makedirs", "paths", paths)
	for _, path := range paths {
		err := os.Mkdir(path, 0750)
		if err != nil && !os.IsExist(err) {
			log.Fatal(err)
		}
	}
}

func Make(cwd string, args ...string) {
	Cmd(cwd, "make", args...)
}

func DownloadTo(url string, directory string, extractToDir string) {
	log.Info("DownloadTo", "url", url, "to", directory, "extract_to")
	Cmd(".", "wget", "-P", directory, url)
	var name = filepath.Base(url)
	Cmd(directory, "tar", "xvf", name, "-C", extractToDir)
}

func GitClone(url string, branch string, directory string, recurse bool) {
	var target = filepath.Join(directory, filepathStem(filepath.Base(url)))
	var args = []string{"clone", "--depth=1"}
	if recurse {
		args = append(args, "--recurse-submodules", "--shallow-submodules")
	}
	args = append(args, "--branch", branch, url, target)
	log.Info("GitClone", "exe", "git", "args", args)
	Cmd(".", "git", args...)
}

func CmakeConfigure(srcdir string, builddir string, options ...string) {
	var args = []string{"-S", srcdir, "-B", builddir}
	args = append(args, options...)
	log.Info("CmakeConfigure", "exe", "cmake", "args", args)
	cmake := exec.Command("cmake", args...)
	if err := cmake.Run(); err != nil {
		log.Fatal(err)
	}
}

func CmakeBuild(builddir string, release bool) {
	var args = []string{"--build", builddir}
	if release {
		args = append(args, "--config", "Release")
	}
	log.Info("CmakeBuild", "exe", "cmake", "args", args)
	Cmd(".", "cmake", args...)
}

func CmakeInstall(builddir string, prefix string) {
	var args = []string{"--install", builddir, "--prefix", prefix}
	log.Info("CmakeInstall", "exe", "cmake", "args", args)
	Cmd(".", "cmake", args...)
}

func RecursiveRemove(root string, patterns []string) {
	log.Info("RecursiveRemove", "root", root, "patterns", patterns)
	cache := make(map[string]bool) // to skip pesky duplicates
	base, _ := os.Getwd()
	visit := func(path string, entry fs.DirEntry, err error) error {
		for _, p := range patterns {
			res, _ := filepath.Match(p, entry.Name())
			if res {
				rel, _ := filepath.Rel(base, path)
				if _, ok := cache[rel]; ok {
					return nil
				} else {
					cache[rel] = true
					var key = "f"
					if entry.IsDir() {
						key = "d"
					}
					log.Info("rm", key, rel)
					os.RemoveAll(path)
				}
				return nil
			}
		}
		return nil
	}
	err := filepath.WalkDir(root, visit)
	if err != nil {
		log.Fatal("filepath.WalkDir() returned %v", err)
	}

}

func Move(src string, dst string) {
	log.Info("Move", "from", src, "to", dst)
	err := os.Rename(src, dst)
	if err != nil {
		log.Fatal(err)
	}
}

func ZipLib(zipPath string, libpath string) {
	Cmd(libpath, "zip", "-r", zipPath, ".")
}
