package shell

import (
	"io/fs"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"github.com/charmbracelet/log"
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

func DownloadTo(url string, directory string, extractToDir string) {
	log.Info("Download", "url", url, "todir", directory, "extractodir", extractToDir)
	download := exec.Command("wget", "-P", directory, url)
	if err := download.Run(); err != nil {
		log.Fatal(err)
	}
	log.Info("downloaded", "url", url)
	var name = filepath.Base(url)
	log.Info("extracting", "name", name)
	extract := exec.Command("tar", "xvf", name, "-C", extractToDir)
	extract.Dir = directory
	if err := extract.Run(); err != nil {
		log.Fatal(err)
	}
	log.Info("extracted: DONE")
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

func RecursiveRemove(root string, patterns []string) {
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
					log.Info("del", key, rel)
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
