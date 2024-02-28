package shell

import (
	"log"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

func filepath_stem(fileName string) string {
	return strings.TrimSuffix(fileName, filepath.Ext(fileName))
}

func Makedir(path string) {
	err := os.Mkdir(path, 0750)
	if err != nil && !os.IsExist(err) {
		log.Fatal(err)
	}
}

func Make(cwd string, args ...string) {
	cmd := exec.Command("make", args...)
	cmd.Dir = cwd
	if err := cmd.Run(); err != nil {
		log.Fatal(err)
	}
	log.Printf("make: %s", strings.Join(args, " "))
}

func DownloadTo(url string, directory string) {
	download := exec.Command("wget", "-P", directory, url)
	if err := download.Run(); err != nil {
		log.Fatal(err)
	}
	log.Printf("downloaded: %s", url)
	var name = filepath.Base(url)
	extract := exec.Command("tar", "xvf", name)
	extract.Dir = directory
	if err := extract.Run(); err != nil {
		log.Fatal(err)
	}
	log.Printf("extracted: %s", name)
}

func GitClone(url string, branch string, directory string, recurse bool) {
	var target = filepath.Join(directory, filepath_stem(filepath.Base(url)))
	var args = []string{"clone", "--depth=1"}
	if recurse {
		args = append(args, "--recurse-submodules", "--shallow-submodules")
	}
	args = append(args, "--branch", branch, url, target)
	clone := exec.Command("git", args...)
	if err := clone.Run(); err != nil {
		log.Fatal(err)
	}
	log.Printf("git cloned: %s", url)
}

func ShellCmd(cwd string, args ...string) {
	scmd := exec.Command("bash", args...)
	scmd.Dir = cwd
	if err := scmd.Run(); err != nil {
		log.Fatal(err)
	}
	log.Printf("shell comand: %s", strings.Join(args, " "))
}
