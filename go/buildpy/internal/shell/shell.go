package shell

import (
	"bytes"
	"io/fs"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"github.com/charmbracelet/log"
)

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

func CmdEnv(cwd string, exe string, envars []string, args ...string) {
	cmd := exec.Command(exe, args...)
	cmd.Env = os.Environ()
	cmd.Env = append(cmd.Env, envars...)
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
	Cmd(cwd, "/bin/sh", args...)
}

func GetFilepathStem(fileName string) string {
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

func Wget(cwd string, downloaddir string, url string) {
	Cmd(cwd, "wget", "-P", downloaddir, url)
}

// func Curl(cwd string, downloaddir string, url string) {
// 	// see: https://www.warp.dev/terminus/curl-vs-wget
// 	Cmc(cwd, "curl", "-L", "-O", )
// 	Cmc(cwd, "curl", "-L" "--output-dir", downloaddir, "-o", as_name, url)
// 	Cmc(cwd, "curl", "-L" "--output-dir", downloaddir, "-O", url)
// }

func DownloadTo(url string, downloaddir string, srcdir string) {
	log.Info("DownloadTo", "url", url, "to", downloaddir, "extract_to", srcdir)
	var archive_name = filepath.Base(url)
	var archive = filepath.Join(downloaddir, archive_name)
	var extracted_name = GetFilepathStem(GetFilepathStem(archive_name))
	var extracted = filepath.Join(srcdir, extracted_name)

	if _, err := os.Stat(archive); err != nil {
		// archive does not exist in downloads, then download it
		Wget(".", downloaddir, url)
	}
	Cmd(downloaddir, "tar", "xvf", archive_name, "-C", srcdir)
	// normalize extracted src_dir
	os.Rename(extracted, filepath.Join(srcdir, "python"))
}

func GitClone(url string, branch string, as_directory string, recurse bool) {
	var args = []string{"clone", "--depth=1"}
	if recurse {
		args = append(args, "--recurse-submodules", "--shallow-submodules")
	}
	args = append(args, "--branch", branch, url, as_directory)
	log.Info("GitClone")
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

func CmakeConfigureEnv(srcdir string, builddir string, envars []string, options ...string) {
	var args = []string{"-S", srcdir, "-B", builddir}
	args = append(args, options...)
	log.Info("CmakeConfigure", "exe", "cmake", "env", envars, "args", args)
	CmdEnv(".", "cmake", envars, args...)
}

func CmakeBuildEnv(builddir string, release bool, envars []string) {
	var args = []string{"--build", builddir}
	if release {
		args = append(args, "--config", "Release")
	}
	log.Info("CmakeBuild", "exe", "cmake", "args", args)
	CmdEnv(".", "cmake", envars, args...)
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

func CheckDeps(names ...string) {
	for _, name := range names {
		path, err := exec.LookPath(name)
		if err != nil {
			log.Fatal(err)
		}
		log.Info("check", "name", name, "path", path)
	}
}
