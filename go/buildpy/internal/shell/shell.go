package shell

import (
	"archive/zip"
	"io"
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
	log.Info(exe, "args", args)
	if err := cmd.Run(); err != nil {
		log.Fatal(err)
	}
	log.Info(exe, "status", "DONE")
}

func ShellCmd(cwd string, args ...string) {
	Cmd(cwd, "bash", args...)
}

func filepathStem(fileName string) string {
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
	var target = filepath.Join(directory, filepathStem(filepath.Base(url)))
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

func Move(src string, dst string) {
	err := os.Rename(src, dst)
	if err != nil {
		log.Fatal(err)
	}
}

func ZipFileOrFolder(zipPath, filePath, basePath string) (err error) {
	var newZipFile *os.File
	newZipFile, err = os.Create(zipPath)
	if err != nil {
		return err
	}
	defer func() {
		newZipFile.Close()
		if err != nil {
			os.Remove(zipPath)
		}
	}()

	zipWriter := zip.NewWriter(newZipFile)
	defer zipWriter.Close()

	if err = addToZip(zipWriter, filePath, basePath); err != nil {
		return err
	}
	return nil
}

func addToZip(zipWriter *zip.Writer, filename, basePath string) error {
	stat, err := os.Stat(filename)
	if err != nil {
		return err
	}
	if stat.IsDir() {
		infos, err := os.ReadDir(filename)
		if err != nil {
			return err
		}
		for _, info := range infos {
			fname := filepath.Join(filename, info.Name())
			relpath, _ := filepath.Rel(basePath, fname)
			log.Info("zip", "adding", relpath)
			err = addToZip(zipWriter, fname, basePath)
			if err != nil {
				return err
			}
		}
		return nil
	}

	fileToZip, err := os.Open(filename)
	if err != nil {
		return err
	}
	defer fileToZip.Close()

	header, err := zip.FileInfoHeader(stat)
	if err != nil {
		return err
	}
	header.Name, err = filepath.Rel(basePath, filename)
	if err != nil {
		return err
	}
	header.Name = strings.ReplaceAll(header.Name, `\`, `/`) // Linux can only deal with /, while Windows can deal with / and \.
	header.Method = zip.Deflate

	writer, err := zipWriter.CreateHeader(header)
	if err != nil {
		return err
	}
	_, err = io.Copy(writer, fileToZip)
	return err
}
