package shell

import (
    "log"
    "path/filepath"
    "os/exec"
)

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

