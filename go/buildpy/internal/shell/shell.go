package shell

import (
    "archive/tar"
    "compress/gzip"
    "fmt"
    "io"
    "log"
    "path/filepath"
    "net/http"
    "os"
)

func DownloadFile(filepath string, url string) (err error) {

	// Create the file
	out, err := os.Create(filepath)
	if err != nil  {
		return err
	}
	defer out.Close()

	// Get the data
	resp, err := http.Get(url)
	if err != nil {
		return err
	}
	defer resp.Body.Close()

	// Check server response
	if resp.StatusCode != http.StatusOK {
		return fmt.Errorf("bad status: %s", resp.Status)
	}

	// Write the body to file
	_, err = io.Copy(out, resp.Body)
	if err != nil  {
		return err
	}

	return nil
}




func ExtractTarGz(gzipStream io.Reader, toDir string) {
    uncompressedStream, err := gzip.NewReader(gzipStream)
    if err != nil {
        log.Fatal("ExtractTarGz: NewReader failed")
    }

    tarReader := tar.NewReader(uncompressedStream)

    for {
        header, err := tarReader.Next()

        if err == io.EOF {
            break
        }

        if err != nil {
            log.Fatalf("ExtractTarGz: Next() failed: %s", err.Error())
        }

        switch header.Typeflag {
        case tar.TypeDir:
        	dir := filepath.Join(toDir, header.Name)
            if err := os.Mkdir(dir, 0755); err != nil {
                log.Fatalf("ExtractTarGz: Mkdir() failed: %s", err.Error())
            }
        case tar.TypeReg:
        	file := filepath.Join(toDir, header.Name)
            outFile, err := os.Create(file)
            if err != nil {
                log.Fatalf("ExtractTarGz: Create() failed: %s", err.Error())
            }
            if _, err := io.Copy(outFile, tarReader); err != nil {
                log.Fatalf("ExtractTarGz: Copy() failed: %s", err.Error())
            }
            outFile.Close()

        default:
            // log.Fatalf(
            log.Printf(
                "ExtractTarGz: unknown type: %s in %s",
                header.Typeflag,
          		header.Name)
        }

    }
}

