// Some utility functions to tidy up my Golang.
package utils

import (
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"net/http/httputil"
	"os"
	"os/user"
	"path/filepath"
)

// Return the absolute path to the current uesr's home directory.
func HomeDir() string {
	user, err := user.Current()
	if err != nil {
		log.Fatal(err)
	}
	return user.HomeDir
}

// Return true if `path` exists and false otherwise.
func FileExists(path string) bool {
	if _, err := os.Stat(path); os.IsNotExist(err) {
		return false
	} else {
		return true
	}
}

// Return the absolute file path of `file` using the following resolution
// strategy:
// - Traverse and search upwards until you reach the user's home directory
// - Return the first path in `backupPaths` that exists
// - Fail
func Resolve(fileName string, backupPaths []string) string {
	// TODO(wpcarro): Drop hardcoding when whoami behaves as expected.
	boundary := "/home"
	cwd := "."
	files, _ := ioutil.ReadDir(cwd)

	for {
		fullCwd, _ := filepath.Abs(cwd)
		if fullCwd == boundary {
			break
		}
		for _, file := range files {
			if file.Name() == fileName {
				path, _ := filepath.Abs(cwd + "/" + file.Name())
				return path
			}
		}
		cwd += "/.."
		files, _ = ioutil.ReadDir(cwd)
	}

	// TODO(wpcarro): Support expanding these paths to allow the consumer to
	// pass in relative paths, and paths with "~" in them.
	for _, backup := range backupPaths {
		if FileExists(backup) {
			return backup
		}
	}
	log.Fatal("Cannot find a run.json to use.")
	// This code should be unreachable.
	return ""
}

// Call log.Fatal with `err` when it's not nil.
func FailOn(err error) {
	if err != nil {
		log.Fatal(err)
	}
}

// Prints the verbose form of an HTTP request.
func DebugRequest(req *http.Request) {
	bytes, _ := httputil.DumpRequest(req, true)
	fmt.Println(string(bytes))
}

// Prints out the verbose form of an HTTP response.
func DebugResponse(res *http.Response) {
	bytes, _ := httputil.DumpResponse(res, true)
	fmt.Println(string(bytes))
}

// Make a simple GET request to `url`. Fail if anything returns an error. I'd
// like to accumulate a library of these, so that I can write scrappy Go
// quickly. For now, this function just returns the body of the response back as
// a string.
func SimpleGet(url string, headers map[string]string, debug bool) string {
	client := &http.Client{}
	req, err := http.NewRequest("GET", url, nil)
	if err != nil {
		log.Fatal(err)
	}
	for k, v := range headers {
		req.Header.Add(k, v)
	}

	res, err := client.Do(req)
	if err != nil {
		log.Fatal(err)
	}
	defer res.Body.Close()

	if debug {
		DebugRequest(req)
		DebugResponse(res)
	}

	if res.StatusCode == http.StatusOK {
		bytes, err := ioutil.ReadAll(res.Body)
		if err != nil {
			log.Fatal(err)
		}
		return string(bytes)
	} else {
		log.Println(res)
		log.Fatalf("HTTP status code of response not OK: %v\n", res.StatusCode)
		return ""
	}
}
