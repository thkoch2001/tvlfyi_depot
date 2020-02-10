// Some utility functions to tidy up my Golang.
package utils

import (
	"log"
	"io/ioutil"
	"net/http"
	"net/http/httputil"
)

// Call log.Fatal with `err` when it's not nil.
func FailOn(err error) {
	if err != nil {
		log.Fatal(err)
	}
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
		bytes, _ := httputil.DumpRequest(req, true)
		log.Println(string(bytes))
		bytes, _ = httputil.DumpResponse(res, true)
		log.Println(string(bytes))
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
