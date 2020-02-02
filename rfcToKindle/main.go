// Author: wpcarro@gmail.com
//
// Wirelessly transfer RFC documents to your Kindle to device for an alternative
// medium for reading.
//
// Usage:
// ```shell
// > go run rfcToKindle.go -document rfc6479 -recipient username@kindle.com
// ```
//
// This uses `sendgmr` to send the file to the Kindle. Make sure:
// 1. That `sendgmr` is installed and available on $PATH.
// 2. That it is configured to work with your preferred email address.
// 3. That the email address `sendgmr` is configured to use is whitelisted in
//    your Kindle "Personal Document Settings".

package main

import (
	"flag"
	"fmt"
	"io"
	"io/ioutil"
	"log"
	"net/http"
	"os"
	"os/exec"
	"strings"
)

func main() {
	document := flag.String("document", "", "(Required) The name of the document to fetch. For example \"RFC6479\".")
	recipient := flag.String("recipient", "", "(Required) The email address of the Kindle device.")
	subject := flag.String("subject", "", "(Optional) The email address of the Kindle device.")
	flag.Parse()

	if *document == "" {
		// TODO: Is log.Fatal the best function to use here?
		log.Fatal("-document cannot be empty. See -help for more information.")
	}

	if *recipient == "" {
		log.Fatal("-recipient cannot be empty. See -help for more information.")
	}

	*document = strings.ToLower(*document)

	url := fmt.Sprintf("https://www.ietf.org/rfc/%s.txt", *document)
	resp, err := http.Get(url)
	fmt.Printf("Downloading %s ... ", url)

	if err != nil {
		log.Fatal(err)
	}
	defer resp.Body.Close()

	f, err := ioutil.TempFile("", fmt.Sprintf("%s-*.txt", *document))
	if err != nil {
		log.Fatal(err)
	}
	// TODO: Verify if this is cleaning up or not.
	defer os.Remove(f.Name())

	_, err = io.Copy(f, resp.Body)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println("done.")

	if *subject == "" {
		*subject = fmt.Sprintf("%s - Sent from rfcToKindle.go", *document)
	}

	// Although I couldn't find it documented anywhere, the email sent to the
	// Kindle must have a body, even if the body isn't used for anything.
	fmt.Printf("Emailing %s to %s ... ", f.Name(), *recipient)
	cmd := exec.Command("sendgmr",
		fmt.Sprintf("--to=%s", *recipient),
		fmt.Sprintf("--body_file=%s", f.Name()),
		fmt.Sprintf("--subject=%s", *subject),
		fmt.Sprintf("--attachment_files=%s", f.Name()))
	err = cmd.Run()
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println("done.")

	os.Exit(0)
}
