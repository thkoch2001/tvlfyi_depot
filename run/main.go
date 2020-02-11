package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"utils"
)

func main() {
	if len(os.Args) != 2 {
		log.Fatal("You can only call run with a single file at a time.")
	}

	rulesPath := utils.Resolve("run.json", []string{"/home/wpcarro/.config/run/run.json"})
	b, err := ioutil.ReadFile(rulesPath)
	if err != nil {
		log.Fatal("Could not locate a run.json file: ", err)
	}
	rules := map[string]string{}
	err = json.Unmarshal(b, &rules)
	if err != nil {
		log.Fatal("Could not decode run.json as JSON: ", err)
	}

	fileName := os.Args[1]
	ext := filepath.Ext(fileName)
	cmd, ok := rules[ext]

	if !ok {
		log.Fatalf("No rules for extension, %s, have been defined.", ext)
	}

	// TODO(wpcarro): Support more sophisticated parsing than just string
	// splitting. To handle 'cases like this'.
	tokens := strings.Split(strings.Replace(cmd, "$file", fileName, 1), " ")
	c := exec.Command(tokens[0], tokens[1:]...)
	err = c.Start()
	// TODO(wpcarro): Forward STDERR and STDOUT.
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println(c.Wait())
}
