// Copyright 2019 Google LLC.
// SPDX-License-Identifier: Apache-2.0

// This tool analyses external (i.e. not built with `buildGo.nix`) Go
// packages to determine a build plan that Nix can import.
package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"go/build"
	"io/ioutil"
	"log"
	"os"
	"path"
	"path/filepath"
	"strings"
)

// Path to a JSON file describing all standard library import paths.
// This file is generated and set here by Nix during the build
// process.
var stdlibList string

// pkg describes a single Go package within the specified source
// directory.
//
// Return information includes the local (relative from project root)
// and external (none-stdlib) dependencies of this package.
type pkg struct {
	Name        []string   `json:"name"`
	Files       []string   `json:"files"`
	LocalDeps   [][]string `json:"localDeps"`
	ForeignDeps []string   `json:"foreignDeps"`
}

// findGoDirs returns a filepath.WalkFunc that identifies all
// directories that contain Go source code in a certain tree.
func findGoDirs(at string) ([]string, error) {
	var goDirs []string
	dir := ""

	err := filepath.Walk(at, func(path string, info os.FileInfo, err error) error {
		// Skip testdata
		if info.IsDir() && info.Name() == "testdata" {
			return filepath.SkipDir
		}

		// Keep track of the last seen directory.
		if info.IsDir() {
			dir = path
			return nil
		}

		// If the directory has already been "popped", nothing else needs
		// to happen.
		if dir == "" {
			return nil
		}

		// If the current file is a Go file, then the directory is popped
		// (i.e. marked as a Go directory).
		if strings.HasSuffix(info.Name(), ".go") && !strings.HasSuffix(info.Name(), "_test.go") {
			goDirs = append(goDirs, dir)
			dir = ""
			return nil
		}

		return nil
	})

	if err != nil {
		return nil, err
	}

	return goDirs, nil
}

// analysePackage loads and analyses the imports of a single Go
// package, returning the data that is required by the Nix code to
// generate a derivation for this package.
func analysePackage(root, source, importpath string, stdlib map[string]bool) (pkg, error) {
	ctx := build.Default

	p, err := ctx.ImportDir(source, build.IgnoreVendor)
	if err != nil {
		return pkg{}, err
	}

	local := [][]string{}
	foreign := []string{}

	for _, i := range p.Imports {
		if stdlib[i] {
			continue
		}

		if strings.HasPrefix(i, importpath) {
			local = append(local, strings.Split(strings.TrimPrefix(i, importpath+"/"), "/"))
		} else {
			foreign = append(foreign, i)
		}
	}

	prefix := strings.TrimPrefix(source, root+"/")
	files := []string{}
	for _, f := range p.GoFiles {
		files = append(files, path.Join(prefix, f))
	}

	analysed := pkg{
		Name:        strings.Split(prefix, "/"),
		Files:       files,
		LocalDeps:   local,
		ForeignDeps: foreign,
	}

	return analysed, nil
}

func loadStdlibPkgs(from string) (pkgs map[string]bool, err error) {
	f, err := ioutil.ReadFile(from)
	if err != nil {
		return
	}

	err = json.Unmarshal(f, &pkgs)
	return
}

func main() {
	source := flag.String("source", "", "path to directory with sources to process")
	path := flag.String("path", "", "import path for the package")

	flag.Parse()

	if *source == "" {
		log.Fatalf("-source flag must be specified")
	}

	stdlibPkgs, err := loadStdlibPkgs(stdlibList)
	if err != nil {
		log.Fatalf("failed to load standard library index from %q: %s\n", stdlibList, err)
	}

	goDirs, err := findGoDirs(*source)
	if err != nil {
		log.Fatalf("failed to walk source directory '%s': %s\n", source, err)
	}

	all := []pkg{}
	for _, d := range goDirs {
		analysed, err := analysePackage(*source, d, *path, stdlibPkgs)
		if err != nil {
			log.Fatalf("failed to analyse package at %q: %s", d, err)
		}
		all = append(all, analysed)
	}

	j, _ := json.Marshal(all)
	fmt.Println(string(j))
}
