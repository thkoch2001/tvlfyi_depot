package main

import (
	"archive/zip"
	"flag"
	"fmt"
	"io"
	"log"
	"os"
	"path/filepath"
	"sort"
	"strings"
)

var (
	exclude = flag.String("exclude", "", "comma-separated list of filenames to exclude (in any directory)")
)

func init() {
	flag.Usage = func() {
		fmt.Fprintf(flag.CommandLine.Output(), "Usage of %s [zip file] [directory]:\n", os.Args[0])
		flag.PrintDefaults()
	}
}

func listToMap(ss []string) map[string]bool {
	m := make(map[string]bool)
	for _, s := range ss {
		m[s] = true
	}
	return m
}

func main() {
	flag.Parse()
	if flag.NArg() != 2 {
		flag.Usage()
		os.Exit(1)
	}

	outPath := flag.Arg(0)
	dirPath := flag.Arg(1)

	excludeFiles := listToMap(strings.Split(*exclude, ","))

	// Aggregate all files first.
	var files []string
	filepath.Walk(dirPath, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if info.IsDir() {
			return nil
		}
		if excludeFiles[info.Name()] {
			return nil
		}
		files = append(files, path)
		return nil
	})

	// Create zip
	outW, err := os.Create(outPath)
	if err != nil {
		log.Fatalf("Create(%q): %v", outPath, err)
	}

	zipW := zip.NewWriter(outW)

	// Output files in alphabetical order
	sort.Strings(files)
	for _, f := range files {
		fw, err := zipW.CreateHeader(&zip.FileHeader{
			Name:   f,
			Method: zip.Store,
		})
		if err != nil {
			log.Fatalf("creating %q in zip: %v", f, err)
		}

		ff, err := os.Open(f)
		if err != nil {
			log.Fatalf("opening %q: %v", f, err)
		}
		if _, err := io.Copy(fw, ff); err != nil {
			log.Fatalf("copying %q to zip: %v", f, err)
		}
		ff.Close()
	}

	if err := zipW.Close(); err != nil {
		log.Fatalf("writing ZIP central directory: %v", err)
	}
	if err := outW.Close(); err != nil {
		log.Fatalf("closing ZIP file: %v", err)
	}
}
