package main

import (
	"fmt"
	"go/build"
	"io/fs"
	"os"
	"path/filepath"
)

func main() {
	if len(os.Args) < 1 {
		_, _ = fmt.Fprintf(os.Stderr, "usage: %s <src> ...\n", os.Args[0])
		os.Exit(1)
	}

	sources := os.Args[1:]

	files := make([]fileInfo, len(sources))
	for i, source := range sources {
		src, err := readFileInfo(build.Default, source)
		if err != nil {
			panic(err)
		}
		files[i] = src
	}

	dir := filepath.Dir(sources[0])

	embedSrcs := []string{}
	embedRootDirs := []string{dir}

	for _, rootDir := range embedRootDirs {
		_ = filepath.WalkDir(rootDir, func(path string, d fs.DirEntry, err error) error {
			embedSrcs = append(embedSrcs, path)
			return nil
		})
	}

	file, err := buildEmbedcfgFile(files, embedSrcs, embedRootDirs, "./")
	if err != nil {
		panic(err)
	}

	fmt.Fprintln(os.Stdout, file)
}
