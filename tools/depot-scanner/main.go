package main

import (
	"bufio"
	"flag"
	"fmt"
	"io"
	"os/exec"
	"strings"
)

var nixInstantiatePath = flag.String("nix-bin", "/run/current-system/sw/bin/nix-instantiate", "path to nix-instantiate")
var depotRoot = flag.String("depot", "/depot/", "path to tvl.fyi depot at current canon")
var nixStoreRoot = flag.String("store-path", "/nix/store/", "prefix for all valid nix store paths")

const (
	// String that identifies a path as belonging to nix corepkgs.
	corePkgsString = "/share/nix/corepkgs/"

	depotTraceString = "trace: depot-scan: "
)

type fileScanType int

const (
	unknownPath fileScanType = iota
	depotPath
	nixStorePath
	corePkgsPath
)

func launchNix(attr string) (*exec.Cmd, io.ReadCloser, io.ReadCloser, error) {
	cmd := exec.Command(*nixInstantiatePath, "--trace-file-access", "-A", attr)
	stdout, err := cmd.StdoutPipe()
	if err != nil {
		return nil, nil, nil, err
	}
	stderr, err := cmd.StderrPipe()
	if err != nil {
		stdout.Close()
		return nil, nil, nil, err
	}

	err = cmd.Start()
	if err != nil {
		stdout.Close()
		stderr.Close()
		return nil, nil, nil, err
	}

	return cmd, stdout, stderr, nil
}

func categorizePath(path string) fileScanType {
	if strings.HasPrefix(path, *nixStoreRoot) {
		if strings.Contains(path, corePkgsString) {
			return corePkgsPath
		}
		return nixStorePath
	} else if strings.HasPrefix(path, *depotRoot) {
		return depotPath
	} else if strings.Contains(path, corePkgsString) {
		return corePkgsPath
	}
	return unknownPath
}

func addPath(path string, out map[fileScanType]map[string]struct{}) {
	cat := categorizePath(path)
	if out[cat] == nil {
		out[cat] = make(map[string]struct{})
	}

	out[cat][path] = struct{}{}
}

func consumeOutput(stdout, stderr io.ReadCloser) (map[fileScanType]map[string]struct{}, string, error) {
	result := make(map[fileScanType]map[string]struct{})

	scanner := bufio.NewScanner(stderr)
	for scanner.Scan() {
		line := scanner.Text()
		if strings.HasPrefix(line, depotTraceString) {
			addPath(strings.TrimPrefix(line, depotTraceString), result)
		}
	}
	if scanner.Err() != nil {
		return nil, "", scanner.Err()
	}

	// Get derivation path
	derivPath := ""
	scanner = bufio.NewScanner(stdout)
	for scanner.Scan() {
		line := scanner.Text()
		if strings.HasPrefix(line, *nixStoreRoot) {
			derivPath = line
			// consume the rest of the output
		}
	}
	if scanner.Err() != nil {
		return nil, "", scanner.Err()
	}

	return result, derivPath, nil
}

func main() {
	flag.Parse()

	cmd, stdout, stderr, err := launchNix(flag.Arg(0))
	if err != nil {
		panic(fmt.Errorf("could not launch nix: %w", err))
	}
	results, derivPath, err := consumeOutput(stdout, stderr)
	if err != nil {
		err2 := cmd.Wait()
		if err2 != nil {
			panic(fmt.Errorf("nix-instantiate failed: %w\nadditionally, while reading output: %w", err2, err))
		}
		panic(fmt.Errorf("problem reading nix output: %w", err))
	}
	err = cmd.Wait()
	if err != nil {
		panic(fmt.Errorf("nix-instantiate failed: %w", err))
	}

	_ = derivPath

	fmt.Println("nix-store:")
	for k, _ := range results[nixStorePath] {
		fmt.Println(k)
	}
	fmt.Println("depot:")
	for k, _ := range results[depotPath] {
		fmt.Println(k)
	}
	fmt.Println("corepkgs:")
	for k, _ := range results[corePkgsPath] {
		fmt.Println(k)
	}
	fmt.Println("unknown:")
	for k, _ := range results[unknownPath] {
		fmt.Println(k)
	}
}
