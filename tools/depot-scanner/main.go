package main

import (
	"bufio"
	"flag"
	"fmt"
	"io"
	"os"
	"os/exec"
	"strings"

	pb "code.tvl.fyi/tools/depot-scanner/proto"
)

var nixInstantiatePath = flag.String("nix-bin", "/run/current-system/sw/bin/nix-instantiate", "path to nix-instantiate")
var depotRoot = flag.String("depot", envOr("DEPOT_ROOT", "/depot/"), "path to tvl.fyi depot at current canon")
var nixStoreRoot = flag.String("store-path", "/nix/store/", "prefix for all valid nix store paths")

var modeFlag = flag.String("mode", modeArchive, "operation mode. valid values: tar, print")
var onlyFlag = flag.String("only", "", "only enable the listed output types, comma separated. valid values: DEPOT, STORE, CORE, UNKNOWN")

const (
	modeArchive = "tar"
	modePrint   = "print"
)

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

	checkDepotRoot()

	enabledPathTypes := make(map[pb.PathType]bool, 4)
	if len(*onlyFlag) > 0 {
		enabledOutputs := strings.Split(*onlyFlag, ",")
		for _, v := range enabledOutputs {
			i, ok := pb.PathType_value[strings.ToUpper(v)]
			if !ok {
				fmt.Fprintln(os.Stderr, "warning: unrecognized PathType name: ", v)
				continue
			}
			enabledPathTypes[pb.PathType(i)] = true
		}
	} else {
		// Default
		enabledPathTypes = map[pb.PathType]bool{
			pb.PathType_UNKNOWN: true,
			pb.PathType_DEPOT:   true,
			pb.PathType_STORE:   true,
			pb.PathType_CORE:    true,
		}
	}

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

	if *modeFlag == "print" {
		if enabledPathTypes[pb.PathType_STORE] {
			for k, _ := range results[nixStorePath] {
				fmt.Println(k)
			}
		}
		if enabledPathTypes[pb.PathType_DEPOT] {
			for k, _ := range results[depotPath] {
				fmt.Println(k)
			}
		}
		if enabledPathTypes[pb.PathType_CORE] {
			for k, _ := range results[corePkgsPath] {
				fmt.Println(k)
			}
		}
		if enabledPathTypes[pb.PathType_UNKNOWN] {
			for k, _ := range results[unknownPath] {
				fmt.Println(k)
			}
		}
	} else {
		panic("unimplemented")
	}
}

func envOr(envVar, def string) string {
	v := os.Getenv(envVar)
	if v == "" {
		return def
	}
	return v
}

func checkDepotRoot() {
	if *depotRoot == "" {
		fmt.Fprintln(os.Stderr, "error: DEPOT_ROOT / -depot not set")
		os.Exit(2)
	}
	_, err := os.Stat(*depotRoot)
	if os.IsNotExist(err) {
		fmt.Fprintf(os.Stderr, "error: %q does not exist\ndid you forget to set DEPOT_ROOT / --depot ?\n", *depotRoot)
		os.Exit(1)
	} else if err != nil {
		fmt.Fprintf(os.Stderr, "error: could not stat %q: %v\n", *depotRoot, err)
		os.Exit(1)
	}

}
