package main

import (
	"bytes"
	"flag"
	"fmt"
	"log"
	"os"
	"os/exec"
	"path/filepath"
)

////////////////////////////////////////////////////////////////////////////////
// Variables
////////////////////////////////////////////////////////////////////////////////

var (
	hostnameFlag = flag.String("hostname", "", "The hostname for which a given NixOS will be built. This is passed to ops.nixos.findSystem, and it defaults to your current machine's hostname.")
	depotPath    = flag.String("depot", "/depot", "Path to the local copy of the depot repository.")
	isRemote     = flag.Bool("remote", false, "Rebuild your system from the latest revision on origin/canon. Mutually exclusive with -local.")
	dryRun       = flag.Bool("dry_run", false, "Set this to avoid calling switch-to-configuration after rebuilding.")
	stateDir     = flag.String("state_dir", "/var/lib/rebuild-system", "Path to the directory hosting this tool's runtime state.")
	gitRemote    = flag.String("git_remote", "https://cl.tvl.fyi/depot.git", "URL of git remote to fetch.")
)

////////////////////////////////////////////////////////////////////////////////
// Library
////////////////////////////////////////////////////////////////////////////////

// Rebuild NixOS for the given `hostname` using the state from `depotPath`.
func buildNixOSFor(hostname string, depotPath string) {
	system := shell("nix-build", "-E", fmt.Sprintf("((import %s {}).ops.nixos.findSystem %q).system", depotPath, hostname), "--no-out-link", "--show-trace")
	shell("nix-env", "-p", "/nix/var/nix/profiles/system", "--set", system)

	if !*dryRun {
		shell(fmt.Sprintf("%s/bin/switch-to-configuration", system), "switch")
	}
}

// Rebuild NixOS for the given `hostname` using the state from a local git
// repository.
func handleLocal(hostname string) {
	buildNixOSFor(hostname, *depotPath)
}

// Rebuild NixOS for the given `hostname` using the state from a remote git
// repository, `gitRemote`.
func handleRemote(hostname string) {
	os.MkdirAll(*stateDir, os.ModePerm)
	depot := filepath.Join(*stateDir, "depot.git")
	worktree := filepath.Join(*stateDir, "build")

	if _, err := os.Stat(depot); os.IsNotExist(err) {
		shell("git", "clone", "--bare", *gitRemote, depot)
	}

	// clean-up for premature exits
	defer func() {
		shell("git", "worktree", "remove", worktree)
	}()

	shell("git", "-C", depot, "fetch", "origin")
	shell("git", "-C", depot, "worktree", "add", "--force", worktree, "FETCH_HEAD")

	// # unsure why, but without this switch-to-configuration attempts to install
	// # NixOS in $state_directory
	os.Chdir("/")
	buildNixOSFor(hostname, worktree)
}

// Run shell command, `x`, with argv, `rest`, and return the STDOUT.
func shell(x string, rest ...string) string {
	fmt.Printf("%s %v\n", x, rest)

	var stdout, stderr bytes.Buffer
	cmd := exec.Command(x, rest...)
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr

	err := cmd.Run()
	fmt.Printf(stderr.String())
	if err != nil {
		log.Fatal(err)
	}

	return stdout.String()
}

////////////////////////////////////////////////////////////////////////////////
// Main
////////////////////////////////////////////////////////////////////////////////

func main() {
	flag.Parse()

	if os.Geteuid() != 0 {
		fmt.Println("Oh no! Only root is allowed to run upgrade-system!")
		os.Exit(1)
	}

	var hostname string
	if *hostnameFlag == "" {
		hostname, _ = os.Hostname()
	} else {
		hostname = *hostnameFlag
	}

	if hostname == "" {
		fmt.Println("Cannot read hostname. Try setting the -hostname flag. Exiting...")
		os.Exit(1)
	}
	fmt.Printf("Rebuilding NixOS for %q...\n", hostname)

	if *isRemote {
		handleRemote(hostname)
	} else {
		handleLocal(hostname)
	}
}
