// Find and delete all symlinks to the dotfiles defined in $BRIEFCASE.
//
// Oftentimes I corrupt the state of my dotfiles. The intention with this script
// is to write some tooling to help me better manage my dotfile cleanliness. An
// example workflow might look like:
//
// ```shell
// > go run delete_dotfile_symlinks.go --audit
// > go run delete_dotfile_symlinks.go --seriously
// > cd ..
// > make install
// ```
//
// Outstanding TODOs:
// - Package this with <depot>buildGo.nix.
// - How can this be run as script without `go run`? She-bang at the top?
// - See TODOs within this package.

package main

import (
	"errors"
	"flag"
	"fmt"
	"log"
	"os"
	"path/filepath"
	"strings"
)

// Wanted for go tooling:
// 1. jump-to-def
// 2. documentation at point
// 3. autocompletion

// TODO: Consider adding this to a utils.go package.
func failOn(err error) {
	if err != nil {
		log.Fatal(err)
	}
}

// TODO: Consider adding this to a utils.go package.
func isSymlink(m os.FileMode) bool {
	return m&os.ModeSymlink != 0
}

var hostnames = map[string]string{
	os.Getenv("DESKTOP"):  "desktop",
	os.Getenv("LAPTOP"):   "work_laptop",
	os.Getenv("CLOUDTOP"): "cloudtop",
}

func main() {
	audit := flag.Bool("audit", false, "Output all symlinks that would be deleted. This is the default behavior. This option is mutually exclusive with the --seriously option.")
	seriously := flag.Bool("seriously", false, "Actually delete the symlinks. This option is mutually exclusive with the --audit option.")
	repoName := flag.String("repo-name", "briefcase", "The name of the repository.")
	deviceOnly := flag.Bool("device-only", false, "Only output the device-specific dotfiles.")
	flag.Parse()

	if !*audit && !*seriously {
		log.Fatal(errors.New("Either -audit or -seriously needs to be set."))
	}
	if *audit == *seriously {
		log.Fatal(errors.New("Arguments -audit and -seriously are mutually exclusive"))
	}

	home, err := os.UserHomeDir()
	failOn(err)
	count := 0

	err = filepath.Walk(home, func(path string, info os.FileInfo, err error) error {
		if isSymlink(info.Mode()) {
			dest, err := os.Readlink(path)
			failOn(err)

			var predicate func(string) bool

			if *deviceOnly {
				predicate = func(dest string) bool {
					var hostname string
					hostname, err = os.Hostname()
					failOn(err)
					seeking, ok := hostnames[hostname]
					if !ok {
						log.Fatal(fmt.Sprintf("Hostname \"%s\" not supported in the hostnames map.", hostname))
					}
					return strings.Contains(dest, *repoName) && strings.Contains(dest, seeking)
				}
			} else {
				predicate = func(dest string) bool {
					return strings.Contains(dest, *repoName)
				}
			}

			if predicate(dest) {
				if *audit {
					fmt.Printf("%s -> %s\n", path, dest)
				} else if *seriously {
					fmt.Printf("rm %s\n", path)
					err = os.Remove(path)
					failOn(err)
				}
				count += 1
			}
		}
		return nil
	})
	failOn(err)
	if *audit {
		fmt.Printf("Would have deleted %d symlinks.\n", count)
	} else if *seriously {
		fmt.Printf("Successfully deleted %d symlinks.\n", count)
	}

	os.Exit(0)
}
