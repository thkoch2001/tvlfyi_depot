// Eagle Mode configuration wrapper that recreates the required directory
// structure for Eagle Mode based on the output of depot.tools.eaglemode.etcDir
//
// This will replace *all* symlinks in the Eagle Mode configuration directory,
// but it will not touch actual files. Missing folders will be created.
package main

import (
	"flag"
	"fmt"
	"io/fs"
	"log"
	"os"
	"os/user"
	"path"
	"path/filepath"
	"strings"
)

func configDir() (string, error) {
	v := os.Getenv("EM_USER_CONFIG_DIR")
	if v != "" {
		return v, nil
	}

	usr, err := user.Current()
	if err != nil {
		return "", fmt.Errorf("failed to get current user: %w", err)
	}

	return path.Join(usr.HomeDir, ".eaglemode"), nil
}

// cleanupConfig removes *all* existing symlinks in the configuration which do
// not point into the right Nix store path.
func cleanupConfig(conf string, dir string) (map[string]bool, error) {
	// In case of first launch, we might have to create the directory.
	_ = os.MkdirAll(dir, 0755)
	c := 0

	currentFiles := map[string]bool{}

	walker := func(p string, d fs.DirEntry, e error) error {
		if e != nil {
			return fmt.Errorf("could not walk %s in config directory: %w", p, e)
		}

		if d.Type()&fs.ModeSymlink != 0 {
			target, err := os.Readlink(p)
			if err != nil {
				return fmt.Errorf("could not read link for %s: %w", p, err)
			}

			if !strings.HasPrefix(target, conf) {
				err = os.Remove(p)
				c++
				if err != nil {
					return fmt.Errorf("could not remove stale link %q: %w", p, err)
				}
				log.Printf("removed stale symlink %q", p)
			} else {
				currentFiles[p] = false
			}
		}

		if d.Type().IsRegular() {
			currentFiles[p] = true
		}

		return nil
	}

	err := filepath.WalkDir(dir, walker)
	if err != nil {
		return nil, err
	}

	if c > 0 {
		log.Printf("removed %v stale symlinks", c)
	}

	return currentFiles, nil
}

// linkConfig traverses the given Eagle Mode configuration and links everything
// to the expected location in the user's configuration directory.
//
// If the user placed actual files in the configuration directory at paths that
// would be overwritten, they will not be touched.
func linkConfig(conf string, dir string, existing map[string]bool) error {
	walker := func(p string, d fs.DirEntry, e error) error {
		if e != nil {
			return fmt.Errorf("could not walk %s in config directory: %w", p, e)
		}

		target := path.Join(dir, strings.TrimPrefix(p, conf))

		if d.Type().IsDir() {
			err := os.MkdirAll(target, 0755)
			if err != nil {
				return fmt.Errorf("could not create directory %q: %w", target, err)
			}

			return nil
		}

		if shadow, exists := existing[target]; exists {
			if shadow {
				log.Printf("WARN: file %q already exists and shadows a file from configuration", target)
			}

			return nil
		}

		err := os.Symlink(p, target)
		if err != nil {
			return fmt.Errorf("failed to link %q: %w", target, err)
		}

		return nil
	}

	return filepath.WalkDir(conf, walker)
}

func main() {
	emConfig := flag.String("em-config", "", "path to em-config dir")

	flag.Parse()
	log.Println("verifying current Eagle Mode configuration")

	if *emConfig == "" {
		log.Fatalf("Eagle Mode configuration must be given")
	}

	if !strings.HasPrefix(*emConfig, "/nix/store/") {
		log.Fatalf("Eagle Mode configuration must be in Nix store")
	}

	dir, err := configDir()
	if err != nil {
		log.Fatalf("could not determine Eagle Mode config dir: %v", err)
	}

	currentFiles, err := cleanupConfig(*emConfig, dir)
	if err != nil {
		log.Fatalf("failed to remove stale symlinks: %v", err)
	}

	err = linkConfig(*emConfig, dir, currentFiles)
	if err != nil {
		log.Fatalf("failed to link new configuration: %v", err)
	}

	log.Println("Eagle Mode configuration updated")
}
