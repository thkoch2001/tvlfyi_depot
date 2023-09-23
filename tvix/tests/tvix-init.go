package main

import (
	"log"
	"os"
	"os/exec"
)

// run the given command, connecting std{in,err,out} with the OS one.
func run(args ...string) error {
	cmd := exec.Command(args[0], args[1:]...)
	cmd.Stdin = os.Stdin
	cmd.Stderr = os.Stderr
	cmd.Stdout = os.Stdout

	return cmd.Run()
}

func main() {
	log.Println("Running tvix-init…")

	log.Println("Creating /nix/store")
	os.MkdirAll("/nix/store", os.ModePerm)

	cmdline, err := os.ReadFile("/proc/cmdline")
	if err != nil {
		log.Printf("Failed to read cmdline: %s\n", err)
	}
	log.Printf("cmdline: %s", cmdline)

	log.Println("Mounting…")
	if err := run("mount", "-t", "virtiofs", "tvix", "/nix/store", "-o", "ro"); err != nil {
		log.Printf("Failed to run command: %v\n", err)
	}

	log.Println("Listing…")
	if err := run("find", "/nix/store"); err != nil {
		log.Printf("Failed to run command: %v\n", err)
	}

	log.Println("Powering off")
	if err := run("poweroff"); err != nil {
		log.Printf("Failed to run command: %v\n", err)
	}
}
