package server

import (
	"fmt"
	nixhash "github.com/nix-community/go-nix/pkg/hash"
)

// parseNarHashFromUrl parses a nixbase32 string representing a NarHash in either sha256 or sha512 (depending on the length),
// and returns a nixhash.Hash when it was able to parse, or an error.
func parseNarHashFromUrl(narHashFromUrl string) (*nixhash.Hash, error) {
	// peek at the length. If it's 52 characters, assume sha256,
	// if it's 103 characters, it's sha512
	var hashTypeString string
	l := len(narHashFromUrl)
	if l == 52 {
		hashTypeString = "sha256"
	} else if l == 103 {
		hashTypeString = "sha512"
	} else {
		return nil, fmt.Errorf("invalid length of narHash: %v", l)
	}

	nixHash, err := nixhash.ParseNixBase32(hashTypeString + ":" + narHashFromUrl)
	if err != nil {
		return nil, fmt.Errorf("unable to parse nixbase32 hash: %w", err)
	}

	return nixHash, nil
}
