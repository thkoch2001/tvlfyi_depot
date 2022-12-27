package storev1

import (
	"fmt"
	"strings"

	"google.golang.org/protobuf/proto"
	"lukechampine.com/blake3"
)

// The size of a directory is the number of all regular and symlink elements,
// the number of directory elements, and their size fields.
func (d *Directory) Size() uint32 {
	var size uint32
	size = uint32(len(d.Files) + len(d.Symlinks))
	for _, d := range d.Directories {
		size += 1 + d.Size
	}
	return size
}

func (d *Directory) Digest() ([]byte, error) {
	b, err := proto.MarshalOptions{
		Deterministic: true,
	}.Marshal(d)

	if err != nil {
		return nil, fmt.Errorf("error while marshalling directory: %w", err)
	}

	h := blake3.New(32, nil)

	_, err = h.Write(b)
	if err != nil {
		return nil, fmt.Errorf("error writing to hasher: %w", err)
	}

	return h.Sum(nil), nil
}

// isValidName checks a name for validity.
// We disallow slashes, null bytes, . and ..
// Whether the empty string is fine or not is still up for debate.
func isValidName(n string) bool {
	if n == ".." || n == "." || strings.Contains(n, "\x00") || strings.Contains(n, "/") {
		return false
	}
	return true
}

// Validate thecks the Directory message for invalid data, such as:
// - violations of name restrictions
// - invalid digest lengths
// - not properly sorted lists
// - duplicate names in the three lists
func (d *Directory) Validate() error {
	// seenNames contains all seen names so far.
	// We populate this to ensure node names are unique across all three lists.
	seenNames := make(map[string]interface{})

	// We also populate three other lists, to ensure nodes are sorted by their
	// names.
	var seenDirectoryNames []string
	var seenFileNames []string
	var seenSymlinkNames []string

	// helper function to only insert in sorted order.
	// used with the three lists above.
	// Note this consumes a *pointer to* a slice as first argument,
	// as it mutates (inserts) into the slice.
	insertIfGt := func(names *[]string, name string) error {
		// if the slice is empty, add without check
		if len(*names) == 0 {
			*names = append(*names, name)
			return nil
		}

		// else, add if it's greater than the previous name
		if name > (*names)[len(*names)-1] {
			*names = append(*names, name)
			return nil
		} else {
			return fmt.Errorf("%v is not sorted", name)
		}
	}

	// insertOnce inserts into seenNames if the key doesn't exist yet.
	insertOnce := func(name string) error {
		if _, found := seenNames[name]; found {
			return fmt.Errorf("duplicate name: %v", name)
		}
		seenNames[name] = nil
		return nil
	}

	// Loop over all Directories, Files and Symlinks individually.
	// Check the name for validity, check a potential digest for length,
	// then check for sorting in the current list, and uniqueness across all three lists.
	for _, directoryNode := range d.Directories {
		directoryName := directoryNode.GetName()

		// check name for validity
		if !isValidName(directoryName) {
			return fmt.Errorf("invalid name for DirectoryNode: %v", directoryName)
		}

		// check digest to be 32 bytes
		digestLen := len(directoryNode.GetDigest())
		if digestLen != 32 {
			return fmt.Errorf("invalid digest length for DirectoryNode: %d", digestLen)
		}

		// add to seenDirectoryNames if sorted
		if err := insertIfGt(&seenDirectoryNames, directoryName); err != nil {
			return err
		}

		// add to seenNames
		if err := insertOnce(directoryName); err != nil {
			return err
		}

	}

	for _, fileNode := range d.Files {
		fileName := fileNode.GetName()

		// check name for validity
		if !isValidName(fileName) {
			return fmt.Errorf("invalid name for FileNode: %v", fileName)
		}

		// check digest to be 32 bytes
		digestLen := len(fileNode.GetDigest())
		if digestLen != 32 {
			return fmt.Errorf("invalid digest length for FileNode: %d", digestLen)
		}

		// add to seenFileNames if sorted
		if err := insertIfGt(&seenFileNames, fileName); err != nil {
			return err
		}

		// add to seenNames
		if err := insertOnce(fileName); err != nil {
			return err
		}
	}

	for _, symlinkNode := range d.Symlinks {
		symlinkName := symlinkNode.GetName()

		// check name for validity
		if !isValidName(symlinkName) {
			return fmt.Errorf("invalid name for SymlinkNode: %v", symlinkName)
		}

		// add to seenFileNames if sorted
		if err := insertIfGt(&seenSymlinkNames, symlinkName); err != nil {
			return err
		}

		// add to seenNames
		if err := insertOnce(symlinkName); err != nil {
			return err
		}
	}

	return nil
}
