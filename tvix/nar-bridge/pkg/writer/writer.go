package writer

import (
	"fmt"
	"io"

	storev1pb "code.tvl.fyi/tvix/store/protos"
	"github.com/nix-community/go-nix/pkg/nar"
)

type DirectoryLookupFn func([]byte) (*storev1pb.Directory, error)
type BlobLookupFn func([]byte) (io.ReadCloser, error)

// Export will traverse a given pathInfo structure, and write the contents
// in NAR format to the passed Writer.
// It uses directoryLookupFn and blobLookupFn to resolve references.
func Export(
	w io.Writer,
	pathInfo *storev1pb.PathInfo,
	directoryLookupFn DirectoryLookupFn,
	blobLookupFn BlobLookupFn,
) error {
	// initialize a NAR writer
	narWriter, err := nar.NewWriter(w)
	if err != nil {
		return fmt.Errorf("unable to initialize nar writer: %w", err)
	}
	defer narWriter.Close()

	// populate rootHeader
	rootHeader := &nar.Header{
		Path: "/",
	}

	// populate a stack
	// we will push paths and directories to it when entering a directory,
	// and emit individual elements to the NAR writer, draining the Directory object.
	// once it's empty, we can pop it off the stack.
	var stackPaths = []string{}
	var stackDirectories = []*storev1pb.Directory{}

	// peek at the pathInfo root and assemble the root node and write to writer
	// in the case of a regular file, we retrieve and write the contents, close and exit
	// in the case of a symlink, we write the symlink, close and exit
	switch v := (pathInfo.Node).(type) {
	case *storev1pb.PathInfo_File:
		rootHeader.Type = nar.TypeRegular
		rootHeader.Size = int64(v.File.GetSize())
		rootHeader.Executable = v.File.GetExecutable()
		narWriter.WriteHeader(rootHeader)

		// if it's a regular file, retrieve and write the contents
		contentReader, err := blobLookupFn(v.File.GetDigest())
		if err != nil {
			return fmt.Errorf("unable to lookup blob: %w", err)
		}
		defer contentReader.Close()

		_, err = io.Copy(narWriter, contentReader)
		if err != nil {
			return fmt.Errorf("unable to copy contents to contentReader: %w", err)
		}

		contentReader.Close()
		narWriter.Close()

		return nil

	case *storev1pb.PathInfo_Symlink:
		rootHeader.Type = nar.TypeSymlink
		rootHeader.LinkTarget = v.Symlink.GetTarget()
		narWriter.WriteHeader(rootHeader)
		narWriter.Close()
		return nil
	case *storev1pb.PathInfo_Directory:
		// We have a directory at the root, look it up and put in on the stack.
		directory, err := directoryLookupFn(v.Directory.Digest)
		if err != nil {
			return fmt.Errorf("unable to lookup directory: %w", err)
		}
		stackDirectories = append(stackDirectories, directory)
		stackPaths = append(stackPaths, "/")
	}

	// as long as the stack is not empty, we keep running.
	for {
		if len(stackDirectories) == 0 {
			return nil
		}

		// Peek at the current top of the stack.

		topOfStack := stackDirectories[len(stackDirectories)-1]
		topOfStackPath := stackPaths[len(stackPaths)-1]

		// If it's already drained, we can emit itself, and pop it from the stack.
		if len(topOfStack.GetDirectories()) == 0 && len(topOfStack.GetFiles()) == 0 && len(topOfStack.GetSymlinks()) == 0 {
			// emit it
			err := narWriter.WriteHeader(&nar.Header{
				Path: topOfStackPath,
				Type: nar.TypeDirectory,
			})

			if err != nil {
				return fmt.Errorf("error writing header: %w", err)
			}

			// pop off stack
			stackDirectories = stackDirectories[:len(stackDirectories)-1]
			stackPaths = stackPaths[:len(stackPaths)-1]
		}
	}
}