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
	directoriesLookupFn DirectoryLookupFn,
	blobLookupFn BlobLookupFn,
) error {
	// initialize a NAR Writer
	narWriter, err := nar.NewWriter(w)
	if err != nil {
		return fmt.Errorf("unable to initialize nar writer: %w", err)
	}
	defer narWriter.Close()

	// populate rootHeader
	rootHeader := &nar.Header{
		Path: "/",
	}
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
		rootHeader.Type = nar.TypeDirectory
		narWriter.WriteHeader(rootHeader)
	}

	// We have a directory at the root, so need to traverse the directory
	// structure here.
	// TODO: implement
	return nil
}