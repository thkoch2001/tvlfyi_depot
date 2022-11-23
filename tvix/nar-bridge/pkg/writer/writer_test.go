package writer_test

import (
	"bytes"
	"io"
	"os"
	"testing"

	"code.tvl.fyi/tvix/nar-bridge/pkg/writer"
	storev1pb "code.tvl.fyi/tvix/store/protos"
	"github.com/stretchr/testify/require"
)

func mustDigest(d *storev1pb.Directory) []byte {
	dgst, err := d.Digest()
	if err != nil {
		panic(err)
	}
	return dgst
}

func TestSymlink(t *testing.T) {
	pathInfo := &storev1pb.PathInfo{
		Node: &storev1pb.PathInfo_Symlink{
			Symlink: &storev1pb.SymlinkNode{
				Name:   "doesntmatter",
				Target: "/nix/store/somewhereelse",
			},
		},
	}

	var buf bytes.Buffer

	err := writer.Export(&buf, pathInfo, func([]byte) (*storev1pb.Directory, error) {
		panic("no directories expected")
	}, func([]byte) (io.ReadCloser, error) {
		panic("no files expected")
	})
	require.NoError(t, err, "exporter shouldn't fail")

	f, err := os.Open("../../testdata/symlink.nar")
	require.NoError(t, err)

	bytesExpected, err := io.ReadAll(f)

	require.Equal(t, bytesExpected, buf.Bytes(), "expected nar contents to match")
}

func TestRegular(t *testing.T) {
	// The blake3 digest of the 0x01 byte.
	BLAKE3_DIGEST_0X01 := []byte{
		0x48, 0xfc, 0x72, 0x1f, 0xbb, 0xc1, 0x72, 0xe0, 0x92, 0x5f, 0xa2, 0x7a, 0xf1, 0x67, 0x1d,
		0xe2, 0x25, 0xba, 0x92, 0x71, 0x34, 0x80, 0x29, 0x98, 0xb1, 0x0a, 0x15, 0x68, 0xa1, 0x88,
		0x65, 0x2b,
	}

	pathInfo := &storev1pb.PathInfo{
		Node: &storev1pb.PathInfo_File{
			File: &storev1pb.FileNode{
				Name:       "doesntmatter",
				Digest:     BLAKE3_DIGEST_0X01,
				Size:       1,
				Executable: false,
			},
		},
	}

	var buf bytes.Buffer

	err := writer.Export(&buf, pathInfo, func([]byte) (*storev1pb.Directory, error) {
		panic("no directories expected")
	}, func(blobRef []byte) (io.ReadCloser, error) {
		if !bytes.Equal(blobRef, BLAKE3_DIGEST_0X01) {
			panic("unexpected blobref")
		}
		return io.NopCloser(bytes.NewBuffer([]byte{0x01})), nil
	})
	require.NoError(t, err, "exporter shouldn't fail")

	f, err := os.Open("../../testdata/onebyteregular.nar")
	require.NoError(t, err)

	bytesExpected, err := io.ReadAll(f)

	require.Equal(t, bytesExpected, buf.Bytes(), "expected nar contents to match")
}

func TestEmptyDirectory(t *testing.T) {
	// construct empty directory node this refers to
	emptyDirectory := &storev1pb.Directory{
		Directories: []*storev1pb.DirectoryNode{},
		Files:       []*storev1pb.FileNode{},
		Symlinks:    []*storev1pb.SymlinkNode{},
	}
	emptyDirectoryDigest := mustDigest(emptyDirectory)

	pathInfo := &storev1pb.PathInfo{
		Node: &storev1pb.PathInfo_Directory{
			Directory: &storev1pb.DirectoryNode{
				Name:   "doesntmatter",
				Digest: emptyDirectoryDigest,
				Size:   0,
			},
		},
	}

	var buf bytes.Buffer

	err := writer.Export(&buf, pathInfo, func(directoryRef []byte) (*storev1pb.Directory, error) {
		if !bytes.Equal(directoryRef, emptyDirectoryDigest) {
			panic("unexpected directoryRef")
		}
		return emptyDirectory, nil
	}, func([]byte) (io.ReadCloser, error) {
		panic("no files expected")
	})
	require.NoError(t, err, "exporter shouldn't fail")

	f, err := os.Open("../../testdata/emptydirectory.nar")
	require.NoError(t, err)

	bytesExpected, err := io.ReadAll(f)

	require.Equal(t, bytesExpected, buf.Bytes(), "expected nar contents to match")

}
