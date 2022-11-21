package reader_test

import (
	"context"
	"io"
	"io/ioutil"
	"os"
	"testing"

	"code.tvl.fyi/tvix/nar-bridge/pkg/reader"
	storev1pb "code.tvl.fyi/tvix/store/protos"
	"github.com/google/go-cmp/cmp"
	"github.com/stretchr/testify/require"
	"google.golang.org/protobuf/testing/protocmp"
)

func requireProtoEq(t *testing.T, expected interface{}, actual interface{}) {
	if diff := cmp.Diff(expected, actual, protocmp.Transform()); diff != "" {
		t.Errorf("unexpected difference:\n%v", diff)
	}
}

func TestSymlink(t *testing.T) {
	f, err := os.Open("../../testdata/symlink.nar")
	require.NoError(t, err)

	r := reader.New(f)

	actualPathInfo, err := r.Import(
		context.Background(),
		func(fileReader io.Reader) error {
			panic("no file contents expected!")
		}, func(directory *storev1pb.Directory) error {
			panic("no directories expected!")
		},
	)
	require.NoError(t, err)

	expectedPathInfo := &storev1pb.PathInfo{
		Node: &storev1pb.PathInfo_Symlink{
			Symlink: &storev1pb.SymlinkNode{
				Name:   "",
				Target: "/nix/store/somewhereelse",
			},
		},
		References: [][]byte{},
		Narinfo: &storev1pb.NARInfo{
			NarSize: 136,
			NarHashes: []*storev1pb.NARInfo_NarHash{
				{
					Algo: storev1pb.NARInfo_SHA256,
					Digest: []byte{
						0x09, 0x7d, 0x39, 0x7e, 0x9b, 0x58, 0x26, 0x38, 0x4e, 0xaa, 0x16, 0xc4, 0x57, 0x71, 0x5d, 0x1c, 0x1a, 0x51, 0x67, 0x03, 0x13, 0xea, 0xd0, 0xf5, 0x85, 0x66, 0xe0, 0xb2, 0x32, 0x53, 0x9c, 0xf1,
					},
				},
				{
					Algo: storev1pb.NARInfo_SHA512,
					Digest: []byte{
						0x10, 0x45, 0x22, 0xb5, 0x59, 0x9e, 0x95, 0x53, 0xd9, 0x4d, 0x05, 0x15, 0x01, 0x84, 0x70, 0x17, 0xe2, 0xbb, 0x50, 0x55, 0x3a, 0x2e, 0x95, 0x14, 0x53, 0x56, 0x80, 0xbb, 0x1e, 0xa3, 0x12, 0x5f, 0x65, 0xbc, 0xc5, 0xa6, 0x99, 0xeb, 0x12, 0xff, 0xca, 0x05, 0xdc, 0x4e, 0xbd, 0x94, 0xef, 0x13, 0x32, 0xfe, 0x44, 0x40, 0x5c, 0x54, 0x50, 0x72, 0xa5, 0x29, 0x83, 0x00, 0x81, 0x15, 0x7e, 0xb2,
					},
				},
			},
			Signatures:     []*storev1pb.NARInfo_Signature{},
			ReferenceNames: []string{},
		},
	}

	requireProtoEq(t, expectedPathInfo, actualPathInfo)
}

func TestRegular(t *testing.T) {
	f, err := os.Open("../../testdata/onebyteregular.nar")
	require.NoError(t, err)

	r := reader.New(f)

	actualPathInfo, err := r.Import(
		context.Background(),
		func(fileReader io.Reader) error {
			contents, err := ioutil.ReadAll(fileReader)
			require.NoError(t, err, "reading fileReader should not error")
			require.Equal(t, []byte{0x01}, contents, "contents read from fileReader should match expectations")
			return nil
		}, func(directory *storev1pb.Directory) error {
			panic("no directories expected!")
		},
	)
	require.NoError(t, err)

	// The blake3 digest of the 0x01 byte.
	BLAKE3_DIGEST_0X01 := []byte{
		0x48, 0xfc, 0x72, 0x1f, 0xbb, 0xc1, 0x72, 0xe0, 0x92, 0x5f, 0xa2, 0x7a, 0xf1, 0x67, 0x1d,
		0xe2, 0x25, 0xba, 0x92, 0x71, 0x34, 0x80, 0x29, 0x98, 0xb1, 0x0a, 0x15, 0x68, 0xa1, 0x88,
		0x65, 0x2b,
	}

	expectedPathInfo := &storev1pb.PathInfo{
		Node: &storev1pb.PathInfo_File{
			File: &storev1pb.FileNode{
				Name:       "",
				Digest:     BLAKE3_DIGEST_0X01,
				Size:       1,
				Executable: false,
			},
		},
		References: [][]byte{},
		Narinfo: &storev1pb.NARInfo{
			NarSize: 120,
			NarHashes: []*storev1pb.NARInfo_NarHash{
				{
					Algo: storev1pb.NARInfo_SHA256,
					Digest: []byte{
						0x73, 0x08, 0x50, 0xa8, 0x11, 0x25, 0x9d, 0xbf, 0x3a, 0x68, 0xdc, 0x2e, 0xe8, 0x7a, 0x79, 0xaa, 0x6c, 0xae, 0x9f, 0x71, 0x37, 0x5e, 0xdf, 0x39, 0x6f, 0x9d, 0x7a, 0x91, 0xfb, 0xe9, 0x13, 0x4d,
					},
				},
				{
					Algo: storev1pb.NARInfo_SHA512,
					Digest: []byte{
						0xbc, 0xae, 0x02, 0x42, 0x5f, 0xb7, 0x1c, 0x76, 0xd3, 0x9e, 0x46, 0x21, 0x15, 0xa4, 0xeb, 0x73, 0xf5, 0x4e, 0xca, 0xd0, 0x19, 0xc0, 0x68, 0xb6, 0x32, 0x3f, 0x99, 0x05, 0x21, 0xcd, 0xd5, 0x94, 0x2b, 0x7e, 0xa8, 0x77, 0x91, 0x94, 0xfe, 0xf7, 0xb4, 0x0c, 0xa6, 0x81, 0xbd, 0xe6, 0xf8, 0xf9, 0x89, 0xd3, 0xe0, 0x6d, 0xe2, 0xa5, 0xdb, 0x98, 0x1c, 0x65, 0x53, 0x0b, 0x4a, 0x87, 0x70, 0xab,
					},
				},
			},
			Signatures:     []*storev1pb.NARInfo_Signature{},
			ReferenceNames: []string{},
		},
	}

	requireProtoEq(t, expectedPathInfo, actualPathInfo)
}

func TestEmptyDirectory(t *testing.T) {
	f, err := os.Open("../../testdata/emptydirectory.nar")
	require.NoError(t, err)

	r := reader.New(f)

	expectedDirectory := &storev1pb.Directory{
		Directories: []*storev1pb.DirectoryNode{},
		Files:       []*storev1pb.FileNode{},
		Symlinks:    []*storev1pb.SymlinkNode{},
	}
	expectedDirectoryDgst, err := expectedDirectory.Digest()
	require.NoError(t, err)

	actualPathInfo, err := r.Import(
		context.Background(),
		func(fileReader io.Reader) error {
			panic("no file contents expected!")
		}, func(directory *storev1pb.Directory) error {
			requireProtoEq(t, expectedDirectory, directory)
			return nil
		},
	)
	require.NoError(t, err)

	expectedPathInfo := &storev1pb.PathInfo{
		Node: &storev1pb.PathInfo_Directory{
			Directory: &storev1pb.DirectoryNode{
				Name:   "",
				Digest: expectedDirectoryDgst,
				Size:   expectedDirectory.Size(),
			},
		},
		References: [][]byte{},
		Narinfo: &storev1pb.NARInfo{
			NarSize: 96,
			NarHashes: []*storev1pb.NARInfo_NarHash{
				{
					Algo: storev1pb.NARInfo_SHA256,
					Digest: []byte{
						0xa5, 0x0a, 0x5a, 0xb6, 0xd9, 0x92, 0xf5, 0x59, 0x8e, 0xdd, 0x92, 0x10, 0x50, 0x59, 0xfa, 0xe9, 0xac, 0xfc, 0x19, 0x29, 0x81, 0xe0, 0x8b, 0xd8, 0x85, 0x34, 0xc2, 0x16, 0x7e, 0x92, 0x52, 0x6a,
					},
				},
				{
					Algo: storev1pb.NARInfo_SHA512,
					Digest: []byte{
						0x4e, 0x3e, 0x81, 0xcd, 0xed, 0x54, 0xd8, 0xde, 0x37, 0xcf, 0x74, 0x92, 0xc6, 0x96, 0xfd, 0xdb, 0x5d, 0x4c, 0x2c, 0xe5, 0x22, 0xdf, 0x4a, 0x72, 0x3c, 0x0c, 0xb4, 0x7c, 0x51, 0x32, 0x1f, 0x90, 0x7d, 0xdd, 0xb1, 0x61, 0xcf, 0x76, 0x0b, 0x5a, 0xcb, 0xeb, 0xa1, 0xde, 0xa3, 0x77, 0x3c, 0x4e, 0x4e, 0x15, 0xe8, 0x37, 0x8f, 0x74, 0x27, 0xd4, 0xf9, 0x1d, 0xd9, 0xb5, 0xcc, 0xc2, 0x1a, 0x34,
					},
				},
			},
			Signatures:     []*storev1pb.NARInfo_Signature{},
			ReferenceNames: []string{},
		},
	}
	requireProtoEq(t, expectedPathInfo, actualPathInfo)
}

func TestFull(t *testing.T) {
	f, err := os.Open("../../testdata/nar_1094wph9z4nwlgvsd53abfz8i117ykiv5dwnq9nnhz846s7xqd7d.nar")
	require.NoError(t, err)

	r := reader.New(f)

	actualPathInfo, err := r.Import(
		context.Background(),
		func(fileReader io.Reader) error {
			// TODO: keep track
			return nil
		}, func(directory *storev1pb.Directory) error {
			// TODO: keep track
			return nil
		},
	)
	require.NoError(t, err)

	expectedPathInfo := &storev1pb.PathInfo{
		Node:       nil,
		References: [][]byte{},
		Narinfo: &storev1pb.NARInfo{
			NarSize: 464152,
			NarHashes: []*storev1pb.NARInfo_NarHash{

				{
					Algo: storev1pb.NARInfo_SHA256,
					Digest: []byte{
						0xc6, 0xe1, 0x55, 0xb3, 0x45, 0x6e, 0x30, 0xb7, 0x61, 0x22, 0x63, 0xec, 0x09, 0x50, 0x70, 0x81, 0x1c, 0xaf, 0x8a, 0xbf, 0xd5, 0x9f, 0xaa, 0x72, 0xab, 0x82, 0xa5, 0x92, 0xef, 0xde, 0xb2, 0x53,
					},
				},
				{
					Algo: storev1pb.NARInfo_SHA512,
					Digest: []byte{
						0xf2, 0x48, 0x79, 0xa1, 0xee, 0x71, 0x89, 0x13, 0xd5, 0x90, 0xde, 0x5a, 0x47, 0x05, 0x62, 0xf0, 0x32, 0x48, 0x21, 0xd6, 0x1c, 0x38, 0x7b, 0x9a, 0xf4, 0x5e, 0x64, 0x39, 0xb7, 0x1a, 0x2a, 0xaf, 0x36, 0x83, 0xd5, 0x4d, 0x31, 0xf7, 0x9d, 0x77, 0xc7, 0x8e, 0xd1, 0x87, 0x6a, 0x55, 0x1c, 0xe4, 0x90, 0x30, 0x7a, 0xde, 0x80, 0x4b, 0x63, 0x54, 0x5b, 0xc2, 0x3e, 0x72, 0xa7, 0x25, 0x70, 0x41,
					},
				},
			},
			Signatures:     []*storev1pb.NARInfo_Signature{},
			ReferenceNames: []string{},
		},
	}
	requireProtoEq(t, expectedPathInfo, actualPathInfo)
}

// TODO: test reader not advancing is still fine
// TODO: test for cb function error being propagated