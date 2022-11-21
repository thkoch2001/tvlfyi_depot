package reader_test

import (
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

	actualPathInfo, err := r.Import()
	require.NoError(t, err)

	expectedPathInfo := &storev1pb.PathInfo{
		Node: &storev1pb.PathInfo_Symlink{
			Symlink: &storev1pb.SymlinkNode{
				Name:   "",
				Target: "/nix/store/somewhereelse",
			},
		},
		References: [][]byte{},
		Narinfo:    &storev1pb.NARInfo{},
	}

	requireProtoEq(t, expectedPathInfo, actualPathInfo)
}

func TestRegular(t *testing.T) {
	f, err := os.Open("../../testdata/onebyteregular.nar")
	require.NoError(t, err)

	r := reader.New(f)

	actualPathInfo, err := r.Import()
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
		Narinfo:    &storev1pb.NARInfo{},
	}

	requireProtoEq(t, expectedPathInfo, actualPathInfo)
}
