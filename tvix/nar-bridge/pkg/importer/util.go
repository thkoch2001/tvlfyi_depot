package importer

import (
	"fmt"

	castorev1pb "code.tvl.fyi/tvix/castore/protos"
	storev1pb "code.tvl.fyi/tvix/store/protos"
	"github.com/nix-community/go-nix/pkg/narinfo"
	"github.com/nix-community/go-nix/pkg/storepath"
)

// RenamedNode returns a node with a new name.
func RenamedNode(node *castorev1pb.Node, name string) *castorev1pb.Node {
	if directoryNode := node.GetDirectory(); directoryNode != nil {
		return &castorev1pb.Node{
			Node: &castorev1pb.Node_Directory{
				Directory: &castorev1pb.DirectoryNode{
					Name:   []byte(name),
					Digest: directoryNode.GetDigest(),
					Size:   directoryNode.GetSize(),
				},
			},
		}
	} else if fileNode := node.GetFile(); fileNode != nil {
		return &castorev1pb.Node{
			Node: &castorev1pb.Node_File{
				File: &castorev1pb.FileNode{
					Name:       []byte(name),
					Digest:     fileNode.GetDigest(),
					Size:       fileNode.GetSize(),
					Executable: fileNode.GetExecutable(),
				},
			},
		}
	} else if symlinkNode := node.GetSymlink(); symlinkNode != nil {
		return &castorev1pb.Node{
			Node: &castorev1pb.Node_Symlink{
				Symlink: &castorev1pb.SymlinkNode{
					Name:   []byte(name),
					Target: symlinkNode.GetTarget(),
				},
			},
		}
	} else {
		panic("unreachable")
	}

}
