package main

import (
	"context"
	"flag"
	"fmt"
	"io"
	"log"
	"os"
	"syscall"
	"time"

	"bazil.org/fuse"
	"bazil.org/fuse/fs"
	"github.com/go-git/go-billy/v5/osfs"
	"github.com/go-git/go-git/v5"
	"github.com/go-git/go-git/v5/plumbing"
	"github.com/go-git/go-git/v5/plumbing/filemode"
	"github.com/go-git/go-git/v5/plumbing/object"
	"github.com/go-git/go-git/v5/storage/filesystem"
)

func usage() {
	fmt.Fprintln(os.Stderr, "hi")
}

func main() {
	flag.Usage = usage
	flag.Parse()

	if flag.NArg() != 2 {
		usage()
		os.Exit(2)
	}
	mountpoint := flag.Arg(0)
	gitDir := flag.Arg(1)

	nextInode = make(chan uint64)
	go inodeMaker()

	fs := osfs.New(gitDir)
	storage := filesystem.NewStorage(fs)
	repo := git.Open(storage, nil)

	c, err := fuse.Mount(
		mountpoint,
		fuse.FSName("helloworld"),
		fuse.Subtype("hellofs"),
	)
	if err != nil {
		log.Fatal(err)
	}
	defer c.Close()

	err = fs.Serve(c, &FS{
		gfs: &GFS{
			repo: repo,
		},
	})
	if err != nil {
		log.Fatal(err)
	}
}

func makeContext() (context.Context, func()) {
	return context.WithTimeout(context.Background(), 50*time.Millisecond)
}

var nextInode chan uint64

const dynINodeStart = 0x1000_0000_0000_0000

func inodeMaker() {
	var i uint64 = dynINodeStart
	for {
		i++
		nextInode <- i
	}
}

type GFS struct {
	repo        *git.Repository
	overlayRoot string

	uid, gid  uint32
	blocksize uint32
}

type FS struct {
	gfs *GFS
}

func (root *FS) Root() (fs.Node, error) {
	headRef, err := root.gfs.repo.Head()
	if err != nil {
		return nil, err // TODO
	}
	headCommit, err := root.gfs.repo.CommitObject(headRef.Hash())
	if err != nil {
		return nil, err // TODO
	}
	headTree, err := root.gfs.repo.TreeObject(headCommit.TreeHash)
	if err != nil {
		return nil, err // TODO
	}

	return &RepoRoot{
		Tree: Tree{
			gfs:    root.gfs,
			t:      headTree,
			inode:  <-nextInode,
			mypath: "",
		},
	}, nil
}

type RepoRoot struct {
	Tree
}

type Tree struct {
	gfs    *GFS
	inode  uint64
	mode   filemode.FileMode
	t      *object.Tree
	mypath string
}

func (t *Tree) Attr(ctx context.Context, attr *fuse.Attr) error {
	var err error
	attr.Inode = t.inode
	attr.Size = uint64(len(t.t.Entries))
	attr.Mode, err = t.mode.ToOSFileMode()
	if err != nil {
		return fmt.Errorf("stat %s: %w", t.mypath, err)
	}
	attr.Uid = t.gfs.uid
	attr.Gid = t.gfs.gid
	attr.BlockSize = readDirChunkSize

	return nil
}

func (t *Tree) Lookup(ctx context.Context, name string) (fs.Node, error) {
	ent, err := t.t.FindEntry(name)
	if err == object.ErrEntryNotFound {
		return nil, syscall.ENOENT
	} else if err == object.ErrDirectoryNotFound {
		return nil, syscall.ENOENT
	} else if err != nil {
		return nil, err
	}

	if ent.Mode.IsFile() || ent.Mode == filemode.Symlink {
		b, err := t.gfs.repo.BlobObject(ent.Hash)
		if err == plumbing.ErrObjectNotFound {
			log.Printf("blob %s referenced from %s but not found", ent.Hash, t.mypath)
			return nil, syscall.ENOENT
		} else if err != nil {
			return nil, err
		}

		return &File{
			gfs:    t.gfs,
			inode:  fs.GenerateDynamicInode(t.inode, ent.Name),
			mode:   ent.Mode,
			b:      b,
			mypath: t.mypath + "/" + ent.Name,
		}, nil
	} else if ent.Mode == filemode.Dir {
		subtree, err := t.gfs.repo.TreeObject(ent.Hash)
		if err == plumbing.ErrObjectNotFound {
			log.Printf("tree %s referenced from %s but not found", ent.Hash, t.mypath)
			return nil, syscall.ENOENT
		} else if err != nil {
			return nil, err
		}

		return &Tree{
			gfs:    t.gfs,
			inode:  fs.GenerateDynamicInode(t.inode, ent.Name),
			mode:   ent.Mode,
			t:      subtree,
			mypath: t.mypath + "/" + ent.Name,
		}, nil
	} else if ent.Mode == filemode.Submodule {
		return nil, syscall.ENOTSUP
	} else {
		log.Printf("unknown file mode %s at path %s file %s", ent.Mode, t.mypath, name)
		return nil, syscall.ENOTSUP
	}
}

// Open prepares a Readdir handle for a Tree.
func (t *Tree) Open(ctx context.Context, req *fuse.OpenRequest, resp *fuse.OpenResponse) (fs.Handle, error) {
	if !req.Dir {
		return nil, syscall.EISDIR
	}
	return &TreeHandle{
		tree: t,
		iter: nil,
	}, nil
}

type TreeHandle struct {
	tree *Tree
	iter *object.TreeWalker
	seen map[plumbing.Hash]bool

	nextOffset int64
	// If a dirent does not fit, it is stashed here.
	pendingName  string
	pendingEntry object.TreeEntry
}

func modeToDirentType(mode filemode.FileMode) fuse.DirentType {
	switch mode {
	case filemode.Dir:
		return fuse.DT_Dir
	case filemode.Symlink:
		return fuse.DT_Link
	case filemode.Submodule:
		return fuse.DT_Dir
	}
	if mode.IsFile() {
		return fuse.DT_File
	}
	return fuse.DT_Unknown
}

const readDirChunkSize = 4096

func (th *TreeHandle) Read(ctx context.Context, req *fuse.ReadRequest, resp *fuse.ReadResponse) error {
	if req.Offset == 0 || th.iter == nil {
		th.seen = make(map[plumbing.Hash]bool)
		th.iter = object.NewTreeWalker(th.tree.t, false, th.seen)
	} else {
		// resuming read - need to go in exact order
		if req.Offset != th.nextOffset {
			return syscall.ESTALE
		}
	}

	const direntSize = 8 + 8 + 4 + 4 // plus name
	var data = make([]byte, 0, readDirChunkSize)

	appendDirent := func(name string, ent object.TreeEntry) {
		data = fuse.AppendDirent(data, fuse.Dirent{
			Inode: fs.GenerateDynamicInode(th.tree.inode, ent.Name),
			Type:  modeToDirentType(ent.Mode),
			Name:  ent.Name,
		})
	}

	if th.pendingName != "" {
		appendDirent(th.pendingName, th.pendingEntry)
		th.pendingName = ""
		th.pendingEntry = object.TreeEntry{}
	}

	var (
		name  string
		entry object.TreeEntry
		err   error
	)

	for {
		name, entry, err = th.iter.Next()
		if err == io.EOF {
			break
		}

		if len(data)+direntSize+len(name) > readDirChunkSize {
			th.pendingName = name
			th.pendingEntry = entry
			break
		}
		appendDirent(name, entry)
	}

	resp.Data = data
	th.nextOffset = req.Offset + int64(len(resp.Data))
	return nil
}

type File struct {
	gfs    *GFS
	inode  uint64
	mode   filemode.FileMode
	hash   object.Hash
	b      *object.Blob
	mypath string
}

func (f *File) Attr(ctx context.Context, attr *fuse.Attr) error {
	var err error
	attr.Inode = f.inode
	attr.Size = uint64(f.b.Size)
	attr.Mode, err = f.mode.ToOSFileMode()
	if err != nil {
		return fmt.Errorf("stat %s: %w", f.mypath, err)
	}
	attr.Uid = f.gfs.uid
	attr.Gid = f.gfs.gid
	attr.BlockSize = f.gfs.blocksize

	return nil
}
