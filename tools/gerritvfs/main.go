package main

import (
	"context"
	"errors"
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
	"github.com/go-git/go-git/v5/plumbing/cache"
	"github.com/go-git/go-git/v5/plumbing/filemode"
	"github.com/go-git/go-git/v5/plumbing/object"
	"github.com/go-git/go-git/v5/storage/filesystem"
)

func usage() {
	fmt.Fprintln(os.Stderr, "hi")
}

func checkMountpoint(mountpoint string) (os.FileMode, error) {
	stat, err := os.Stat(mountpoint)
	if os.IsNotExist(err) {
		err := os.Mkdir(mountpoint, 0o755)
		if err != nil {
			return 0, err
		}
		return 0o755, nil
	} else if errors.Is(err, syscall.EIO) || errors.Is(err, syscall.ENOTCONN) {
		fmt.Fprintf(os.Stderr, "unmounting previous driver of %q\n", mountpoint)
		err = fuse.Unmount(mountpoint)
		if err != nil {
			return 0, fmt.Errorf("unmount: %w", err)
		}
		return checkMountpoint(mountpoint)
	} else if err != nil {
		return 0, err
	} else if !stat.IsDir() {
		return 0, fmt.Errorf("%q is not a directory", mountpoint)
	} else {
		f, err := os.Open(mountpoint)
		if err != nil {
			return 0, err
		}
		defer f.Close()
		names, rdErr := f.Readdirnames(3)
		if rdErr == io.EOF {
			// Nothing in the directory
			return stat.Mode(), nil
		} else if rdErr != nil {
			return 0, rdErr
		}
		isEmpty := true
		for _, v := range names {
			if v != "." && v != ".." {
				isEmpty = false
				break
			}
		}

		if !isEmpty {
			fmt.Fprint(os.Stderr, "WARNING: Mountpoint is not empty\n"+
				"Press CTRL-C within 3 seconds to cancel mounting\n")
			time.Sleep(3 * time.Second)
		}
		return stat.Mode(), nil
	}
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

	fuse.Debug = func(msg interface{}) {
		log.Println("fuse-debug", msg)
	}

	nextInode = make(chan uint64)
	go inodeMaker()

	gitSourceFs := osfs.New(gitDir)
	objCache := cache.NewObjectLRU(1 << 20)
	storage := filesystem.NewStorage(gitSourceFs, objCache)
	repo, err := git.Open(storage, nil)
	if err != nil {
		fmt.Fprintf(os.Stderr, "error opening git repo: %v\n", err)
		os.Exit(1)
	}

	dirMode, err := checkMountpoint(mountpoint)
	if err != nil {
		fmt.Fprintf(os.Stderr, "checking mountpoint: %v\n", err)
		os.Exit(1)
	}

	c, err := fuse.Mount(
		mountpoint,
		fuse.FSName("helloworld"),
		fuse.Subtype("hellofs"),
	)
	if err != nil {
		log.Fatal(err)
	}
	defer c.Close()

	gfs := &GFS{
		repo:      repo,
		uid:       uint32(os.Getuid()),
		gid:       uint32(os.Getgid()),
		umask:     dirMode,
		blocksize: 4096,
	}
	err = fs.Serve(c, &FS{gfs: gfs})
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
	umask     os.FileMode
	blocksize uint32
}

func (g *GFS) FilterMode(m os.FileMode) os.FileMode {
	return (m &^ os.ModePerm) | ((m & g.umask) & os.ModePerm)
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
			mode:   filemode.Dir,
			inode:  1,
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
	log.Printf("stat %q", t.mypath)
	attr.Inode = t.inode
	attr.Size = uint64(len(t.t.Entries))
	mode, err := t.mode.ToOSFileMode()
	if err != nil {
		return fmt.Errorf("stat %s: %w", t.mypath, err)
	}
	attr.Mode = t.gfs.FilterMode(mode)
	attr.Uid = t.gfs.uid
	attr.Gid = t.gfs.gid

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

func (t *Tree) ReadDirAll(ctx context.Context) ([]fuse.Dirent, error) {
	var (
		name string
		ent  object.TreeEntry
		err  error

		data []fuse.Dirent
	)

	seen := make(map[plumbing.Hash]bool)
	iter := object.NewTreeWalker(t.t, false, seen)
	defer iter.Close()

	/*
	 * "In the current implementation any objects which cannot be found in
	 * the underlying repository will be skipped automatically. It is
	 * possible that this may change in future versions."
	 */

	for {
		name, ent, err = iter.Next()
		if err == io.EOF {
			break
		} else if err != nil {
			return data, err
		}

		data = append(data, fuse.Dirent{
			Inode: fs.GenerateDynamicInode(t.inode, name),
			Type:  modeToDirentType(ent.Mode),
			Name:  ent.Name,
		})
	}

	return data, nil
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
	m, err := f.mode.ToOSFileMode()
	if err != nil {
		return fmt.Errorf("stat %s: %w", f.mypath, err)
	}
	attr.Mode = f.gfs.FilterMode(m)
	attr.Uid = f.gfs.uid
	attr.Gid = f.gfs.gid
	attr.BlockSize = f.gfs.blocksize

	return nil
}
