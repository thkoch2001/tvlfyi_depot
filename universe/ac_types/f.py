import os
import string
from test_utils import simple_assert


def join(*args):
    return os.path.join(*args)


simple_assert(join("/", "tmp", "a.txt"), "/tmp/a.txt", name="join")


def ensure_absolute(path):
    """Ensures `path` is an absolute path."""
    return os.path.abspath(os.path.expanduser(path))


simple_assert(ensure_absolute("~/a.txt"),
              "/usr/local/google/home/wpcarro/a.txt",
              name="ensure_absolute")


def filename(path):
    """Return just the filename of `path`."""
    return os.path.basename(path)


simple_assert(filename("~/a.txt"), "a.txt", name="filename")
simple_assert(filename("/path/to/file"), "file", name="filename")


def strip_extension(path):
    """Remove file extension from path."""
    return os.path.splitext(path)[0]


simple_assert(strip_extension("~/a.txt"), "~/a", name="filename")
simple_assert(strip_extension("/path/to/file.txt"),
              "/path/to/file",
              name="strip_extension")


def change_extension(ext, path):
    """Change `path`'s file extension to `ext`."""
    assert string.starts_with('.', ext)
    return strip_extension(path) + ext
