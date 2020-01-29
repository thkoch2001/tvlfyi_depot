from glob import glob
import f


def ls(pattern):
    """Return a list of files that match `pattern`. This is a DWIM function and
    will handle relative paths, absolute paths, etc. It should behave
    approximately similarly to GNU's ls."""
    return glob(f.ensure_absolute(pattern))
