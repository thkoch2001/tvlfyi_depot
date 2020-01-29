enable = False


def warn(x):
    """Print `x` as a warning."""
    if enable:
        print('[Warning]: {}'.format(x))
    else:
        return None
