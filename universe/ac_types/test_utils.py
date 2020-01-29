silent_on_success = True


# I need to define this herein to avoid introducing a circular dependency.
def with_banner(x):
    header = '#################################################################'
    text = '# {}'.format(x)
    footer = '#################################################################'
    return '\n'.join([header, text, footer])


def simple_assert(actual, expected, name=None):
    try:
        assert actual == expected
        if silent_on_success:
            return None
        else:
            print(with_banner('{}: Test passes!'.format(name)))
    except:
        print(with_banner('{}: Test failure.'.format(name)))
        print(actual)
