import re
from test_utils import simple_assert


def remove(regex, x):
    """Attempt to remove the substring matching `re` from `x`."""
    return re.sub(regex, '', x)


# No occurence
simple_assert(remove(r'\s\([\w\s]+\)$', 'Atlas.CustomerId'),
              'Atlas.CustomerId',
              name="remove")
# Single occurence
simple_assert(remove(r'\s\([\w\s]+\)$',
                     'Atlas.CustomerId (adjusted for MCC IDs)'),
              'Atlas.CustomerId',
              name="remove")
