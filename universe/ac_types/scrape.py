from test_utils import simple_assert
import re
import log
import regex


# Warning: This is a linear search each time. Quite scrappy and hackish, but it
# works. Trademarked...
def attribute_id_for(x, error_when_absent=True):
    """Return the Attribute ID for `x` as defined in attributes.pb."""
    is_next = False
    known = {
        'Form.description': '^2941',
        'Form.country': '^16296',
        # Entering this here since some of the CSV have malformed data, and I'm
        # not currently interested in a more robust solution.
        'form.country': '^16296',
        'Form.countries_business_serve': '^14659',
        'Form.name': '^1665',
    }
    if x in known:
        return known[x]

    for line in open('attributes.pb', 'r').readlines():
        if is_next:
            return line[7:-2]
        if x in line:
            is_next = True
        else:
            is_next = False
    if error_when_absent:
        raise Exception("Could not find \"{}\" in the protobuf.".format(x))
    else:
        return '{} # TODO(wpcarro): scrape.attribute_id_for could not find the ID.'.format(
            x)


actual = [
    attribute_id_for('Form.description'),
    attribute_id_for('Form.country'),
    attribute_id_for('Form.countries_business_serve'),
    attribute_id_for('Form.name'),
    attribute_id_for('Atlas.CustomerId'),
    attribute_id_for('Form.form-id'),
    attribute_id_for('Form.Ar_descr_textbox'),
    attribute_id_for('AR.ART.LastDecisionRecommended'),
]
expected = [
    '^2941', '^16296', '^14659', '^1665', '^1421', '^4297', '^6664', '^106918'
]
simple_assert(actual, expected)
