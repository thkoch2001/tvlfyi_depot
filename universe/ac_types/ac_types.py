from itertools import product
import string
from pretty import pretty_print
import csv
import parse
import serialize
import dict
import scrape
import fs
import f
import log
import regex
import input_constant
from test_utils import simple_assert

################################################################################
# Main
################################################################################
enable_tests = True


# parse_csv :: Path -> [RowAsDict]
def parse_csv(path):
    parser = {
        "Name": parse.required("Name", parse.identity),
        "Type": parse.required("Type", parse.as_type),
        # TODO: Are we sure we want to default to False here?
        "Optional": parse.if_empty(False, parse.as_yes_no),
        # We'd like to skip rows where "Data Source Type" is empty.
        "Data Source Type": parse.nullable(parse.as_data_source_type),
        "Data Source/Value": parse.nullable(parse.identity),
    }
    result = []
    # Below are only the column in which we're interested:
    columns = [
        'Name',
        'Type',
        'Optional',
        'Data Source Type',
        'Data Source/Value',
    ]
    assert set(columns) == set(parser.keys())
    with open(path, 'r') as f:
        reader = csv.DictReader(f)
        for row in reader:
            try:
                parsed = parse.apply_parser(parser, row)
                result.append(dict.take(columns, parsed))
            except Exception as e:
                pretty_print(row)
                raise Exception('Failed parsing the following row: {}'.format(
                    ','.join(row.values())))
                return

    return result


def serialize_id_value(xs):
    return string.indent('\n'.join([
        'id: {}'.format(serialize.literal(xs['id'])),
        'value: {}'.format(serialize.literal(xs['value'])),
    ]))


# serialize_parent_routing_frd :: RowAsDict -> [String]
def serialize_parent_routing_frd(row):
    # All parent_routing_frds should be set as Hard-Code
    assert row['Data Source Type'] == 'Hard-Code'

    frds = {
        'Consult Type': '^88149',
        'neoenum.program': '^87379',
        'form.country': '^16296',
        'Form.ad_language': "^3906"
    }
    name, value = row['Name'], parse.as_union_type(row['Data Source/Value'])

    result = []
    for x in value:
        header = 'parent_lookup_frds {'
        fields = serialize_id_value({'id': frds[name], 'value': x})
        footer = '}'
        result.append('\n'.join([header, fields, footer]))

    return result


actual = serialize_parent_routing_frd({
    'Name':
    'neoenum.program',
    'Data Source Type':
    'Hard-Code',
    'Data Source/Value':
    '"olympus" or "olympus_plus"',
})
expected = [
    """parent_lookup_frds {
  id: "^87379"
  value: "olympus"
}""",
    """parent_lookup_frds {
  id: "^87379"
  value: "olympus_plus"
}""",
]
simple_assert(actual, expected, name='serialize_parent_routing_frd')

actual = serialize_parent_routing_frd({
    'Name':
    'Consult Type',
    'Type':
    'Parent Routing FRD',
    'AIV':
    False,
    'Optional':
    False,
    'Data Source Type':
    'Hard-Code',
    'Data Source/Value':
    'ads_accountappeals_autoconsult'
})
expected = [
    """parent_lookup_frds {
  id: "^88149"
  value: "ads_accountappeals_autoconsult"
}"""
]
simple_assert(actual, expected, name='serialize_parent_routing_frd')


def serialize_return_routing_frd(row):
    header = 'parent_return_routing_frds {'
    fields = serialize_id_value({
        'id': row['Name'],
        'value': row['Data Source/Value'],
    })
    footer = '}'
    return '\n'.join([header, fields, footer])


def serialize_consult_routing_frd(row):
    header = 'consult_routing_frds {'
    fields = serialize_id_value({
        'id': row['Name'],
        'value': row['Data Source/Value'],
    })
    footer = '}'
    return '\n'.join([header, fields, footer])


# TODO: Reconcile this definition with serialize.input.
# serialize_inputs :: RowAsDict -> [String]
def serialize_input(row):
    value_type = row['Data Source Type']
    name, value = string.trim_prefix('IDENTIFIER_',
                                     row['Name']), row['Data Source/Value']

    if ' or ' in value and value_type != 'Hard-Code':
        log.warn('Found a union type in a non-"Hard-Code" row: {}'.format(row))

    # TODO: We need to resolve row['Name'] into "^<id-number>". But only
    # Sometimes... so... when is that sometimes?
    if value_type == 'Hard-Code':
        return serialize.input(
            input_type='CONSTANT',
            fields={
                'consult_frd_id': name,
                'is_optional': row['Optional'],
                # TODO: Call resolution function.
                'constant_value': input_constant.to_rule_id(value),
            })
    elif value_type == 'Atlas':
        # We need to remove the trailing parens if they exist. See the CSVs for more
        # context.
        value = regex.remove(r'\s\([\w\s]+\)$', value)
        return serialize.input(input_type='SIGNAL',
                               fields={
                                   'consult_frd_id': name,
                                   'is_optional': row['Optional'],
                               })
    elif value_type == 'Form':
        # TODO: Prefer a generic serialize.dictionary
        # We need to remove the trailing parens if they exist. See the CSVs for more
        # context.
        value = regex.remove(r'\s\([\w\s]+\)$', value)
        return serialize.input(input_type='PARENT_FRD',
                               fields={
                                   'consult_frd_id':
                                   name,
                                   'is_optional':
                                   row['Optional'],
                                   'parent_frd_id':
                                   scrape.attribute_id_for(
                                       value, error_when_absent=False),
                               })
    else:
        raise Exception("This should not have occurred.")


# csv_to_proto :: Path -> [Protobuf]
def csv_to_proto(path):
    """Maps the CSV located at `path` into a textproto that Auto Consult will consume."""
    # ORGANIZATION, which is currently a 'Consult Routing FRD' should become a
    # 'Consult Parameter' as "neo_organization".
    consult_routing_frds_blacklist = {'ORGANIZATION'}
    index = {
        'Parent Routing FRD': [],
        'Consult Parameter': [],
        'Return Routing FRD': [],
        'Consult Routing FRD': [],
        'Input': []
    }

    # Index each row according to its "Type" column.
    for row in parse_csv(path):
        name, rtype, dst = row['Name'], row['Type'], row['Data Source Type']
        # Here we need to mutate the spreadsheet because the curators encoded a
        # 'Consult Parameter', "ORGANIZATION", as a 'Consult Routing FRD'.
        if name == 'ORGANIZATION' and rtype == 'Consult Routing FRD':
            row['Type'] = 'Consult Parameter'
            index['Consult Parameter'].append(row)
            continue
        if dst is None:
            log.warn('Column "Data Source Type" is None. Skipping this row.')
            continue
        if dst == 'N/A':
            continue
        index[row['Type']].append(row)

    return serialize_index(index)


def serialize_consult_parameters(xs):
    result = []
    transforms = {
        'Taxonomy ID': 'taxonomy_id',
        'View ID': 'view_id',
        'Timeout': 'max_wait_time_for_consult_secs',
        'Re-Route if Customer Responds': 'reroute_on_customer_interaction',
        'ORGANIZATION': 'neo_organization'
    }
    parsers = {
        'Taxonomy ID':
        parse.identity,
        'View ID':
        parse.identity,
        'Timeout':
        lambda x: parse.as_hours(x) * 60 * 60,
        'Re-Route if Customer Responds':
        parse.as_mapping({
            'TRUE': True,
            'FALSE': False
        }),
        'ORGANIZATION':
        parse.identity
    }
    for row in xs:
        name = row['Name']
        parser = parsers[name]
        key, value = transforms[name], parser(row['Data Source/Value'])
        result.append('  {}: {}'.format(key, serialize.literal(value)))

    return '\n'.join(result)


def serialize_index(index):
    header = 'consult_settings {'

    consult_parameters = serialize_consult_parameters(
        index['Consult Parameter'])

    product_xs = []
    parent_lookup_frds = []
    for row in index['Parent Routing FRD']:
        product_xs.append(serialize_parent_routing_frd(row))
    for frds in product(*product_xs):
        parent_lookup_frds.append('\n'.join(frds))

    # TODO: Cover with tests.
    parent_return_routing_frds = string.indent('\n'.join([
        serialize_return_routing_frd(row)
        for row in index['Return Routing FRD']
    ]))

    # TODO: Cover with tests.
    consult_routing_frds = string.indent('\n'.join([
        serialize_consult_routing_frd(row)
        for row in index['Consult Routing FRD']
    ]))

    inputs = string.indent('\n'.join(
        [serialize_input(row) for row in index['Input']]))

    footer = '}'

    result = []
    for parent_frd in parent_lookup_frds:
        result.append('\n'.join([
            header, consult_parameters,
            string.indent(parent_frd), parent_return_routing_frds,
            consult_routing_frds, inputs, footer
        ]))

    return '\n'.join(result)


csv_directory = f.ensure_absolute("~/auto-consult-csv")
# TODO: Add missing files.
csvs = [
    'Non-Sensitive Ads Review (Olympus).csv',
    'Ads Review (Olympus).csv',
    'Ad Review (Non-Olympus).csv',
    'Review Account Under Review (Olympus).csv',
    'Accounts Review Requests (Non-Olympus).csv',
    'Review Suspended Account (Olympus).csv',
    'Review Suspended Account (Non-Olympus).csv',
    'Suspended Long Form (Olympus).csv',
    'Suspended Long Form (Non-Olympus).csv',
    'Copyright (Olympus).csv',
    'Copyright (Non-Olympus).csv',
    'EU Election Certification (Olympus).csv',
    'EU Election Certification #2 (Olympus).csv',
    'EU Election Certification (Non-Olympus).csv',
    'EU Election Certification #2 (Non-Olympus).csv',
    'US Election Certification (Olympus).csv',
    'US Election Certification (Non-Olympus).csv',
    'IN Election Certification (Olympus).csv',
    'IN Election Certification (Non-Olympus).csv',
    'NY Election Certification (Olympus).csv',
    'NY Election Certification (Non-Olympus).csv',
    'Ticket Seller Certification (Olympus).csv',
    'Ticket Seller Certification (Non-Olympus).csv',
    'Pharma Certification EMEA (Olympus).csv',
    'Pharma Certification EMEA (Non-Olympus).csv',
    'CSFP Certification (Olympus).csv',
    'CSFP Certification (Non-Olympus).csv',
    'Social Casino Games Certification (Olympus).csv',
    'Social Casino Games Certification (NonOlympus).csv',
    'Gambling Certification (Olympus).csv',
    'Gambling Certification (Non-Olympus).csv',
    'Addiction Services Certification (Olympus).csv',
    'Addiction Services Certification (Non-Olympus).csv',
    'HTML5 Application (Olympus).csv',
    'HTML5 Application (Non-Olympus).csv',
    # TODO: Support this once Jason unblocks.
    # 'Account Take Over (Olympus).csv',
    # TODO: Support this once Jason unblocks.
    # 'Account Take Over (Non-Olympus).csv',
    'Free Desktop Software Policy (Olympus).csv',
    'Free Desktop Software Policy (Non-Olympus).csv',
    'Untrustworthy Promotions (Olympus).csv',
    'Untrustworthy Promotions (Non-Olympus).csv',
]

# TODO: Dump these CSVs into SQL to run basic queries on the " or " count, etc.
# Only 'Hard-Code' fields seem to have " or "s in them; SQL can help me verify
# this claim.

for x in csvs:
    print('# File: "{}"'.format(f.strip_extension(x)))
    print(csv_to_proto(f.join(csv_directory, x)))

################################################################################
# Tests
################################################################################
if enable_tests:
    tests = [
        # 'EU Election Certification (Olympus).csv',
        'EU Election Certification (Non-Olympus).csv',
        # 'Non-Sensitive Ads Review (Olympus).csv',
    ]
    for csv_file in tests:
        textproto_file = f.join('expected',
                                f.change_extension('.textproto', csv_file))
        actual = csv_to_proto(
            f.ensure_absolute(f.join(csv_directory, csv_file)))
        expected = open(textproto_file, 'r').read()
        simple_assert(actual, expected, name='csv_to_proto')
