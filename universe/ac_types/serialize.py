from test_utils import simple_assert
import string as string


def literal(x):
    if x == True:
        return 'true'
    elif x == False:
        return 'false'
    elif isinstance(x, int):
        return str(x)
    elif x is None:
        raise Exception("None!")
    # `x` is a string
    else:
        x = string.trim_surrounding('"', x)
        return "\"{}\"".format(x)


actual = [
    literal(True),
    literal(9249441),
    literal("COMPLEXITY"),
    literal("\"doubly wrapped string\"")
]
expected = ["true", "9249441", "\"COMPLEXITY\"", "\"doubly wrapped string\""]
simple_assert(actual, expected, name="literal")


def input(input_type=None, fields=None):
    header = 'inputs {'
    input_type_field = '  type: {}'.format(input_type)
    fields = '\n'.join(
        ["  {}: {}".format(k, literal(v)) for k, v in fields.items()])
    if input_type == 'SIGNAL':
        fields += '\n  signal_type: CID'
    footer = '}'
    return '\n'.join([header, input_type_field, fields, footer])


actual = input(input_type='CONSTANT',
               fields={
                   'consult_frd_id': 'FEATURE',
                   'is_optional': False,
                   'constant_value': "regular_review",
               })
expected = """inputs {
  type: CONSTANT
  consult_frd_id: "FEATURE"
  is_optional: false
  constant_value: "regular_review"
}"""
simple_assert(actual, expected, name='input')

actual = input(input_type='CONSTANT',
               fields={
                   'consult_frd_id': 'FEATURE',
                   'is_optional': False,
                   'constant_value': "\"doubly wrapped string\"",
               })
expected = """inputs {
  type: CONSTANT
  consult_frd_id: "FEATURE"
  is_optional: false
  constant_value: "doubly wrapped string"
}"""
simple_assert(actual, expected, name='input')
