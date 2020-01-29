from pprint import PrettyPrinter

printer = PrettyPrinter(indent=2)


def pretty_print(x):
    return printer.pprint(x)
