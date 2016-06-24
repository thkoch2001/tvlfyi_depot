#!/usr/bin/python

"""
Forward and Backward lookups for URL escape sequences
"""

import sys, re


literal_to_esccode = {
  ' ': '%20',
  '<': '%3C',
  '>': '%3E',
  '#': '%23',
  '%': '%25',
  '{': '%7B',
  '}': '%7D',
  '|': '%7C',
  '\\': '%5C',
  '^': '%5E',
  '~': '%7E',
  '[': '%5B',
  ']': '%5D',
  '`': '%60',
  ';': '%3B',
  '/': '%2F',
  '?': '%3F',
  ':': '%3A',
  '@': '%40',
  '=': '%3D',
  '&': '%26',
  '$': '%24',
}


esccode_to_literal = {
  v: k for k, v in literal_to_esccode.items()
}


def is_esccode(string):
  p = re.compile(r'^%\w\w$')
  m = p.match(string)
  return bool(p.match(string))


try:
  el = sys.argv[1]
except:
  el = None

if not el:
  print literal_to_esccode
else:
  msg = esccode_to_literal[el] if is_esccode(el) else literal_to_esccode[el]
  print '"{0}": "{1}"'.format(el, msg)
