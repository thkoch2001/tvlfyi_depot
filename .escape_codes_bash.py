#!/usr/bin/python

"""
Forward and Backward lookups for Bash escape sequences
"""

import sys, re


literal_to_bash = {
  'ESC': '^[',

  'UP-ARROW': '^[OA',
  'RIGHT-ARROW': '^[OC',
  'DOWN-ARROW': '^[OB',
  'LEFT-ARROW': '^[OD',

  'F1': '^[OP',
  'F2': '^[OQ',
  'F3': '^[OR',
  'F4': '^[OS',
  'F5': '^[15~',
  'F6': '^[17~',
  'F7': '^[18~',
  'F8': '^[19~',
  'F9': '^[20~',
  'F10': '^[21~',
  'F11': None,
  'F12': '^[24~'
}

bash_to_literal = {
  v: k for k, v in literal_to_bash.items()
}

el = sys.argv[1]

print '{0}: "{1}"'.format(el, literal_to_bash[el])
