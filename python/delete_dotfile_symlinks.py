#!/usr/bin/env python3

# TODO: Prefer a proper Python module doc here.
# Find and delete all symlinks to the dotfiles defined in $BRIEFCASE.
#
# Oftentimes I corrupt the state of my dotfiles. The intention with this script
# is to write some tooling to help me better manage my dotfile cleanliness. An
# example workflow might look like:
#
# ```shell
# > ./delete_dotfile_symlinks.py --audit
# > ./delete_dotfile_symlinks.py --seriously
# > cd ..
# > make install
# ```

################################################################################
# Dependencies
################################################################################

import argparse
import os
import sys

from os.path import expanduser

################################################################################
# Library
################################################################################

def homify(path):
  """Prefer ~ instead of the absolute `path`."""
  home = expanduser('~')
  return path.replace(home, '~')

def main():
  parser = argparse.ArgumentParser(description='Remove symlinks to my managed dotfiles.')
  parser.add_argument('--audit', dest='audit', action='store_true', help='Output all symlinks that would be deleted. This is the default behavior. This option is mutually exclusive with the --seriously option.')
  parser.add_argument('--seriously', dest='seriously', action='store_true', help='Actually delete the symlinks. This option is mutually exclusive with the --audit option.')
  parser.add_argument('--repo-name', dest='name', default='briefcase', help='The name of the repository. Usually "briefcase" or "dotfiles".')
  parser.add_argument('--device-only', dest='device_only', action='store_true', help='Only output the device-specific dotfiles.')
  args = parser.parse_args()

  # TODO: Instead of doing this manually, is this something that argparse supports?
  if not args.audit and not args.seriously:
     print('Either --audit or --seriously must be passed. See --help for more information.')
     # TODO: What's the proper exit code here?
     sys.exit(1)

  # These two options are mutually exclusive.
  assert args.audit != args.seriously

  count = 0

  for cwd, dirs, files in os.walk(expanduser("~")):
    # Skip this repository.
    if args.name in cwd:
      continue
    for file in files:
      source = os.path.join(cwd, file)
      if os.path.islink(source):
        dest = os.readlink(source)
        # We won't know certainly if the link points to a dotfile that I manage
        # with this simple test.
        if args.device_only:
          # TODO: Change 'desktop' with a lookup of hostname to device name.
          if 'configs/desktop' in dest:
            if args.audit:
              print('{} -> {}'.format(homify(source), homify(dest)))
            elif args.seriously:
              print('rm {}'.format(source))
              os.remove(source)
            count += 1
        elif args.name in dest:
          if args.audit:
            print('{} -> {}'.format(homify(source), homify(dest)))
          elif args.seriously:
            print('rm {}'.format(source))
            os.remove(source)
          count += 1

  if args.audit:
    print('Would have deleted {} symlinks.'.format(count))
  elif args.seriously:
    print('Successfully deleted {} symlinks.'.format(count))

if __name__ == '__main__':
   main()
