# Link to human-readable advisory info for a given vulnerability
def link:
  [ "https://rustsec.org/advisories/", .advisory.id, ".html" ] | add;

# Format a list of version constraints
def version_list:
  [ .[] | "`" + . + "`" ] | join("; ");

# show paths to fixing this vulnerability:
#
# - if there are patched releases, show them (the version we are using presumably
#   predates the vulnerability discovery, so we likely want to upgrade to a
#   patched release).
# - if there are no patched releases, show the unaffected versions (in case we
#   want to downgrade).
# - otherwise we state that no unaffected versions are available at this time.
#
# This logic should be useful, but is slightly dumber than cargo-audit's
# suggestion when using the non-JSON output.
def patched:
  if .versions.patched == [] then
    if .versions.unaffected != [] then
       "unaffected: " + (.versions.unaffected | version_list)
    else
      "no unaffected version available"
    end
  else
    "patched: " + (.versions.patched | version_list)
  end;

# if the vulnerability has aliases (like CVE-*) emit them in parens
def aliases:
  if .advisory.aliases == [] then
    ""
  else
    [ " (", (.advisory.aliases | join(", ")), ")" ] | add
  end;

# each vulnerability is rendered as a (normal) sublist item
def format_vulnerability:
  [ "  - "
  , .package.name, " ", .package.version, ": "
  , "[", .advisory.id, "](", link, ")"
  , aliases
  , ", ", patched
  , "\n"
  ] | add;

# be quiet if no found vulnerabilities, otherwise render a GHFM checklist item
if .vulnerabilities.found | not then
  ""
else
  ([ "- [ ] "
   , "`", $attr, "`: "
   , (.vulnerabilities.count | tostring)
   , " vulnerabilities in Cargo.lock\n"
   ] + (.vulnerabilities.list | map(format_vulnerability))
  ) | add
end
