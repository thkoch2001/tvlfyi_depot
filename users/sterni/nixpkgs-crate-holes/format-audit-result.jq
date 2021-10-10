def link:
  [ "https://rustsec.org/advisories/", .advisory.id, ".html" ] | add;

def version_list:
  [ .[] | "`" + . + "`" ] | join("; ");

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

def aliases:
  if .advisory.aliases == [] then
    ""
  else
    [ " (", (.advisory.aliases | join(", ")), ")" ] | add
  end;

def format_vulnerability:
  [ "  - "
  , .package.name, " ", .package.version, ": "
  , "[", .advisory.id, "](", link, ")"
  , aliases
  , ", ", patched
  , "\n"
  ] | add;

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
