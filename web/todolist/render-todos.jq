# Simple jq script to format all the TODO elements as HTML.

# https://stackoverflow.com/a/43269105
def IN_(s): first((s == .) // empty) // false;

def file_link: .
    | (.line | tostring) as $line
    | @uri "https://cs.tvl.fyi/depot/-/blob/\(.file)#L\($line)" as $href
    | "<a href=\"\($href)\">//\(.file):\($line)</a>";

def todo_element: "<p>\(. | file_link):</p>\n<blockquote>\(.todo | @html)</blockquote>\n";

def user_paragraph: .
    | (.todos | map(todo_element) | join("\n")) as $todos
    | "<h3><a name=\"\(.user)\" href=\"#\(.user)\">\(.user)</a></h3>\n\($todos)\n<hr />\n";

# main:
.knownUsers as $knownUsers
    | .allTodos
    # only keep TODO entries for known users
    | map(select(.user | IN_($knownUsers[])))
    # render TODOS
    | map(user_paragraph)
    | add
