#include "libutil/xml-writer.hh"

#include <cassert>

namespace nix {

XMLWriter::XMLWriter(bool indent, std::ostream& output)
    : output(output), indent(indent) {
  output << "<?xml version='1.0' encoding='utf-8'?>" << std::endl;
  closed = false;
}

XMLWriter::~XMLWriter() { close(); }

void XMLWriter::close() {
  if (closed) {
    return;
  }
  while (!pendingElems.empty()) {
    closeElement();
  }
  closed = true;
}

void XMLWriter::indent_(size_t depth) {
  if (!indent) {
    return;
  }
  output << std::string(depth * 2, ' ');
}

void XMLWriter::openElement(const std::string& name, const XMLAttrs& attrs) {
  assert(!closed);
  indent_(pendingElems.size());
  output << "<" << name;
  writeAttrs(attrs);
  output << ">";
  if (indent) {
    output << std::endl;
  }
  pendingElems.push_back(name);
}

void XMLWriter::closeElement() {
  assert(!pendingElems.empty());
  indent_(pendingElems.size() - 1);
  output << "</" << pendingElems.back() << ">";
  if (indent) {
    output << std::endl;
  }
  pendingElems.pop_back();
  if (pendingElems.empty()) {
    closed = true;
  }
}

void XMLWriter::writeEmptyElement(const std::string& name,
                                  const XMLAttrs& attrs) {
  assert(!closed);
  indent_(pendingElems.size());
  output << "<" << name;
  writeAttrs(attrs);
  output << " />";
  if (indent) {
    output << std::endl;
  }
}

void XMLWriter::writeAttrs(const XMLAttrs& attrs) {
  for (auto& i : attrs) {
    output << " " << i.first << "=\"";
    for (char c : i.second) {
      if (c == '"') {
        output << "&quot;";
      } else if (c == '<') {
        output << "&lt;";
      } else if (c == '>') {
        output << "&gt;";
      } else if (c == '&') {
        output << "&amp;";
        /* Escape newlines to prevent attribute normalisation (see
           XML spec, section 3.3.3. */
      } else if (c == '\n') {
        output << "&#xA;";
      } else {
        output << c;
      }
    }
    output << "\"";
  }
}

}  // namespace nix
