#pragma once

#include <iostream>
#include <list>
#include <map>
#include <string>

namespace nix {

using std::list;
using std::map;
using std::string;

typedef map<std::string, std::string> XMLAttrs;

class XMLWriter {
 private:
  std::ostream& output;

  bool indent;
  bool closed;

  std::list<std::string> pendingElems;

 public:
  XMLWriter(bool indent, std::ostream& output);
  ~XMLWriter();

  void close();

  void openElement(const std::string& name, const XMLAttrs& attrs = XMLAttrs());
  void closeElement();

  void writeEmptyElement(const std::string& name,
                         const XMLAttrs& attrs = XMLAttrs());

 private:
  void writeAttrs(const XMLAttrs& attrs);

  void indent_(size_t depth);
};

class XMLOpenElement {
 private:
  XMLWriter& writer;

 public:
  XMLOpenElement(XMLWriter& writer, const std::string& name,
                 const XMLAttrs& attrs = XMLAttrs())
      : writer(writer) {
    writer.openElement(name, attrs);
  }
  ~XMLOpenElement() { writer.closeElement(); }
};

}  // namespace nix
