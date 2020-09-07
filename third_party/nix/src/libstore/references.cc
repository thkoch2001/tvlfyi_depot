#include "libstore/references.hh"

#include <cstdlib>
#include <map>

#include <glog/logging.h>

#include "libutil/archive.hh"
#include "libutil/hash.hh"
#include "libutil/util.hh"

namespace nix {

constexpr unsigned int kRefLength = 32; /* characters */

static void search(const unsigned char* s, size_t len, StringSet& hashes,
                   StringSet& seen) {
  static bool initialised = false;
  static bool isBase32[256];
  if (!initialised) {
    for (bool& i : isBase32) {
      i = false;
    }
    for (char base32Char : base32Chars) {
      isBase32[static_cast<unsigned char>(base32Char)] = true;
    }
    initialised = true;
  }

  for (size_t i = 0; i + kRefLength <= len;) {
    int j = 0;
    bool match = true;
    for (j = kRefLength - 1; j >= 0; --j) {
      if (!isBase32[s[i + j]]) {
        i += j + 1;
        match = false;
        break;
      }
    }
    if (!match) {
      continue;
    }
    std::string ref(reinterpret_cast<const char*>(s) + i, kRefLength);
    if (hashes.find(ref) != hashes.end()) {
      DLOG(INFO) << "found reference to '" << ref << "' at offset " << i;
      seen.insert(ref);
      hashes.erase(ref);
    }
    ++i;
  }
}

struct RefScanSink : Sink {
  HashSink hashSink;
  StringSet hashes;
  StringSet seen;

  std::string tail;

  RefScanSink() : hashSink(htSHA256) {}

  void operator()(const unsigned char* data, size_t len) override;
};

void RefScanSink::operator()(const unsigned char* data, size_t len) {
  hashSink(data, len);

  /* It's possible that a reference spans the previous and current
     fragment, so search in the concatenation of the tail of the
     previous fragment and the start of the current fragment. */
  std::string s = tail + std::string(reinterpret_cast<const char*>(data),
                                     len > kRefLength ? kRefLength : len);
  search(reinterpret_cast<const unsigned char*>(s.data()), s.size(), hashes,
         seen);

  search(data, len, hashes, seen);

  size_t tailLen = len <= kRefLength ? len : kRefLength;
  tail =
      std::string(tail, tail.size() < kRefLength - tailLen
                            ? 0
                            : tail.size() - (kRefLength - tailLen)) +
      std::string(reinterpret_cast<const char*>(data) + len - tailLen, tailLen);
}

PathSet scanForReferences(const std::string& path, const PathSet& refs,
                          HashResult& hash) {
  RefScanSink sink;
  std::map<std::string, Path> backMap;

  /* For efficiency (and a higher hit rate), just search for the
     hash part of the file name.  (This assumes that all references
     have the form `HASH-bla'). */
  for (auto& i : refs) {
    std::string baseName = baseNameOf(i);
    std::string::size_type pos = baseName.find('-');
    if (pos == std::string::npos) {
      throw Error(format("bad reference '%1%'") % i);
    }
    std::string s = std::string(baseName, 0, pos);
    assert(s.size() == kRefLength);
    assert(backMap.find(s) == backMap.end());
    // parseHash(htSHA256, s);
    sink.hashes.insert(s);
    backMap[s] = i;
  }

  /* Look for the hashes in the NAR dump of the path. */
  dumpPath(path, sink);

  /* Map the hashes found back to their store paths. */
  PathSet found;
  for (auto& i : sink.seen) {
    std::map<std::string, Path>::iterator j;
    if ((j = backMap.find(i)) == backMap.end()) {
      abort();
    }
    found.insert(j->second);
  }

  hash = sink.hashSink.finish();

  return found;
}

}  // namespace nix
