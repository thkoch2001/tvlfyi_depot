#include <fstream>

#include <gtest/gtest.h>
#include <gmock/gmock.h>

#include "libutil/archive.hh"
#include "libutil/compression.hh"
#include "libutil/serialise.hh"
#include "nix_config.h"

namespace nix::tests {
namespace {
std::string ReadFile(const char *filename)
{
  std::ifstream in(filename, std::ios::in | std::ios::binary);
  if (in)
  {
    std::string contents;
    in.seekg(0, std::ios::end);
    contents.resize(in.tellg());
    in.seekg(0, std::ios::beg);
    in.read(&contents[0], contents.size());
    in.close();
    return(contents);
  }
  throw(errno);
}
}

constexpr char kDataLocation[] = NIX_SRC_DIR "/src/tests/testdata/0x9hwyxdhyya6zs72ikimwii30wh2fn1qbwav349pb8scwi7h8av.nar.xz";

TEST(XZDecompression, DecompressOneshot) {
  StringSink destination;
  nix::ref<nix::CompressionSink> decompressor = makeDecompressionSink("xz", destination);

  std::string contents = ReadFile(kDataLocation);

  (*decompressor)(reinterpret_cast<unsigned char*>(contents.data()), contents.size());

  EXPECT_TRUE(decompressor->good());
}

TEST(XZDecompression, DecompressChunked) {
  StringSink destination;
  nix::ref<nix::CompressionSink> decompressor = makeDecompressionSink("xz", destination);

  std::string contents = ReadFile(kDataLocation);
  constexpr size_t kChunkSize = 4097;

  size_t pos = 0;
  while (pos < contents.size()) {
    size_t len = contents.size() - pos;
    if (len > kChunkSize) {
      len = kChunkSize;
    }
    const unsigned char* ptr = (reinterpret_cast<unsigned char*>(contents.data())) + pos;
    (*decompressor)(ptr, len);
    pos += len;
  }

  EXPECT_TRUE(decompressor->good());
}

}
