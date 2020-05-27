#pragma once

#include <string>

#include "libutil/ref.hh"
#include "libutil/serialise.hh"
#include "libutil/types.hh"

namespace nix {

struct CompressionSink : BufferedSink {
  virtual void finish() = 0;
};

ref<std::string> decompress(const std::string& method, const std::string& in);

ref<CompressionSink> makeDecompressionSink(const std::string& method,
                                           Sink& nextSink);

ref<std::string> compress(const std::string& method, const std::string& in,
                          const bool parallel = false);

ref<CompressionSink> makeCompressionSink(const std::string& method,
                                         Sink& nextSink,
                                         const bool parallel = false);

MakeError(UnknownCompressionMethod, Error);

MakeError(CompressionError, Error);

}  // namespace nix
