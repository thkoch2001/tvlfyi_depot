#pragma once

#include <absl/status/statusor.h>

#include "libproto/worker.grpc.pb.h"
#include "libutil/serialise.hh"
#include "libutil/types.hh"

namespace nix {

// Size of the hashes rendered in store paths, in bytes
constexpr unsigned int kStorePathHashSize = 20;

MakeError(BadHash, Error);

// TODO(grfn): Replace this with the hash type enum from the daemon proto so we
// don't have to juggle two different types
enum HashType : char { htUnknown, htMD5, htSHA1, htSHA256, htSHA512 };

std::optional<HashType> hash_type_from(nix::proto::HashType hash_type);

nix::proto::HashType HashTypeToProto(HashType hash_type);

const int md5HashSize = 16;
const int sha1HashSize = 20;
const int sha256HashSize = 32;
const int sha512HashSize = 64;

// omitted: E O U T
constexpr char base32Chars[] = "0123456789abcdfghijklmnpqrsvwxyz";

enum Base : int { Base64, Base32, Base16, SRI };

struct Hash {
  static const unsigned int maxHashSize = 64;
  unsigned int hashSize = 0;
  unsigned char hash[maxHashSize] = {};

  HashType type = htUnknown;

  /* Create an unset hash object. */
  Hash(){};

  /* Create a zero-filled hash object. */
  explicit Hash(HashType type) : type(type) { init(); };

  /* Initialize the hash from a string representation, in the format
     "[<type>:]<base16|base32|base64>" or "<type>-<base64>" (a
     Subresource Integrity hash expression). If the 'type' argument
     is htUnknown, then the hash type must be specified in the
     string. */
  explicit Hash(std::string_view s, HashType type = htUnknown);

  /* Status-returning version of above constructor */
  static absl::StatusOr<Hash> deserialize(std::string_view s,
                                          HashType type = htUnknown);

  // Legacy unwrapper for StatusOr. Throws BadHash.
  static Hash unwrap_throw(absl::StatusOr<Hash> hash) noexcept(false);

  void init();

  /* Check whether a hash is set. */
  explicit operator bool() const { return type != htUnknown; }

  /* Check whether two hash are equal. */
  bool operator==(const Hash& h2) const;

  /* Check whether two hash are not equal. */
  bool operator!=(const Hash& h2) const;

  /* For sorting. */
  bool operator<(const Hash& h) const;

  /* Returns the length of a base-16 representation of this hash. */
  size_t base16Len() const { return hashSize * 2; }

  /* Returns the length of a base-32 representation of this hash. */
  size_t base32Len() const { return (hashSize * 8 - 1) / 5 + 1; }

  /* Returns the length of a base-64 representation of this hash. */
  size_t base64Len() const { return ((4 * hashSize / 3) + 3) & ~3; }

  /* Return a string representation of the hash, in base-16, base-32
     or base-64. By default, this is prefixed by the hash type
     (e.g. "sha256:"). */
  std::string to_string(Base base = Base32, bool includeType = true) const;

  /* Returns whether the passed string contains entirely valid base16
     characters. */
  static bool IsValidBase16(absl::string_view s);

  /* Returns whether the passed string contains entirely valid base32
     characters. */
  static bool IsValidBase32(absl::string_view s);

  // Convert this Hash to the format expected in store paths
  [[nodiscard]] std::string ToStorePathHash() const;
};

/* Print a hash in base-16 if it's MD5, or base-32 otherwise. */
std::string printHash16or32(const Hash& hash);

/* Compute the hash of the given string. */
Hash hashString(HashType ht, const std::string& s);

/* Compute the hash of the given file. */
Hash hashFile(HashType ht, const Path& path);

/* A pair of the Hash, and the number of bytes consumed. */
typedef std::pair<Hash, unsigned long long> HashResult;

/* Compute the hash of the given path.  The hash is defined as
   (essentially) hashString(ht, dumpPath(path)). */
HashResult hashPath(HashType ht, const Path& path,
                    PathFilter& filter = defaultPathFilter);

/* Compress a hash to the specified number of bytes by cyclically
   XORing bytes together. */
Hash compressHash(const Hash& hash, unsigned int newSize);

/* Parse a string representing a hash type. */
HashType parseHashType(const std::string& s);

/* And the reverse. */
std::string printHashType(HashType ht);

namespace hash {
union Ctx;
}

class HashSink : public BufferedSink {
 private:
  HashType ht;
  std::unique_ptr<hash::Ctx> ctx;
  unsigned long long bytes;

 public:
  explicit HashSink(HashType ht);
  HashSink(const HashSink& h);
  ~HashSink();
  void write(const unsigned char* data, size_t len);
  HashResult finish();
  HashResult currentHash();
};

}  // namespace nix
