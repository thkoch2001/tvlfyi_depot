#include "libutil/hash.hh"

#include <cstring>
#include <iostream>

#include <absl/strings/escaping.h>
#include <absl/strings/str_format.h>
#include <fcntl.h>
#include <glog/logging.h>
#include <openssl/md5.h>
#include <openssl/sha.h>
#include <sys/stat.h>
#include <sys/types.h>

#include "libutil/archive.hh"
#include "libutil/istringstream_nocopy.hh"
#include "libutil/util.hh"

namespace nix {

std::optional<HashType> hash_type_from(nix::proto::HashType hash_type) {
  switch (hash_type) {
    case nix::proto::HashType::UNKNOWN:
      return HashType::htUnknown;
    case nix::proto::HashType::MD5:
      return HashType::htMD5;
    case nix::proto::HashType::SHA1:
      return HashType::htSHA1;
    case nix::proto::HashType::SHA256:
      return HashType::htSHA256;
    case nix::proto::HashType::SHA512:
      return HashType::htSHA512;
    default:
      return {};
  }
}

nix::proto::HashType HashTypeToProto(HashType hash_type) {
  switch (hash_type) {
    case HashType::htMD5:
      return nix::proto::HashType::MD5;
    case HashType::htSHA1:
      return nix::proto::HashType::SHA1;
    case HashType::htSHA256:
      return nix::proto::HashType::SHA256;
    case HashType::htSHA512:
      return nix::proto::HashType::SHA512;
    default:
      return nix::proto::HashType::UNKNOWN;
  }
}

void Hash::init() {
  if (type == htMD5) {
    hashSize = md5HashSize;
  } else if (type == htSHA1) {
    hashSize = sha1HashSize;
  } else if (type == htSHA256) {
    hashSize = sha256HashSize;
  } else if (type == htSHA512) {
    hashSize = sha512HashSize;
  } else {
    abort();
  }
  assert(hashSize <= maxHashSize);
  memset(hash, 0, maxHashSize);
}

bool Hash::operator==(const Hash& h2) const {
  if (hashSize != h2.hashSize) {
    return false;
  }
  for (unsigned int i = 0; i < hashSize; i++) {
    if (hash[i] != h2.hash[i]) {
      return false;
    }
  }
  return true;
}

bool Hash::operator!=(const Hash& h2) const { return !(*this == h2); }

bool Hash::operator<(const Hash& h) const {
  if (hashSize < h.hashSize) {
    return true;
  }
  if (hashSize > h.hashSize) {
    return false;
  }
  for (unsigned int i = 0; i < hashSize; i++) {
    if (hash[i] < h.hash[i]) {
      return true;
    }
    if (hash[i] > h.hash[i]) {
      return false;
    }
  }
  return false;
}

const std::string base16Chars = "0123456789abcdef";

static std::string printHash16(const Hash& hash) {
  char buf[hash.hashSize * 2];
  for (unsigned int i = 0; i < hash.hashSize; i++) {
    buf[i * 2] = base16Chars[hash.hash[i] >> 4];
    buf[i * 2 + 1] = base16Chars[hash.hash[i] & 0x0f];
  }
  return std::string(buf, hash.hashSize * 2);
}

bool Hash::IsValidBase16(absl::string_view s) {
  for (char c : s) {
    if ('0' <= c && c <= '9') {
      continue;
    }
    if ('a' <= c && c <= 'f') {
      continue;
    }
    if ('A' <= c && c <= 'F') {
      continue;
    }
    return false;
  }
  return true;
}

constexpr signed char kUnBase32[] = {
    -1, -1, -1, -1, -1, -1, -1, -1, /* unprintables */
    -1, -1, -1, -1, -1, -1, -1, -1, /* unprintables */
    -1, -1, -1, -1, -1, -1, -1, -1, /* unprintables */
    -1, -1, -1, -1, -1, -1, -1, -1, /* unprintables */
    -1, -1, -1, -1, -1, -1, -1, -1, /* SP..' */
    -1, -1, -1, -1, -1, -1, -1, -1, /* (../ */
    0,  1,  2,  3,  4,  5,  6,  7,  /* 0..7 */
    8,  9,  -1, -1, -1, -1, -1, -1, /* 8..? */
    -1, -1, -1, -1, -1, -1, -1, -1, /* @..G */
    -1, -1, -1, -1, -1, -1, -1, -1, /* H..O */
    -1, -1, -1, -1, -1, -1, -1, -1, /* P..W */
    -1, -1, -1, -1, -1, -1, -1, -1, /* X.._ */
    -1, 10, 11, 12, 13, -1, 14, 15, /* `..g */
    16, 17, 18, 19, 20, 21, 22, -1, /* h..o */
    23, 24, 25, 26, -1, -1, 27, 28, /* p..w */
    29, 30, 31, -1, -1, -1, -1, -1, /* x..DEL */

    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, /* high */
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, /* high */
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, /* high */
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, /* high */
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, /* high */
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, /* high */
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, /* high */
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, /* high */
};

bool Hash::IsValidBase32(absl::string_view s) {
  static_assert(sizeof(kUnBase32) == 256);

  for (char c : s) {
    if (kUnBase32[static_cast<unsigned char>(c)] == -1) {
      return false;
    }
  }
  return true;
}

std::string Hash::ToStorePathHash() const {
  return compressHash(*this, kStorePathHashSize).to_string(Base32, false);
}

static std::string printHash32(const Hash& hash) {
  assert(hash.hashSize);
  size_t len = hash.base32Len();
  assert(len);

  std::string s;
  s.reserve(len);

  for (int n = static_cast<int>(len) - 1; n >= 0; n--) {
    unsigned int b = n * 5;
    unsigned int i = b / 8;
    unsigned int j = b % 8;
    unsigned char c =
        (hash.hash[i] >> j) |
        (i >= hash.hashSize - 1 ? 0 : hash.hash[i + 1] << (8 - j));
    s.push_back(base32Chars[c & 0x1f]);
  }

  return s;
}

std::string printHash16or32(const Hash& hash) {
  return hash.to_string(hash.type == htMD5 ? Base16 : Base32, false);
}

std::string Hash::to_string(Base base, bool includeType) const {
  std::string s;
  if (base == SRI || includeType) {
    s += printHashType(type);
    s += base == SRI ? '-' : ':';
  }
  switch (base) {
    case Base16:
      s += printHash16(*this);
      break;
    case Base32:
      s += printHash32(*this);
      break;
    case Base64:
    case SRI:
      std::string b64;
      absl::Base64Escape(
          std::string(reinterpret_cast<const char*>(hash), hashSize), &b64);
      s += b64;
      break;
  }
  return s;
}

Hash::Hash(std::string_view s, HashType type) : type(type) {
  absl::StatusOr<Hash> result = deserialize(s, type);
  *this = unwrap_throw(result);
}

// TODO(riking): change ht to an optional
absl::StatusOr<Hash> Hash::deserialize(std::string_view s, HashType type) {
  size_t pos = 0;
  bool isSRI = false;

  auto sep = s.find(':');
  if (sep == std::string::npos) {
    sep = s.find('-');
    if (sep != std::string::npos) {
      isSRI = true;
    } else if (type == htUnknown) {
      return absl::InvalidArgumentError(
          absl::StrCat("hash string '", s, " does not include a type"));
    }
  }

  HashType parsedType = type;
  if (sep != std::string::npos) {
    std::string hts = std::string(s, 0, sep);
    parsedType = parseHashType(hts);
    if (type != htUnknown && parsedType != type) {
      return absl::InvalidArgumentError(
          absl::StrCat("hash '", s, "' should have type '", printHashType(type),
                       "', found '", printHashType(parsedType), "'"));
    }
    pos = sep + 1;
  }

  Hash dest(parsedType);

  size_t size = s.size() - pos;
  absl::string_view sv(s.data() + pos, size);

  if (!isSRI && size == dest.base16Len()) {
    std::string bytes;
    if (!IsValidBase16(sv)) {
      return absl::InvalidArgumentError(
          absl::StrCat("invalid base-16 hash: bad character in '", s, "'"));
    }
    bytes = absl::HexStringToBytes(sv);
    if (bytes.size() != dest.hashSize) {
      return absl::InvalidArgumentError(
          absl::StrCat("hash '", s, "' has wrong length for base16 ",
                       printHashType(dest.type)));
    }
    memcpy(dest.hash, bytes.data(), dest.hashSize);
  }

  else if (!isSRI && size == dest.base32Len()) {
    for (unsigned int n = 0; n < size; ++n) {
      char c = sv[size - n - 1];
      // range: -1, 0..31
      signed char digit = kUnBase32[static_cast<unsigned char>(c)];
      if (digit < 0) {
        return absl::InvalidArgumentError(
            absl::StrCat("invalid base-32 hash: bad character ",
                         absl::CEscape(absl::string_view(&c, 1))));
      }
      unsigned int b = n * 5;
      unsigned int i = b / 8;
      unsigned int j = b % 8;
      dest.hash[i] |= digit << j;

      if (i < dest.hashSize - 1) {
        dest.hash[i + 1] |= digit >> (8 - j);
      } else {
        if ((digit >> (8 - j)) != 0) {
          return absl::InvalidArgumentError(
              absl::StrCat("invalid base-32 hash '", s, "'"));
        }
      }
    }
  }

  else if (isSRI || size == dest.base64Len()) {
    std::string decoded;
    if (!absl::Base64Unescape(sv, &decoded)) {
      return absl::InvalidArgumentError("invalid base-64 hash");
    }
    if (decoded.size() != dest.hashSize) {
      return absl::InvalidArgumentError(
          absl::StrCat("hash '", s, "' has wrong length for base64 ",
                       printHashType(dest.type)));
    }
    memcpy(dest.hash, decoded.data(), dest.hashSize);
  }

  else {
    return absl::InvalidArgumentError(absl::StrCat(
        "hash '", s, "' has wrong length for ", printHashType(dest.type)));
  }

  return dest;
}

Hash Hash::unwrap_throw(absl::StatusOr<Hash> hash) {
  if (hash.ok()) {
    return *hash;
  } else {
    throw BadHash(hash.status().message());
  }
}

namespace hash {

union Ctx {
  MD5_CTX md5;
  SHA_CTX sha1;
  SHA256_CTX sha256;
  SHA512_CTX sha512;
};

static void start(HashType ht, Ctx& ctx) {
  if (ht == htMD5) {
    MD5_Init(&ctx.md5);
  } else if (ht == htSHA1) {
    SHA1_Init(&ctx.sha1);
  } else if (ht == htSHA256) {
    SHA256_Init(&ctx.sha256);
  } else if (ht == htSHA512) {
    SHA512_Init(&ctx.sha512);
  }
}

static void update(HashType ht, Ctx& ctx, const unsigned char* bytes,
                   size_t len) {
  if (ht == htMD5) {
    MD5_Update(&ctx.md5, bytes, len);
  } else if (ht == htSHA1) {
    SHA1_Update(&ctx.sha1, bytes, len);
  } else if (ht == htSHA256) {
    SHA256_Update(&ctx.sha256, bytes, len);
  } else if (ht == htSHA512) {
    SHA512_Update(&ctx.sha512, bytes, len);
  }
}

static void finish(HashType ht, Ctx& ctx, unsigned char* hash) {
  if (ht == htMD5) {
    MD5_Final(hash, &ctx.md5);
  } else if (ht == htSHA1) {
    SHA1_Final(hash, &ctx.sha1);
  } else if (ht == htSHA256) {
    SHA256_Final(hash, &ctx.sha256);
  } else if (ht == htSHA512) {
    SHA512_Final(hash, &ctx.sha512);
  }
}

}  // namespace hash

Hash hashString(HashType ht, const std::string& s) {
  hash::Ctx ctx{};
  Hash hash(ht);
  start(ht, ctx);
  update(ht, ctx, reinterpret_cast<const unsigned char*>(s.data()), s.length());
  finish(ht, ctx, hash.hash);
  return hash;
}

Hash hashFile(HashType ht, const Path& path) {
  hash::Ctx ctx{};
  Hash hash(ht);
  start(ht, ctx);

  AutoCloseFD fd = open(path.c_str(), O_RDONLY | O_CLOEXEC);
  if (!fd) {
    throw SysError(format("opening file '%1%'") % path);
  }

  std::vector<unsigned char> buf(8192);
  ssize_t n;
  while ((n = read(fd.get(), buf.data(), buf.size())) != 0) {
    checkInterrupt();
    if (n == -1) {
      throw SysError(format("reading file '%1%'") % path);
    }
    update(ht, ctx, buf.data(), n);
  }

  finish(ht, ctx, hash.hash);
  return hash;
}

HashSink::HashSink(HashType ht)
    : ht(ht), ctx(std::make_unique<hash::Ctx>()), bytes(0) {
  start(ht, *ctx);
}

HashSink::~HashSink() { bufPos = 0; }

void HashSink::write(const unsigned char* data, size_t len) {
  bytes += len;
  nix::hash::update(ht, *ctx, data, len);
}

HashResult HashSink::finish() {
  flush();
  Hash hash(ht);
  nix::hash::finish(ht, *ctx, hash.hash);
  return HashResult(hash, bytes);
}

HashResult HashSink::currentHash() {
  flush();
  nix::hash::Ctx ctx2 = *ctx;
  Hash hash(ht);
  nix::hash::finish(ht, ctx2, hash.hash);
  return HashResult(hash, bytes);
}

HashResult hashPath(HashType ht, const Path& path, PathFilter& filter) {
  HashSink sink(ht);
  dumpPath(path, sink, filter);
  return sink.finish();
}

Hash compressHash(const Hash& hash, unsigned int newSize) {
  Hash h;
  h.hashSize = newSize;
  for (unsigned int i = 0; i < hash.hashSize; ++i) {
    h.hash[i % newSize] ^= hash.hash[i];
  }
  return h;
}

HashType parseHashType(const std::string& s) {
  if (s == "md5") {
    return htMD5;
  }
  if (s == "sha1") {
    return htSHA1;
  } else if (s == "sha256") {
    return htSHA256;
  } else if (s == "sha512") {
    return htSHA512;
  } else {
    return htUnknown;
  }
}

std::string printHashType(HashType ht) {
  if (ht == htMD5) {
    return "md5";
  }
  if (ht == htSHA1) {
    return "sha1";
  } else if (ht == htSHA256) {
    return "sha256";
  } else if (ht == htSHA512) {
    return "sha512";
  } else if (ht == htUnknown) {
    return "<unknown>";
  } else {
    LOG(FATAL) << "Unrecognized hash type: " << static_cast<int>(ht);
    abort();
  }
}

}  // namespace nix
