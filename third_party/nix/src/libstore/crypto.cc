#include "libstore/crypto.hh"

#include <absl/strings/escaping.h>

#include "libstore/globals.hh"
#include "libutil/util.hh"

#if HAVE_SODIUM
#include <sodium.h>
#endif

namespace nix {

// TODO(riking): convert to string_view to reduce allocations
static std::pair<std::string, std::string> split(const std::string& s) {
  size_t colon = s.find(':');
  if (colon == std::string::npos || colon == 0) {
    return {"", ""};
  }
  return {std::string(s, 0, colon), std::string(s, colon + 1)};
}

Key::Key(const std::string& s) {
  auto ss = split(s);

  name = ss.first;
  std::string keyb64 = ss.second;

  if (name.empty() || keyb64.empty()) {
    throw Error("secret key is corrupt");
  }

  if (!absl::Base64Unescape(keyb64, &key)) {
    // TODO(grfn): replace this with StatusOr
    throw Error("Invalid Base64");
  }
}

SecretKey::SecretKey(const std::string& s) : Key(s) {
#if HAVE_SODIUM
  if (key.size() != crypto_sign_SECRETKEYBYTES) {
    throw Error("secret key is not valid");
  }
#endif
}

#if !HAVE_SODIUM
[[noreturn]] static void noSodium() {
  throw Error(
      "Nix was not compiled with libsodium, required for signed binary cache "
      "support");
}
#endif

std::string SecretKey::signDetached(const std::string& data) const {
#if HAVE_SODIUM
  unsigned char sig[crypto_sign_BYTES];
  unsigned long long sigLen;
  crypto_sign_detached(
      sig, &sigLen, reinterpret_cast<const unsigned char*>(data.data()),
      data.size(), reinterpret_cast<const unsigned char*>(key.data()));
  return name + ":" +
         absl::Base64Escape(std::string(reinterpret_cast<char*>(sig), sigLen));
#else
  noSodium();
#endif
}

PublicKey SecretKey::toPublicKey() const {
#if HAVE_SODIUM
  unsigned char pk[crypto_sign_PUBLICKEYBYTES];
  crypto_sign_ed25519_sk_to_pk(
      pk, reinterpret_cast<const unsigned char*>(key.data()));
  return PublicKey(name, std::string(reinterpret_cast<char*>(pk),
                                     crypto_sign_PUBLICKEYBYTES));
#else
  noSodium();
#endif
}

PublicKey::PublicKey(const std::string& s) : Key(s) {
#if HAVE_SODIUM
  if (key.size() != crypto_sign_PUBLICKEYBYTES) {
    throw Error("public key is not valid");
  }
#endif
}

bool verifyDetached(const std::string& data, const std::string& sig,
                    const PublicKeys& publicKeys) {
#if HAVE_SODIUM
  auto ss = split(sig);

  auto key = publicKeys.find(ss.first);
  if (key == publicKeys.end()) {
    return false;
  }

  std::string sig2;
  if (!absl::Base64Unescape(ss.second, &sig2)) {
    // TODO(grfn): replace this with StatusOr
    throw Error("Invalid Base64");
  }
  if (sig2.size() != crypto_sign_BYTES) {
    throw Error("signature is not valid");
  }

  return crypto_sign_verify_detached(
             reinterpret_cast<unsigned char*>(sig2.data()),
             reinterpret_cast<const unsigned char*>(data.data()), data.size(),
             reinterpret_cast<const unsigned char*>(key->second.key.data())) ==
         0;
#else
  noSodium();
#endif
}

PublicKeys getDefaultPublicKeys() {
  PublicKeys publicKeys;

  // FIXME: filter duplicates

  for (const auto& s : settings.trustedPublicKeys.get()) {
    PublicKey key(s);
    publicKeys.emplace(key.name, key);
  }

  for (const auto& secretKeyFile : settings.secretKeyFiles.get()) {
    try {
      SecretKey secretKey(readFile(secretKeyFile));
      publicKeys.emplace(secretKey.name, secretKey.toPublicKey());
    } catch (SysError& e) {
      /* Ignore unreadable key files. That's normal in a
         multi-user installation. */
    }
  }

  return publicKeys;
}

}  // namespace nix
