// This file exists to preserve compatibility with the pre-fork
// version of Nix (2.3.4).
//
// During the refactoring, various structures are getting ripped out
// and replaced with the dummies below while code is being cleaned up.

#ifndef NIX_SRC_LIBUTIL_PREFORK_COMPAT_H_
#define NIX_SRC_LIBUTIL_PREFORK_COMPAT_H_

namespace nix::compat {

// This is used in remote-store.cc for various things that expect the
// old logging protocol when talking over the wire. It will be removed
// hen the worker protocol is redone.
enum [[deprecated("old logging compat only")]] Verbosity{
    kError = 0, kWarn, kInfo, kTalkative, kChatty, kDebug, kVomit,
};

}  // namespace nix::compat

#endif  // NIX_SRC_LIBUTIL_PREFORK_COMPAT_H_
