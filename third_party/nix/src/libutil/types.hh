#pragma once

#include <boost/format.hpp>
#include <list>
#include <map>
#include <memory>
#include <set>
#include <string>

#include "ref.hh"

/* Before 4.7, gcc's std::exception uses empty throw() specifiers for
 * its (virtual) destructor and what() in c++11 mode, in violation of spec
 */
#ifdef __GNUC__
#if __GNUC__ < 4 || (__GNUC__ == 4 && __GNUC_MINOR__ < 7)
#define EXCEPTION_NEEDS_THROW_SPEC
#endif
#endif

namespace nix {

/* Inherit some names from other namespaces for convenience. */
using boost::format;

/* A variadic template that does nothing. Useful to call a function
   for all variadic arguments but ignoring the result. */
struct nop {
  template <typename... T>
  nop(T...) {}
};

struct FormatOrString {
  std::string s;
  FormatOrString(const std::string& s) : s(s){};
  FormatOrString(const format& f) : s(f.str()){};
  FormatOrString(const char* s) : s(s){};
};

/* A helper for formatting strings. ‘fmt(format, a_0, ..., a_n)’ is
   equivalent to ‘boost::format(format) % a_0 % ... %
   ... a_n’. However, ‘fmt(s)’ is equivalent to ‘s’ (so no %-expansion
   takes place). */

inline std::string fmt(const std::string& s) { return s; }

inline std::string fmt(const char* s) { return s; }

inline std::string fmt(const FormatOrString& fs) { return fs.s; }

template <typename... Args>
inline std::string fmt(const std::string& fs, Args... args) {
  boost::format f(fs);
  f.exceptions(boost::io::all_error_bits ^ boost::io::too_many_args_bit);
  nop{boost::io::detail::feed(f, args)...};
  return f.str();
}

/* BaseError should generally not be caught, as it has Interrupted as
   a subclass. Catch Error instead. */
class BaseError : public std::exception {
 protected:
  std::string prefix_;  // used for location traces etc.
  std::string err;

 public:
  unsigned int status = 1;  // exit status

  template <typename... Args>
  BaseError(unsigned int status, Args... args)
      : err(fmt(args...)), status(status) {}

  template <typename... Args>
  BaseError(Args... args) : err(fmt(args...)) {}

#ifdef EXCEPTION_NEEDS_THROW_SPEC
  ~BaseError() noexcept {};
  const char* what() const noexcept { return err.c_str(); }
#else
  const char* what() const noexcept { return err.c_str(); }
#endif

  const std::string& msg() const { return err; }
  const std::string& prefix() const { return prefix_; }
  BaseError& addPrefix(const FormatOrString& fs);
};

#define MakeError(newClass, superClass) \
  class newClass : public superClass {  \
   public:                              \
    using superClass::superClass;       \
  };

MakeError(Error, BaseError)

    class SysError : public Error {
 public:
  int errNo;

  template <typename... Args>
  SysError(Args... args) : Error(addErrno(fmt(args...))) {}

 private:
  std::string addErrno(const std::string& s);
};

typedef std::list<std::string> Strings;
typedef std::set<std::string> StringSet;
typedef std::map<std::string, std::string> StringMap;

/* Paths are just strings. */
typedef std::string Path;
typedef std::list<Path> Paths;
typedef std::set<Path> PathSet;

}  // namespace nix
