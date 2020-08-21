#pragma once

#include <locale>

#include <absl/strings/numbers.h>
#include <signal.h>

#include "libmain/common-args.hh"
#include "libutil/args.hh"
#include "libutil/util.hh"

namespace nix {

class Exit : public std::exception {
 public:
  int status;
  Exit() : status(0) {}
  Exit(int status) : status(status) {}
  virtual ~Exit();
};

int handleExceptions(const std::string& programName,
                     const std::function<void()>& fun);

void initNix();

void parseCmdLine(
    int argc, char** argv,
    std::function<bool(Strings::iterator& arg, const Strings::iterator& end)>
        parseArg);

void parseCmdLine(
    const std::string& programName, const Strings& args,
    std::function<bool(Strings::iterator& arg, const Strings::iterator& end)>
        parseArg);

void printVersion(const std::string& programName);

/* Ugh.  No better place to put this. */
void printGCWarning();

class Store;

void printMissing(const ref<Store>& store, const PathSet& paths);

void printMissing(const ref<Store>& store, const PathSet& willBuild,
                  const PathSet& willSubstitute, const PathSet& unknown,
                  unsigned long long downloadSize, unsigned long long narSize);

std::string getArg(const std::string& opt, Strings::iterator& i,
                   const Strings::iterator& end);

template <class N>
N getIntArg(const std::string& opt, Strings::iterator& i,
            const Strings::iterator& end, bool allowUnit) {
  ++i;
  if (i == end) {
    throw UsageError(format("'%1%' requires an argument") % opt);
  }
  std::string s = *i;
  N multiplier = 1;
  if (allowUnit && !s.empty()) {
    char u = std::toupper(*s.rbegin());
    if (std::isalpha(u)) {
      if (u == 'K') {
        multiplier = 1ULL << 10;
      } else if (u == 'M') {
        multiplier = 1ULL << 20;
      } else if (u == 'G') {
        multiplier = 1ULL << 30;
      } else if (u == 'T') {
        multiplier = 1ULL << 40;
      } else {
        throw UsageError(format("invalid unit specifier '%1%'") % u);
      }

      s.resize(s.size() - 1);
    }
  }
  N n;
  if (!absl::SimpleAtoi(s, &n)) {
    throw UsageError(format("'%1%' requires an integer argument") % opt);
  }
  return n * multiplier;
}

struct LegacyArgs : public MixCommonArgs {
  std::function<bool(Strings::iterator& arg, const Strings::iterator& end)>
      parseArg;

  LegacyArgs(
      const std::string& programName,
      std::function<bool(Strings::iterator& arg, const Strings::iterator& end)>
          parseArg);

  bool processFlag(Strings::iterator& pos, Strings::iterator end) override;

  bool processArgs(const Strings& args, bool finish) override;
};

/* Show the manual page for the specified program. */
void showManPage(const std::string& name);

/* The constructor of this class starts a pager if stdout is a
   terminal and $PAGER is set. Stdout is redirected to the pager. */
class RunPager {
 public:
  RunPager();
  ~RunPager();

 private:
  Pid pid;
};

extern volatile ::sig_atomic_t blockInt;

/* GC helpers. */

std::string showBytes(unsigned long long bytes);

struct GCResults;

struct PrintFreed {
  bool show;
  const GCResults& results;
  PrintFreed(bool show, const GCResults& results)
      : show(show), results(results) {}
  ~PrintFreed();
};

/* Install a SIGSEGV handler to detect stack overflows. */
void detectStackOverflow();

}  // namespace nix
