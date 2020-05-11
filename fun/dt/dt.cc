#include <iostream>
#include <vector>

#include "absl/flags/flag.h"
#include "absl/flags/parse.h"
#include "absl/hash/hash.h"
#include "absl/strings/str_cat.h"
#include "absl/time/clock.h"
#include "absl/time/time.h"
#include "absl/types/optional.h"
#include "farmhash.h"

ABSL_FLAG(std::vector<std::string>, word, {}, "words to use");
ABSL_FLAG(int, range, 100, "operating range");

struct Result {
  std::string a;
  absl::optional<std::string> p;
};

std::string which(const std::vector<std::string>& words) {
  uint64_t fp;
  std::string word;

  for (const auto& w : words) {
    auto nfp = util::Fingerprint64(w);
    if (nfp > fp) {
      fp = nfp;
      word = w;
    }
  }

  return word;
}

Result decide(const std::vector<std::string>& words) {
  auto input = absl::FormatTime("%Y%m%d", absl::Now(), absl::UTCTimeZone());
  for (const auto& w : words) {
    input += w;
  }

  auto base = util::Fingerprint64(input) % (absl::GetFlag(FLAGS_range) + 1);

  Result result = { "nope" };

  if (base % 10 == 0) {
    result.a = "ca";
  } else if (base % 9 == 0) {
    result.a = "c1";
    result.p = which(words);
  } else if (base % 8 == 0) {
    result.a = "ea";
  } else if (base % 7 == 0) {
    result.a = "e1";
    result.p = which(words);
  } else if (base % 3 == 0) {
    result.a = "skip";
  }

  return result;
}

int main(int argc, char *argv[]) {
  absl::ParseCommandLine(argc, argv);

  auto words = absl::GetFlag(FLAGS_word);
  if (words.size() < 2) {
    std::cerr << "needs at least two!" << std::endl;
    return 1;
  }

  auto result = decide(words);
  std::cout << result.a
            << (result.p.has_value() ? absl::StrCat(" ", "(", result.p.value(), ")")
                                     : "")
            << std::endl;
}
