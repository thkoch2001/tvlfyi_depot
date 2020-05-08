#include <iostream>

#include "absl/flags/flag.h"
#include "absl/flags/parse.h"
#include "absl/hash/hash.h"
#include "absl/time/time.h"
#include "absl/time/clock.h"
#include "absl/strings/str_cat.h"
#include "farmhash.h"

ABSL_FLAG(std::string, one, "", "first word");
ABSL_FLAG(std::string, two, "", "second word");
ABSL_FLAG(int, range, 100, "operating range");

void which(std::string one, std::string two) {
  if (util::Fingerprint64(one) > util::Fingerprint64(two)) {
    std::cout << one << std::endl;
  } else {
    std::cout << two << std::endl;
  }
}

std::string decide(std::string one, std::string two) {
  auto date = absl::FormatTime("%Y%m%d", absl::Now(), absl::UTCTimeZone());
  auto base = util::Fingerprint64(absl::StrCat(date, one, two))
              % (absl::GetFlag(FLAGS_range) + 1);

  if (base % 10 == 0) {
    return "c2";
  } else if (base % 9 == 0) {
    which(one, two);
    return "c1";
  } else if (base % 8 == 0) {
    return "e2";
  } else if (base % 7 == 0) {
    which(one, two);
    return "e1";
  } else if (base % 3 == 0) {
    return "skip";
  }

  return "nope";
}

int main(int argc, char *argv[]) {
  absl::ParseCommandLine(argc, argv);
  auto one = absl::GetFlag(FLAGS_one);
  auto two = absl::GetFlag(FLAGS_two);

  if (one.empty() || two.empty()) {
    std::cerr << "both are required!" << std::endl;
    return 1;
  }

  std::cout << decide(one, two) << std::endl;
}
