#include <algorithm>
#include <cstdint>
#include <fstream>
#include <iostream>
#include <string_view>

/* NAR grammar:
 *  "nix-archive-1"
 *  ( "type" "symlink" "target" string )
 *  ( "type" "regular" "executable"? "contents" bytes )
 *  ( "type" "directory"
 *    repeat: "entry" ( "name" string "node" node )
 *  )
 */

class writer {
  std::ostream& stream_;

  void pad(std::uint64_t n) {
    static const char zeroes[8] = {};
    if (n % 8) {
      stream_.write(zeroes, 8 - (n % 8));
    }
  }

  auto operator<<(std::uint64_t value) -> writer& {
    char buffer[8];
    for (int i = 0; i < 8; i++) {
      buffer[i] = value >> (8 * i);
    }

    stream_.write(buffer, sizeof buffer);
    return *this;
  };

  auto operator<<(std::string_view value) -> writer& {
    *this << value.size();
    stream_.write(value.data(), value.size());
    pad(value.size());

    return *this;
  };

  auto operator<<(std::streambuf& src) -> writer& {
    std::streampos pos =
        src.pubseekoff(0, std::ios_base::cur, std::ios_base::in);
    std::streampos end =
        src.pubseekoff(0, std::ios_base::end, std::ios_base::in);
    src.pubseekpos(pos, std::ios_base::in);
    std::uint64_t len = end - pos;

    *this << len;
    stream_ << &src;
    pad(len);

    return *this;
  }

 public:
  writer() = delete;
  writer(std::ostream& stream) : stream_(stream) { *this << "nix-archive-1"; };

  void symlink(std::string_view target) {
    *this << "("
          << "type"
          << "symlink"
          << "target" << target << ")";
  };

  void file(bool executable, std::streambuf& src) {
    *this << "("
          << "type"
          << "regular";
    if (executable) {
      *this << "executable"
            << "";
    }
    *this << "contents" << src << ")";
  };

  class directory_entries {
    writer& writer_;

    directory_entries() = delete;
    friend writer;

   protected:
    directory_entries(class writer& w) : writer_(w){};

   public:
    template <class F>
    void operator()(std::string_view name, F f) {
      writer_ << "entry"
              << "("
              << "name" << name << "node";
      f(writer_);
      writer_ << ")";
    }
  };

  template <class F>
  void directory(F f) {
    *this << "("
          << "type"
          << "directory";
    directory_entries entries(*this);
    f(entries);
    *this << ")";
  };
};
