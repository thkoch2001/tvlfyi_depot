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

class Writer {
  void Pad(std::uint64_t n) {
    static const char zeroes[8] = {};
    if (n % 8) {
      stream_.write(zeroes, 8 - (n % 8));
    }
  }

  Writer& operator<<(std::uint64_t value) {
    char buffer[8];
    for (int i = 0; i < 8; i++) {
      buffer[i] = value >> (8 * i);
    }

    stream_.write(buffer, sizeof buffer);
    return *this;
  };

  Writer& operator<<(std::string_view value) {
    *this << value.size();
    stream_.write(value.data(), value.size());
    Pad(value.size());

    return *this;
  };

  Writer& operator<<(std::streambuf& src) {
    std::streampos pos =
        src.pubseekoff(0, std::ios_base::cur, std::ios_base::in);
    std::streampos end =
        src.pubseekoff(0, std::ios_base::end, std::ios_base::in);
    src.pubseekpos(pos, std::ios_base::in);
    std::uint64_t len = end - pos;

    *this << len;
    stream_ << &src;
    Pad(len);

    return *this;
  }

 public:
  Writer() = delete;
  Writer(const Writer&) = delete;
  Writer& operator=(Writer const&) = delete;

  explicit Writer(std::ostream& stream) : stream_(stream) {
    *this << "nix-archive-1";
  };

  void Symlink(std::string_view target) {
    *this << "("
          << "type"
          << "symlink"
          << "target" << target << ")";
  };

  void File(bool executable, std::streambuf& src) {
    *this << "("
          << "type"
          << "regular";
    if (executable) {
      *this << "executable"
            << "";
    }
    *this << "contents" << src << ")";
  };

  class DirectoryEntries {
    Writer& writer_;

    DirectoryEntries() = delete;
    DirectoryEntries(const DirectoryEntries&) = delete;
    DirectoryEntries& operator=(DirectoryEntries const&) = delete;
    friend Writer;

   protected:
    explicit DirectoryEntries(class Writer& w) : writer_(w){};

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
  void Directory(F f) {
    *this << "("
          << "type"
          << "directory";
    DirectoryEntries entries(*this);
    f(entries);
    *this << ")";
  };

 private:
  std::ostream& stream_;
};
