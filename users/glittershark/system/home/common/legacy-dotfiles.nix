with import <nixpkgs> {};
fetchgit {
  url = "https://github.com/glittershark/dotfiles.git";
  rev = "e0c7f2592fbc2f9942763d2146d362a1314630e9";
  # date = "2020-03-25T20:38:51-04:00";
  sha256 = "126zy4ff6nl2vma2s74waksim7j5h3n6qpaxnnn17vkc1cq0fcd9";
  fetchSubmodules = false;
}
