#include <algorithm>
#include <cctype>
#include <iostream>
#include <string>

namespace {
template <std::bidirectional_iterator It>
int find_digit(It begin, It end) {
  return *std::find_if(begin, end, [](char c) { return std::isdigit(c); }) -
         '0';
}

int extract_num(std::string line) {
  return find_digit(std::begin(line), std::end(line)) * 10 +
         find_digit(std::rbegin(line), std::rend(line));
}
}  // namespace

int main() {
  std::string line{};
  int total{};
  while (std::getline(std::cin, line)) {
    total += extract_num(line);
  }
  std::cout << total << '\n';
}
