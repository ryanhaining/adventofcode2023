#include <algorithm>
#include <cctype>
#include <functional>
#include <iostream>
#include <string>
#include <unordered_map>

namespace {
std::unordered_map<std::string, int> DIGIT_WORDS = {
    {"zero", 0}, {"one", 1}, {"two", 2},   {"three", 3}, {"four", 4},
    {"five", 5}, {"six", 6}, {"seven", 7}, {"eight", 8}, {"nine", 9},
};

int find_digit(std::string::iterator it, std::string::iterator end,
               std::function<void(std::string::iterator&)> update) {
  while (true) {
    if (std::isdigit(*it)) {
      return *it - '0';
    }
    for (const auto& [num, value] : DIGIT_WORDS) {
      if (static_cast<std::size_t>(std::distance(it, end)) >= num.size() &&
          num == std::string_view(it, it + num.size())) {
        return value;
      }
    }
    update(it);
  }
}

int extract_num(std::string line) {
  return find_digit(std::begin(line), std::end(line),
                    [](std::string::iterator& it) { ++it; }) *
             10 +
         find_digit(std::end(line) - 1, std::end(line),
                    [](std::string::iterator& it) { --it; });
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
