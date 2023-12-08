#include <format>
#include <iostream>
#include <sstream>
#include <string>
#include <unordered_map>

namespace {
class ColorCounts {
 public:
  int power() const {
    // I thought they might've given a game where there was never a single
    // cube of a certain color, meaning the power is 0.
    // I didn't get anything like that so I didn't have to add the actual
    // special case for it but this was just to crash if it happened.
    if (counts_.size() < 3) {
      std::cerr << std::format("size = {}\n", counts_.size());
      std::terminate();
    }
    int result = 1;
    for (const auto& [_, v] : counts_) {
      result *= v;
    }
    return result;
  }

  void update(char c, int num) {
    if (num > counts_[c]) {
      counts_[c] = num;
    }
  }

 private:
  std::unordered_map<char, int> counts_;
};
}  // namespace

int main() {
  std::string line{};
  std::string color{};  // defined outside loop for reuse.
  int power_total{};
  while (std::getline(std::cin, line)) {
    line.erase(0, line.find(':') + 2);
    std::istringstream line_stream{line};
    int num{};
    ColorCounts counts{};
    while (line_stream >> num) {
      line_stream >> color;
      counts.update(color[0], num);
    }
    power_total += counts.power();
  }
  std::cout << power_total << '\n';
}
