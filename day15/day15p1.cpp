#include <format>
#include <iostream>
#include <string>

int main() {
  std::string term{};
  long total = 0;
  while (std::getline(std::cin, term, ',')) {
    int value{};
    for (char c : term) {
      if (c == '\n') {
        continue;
      }
      value = ((value + c) * 17) % 256;
    }
    total += value;
  }
  std::cout << total << '\n';
}
