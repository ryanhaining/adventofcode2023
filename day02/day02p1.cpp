#include <format>
#include <iostream>
#include <sstream>
#include <string>

namespace {
constexpr int MAX_RED = 12;
constexpr int MAX_GREEN = 13;
constexpr int MAX_BLUE = 14;

bool under_limit(char color, int num) {
  switch (color) {
    case 'r':
      return num <= MAX_RED;
    case 'g':
      return num <= MAX_GREEN;
    case 'b':
      return num <= MAX_BLUE;
    default:
      std::cerr << std::format("invalid color color: {}\n", color);
      std::terminate();
  }
}
}  // namespace

int main() {
  std::string line{};
  std::string color{};  // defined outside loop for reuse.
  int id_total{};
  while (std::getline(std::cin, line)) {
    std::istringstream line_stream{line};
    line_stream.ignore(1024, ' ');  // skip the word "Game"
    int game_id{};
    line_stream >> game_id;
    line_stream.ignore(2, ':');
    int num{};
    bool legal = true;
    while (line_stream >> num) {
      line_stream >> color;
      if (!under_limit(color[0], num)) {
        legal = false;
        break;
      }
    }
    if (legal) {
      id_total += game_id;
    }
  }
  std::cout << id_total << '\n';
}
