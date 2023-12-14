#include <cassert>
#include <format>
#include <iostream>
#include <iterator>
#include <string>
#include <utility>
#include <vector>

namespace {
struct Point {
  std::size_t row;
  std::size_t col;
  bool operator==(const Point&) const = default;
  bool operator!=(const Point&) const = default;
};

Point find_start(const std::vector<std::string>& lines) {
  for (std::size_t row{}; row < lines.size(); ++row) {
    for (std::size_t col{}; col < lines[row].size(); ++col) {
      if (lines[row][col] == 'S') {
        return {row, col};
      }
    }
  }
  std::cerr << "no Start position\n";
  std::terminate();
}

enum class Direction { left, right, up, down };
bool can_connect(char c, Direction d) {
  return c == '-'   ? d == Direction::left || d == Direction::right
         : c == '|' ? d == Direction::up || d == Direction::down
         : c == 'F' ? d == Direction::right || d == Direction::down
         : c == '7' ? d == Direction::left || d == Direction::down
         : c == 'L' ? d == Direction::right || d == Direction::up
         : c == 'J' ? d == Direction::left || d == Direction::up
                    : false;
}

Point find_next(const std::vector<std::string>& lines, Point prev,
                Point current) {
  if (current.row != 0 && current.row - 1 != prev.row &&
      can_connect(lines[current.row][current.col], Direction::up)) {
    return {current.row - 1, current.col};
  }
  if (current.row != lines.size() - 1 && current.row + 1 != prev.row &&
      can_connect(lines[current.row][current.col], Direction::down)) {
    return {current.row + 1, current.col};
  }
  if (current.col != 0 && current.col - 1 != prev.col &&
      can_connect(lines[current.row][current.col], Direction::left)) {
    return {current.row, current.col - 1};
  }
  if (current.col != lines[current.row].size() - 1 &&
      current.col + 1 != prev.col &&
      can_connect(lines[current.row][current.col], Direction::right)) {
    return {current.row, current.col + 1};
  }
  std::cerr << "stuck\n";
  std::terminate();
}

Point find_first(const std::vector<std::string>& lines, Point start) {
  assert(lines[start.row][start.col] == 'S');
  if (start.row != 0 &&
      can_connect(lines[start.row - 1][start.col], Direction::down)) {
    return Point{start.row - 1, start.col};
  }
  if (start.row != lines.size() - 1 &&
      can_connect(lines[start.row + 1][start.col], Direction::up)) {
    return Point{start.row + 1, start.col};
  }
  if (start.col != 0 &&
      can_connect(lines[start.row][start.col - 1], Direction::right)) {
    return Point{start.row, start.col - 1};
  }
  if (start.col != lines[start.row].size() - 1 &&
      can_connect(lines[start.row][start.col + 1], Direction::left)) {
    return Point{start.row, start.col + 1};
  }
  std::cerr << "nothing connects to start\n";
  std::terminate();
}

}  // namespace

int main() {
  std::vector<std::string> lines{std::istream_iterator<std::string>{std::cin},
                                 std::istream_iterator<std::string>{}};
  auto start = find_start(lines);

  auto prev = start;
  auto current = find_first(lines, start);
  std::size_t steps{1};
  while (current != start) {
    auto next = find_next(lines, prev, current);
    prev = current;
    current = next;
    ++steps;
  }
  // the furthest one away will be half the length of the loop.
  std::cout << (steps / 2) + (steps % 2) << '\n';
}
