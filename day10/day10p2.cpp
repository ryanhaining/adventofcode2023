#include <algorithm>
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

constexpr std::string pipe_chars{"S-|JLF7"};
constexpr std::string non_loop_chars{"_."};
constexpr std::string open_right{"S-FL"};
constexpr std::string open_left{"S-J7"};
constexpr std::string open_bottom{"S|F7"};
constexpr std::string open_top{"S|JL"};

template <typename T, typename V>
void stretch(T& container, V default_value) {
  container.resize(container.size() * 2, default_value);
  auto fwdit = container.begin() + container.size() / 2 - 1;
  for (auto it = container.rbegin(); it != container.rend(); it += 2, --fwdit) {
    *it = *fwdit;
    *fwdit = default_value;
  }
  container.push_back(default_value);
}

void flood_fill(std::vector<std::string>& lines, Point current) {
  auto& cur_char = lines[current.row][current.col];
  if (cur_char == 'X') {
    return;
  }
  if (pipe_chars.contains(cur_char)) {
    return;
  }
  cur_char = 'X';
  if (current.row != 0) {
    flood_fill(lines, {current.row - 1, current.col});
  }
  if (current.row != lines.size() - 1) {
    flood_fill(lines, {current.row + 1, current.col});
  }
  if (current.col != 0) {
    flood_fill(lines, {current.row, current.col - 1});
  }
  if (current.col != lines[current.row].size() - 1) {
    flood_fill(lines, {current.row, current.col + 1});
  }
}

}  // namespace

int main() {
  std::vector<std::string> lines{std::istream_iterator<std::string>{std::cin},
                                 std::istream_iterator<std::string>{}};

  auto start = find_start(lines);

  auto prev = start;
  auto current = find_first(lines, start);
  while (current != start) {
    auto next = find_next(lines, prev, current);
    prev = current;
    // offsetting all the ascii values for the actual loop so the
    // replace_if below doesn't see them.
    --lines[current.row][current.col];
    current = next;
  }
  --lines[current.row][current.col];

  for (auto& line : lines) {
    std::replace_if(
        line.begin(), line.end(), [](char c) { return pipe_chars.contains(c); },
        '_');
  }

  // switch the pipe pieces back. We could skip this by creating a nother set
  // of pipe characters but it would be too hard to look at.
  for (auto& line : lines) {
    for (auto& c : line) {
      if (!non_loop_chars.contains(c)) {
        ++c;
      }
    }
  }

  // double the size of everything and add connecting pieces
  // "F7" becomes "F-7", but "J7" becomes "J 7"
  // the spaces between the unconnected sections allows the flood fill to
  // squeeze between the pipes
  // also adding blanks around the edges so the flood fill can reach everything
  for (auto& line : lines) {
    stretch(line, ' ');
    for (auto it = line.begin() + 2, end = line.end() - 1; it != end; it += 2) {
      if (open_right.contains(*(it - 1)) && open_left.contains(*(it + 1))) {
        *it = '-';
      }
    }
  }
  stretch(lines, std::string(lines[0].size(), ' '));
  for (auto it = lines.begin() + 2, end = lines.end() - 1; it != end; it += 2) {
    for (auto prev = (it - 1)->begin(), current = it->begin(),
              next = (it + 1)->begin();
         current != it->end(); ++prev, ++current, ++next) {
      if (open_bottom.contains(*prev) && open_top.contains(*next)) {
        *current = '|';
      }
    }
  }

  flood_fill(lines, {0, 0});

  std::size_t total = 0;
  for (const auto& line : lines) {
    total += std::count_if(line.begin(), line.end(),
                           [](char c) { return non_loop_chars.contains(c); });
  }
  std::cout << total << '\n';
}
