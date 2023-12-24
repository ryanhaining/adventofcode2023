#include <algorithm>
#include <array>
#include <cassert>
#include <iostream>
#include <iterator>
#include <optional>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

namespace {

using Point = std::pair<std::size_t, std::size_t>;
using Grid = std::vector<std::string>;

enum class Direction { up, down, left, right };

constexpr std::array ALL_DIRECTIONS = {
    Direction::up,
    Direction::down,
    Direction::left,
    Direction::right,
};

Point adjust(Direction d, Point p) {
  switch (d) {
    case Direction::up:
      --p.first;
      break;
    case Direction::down:
      ++p.first;
      break;
    case Direction::left:
      --p.second;
      break;
    case Direction::right:
      ++p.second;
      break;
  }
  return p;
}

struct Node;

struct Edge {
  Node* dest{};
  int length{};
};

struct Node {
  bool is_used{};
  int distance{};
  std::vector<Edge> edges;
};

struct PointHash {
  std::size_t operator()(const Point& p) const {
    return std::hash<std::size_t>{}(p.first) ^
           (std::hash<std::size_t>{}(p.second) << 1);
  }
};

using PointToNode = std::unordered_map<Point, Node, PointHash>;

constexpr std::string PATH_CHARS = ".X";

bool is_in_bounds(const Grid& grid, const Point& p) {
  return p.first > 0 && p.first < grid.size() && p.second > 0 &&
         p.second < grid[0].size();
}

bool is_path(const Grid& grid, const Point& p) {
  return is_in_bounds(grid, p) && PATH_CHARS.contains(grid[p.first][p.second]);
}

bool is_free_path(const Grid& grid, const Point& p) {
  return is_in_bounds(grid, p) && grid[p.first][p.second] == '.';
}

bool is_vertex(const Grid& grid, Point p) {
  return is_path(grid, p) &&
         std::count_if(ALL_DIRECTIONS.begin(), ALL_DIRECTIONS.end(),
                       [&p, &grid](Direction d) {
                         return is_path(grid, adjust(d, p));
                       }) != 2;
}

void collect_vertexes_impl(Grid& grid, PointToNode& all_nodes, Node* prev,
                           Point current, int distance) {
  assert(prev != nullptr);
  if (!is_path(grid, current)) {
    return;
  }
  if (is_vertex(grid, current)) {
    auto& n = all_nodes[current];
    n.edges.push_back({prev, distance});
    prev->edges.push_back({&n, distance});

    prev = &n;
    distance = 0;
  }
  if (!is_free_path(grid, current)) {
    return;
  }

  grid[current.first][current.second] = 'X';
  for (auto d : ALL_DIRECTIONS) {
    collect_vertexes_impl(grid, all_nodes, prev, adjust(d, current),
                          distance + 1);
  }
}

PointToNode collect_vertexes(Grid grid) {
  PointToNode all_nodes = {{{0, 1}, {}}};
  grid[0][1] = 'X';
  collect_vertexes_impl(grid, all_nodes, &all_nodes[{0, 1}], {1, 1}, 1);
  return all_nodes;
}

std::optional<int> dfs(Node* current, Node* end, int distance) {
  if (current == end) {
    return distance;
  }
  if (current->is_used) {
    return std::nullopt;
  }
  std::vector<int> results;
  current->is_used = true;
  for (const auto& edge : current->edges) {
    auto ans = dfs(edge.dest, end, distance + edge.length);
    if (ans.has_value()) {
      results.push_back(ans.value());
    }
  }
  current->is_used = false;
  auto max = std::max_element(results.begin(), results.end());
  return max != results.end() ? std::optional<int>{*max} : std::nullopt;
}

int find_longest(Node* current, Node* end) {
  return dfs(current, end, 0).value();
}

constexpr std::string SLOPE_CHARS = "^v<>";
}  // namespace

int main() {
  Grid grid(std::istream_iterator<std::string>{std::cin},
            std::istream_iterator<std::string>{});
  for (auto& line : grid) {
    std::replace_if(
        line.begin(), line.end(),
        [](char c) { return SLOPE_CHARS.contains(c); }, '.');
  }
  auto all_nodes = collect_vertexes(grid);

  auto result =
      find_longest(&all_nodes[{0, 1}],
                   &all_nodes[{grid.size() - 1, grid.back().size() - 2}]);
  std::cout << result << '\n';
}
