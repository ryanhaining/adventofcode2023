#include <algorithm>
#include <cassert>
#include <format>
#include <iostream>
#include <iterator>
#include <unordered_set>
#include <vector>

namespace {
struct Coords {
  std::pair<int, int> xs{};
  std::pair<int, int> ys{};
  std::pair<int, int> zs{};
};

std::istream& operator>>(std::istream& in, Coords& coords) {
  in >> coords.xs.first;
  in.ignore(1);  // ,
  in >> coords.ys.first;
  in.ignore(1);  // ,
  in >> coords.zs.first;
  --coords.zs.first;  // idk why the z coords 1-indexed in the input

  in.ignore(1);  // ~

  in >> coords.xs.second;
  in.ignore(1);  // ,
  in >> coords.ys.second;
  in.ignore(1);  // ,
  in >> coords.zs.second;
  --coords.zs.second;

  return in;
}

struct Brick {
  int id{};
  std::pair<int, int> xs{};
  std::pair<int, int> ys{};
};

struct Level {
  std::vector<Brick> bricks;
};

bool overlap_impl(const std::pair<int, int>& p1,
                  const std::pair<int, int>& p2) {
  return (p1.first <= p2.first && p2.first <= p1.second) ||
         (p1.first <= p2.second && p2.second <= p1.second);
}

bool overlap(const std::pair<int, int>& p1, const std::pair<int, int>& p2) {
  return overlap_impl(p1, p2) || overlap_impl(p2, p1);
}

std::unordered_set<int> check_collisions(const std::vector<Brick>& v,
                                         const Coords& coords) {
  std::unordered_set<int> collisions;
  for (const auto& brick : v) {
    if (overlap(brick.xs, coords.xs) && overlap(brick.ys, coords.ys)) {
      collisions.insert(brick.id);
    }
  }
  return collisions;
}

}  // namespace

int main() {
  std::vector<Coords> all_coords(std::istream_iterator<Coords>{std::cin},
                                 std::istream_iterator<Coords>{});
  // This feels like it might be against the spirit of the problem.
  std::sort(
      all_coords.begin(), all_coords.end(),
      [](const auto& c1, const auto& c2) { return c1.zs.first < c2.zs.first; });

  std::vector<std::vector<bool>> rests_on(all_coords.size(),
                                          std::vector<bool>(all_coords.size()));

  std::vector<Level> levels(1);

  int brick_id_tracker = 0;
  for (const auto& coord : all_coords) {
    int brick_id = brick_id_tracker++;
    long level_index = static_cast<long>(levels.size() - 1);
    while (level_index >= 0) {
      auto& level = levels[level_index];
      auto collisions = check_collisions(level.bricks, coord);

      if (!collisions.empty()) {
        for (auto collision_bid : collisions) {
          assert(brick_id != collision_bid);
          rests_on[brick_id][collision_bid] = true;
        }
        break;
      }
      --level_index;
    }
    for (auto zi = coord.zs.first; zi <= coord.zs.second; ++zi) {
      ++level_index;  // go up to where there is space
      if (level_index >= static_cast<long>(levels.size())) {
        levels.emplace_back(Level{});
      }
      auto& insert_level = levels[level_index];
      insert_level.bricks.push_back({brick_id, coord.xs, coord.ys});
    }
  }
  std::vector<bool> can_remove(all_coords.size(), true);
  for (const auto& row : rests_on) {
    if (std::count(row.begin(), row.end(), true) == 1) {
      auto supporter = std::find(row.begin(), row.end(), true) - row.begin();
      can_remove[supporter] = false;
    }
  }
  std::cout << std::count(can_remove.begin(), can_remove.end(), true) << '\n';
}
