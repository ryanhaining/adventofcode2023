#include <iostream>
#include <iterator>
#include <string>
#include <vector>

namespace {

struct Vec {
  long dx;
  long dy;
};

struct Point {
  long long x;
  long long y;

  Point operator+(const Vec& v) const {
    auto result = *this;
    result.x += v.dx;
    result.y += v.dy;
    return result;
  }
};

bool same_direction(long src, double dest, long diff) {
  return (dest > src && diff > 0) || (dest < src && diff < 0);
}

struct HailStone {
  Point position{};
  Vec velocity{};

  bool is_in_future(double px, double py) const {
    return same_direction(position.x, px, velocity.dx) &&
           same_direction(position.y, py, velocity.dy);
  }
};

std::istream& operator>>(std::istream& in, HailStone& hs) {
  in >> hs.position.x;
  in.ignore(1);  // ,
  in >> hs.position.y;
  in.ignore(1);  // ,
  long ignore;
  in >> ignore;

  in.ignore(3);  // " @ "

  in >> hs.velocity.dx;
  in.ignore(1);  // ,
  in >> hs.velocity.dy;
  in.ignore(1);  // ,
  in >> ignore;

  return in;
}

std::pair<double, double> intersect_at(const HailStone& hs1,
                                       const HailStone& hs2) {
  const auto& p1_1 = hs1.position;
  auto p1_2 = p1_1 + hs1.velocity;

  const auto& p2_1 = hs2.position;
  auto p2_2 = p2_1 + hs2.velocity;

  // Need to convert to double here otherwise the multiplications
  // produce values that can't fit in 64 bit longs (-fsanitize=undefined)
  double x1 = p1_1.x;
  double x2 = p1_2.x;
  double x3 = p2_1.x;
  double x4 = p2_2.x;

  double y1 = p1_1.y;
  double y2 = p1_2.y;
  double y3 = p2_1.y;
  double y4 = p2_2.y;

  // https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection#Given_two_points_on_each_line
  auto px =
      ((x1 * y2 - y1 * x2) * (x3 - x4) - (x1 - x2) * (x3 * y4 - y3 * x4)) /
      ((x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4));

  auto py =
      ((x1 * y2 - y1 * x2) * (y3 - y4) - (y1 - y2) * (x3 * y4 - y3 * x4)) /
      ((x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4));
  return {px, py};
}

constexpr double MIN_POSITION = 200'000'000'000'000;
constexpr double MAX_POSITION = 400'000'000'000'000;

bool is_in_bounds(double d) { return MIN_POSITION <= d && d <= MAX_POSITION; }

constexpr auto VELOCITY_SCALE = 1'000'000'000L;

}  // namespace

int main() {
  std::vector<HailStone> hailstones(std::istream_iterator<HailStone>{std::cin},
                                    std::istream_iterator<HailStone>{});

  for (auto& hs : hailstones) {
    // We're dealing with really big position values
    // which makes the velocities too insignificant to be accurate
    // once we are dealing with doubles. (e.g. we can't accurately
    // add 57 to >250 trillion).
    // The alternative is to rewrite in Python where int can store
    // those huge values.
    hs.velocity.dx *= VELOCITY_SCALE;
    hs.velocity.dy *= VELOCITY_SCALE;
  }

  long count{};
  for (auto it1 = hailstones.begin(); it1 != hailstones.end(); ++it1) {
    for (auto it2 = it1 + 1; it2 != hailstones.end(); ++it2) {
      auto [px, py] = intersect_at(*it1, *it2);
      if (is_in_bounds(px) && is_in_bounds(py) && it1->is_in_future(px, py) &&
          it2->is_in_future(px, py)) {
        ++count;
      }
    }
  }
  std::cout << count << '\n';
}
