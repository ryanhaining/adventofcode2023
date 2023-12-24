import itertools
import sys
from dataclasses import dataclass


@dataclass
class Vec:
    dx: int
    dy: int


@dataclass
class Point:
    x: int
    y: int

    def __add__(self, v: Vec) -> "Point":
        return Point(self.x + v.dx, self.y + v.dy)


def same_direction(src: int, dest: float, diff: int) -> bool:
    return (dest > src and diff > 0) or (dest < src and diff < 0)


@dataclass
class HailStone:
    position: Point
    velocity: Vec

    def is_in_future(self, px: float, py: float) -> bool:
        return same_direction(self.position.x, px, self.velocity.dx) and same_direction(
            self.position.y, py, self.velocity.dy
        )


MIN_POSITION = 200_000_000_000_000
MAX_POSITION = 400_000_000_000_000


def is_in_bounds(p: float) -> bool:
    return MIN_POSITION <= p <= MAX_POSITION


def have_valid_intersection(hs1: HailStone, hs2: HailStone) -> bool:
    p1_1 = hs1.position
    p1_2 = p1_1 + hs1.velocity

    p2_1 = hs2.position
    p2_2 = p2_1 + hs2.velocity
    x1 = p1_1.x
    x2 = p1_2.x
    x3 = p2_1.x
    x4 = p2_2.x
    y1 = p1_1.y
    y2 = p1_2.y
    y3 = p2_1.y
    y4 = p2_2.y
    # https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection#Given_two_points_on_each_line
    try:
        px = ((x1 * y2 - y1 * x2) * (x3 - x4) - (x1 - x2) * (x3 * y4 - y3 * x4)) / (
            (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
        )

        py = ((x1 * y2 - y1 * x2) * (y3 - y4) - (y1 - y2) * (x3 * y4 - y3 * x4)) / (
            (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
        )

        return (
            is_in_bounds(px)
            and is_in_bounds(py)
            and hs1.is_in_future(px, py)
            and hs2.is_in_future(px, py)
        )
    except ZeroDivisionError:
        return False


def line_to_hailstone(line: str) -> HailStone:
    pts, vecs = line.split(" @ ")
    x, y, _ = [int(i) for i in pts.split(", ")]
    dx, dy, _ = [int(i) for i in vecs.split(", ")]
    return HailStone(Point(x, y), Vec(dx, dy))


def main():
    hailstones = [line_to_hailstone(line.rstrip()) for line in sys.stdin]

    count = sum(
        have_valid_intersection(hs1, hs2)
        for i, hs1 in enumerate(hailstones)
        for hs2 in itertools.islice(hailstones, i + 1, None)
    )
    print(count)


if __name__ == "__main__":
    main()
