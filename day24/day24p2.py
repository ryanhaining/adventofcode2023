import itertools
import sys
from dataclasses import dataclass

import z3  # type: ignore


@dataclass
class Vec:
    dx: int
    dy: int
    dz: int


@dataclass
class Point:
    x: int
    y: int
    z: int


@dataclass
class HailStone:
    position: Point
    velocity: Vec


def line_to_hailstone(line: str) -> HailStone:
    pts, vecs = line.split(" @ ")
    x, y, z = [int(i) for i in pts.split(", ")]
    dx, dy, dz = [int(i) for i in vecs.split(", ")]
    return HailStone(Point(x, y, z), Vec(dx, dy, dz))


def main():
    # we only actually need the first 3 hailstones.
    hailstones = [
        line_to_hailstone(line.rstrip()) for line in itertools.islice(sys.stdin, 3)
    ]

    t1 = z3.Int("t1")
    t2 = z3.Int("t2")
    t3 = z3.Int("t3")

    x0 = z3.Int("x0")
    y0 = z3.Int("y0")
    z0 = z3.Int("z0")

    dx = z3.Int("dx")
    dy = z3.Int("dy")
    dz = z3.Int("dz")

    solver = z3.Solver()
    for hs, t in zip(hailstones, [t1, t2, t3]):
        solver.add(
            [
                hs.position.x + t * hs.velocity.dx == x0 + t * dx,
                hs.position.y + t * hs.velocity.dy == y0 + t * dy,
                hs.position.z + t * hs.velocity.dz == z0 + t * dz,
            ]
        )
    solver.check()
    m = solver.model()
    print(sum(m.evaluate(n).as_long() for n in [x0, y0, z0]))


if __name__ == "__main__":
    main()
