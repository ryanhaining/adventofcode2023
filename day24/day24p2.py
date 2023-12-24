import itertools
import sys
from dataclasses import dataclass
from typing import Sequence

import z3  # type: ignore


@dataclass
class HailStone:
    positions: Sequence[int]
    velocities: Sequence[int]


def line_to_hailstone(line: str) -> HailStone:
    pts, vecs = line.split(' @ ')
    return HailStone(
        positions=[int(i) for i in pts.split(', ')],
        velocities=[int(i) for i in vecs.split(', ')],
    )


def main():
    # we only actually need the first 3 hailstones.
    hailstones = [
        line_to_hailstone(line.rstrip()) for line in itertools.islice(sys.stdin, 3)
    ]

    time_vars = [z3.Int(f't{i+1}') for i in range(3)]
    pos_vars = [z3.Int(f'{c}0') for c in 'xyz']
    vel_vars = [z3.Int(f'd{c}0') for c in 'xyz']

    assert len(time_vars) == len(hailstones)
    assert len(pos_vars) == len(vel_vars)
    for hs in hailstones:
        assert len(pos_vars) == len(hailstones[0].positions)
        assert len(vel_vars) == len(hailstones[0].velocities)

    solver = z3.Solver()
    for hs, t in zip(hailstones, time_vars):
        for hs_pos, hs_vel, pos, vel in zip(
            hs.positions, hs.velocities, pos_vars, vel_vars
        ):
            solver.add(hs_pos + t * hs_vel == pos + t * vel)
    solver.check()
    m = solver.model()
    print(sum(m.evaluate(n).as_long() for n in pos_vars))


if __name__ == '__main__':
    main()
