import abc
import itertools
import sys

from dataclasses import dataclass
from typing import Dict, List, Optional, Tuple

class Tile(abc.ABC):
    pass

class Blank(Tile):
    def __repr__(self) -> str:
        return 'Blank'

class MaybeGear(Tile):
    def __repr__(self) -> str:
        return 'Gear'

@dataclass
class Num(Tile):
    value: int = 0
    def append_digit(self, digit: int) -> None:
        self.value = (self.value*10) + digit

def read_line(line: str) -> List[Tile]:
    tiles: List[Tile] = [Blank()] * len(line)
    cur_num: Optional[Num] = None
    for i, c in enumerate(line):
        if c.isdigit():
            if cur_num is None:
                cur_num = Num()
            cur_num.append_digit(int(c))
            tiles[i] = cur_num
        else:
            if c == '*':
                tiles[i] = MaybeGear()
            cur_num = None
    return tiles

def sum_gears(
        prev_line: List[Tile],
        current_line: List[Tile],
        next_line: List[Tile]) -> int:
    total = 0
    # yikes.
    for i, tile in enumerate(current_line):
        match tile:
            case MaybeGear():
                nums: Dict[int, Num] = {}
                for col in range(max(0, i-1), min(len(current_line), i+2)):
                    for line in (prev_line, current_line, next_line):
                        match n := line[col]:
                            case Num(v):
                                if id(v) not in nums:
                                    nums[id(v)] = n
                match list(nums.values()):
                    case [Num(v1), Num(v2)]:
                        total += v1 * v2
    return total


total = 0
current_line = read_line(sys.stdin.readline().rstrip())
prev_line: List[Tile] = [Blank()] * len(current_line) 
for line in itertools.chain(sys.stdin, ['.'*len(current_line)]):
    line = line.rstrip()
    next_line = read_line(line)
    total += sum_gears(prev_line, current_line, next_line)
    prev_line, current_line = current_line, next_line

print(total)
