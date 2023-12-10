import sys

from dataclasses import dataclass
from typing import List

@dataclass
class Copy:
    num: int
    remaining: int

total = 0
copies: List[Copy] = []
for line in sys.stdin:
    current_copies = 1 + sum(c.num for c in copies)
    for c in copies:
        c.remaining -= 1
    copies = [c for c in copies if c.remaining]

    num_matches = len(
            set.intersection(*[{int(w) for w in section.strip().split()}
                for section in line.split(':')[1].strip().split('|')]))
    if num_matches:
        copies.append(Copy(current_copies, num_matches))
    total += current_copies

print(total)
