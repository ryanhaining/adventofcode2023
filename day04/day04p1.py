import sys

from typing import Set

def score(winning: Set[int], have: Set[int]) -> int:
    return (1 << (count-1)) if (count := len(winning & have)) else 0

total = sum(score(*[{int(w) for w in section.strip().split()}
            for section in line.split(':')[1].strip().split('|')])
            for line in sys.stdin)

print(total)
