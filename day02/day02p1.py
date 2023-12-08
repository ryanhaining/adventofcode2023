import sys
from typing import Iterable, Sequence, Tuple

# itertools.batched exists in python 3.12
# this is just batched(seq, 2)
def pairs(seq: Sequence[str]) -> Iterable[Tuple[str, str]]:
    it = iter(seq)
    while True:
        try:
            yield next(it), next(it)
        except StopIteration:
            break

MAX_RED = 12
MAX_GREEN = 13
MAX_BLUE = 14

def under_limit(c: str, num: int) -> bool:
    match c:
        case 'r':
            return num <= MAX_RED
        case 'g':
            return num <= MAX_GREEN
        case 'b':
            return num <= MAX_BLUE
        case _:
            raise ValueError(f'unsupported color: {c}')

id_total = 0
for line in sys.stdin:
    _, id_str, *pulls = line.split()
    game_id = int(id_str[:-1])
    if all(under_limit(c[0], int(n)) for n, c in pairs(pulls)):
        id_total += game_id
print(id_total)
        
