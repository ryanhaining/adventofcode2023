import sys

print(sum(
        (1 << (count-1)) if (count := len(set.intersection(*[{int(w) for w in section.strip().split()}
            for section in line.split(':')[1].strip().split('|')]))) else 0
            for line in sys.stdin))
