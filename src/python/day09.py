# ~/~ begin <<docs/day09.md#docs/day09.md::src/python/day09.py>>[init]
from collections.abc import Generator

def readlines() -> Generator[str]:
    while True:
        try:
            yield input()
        except EOFError:
            return

type Pt = tuple[int, int]

def read_input() -> Iterable[Pt]:
    return map(lambda l: tuple(map(int, l.split(","))), readlines())

def area(a: Pt, b: Pt) -> int:
    return (abs(a[0] - b[0]) + 1) * (abs(a[1] - b[1]) + 1)

if __name__ == "__main__":
    pts = list(read_input())
    print(max(area(i, j) for i in pts for j in pts))
# ~/~ end
