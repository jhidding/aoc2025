# ~/~ begin <<docs/day03.md#docs/day03.md::src/python/day03.py>>[init]
from collections.abc import Generator
from functools import partial

# ~/~ begin <<docs/day03.md#docs/day03.md::read-input>>[init]
def readlines() -> Generator[str]:
    while True:
        try:
            yield input()
        except EOFError:
            return
# ~/~ end
# ~/~ begin <<docs/day03.md#docs/day03.md::joltage>>[init]
def joltage(n: int, bank: str) -> int:
    num = ""
    pos = 0

    for i in reversed(range(1, n)):
        delta, d = max(enumerate(bank[pos:-i]), key=lambda t: t[1])
        pos += delta + 1
        num += d

    num += max(*bank[pos:])

    return int(num)
# ~/~ end
# ~/~ begin <<docs/day03.md#docs/day03.md::main>>[init]
if __name__ == "__main__":
    print("# Day 03")
    inp: list[str] = list(readlines())
    print("Part 1:", sum(map(partial(joltage, 2), inp)))
    print("Part 2:", sum(map(partial(joltage, 12), inp)))
# ~/~ end
# ~/~ end
