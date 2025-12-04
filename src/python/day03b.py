# ~/~ begin <<docs/day03.md#docs/day03.md::src/python/day03b.py>>[init]
from abc import ABCMeta, abstractmethod
from dataclasses import dataclass
from collections.abc import Callable, Iterable, Generator
from functools import partial, reduce

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

# ~/~ begin <<docs/day03.md#docs/day03.md::duct>>[init]
class Duct[R, X](metaclass=ABCMeta):
    @abstractmethod
    def init(self) -> R:
        """Initializes the value into which the stream is reduced."""
        ...

    def fini(self, r: R) -> R:
        return r

    @abstractmethod
    def step(self, r: R, x: X) -> R:
        ...

    def reduce(self, it: Iterable[X]):
        return self.fini(reduce(self.step, it, initial=self.init()))
# ~/~ end
# ~/~ begin <<docs/day03.md#docs/day03.md::sum>>[init]
class Sum(Duct[int, int]):
    def init(self):
        return 0

    def step(self, r: int, x: int) -> int:
        return r + x
# ~/~ end
# ~/~ begin <<docs/day03.md#docs/day03.md::map>>[init]
@dataclass
class Map[R, A, B](Duct[R, A]):
    foo: Callable[[A], B]
    sink: Duct[R, B]

    def init(self):
        return self.sink.init()

    def step(self, r: R, x: A) -> R:
        return self.sink.step(r, self.foo(x))
# ~/~ end
# ~/~ begin <<docs/day03.md#docs/day03.md::tee>>[init]
class Tee[*Rs, A](Duct[tuple[*Rs], A]):
    sinks: tuple[Duct[Any, A],...]

    def __init__(self, *sinks):
        self.sinks = sinks

    def init(self):
        return tuple(s.init() for s in self.sinks)

    def step(self, r: tuple[*Rs], x: A) -> tuple[*Rs]:
        return tuple(s.step(rn, x) for s, rn in zip(self.sinks, r))
# ~/~ end
# ~/~ begin <<docs/day03.md#docs/day03.md::transmain>>[init]
if __name__ == "__main__":
    print("# Day 03")

    result = Tee(
        Map(partial(joltage, 2), Sum()),
        Map(partial(joltage, 12), Sum())
    ).reduce(readlines())

    for i, r in enumerate(result, start=1):
        print(f"Part {i}: {r}")
# ~/~ end
# ~/~ end
