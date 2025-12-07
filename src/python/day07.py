# ~/~ begin <<docs/day07.md#docs/day07.md::src/python/day07.py>>[init]
from abc import ABCMeta, abstractmethod
from dataclasses import dataclass
from collections.abc import Generator
from functools import reduce

# ~/~ begin <<docs/day07.md#docs/day07.md::read-input>>[init]
def read_input() -> Generator[str]:
    while True:
        try:
            yield input()
        except EOFError:
            return
# ~/~ end
# ~/~ begin <<docs/day07.md#docs/day07.md::part-1>>[init]
def process_rays_1(state: tuple[str, int], line: str) -> tuple[str, int]:
    output = ['.'] * len(line)
    split_count = state[1]
    for i, (s, l) in enumerate(zip(state[0], line)):
        match (s, l):
            case ('|', '^'):
                output[i-1] = '|'
                output[i+1] = '|'
                split_count += 1
            case ('|', '.'):
                output[i] = '|'
            case _:
                pass

    return "".join(output), split_count
# ~/~ end
# ~/~ begin <<docs/day07.md#docs/day07.md::part-2>>[init]
def process_rays_2(state: tuple[list[int]], line: str) -> list[int]:
    output = [0] * len(line)
    for i, (s, l) in enumerate(zip(state, line)):
        match (s, l):
            case (x, '^'):
                output[i-1] += x
                output[i+1] += x
            case (x, '.'):
                output[i] += x
            case _:
                pass

    return output
# ~/~ end
# ~/~ begin <<docs/day07.md#docs/day07.md::fold-duct>>[init]
# ~/~ begin <<docs/day03.md#day03::duct>>[init]
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
# ~/~ begin <<docs/day03.md#day03::tee>>[init]
class Tee[*Rs, A](Duct[tuple[*Rs], A]):
    sinks: tuple[Duct[Any, A],...]

    def __init__(self, *sinks):
        self.sinks = sinks

    def init(self):
        return tuple(s.init() for s in self.sinks)

    def step(self, r: tuple[*Rs], x: A) -> tuple[*Rs]:
        return tuple(s.step(rn, x) for s, rn in zip(self.sinks, r))
# ~/~ end

@dataclass
class Fold[R,X](Duct[R, X]):
    _step: Callable[[R, X], R]
    _init: R

    def init(self) -> R:
        return self._init

    def step(self, r: R, x: X) -> R:
        return self._step(r, x)
# ~/~ end
# ~/~ begin <<docs/day07.md#docs/day07.md::main>>[init]
if __name__ == "__main__":
    inp = read_input()
    init = next(inp)
    
    state01 = (init.replace('S', '|'), 0)
    state02 = [0 if c == '.' else 1 for c in init]

    part1, part2 = Tee(
        Fold(process_rays_1, state01),
        Fold(process_rays_2, state02)
    ).reduce(inp)

    print("# Day 07")
    print("Part 1", part1[1])
    print("Part 2", sum(part2))    
# ~/~ end
# ~/~ end
