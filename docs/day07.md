# Day 07: Laboratories
[![Entangled badge](https://img.shields.io/badge/entangled-Use%20the%20source!-%2300aeff)](https://entangled.github.io/)

We need to trace a ray down a path. We can write the solution as another reduction.

```python
#| file: src/python/day07.py
from abc import ABCMeta, abstractmethod
from dataclasses import dataclass
from collections.abc import Generator
from functools import reduce

<<read-input>>
<<part-1>>
<<part-2>>
<<fold-duct>>
<<main>>
```

Reading Input
-------------

```python
#| id: read-input
def read_input() -> Generator[str]:
    while True:
        try:
            yield input()
        except EOFError:
            return
```

Part 1
------

We keep the decending rays in a separate string containing `.` and `|` characters. By zipping with the line of input we use a `match` statement to deal with all cases.

```python
#| id: part-1
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
```

Part 2
------

This is actually easier than Part 1. We keep the state in a list of integers that count how many paths we have for each ray.

```python
#| id: part-2
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
```

Fold Duct
---------

To perform the reduction, it may be nice to pull our **transducers** off the shelve. We previously only needed the `Sum(Duct)` to reduce. Now, we need a generic `Fold(Duct)` that combines a `step` function and `init` state. This is a bit of boiler plate, but it allows us to write everything in terms of a single streaming process again.

```python
#| id: fold-duct
<<day03::duct>>
<<day03::tee>>

@dataclass
class Fold[R,X](Duct[R, X]):
    _step: Callable[[R, X], R]
    _init: R

    def init(self) -> R:
        return self._init

    def step(self, r: R, x: X) -> R:
        return self._step(r, x)
```

Main
----

```python
#| id: main    
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
```

C3
--

We can use our streaming implementation in Python to motivate the design in C3. It tells us exactly what kind of memory we need to reserve for the computation state.

```c3
//| file: src/c3/day07.c3
module laboratories;

struct ForwardRays {
    int count;
    String rays;
}

fn void ForwardRays.step(ForwardRays *self, String line) => @pool() {
}
```
