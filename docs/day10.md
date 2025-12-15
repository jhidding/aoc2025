Day 10: Factory
===============

For the first time in forever, we need to write an input parser.

Parsing
-------

```python
#| file: src/python/parsing.py
from __future__ import annotations
from dataclasses import dataclass, field
from abc import ABCMeta, abstractmethod
from collections.abc import Callable
from functools import partial, wraps
import re
from typing import cast, Any


@dataclass
class Failure:
    msg: str = "unknown failure"
    inp: str = "unknown input"

    def __str__(self):
        return f"expected `{self.msg}`, got `{self.inp}`"


class Parser[T](metaclass=ABCMeta):
    @abstractmethod
    def parse(self, inp: str) -> tuple[T, str] | Failure:
        ...

    def bind[U](self, foo: Callable[[T], Parser[U]]) -> Parser[U]:
        @parser
        def bound(inp: str) -> tuple[U, str] | Failure:
            match self.parse(inp):
                case Failure() as f:
                    return f
                case (value, more_inp):
                    return foo(value).parse(more_inp)

        return bound

    def map[U](self, foo: Callable[[T], U]) -> Parser[U]:
        return self.bind(partial(safe_map, foo))

    def __or__[U](self, other: Parser[U]) -> Choice[T | U]:
        match (self, other):
            case (Choice(c1), Choice(c2)):
                return Choice(cast(list[Parser[T | U]], c1 + c2))
            case (Choice(c1), _):
                return Choice(cast(list[Parser[T | U]], c1 + [other]))
            case (_, Choice(c2)):
                return Choice(cast(list[Parser[T | U]], [self] + c2))
            case (_, _):
                return Choice(cast(list[Parser[T | U]], [self, other]))


@dataclass
class CompoundFailure(Failure):
    fails: list[Failure] = field(default_factory=list)


@dataclass
class Choice[T](Parser[T]):
    choices: list[Parser[T]]

    def parse(self, inp: str) -> tuple[T, str] | Failure:
        f = CompoundFailure("one of", inp)
        for p in self.choices:
            match x := p.parse(inp):
                case CompoundFailure(_, _, cf):
                    f.fails.extend(cf)
                case Failure():
                    f.fails.append(x)
                case _:
                    return x
        return f

def safe_map[T, U](foo: Callable[[T], U], t: T) -> Parser[U]:
    try:
        return pure(foo(t))
    except Exception as e:
        return fail(str(e))


@dataclass
class FnParser[T](Parser[T]):
    foo: Callable[[str], tuple[T, str] | Failure]

    def parse(self, inp: str) -> tuple[T, str] | Failure:
        return self.foo(inp)


def parser[T](foo: Callable[[str], tuple[T, str] | Failure]) -> FnParser[T]:
    return FnParser(foo)


def fail[T](msg: str) -> Parser[T]:
    return parser(lambda inp: Failure(msg, inp))


def pure[T](value: T) -> Parser[T]:
    return parser(lambda inp: (value, inp))


def match(expr: str) -> Parser[re.Match[str]]:
    c = re.compile(expr)

    @parser
    def matching(inp: str) -> tuple[re.Match[str], str] | Failure:
         if m := re.match(c, inp):
             return m, inp[m.end():]
         else:
             return Failure(expr, inp)

    return matching


def integer() -> Parser[int]:
    return match(r"-?\d+").map(lambda m: int(m.group()))


def many[T](p: Parser[T], init: list[T] | None = None) -> Parser[list[T]]:
    @parser
    def many_p(inp: str) -> tuple[list[T], str] | Failure:
        result = init or []
        while True:
            match p.parse(inp):
                case Failure():
                    return (result, inp)
                case (item, next_inp):
                    result.append(item)
                    inp = next_inp

    return many_p


def singleton[T](v: T) -> list[T]:
    return [v]


def some[T](p: Parser[T]) -> Parser[list[T]]:
    return p.map(singleton).bind(partial(many, p))


def struct[T,**Args](t: type[T], **kwargs: Parser[Any]) -> Parser[T]:
    @parser
    def struct_p(inp: str) -> tuple[T, str] | Failure:
        args = {}
        for k, p in kwargs.items():
            match p.parse(inp):
                case Failure() as f:
                    return f
                case (value, next_inp):
                    if k[0] != '_':
                        args[k] = value
                    inp = next_inp
        return t(**args), inp

    return struct_p


def literal(x: str) -> Parser[str]:
    @parser
    def literal_p(inp: str) -> tuple[str, str] | Failure:
        if inp.startswith(x):
            return x, inp.removeprefix(x)
        else:
            return Failure(x, inp)

    return literal_p


def tokenize[T](p: Parser[T], whitespace=match(r"\s*")) -> Parser[T]:
    return p.bind(lambda x: whitespace.bind(lambda _: pure(x)))


def delimited[T](open: Parser[Any], close: Parser[Any], p: Parser[T]) -> Parser[T]:
    return open.bind(lambda _: p.bind(lambda x: close.bind(lambda _: pure(x))))


def separated_by[T](sep: Parser[Any], p: Parser[T]) -> Parser[list[T]]:
    return p.bind(lambda x: many(sep.bind(lambda _: p), [x]))
```

Parsing Input
-------------

```python
#| file: src/python/day10.py
from __future__ import annotations
from copy import copy
from typing import Protocol, Self
from parsing import match, delimited, integer, some, literal, separated_by, tokenize, struct, Failure
from dataclasses import dataclass
from collections.abc import Callable, Generator, Iterable, Sequence
from itertools import chain, combinations
from functools import reduce
import operator
from heapq import heappush, heappop

@dataclass
class Machine:
    indicator_lights: list[int]
    button_wirings: list[list[int]]
    joltage_requirements: list[int]

integer_sequence = separated_by(literal(','), integer())

machine = struct(
    Machine,
    indicator_lights = tokenize(match(r"\[([.#]*)\]").map(lambda m: list(map(lambda c: int(c == '#'), m.group(1))))),
    button_wirings = some(tokenize(delimited(literal('('), literal(')'), integer_sequence))),
    joltage_requirements = tokenize(delimited(literal('{'), literal('}'), integer_sequence)))

def read_machine(line: str) -> Machine:
    match machine.parse(line):
        case Failure() as f:
            raise ValueError(str(f))
        case (x, _):
            return x

def readlines() -> Generator[str]:
    while True:
        try:
            yield input()
        except EOFError:
            return

def read_input() -> Iterable[Machine]:
    return map(read_machine, readlines())

def powerset[T](s: Iterable[T]) -> Iterable[tuple[T,...]]:
    lst = list(s)
    return chain.from_iterable(combinations(lst, n) for n in range(1, len(lst)))

def part_1(m: Machine) -> int:
    target = sum(2**i for (i, x) in enumerate(m.indicator_lights) if x == 1)
    cols = list(map(lambda a: sum(2**i for i in a), m.button_wirings))
    for s in powerset(cols):
        if reduce(operator.xor, s) == target:
            return len(s)
    raise ValueError("no solution")

@dataclass
class Priority[T, P]:
    value: T
    priority: P

    def __lt__(self, other: Priority) -> bool:
        return self.priority < other.priority

class Ord(Protocol):
    def __lt__(self, other: Self) -> bool:
        ...

def uniform_cost_search[T: Ord, K](start: T, key: Callable[[T], K], is_target: Callable[[T], bool], neighbours: Callable[[T], Iterable[T]]) -> T | None:
    node = start
    queue: list[T] = [start]
    visited: set[K] = set()

    while queue:
        node = heappop(queue)
        if key(node) in visited:
            continue
        if is_target(node):
            return node
        visited.add(key(node))
        for n in neighbours(node):
            if key(n) not in visited:
                heappush(queue, n)

    return None

type Node = Priority[list[int], int]

def part_2(m: Machine) -> int:
    def neighbours(n: Node) -> Iterable[Node]:
        for b in m.button_wirings:
            v = copy(n.value)
            for w in b:
                v[w] += 1
            if all(vi <= ti for vi, ti in zip(v, m.joltage_requirements)):
                yield Priority(v, n.priority+1)

    def key(n: Node) -> tuple[int,...]:
        return tuple(n.value)

    def is_target(n: Node) -> bool:
        return n.value == m.joltage_requirements

    start = Priority([0] * len(m.joltage_requirements), 0)
    result = uniform_cost_search(start, key, is_target, neighbours)
    assert result is not None
    print(result)
    return result.priority

if __name__ == "__main__":
    print("# Day 10")
    machines = list(read_input())
    print("Part 1:", sum(map(part_1, machines)))
    print("Part 2:", sum(map(part_2, machines)))
```

Path finding
------------

I've considered several different algorithms to solve the first exercise. We are basically given a linear system in $\mathbb{Z}_2$. Take the first example:

```
[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
```

This corresponds to

$$\begin{pmatrix}
0 & 0 & 0 & 0 & 1 & 1 \\
0 & 1 & 0 & 0 & 0 & 1 \\
0 & 0 & 1 & 1 & 1 & 0 \\
1 & 1 & 0 & 1 & 0 & 0
\end{pmatrix} a = \begin{pmatrix}
0 \\ 1 \\ 1 \\ 0 \end{pmatrix}.$$

Then we need to find the smallest $a$ that solves this. We could use Gaussian elimination to find a solution, but the system is underdetermined.

Another algorithm would be a BFS with memoization. Yet another would be Dijkstra's algorithm. For now, BFS seems the easiest.
