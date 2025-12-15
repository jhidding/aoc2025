# ~/~ begin <<docs/day10.md#docs/day10.md::src/python/day10.py>>[init]
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
# ~/~ end
