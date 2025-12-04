# ~/~ begin <<docs/day01.md#day01::src/python/day01.py>>[init]
from dataclasses import dataclass
from enum import StrEnum
from functools import reduce
from itertools import accumulate
from typing import Callable, Iterable


class Direction(StrEnum):
    LEFT = "left"
    RIGHT = "right"


@dataclass
class Instruction:
    direction: Direction
    amount: int

    def __str__(self):
        return f"{self.direction}-{self.amount}"


def readlines():
    try:
        while True:
            yield input()
    except EOFError:
        return


def parse_instruction(expr: str) -> Instruction:
    if expr[0] == "L":
        direction = Direction.LEFT
    elif expr[0] == "R":
        direction = Direction.RIGHT
    else:
        raise ValueError(f"Expression should start with L or R, got: {expr}")

    amount = int(expr[1:])

    return Instruction(direction, amount)


@dataclass
class Dial:
    position: int
    clicks: int = 0

    def __bool__(self):
        return self.position == 0


def rotate_check(dial: Dial, instruction: Instruction) -> Dial:
    match instruction.direction:
        case Direction.LEFT:
            newpos = dial.position - instruction.amount
        case Direction.RIGHT:
            newpos = dial.position + instruction.amount

    if newpos > 0:
        clicks = newpos // 100
    elif dial.position == 0:
        clicks = - (newpos // 100 + 1)
    else:
        clicks = - ((newpos - 1) // 100)

    return Dial(newpos % 100, dial.clicks + clicks)


if __name__ == "__main__":
    print("# Day 01")
    out = list(accumulate(map(parse_instruction, readlines()), rotate_check, initial=Dial(50)))
    print(sum(map(bool, out)))
    print(out[-1].clicks)
# ~/~ end
