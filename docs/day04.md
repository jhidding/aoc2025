Day 04: Printing Department
===========================

```python
#| file: src/python/day04.py
from collections.abc import Generator, Iterable
from collections import deque
from itertools import islice, repeat, chain


type Grid = list[str]


def readlines() -> Generator[str]:
    while True:
        try:
            yield input()
        except EOFError:
            return


def window[T](iterable: Iterable[T], n: int, fill: T) -> Generator[tuple[T,...]]:
    """
    Collect data into overlapping fixed-length chunks or blocks.
    (adapted from itertools documentation)

    Example:

        window('ABCD', 3, '*') → *AB ABC BCD CD*
    """
    iterator = iter(iterable)
    window = deque(repeat(fill, n), maxlen=n)

    for x in islice(iterator, n // 2):
        window.append(x)

    for x in iterator:
        window.append(x)
        yield tuple(window)

    for _ in range(n // 2):
        window.append(fill)
        yield tuple(window)


def window_2d[T](image: list[Iterable[T]], n: int, fill: T) -> Iterable[Iterable[tuple[tuple[T,...],...]]]:
    """
    Similar to the 1d `window` function, but now in 2d.
    """
    width = len(image[0])
    return map(
        lambda r: zip(*map(lambda l: window(l, n, fill), r)),
        window(image, n, [fill]*width))


def accessable(a: tuple[tuple[str,...],...]) -> bool:
    return a[1][1] == '@' and sum(map(lambda c: c == '@', chain(*a))) <= 4


def erode_cell(a: tuple[tuple[str,...],...]) -> str:
    return '.' if accessable(a) else a[1][1]


def erode(image: list[str]) -> list[str]:
    return list(map(lambda r: "".join(map(erode_cell, r)), window_2d(image, 3, '.')))


def fixed_point(f, init):
    a = init
    b = f(a)
    while a != b:
        a = b
        b = f(a)
    return b


def count_rolls(image: list[str]) -> int:
    return sum(chain(*map(lambda r: map(lambda c: c == '@', r), image)))


if __name__ == "__main__":
    print("# Day 04")
    inp = list(readlines())
    print("Part 1:", sum(map(accessable, chain(*window_2d(inp, 3, '.')))))
    result = fixed_point(erode, inp)
    # print("\n".join(result))
    print("Part 2:", count_rolls(inp) - count_rolls(result))

```
