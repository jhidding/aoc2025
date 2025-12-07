Day 05: Cafeteria
=================
[![Entangled badge](https://img.shields.io/badge/entangled-Use%20the%20source!-%2300aeff)](https://entangled.github.io/)

We get a set of ranges and a set of numbers, and we have to determine how many of these numbers fall inside at least one of the ranges.

I created a `RangeSet` class that stores a sorted list of non-overlapping ranges. We clean the input ranges by first sorting them by their lower bound, then inserting them at the end of the cleaned list of ranges.

```python
#| id: range-set
@dataclass
class RangeSet:
    ranges: list[range]

    def __post_init__(self):
        sorted_ranges = sorted(self.ranges, key=lambda r: r.start)
        cleaned_ranges = []
            
        for r in sorted_ranges:
            if not cleaned_ranges or r.start not in cleaned_ranges[-1]:
                cleaned_ranges.append(r)
            else:
                x = cleaned_ranges[-1].start
                y = cleaned_ranges[-1].stop
                cleaned_ranges[-1] = range(x, max(y, r.stop))

        self.ranges = cleaned_ranges
                
    def __contains__(self, i: int) -> bool:
        return bool(binary_search(self.ranges, i))
```

Binary Search
-------------

We can test for membership by doing a binary search. The `binary_search` function returns either `Inside(idx)` or `Before(idx)`. The idea was that we can dynamically insert into the container, but this turned out not to be necessary. I overloaded the `__bool__` method of these classes so we can use them to indicate membership. In Haskell these would be one line `data Location = Before Int | Inside Int`, just saying.

```python
#| id: binary-search
@dataclass
class Before:
    idx: int

    @staticmethod
    def __bool__() -> bool:
        return False

@dataclass
class Inside:
    idx: int

    @staticmethod
    def __bool__() -> bool:
        return True

type Location = Before | Inside
```

The binary search itself is a bit boring. At all times, we keep a inclusive lower index `a` and an  exclusive upper index `b`, and we loop until `a == b`, which we should never reach.

```python
#| id: binary-search
def binary_search(rngs: list[range], i: int) -> Location:
    if not rngs or i < rngs[0].start:
        return Before(0)
        
    a = 0
    b = len(rngs)

    if i >= rngs[b-1].stop:
        return Before(b)

    while a != b:
        n = a + (b - a) // 2
        if i in rngs[n]:
            return Inside(n)
        if i < rngs[n].start:
            if n == a or i >= rngs[n-1].stop:
                return Before(n)
            else:
                b = n
        else:  # i >= rngs[n].stop        
            a = n

    print(f"strange: converged on {rngs[a]} with number {i} without result.")
    return Before(b+1)
```

Reading Input
-------------

```python
#| id: read-input
def readlines() -> Generator[str]:
    while True:
        try:
            yield input()
        except EOFError:
            return


def read_input() -> tuple[RangeSet, list[int]]:
    lines = readlines()

    rngs = []
    for line in lines:
        if not line:
            break
        a, b = map(int, line.split('-'))
        rngs.append(range(a, b+1))

    nums = []
    for line in lines:
        nums.append(int(line))

    return RangeSet(rngs), nums
```

Main
----

```python
#| file: src/python/day05.py
from dataclasses import dataclass
from collections.abc import Generator

<<range-set>>
<<binary-search>>
<<read-input>>

if __name__ == "__main__":
    print("# Day 05")
    rset, nums = read_input()
    print("Part 1:", sum(n in rset for n in nums))
    print("Part 2:", sum(len(r) for r in rset.ranges))
```
 
