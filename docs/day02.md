Day 02: Gift Shop
=================

We're given several ranges of numbers in which we need to find numbers that, when written out, consist of repeating sequences. I'm not going to lie: the first time I just filtered over all numbers. It takes a seconds, but that's fine. We know however, that there should be a better way.

I'll start with the easier predicates for both part 1 and 2, then do the faster generative method.

```python
#| file: src/python/day02.py

from dataclasses import dataclass
from itertools import chain
from collections.abc import Generator

<<parsing>>
<<predicates>>
<<generators>>

if __name__ == "__main__":
    <<docs/day02.md::main>>
```

Parsing
-------

I parse the list of ranges to `range` types, and store them in a `RangeCollection` for easier iteration.

```python
#| id: parsing
@dataclass(frozen=True)
class RangeCollection:
    ranges: list[range]

    def __iter__(self):
        return chain(*self.ranges)


def parse_range(t: str) -> range:
    a, b = map(int, t.split("-"))
    return range(a, b+1)


def read_input() -> RangeCollection:
    return RangeCollection(list(map(parse_range, input().split(","))))
```

Predicates
----------

The predicates are easy; just convert the integers to strings and check. In part 2, we need to check all integer divisors for the length of the number. There are better algorithms to find the divisors of an integer, but our numbers are small.

```python
#| id: predicates
def invalid_id_1(x: int) -> bool:
    seq = str(x)
    n = len(seq)
    if n % 2 == 1:
        return False

    return seq[:n//2] == seq[n//2:]


def divisors(x: int) -> Generator[int]:
    for i in range(1, x // 2 + 1):
        if x % i == 0:
            yield i


def invalid_id_2(x: int) -> bool:
    seq = str(x)
    n = len(seq)
    for i in divisors(n):
        m = n // i
        if seq == seq[:i] * m:
            return True
    return False
```

Now we can compute the answers like so:

```python
sum(filter(invalid_id_2, read_input()))
```

But that is slow.

Generators
----------

We can generate all answers directly, without testing intermediate numbers. The optional arguments are to make the function general enough for both part 1 and 2.

```python
#| id: generators
def generate_invalid_ids(r: range, divisors=divisors, check_duplicates=True) -> Generator[int]:
    <<number-strings>>
    <<ensure-homogeneity>>
    <<loop-over-divisors>>
```

We start again by converting the start and end of the range into strings.

```python
#| id: number-strings
seq1 = str(r.start)
n = len(seq1)
seq2 = str(r.stop - 1)
```

If the end of the range is a longer number than the start, we need to split, so that we can assume equal length in the rest of the algorithm.

```python
#| id: ensure-homogeneity
if len(seq2) != n:
    yield from chain(
        generate_invalid_ids(range(r.start, 10**n), divisors, check_duplicates),
        generate_invalid_ids(range(10**n, r.stop), divisors, check_duplicates))
    return
```

Then, for each divisor of the length of digits we loop over all numbers that would start a repeated sequence. At the beginning and end of these ranges the generated number might be outside the input range, so we must check for that. Also, if a number like `2 2 2 2` is picked up when we look at single digits, it is again picked up when we do double digits `22 22`. So the number we're repeating (here called `subseq`) should not be a repeating number itself. I use the predicate we had earlier for this.

```python
#| id: loop-over-divisors
for d in divisors(n):
    x1 = int(seq1[:d])
    x2 = int(seq2[:d])
    for subseq in range(x1, x2 + 1):
        if check_duplicates and invalid_id_2(subseq):
            continue
        num = int(str(subseq) * (n // d))
        if num in r:
            yield num
```

To make this function work for part 1, I made a special `limited_divisors` function, that only iterates all divisors between two and two. In other words, it only acts if the number is divisable by two.

```python
#| id: generators

def limited_divisors(x: int) -> Generator[int]:
    if x % 2 == 0:
        yield x // 2
    return
```

Main
----

Now that we have our generator, we need to iterate over the ranges and not the individual numbers.

```python
#| id: main
print("# Day 02")
inp = read_input()
print("Part 1:", sum(chain(*map(lambda r: generate_invalid_ids(r, limited_divisors, False), inp.ranges))))
print("Part 2:", sum(chain(*map(generate_invalid_ids, inp.ranges))))
```
