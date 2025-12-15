# ~/~ begin <<docs/day02.md#day02::src/python/day02.py>>[init]

from dataclasses import dataclass
from itertools import chain
from collections.abc import Generator

# ~/~ begin <<docs/day02.md#day02::parsing>>[init]
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
# ~/~ end
# ~/~ begin <<docs/day02.md#day02::predicates>>[init]
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
# ~/~ end
# ~/~ begin <<docs/day02.md#day02::generators>>[init]
def generate_invalid_ids(r: range, divisors=divisors, check_duplicates=True) -> Generator[int]:
    # ~/~ begin <<docs/day02.md#day02::number-strings>>[init]
    seq1 = str(r.start)
    n = len(seq1)
    seq2 = str(r.stop - 1)
    # ~/~ end
    # ~/~ begin <<docs/day02.md#day02::ensure-homogeneity>>[init]
    if len(seq2) != n:
        yield from chain(
            generate_invalid_ids(range(r.start, 10**n), divisors, check_duplicates),
            generate_invalid_ids(range(10**n, r.stop), divisors, check_duplicates))
        return
    # ~/~ end
    # ~/~ begin <<docs/day02.md#day02::loop-over-divisors>>[init]
    for d in divisors(n):
        x1 = int(seq1[:d])
        x2 = int(seq2[:d])
        for subseq in range(x1, x2 + 1):
            if check_duplicates and invalid_id_2(subseq):
                continue
            num = int(str(subseq) * (n // d))
            if num in r:
                yield num
    # ~/~ end
# ~/~ end
# ~/~ begin <<docs/day02.md#day02::generators>>[1]

def limited_divisors(x: int) -> Generator[int]:
    if x % 2 == 0:
        yield x // 2
    return
# ~/~ end

if __name__ == "__main__":
    # ~/~ begin <<docs/day02.md#day02::main>>[init]
    print("# Day 02")
    inp = read_input()
    print("Part 1:", sum(chain(*map(lambda r: generate_invalid_ids(r, limited_divisors, False), inp.ranges))))
    print("Part 2:", sum(chain(*map(generate_invalid_ids, inp.ranges))))
    # ~/~ end
# ~/~ end
