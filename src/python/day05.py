# ~/~ begin <<docs/day05.md#docs/day05.md::src/python/day05.py>>[init]
from dataclasses import dataclass
from collections.abc import Generator

# ~/~ begin <<docs/day05.md#docs/day05.md::range-set>>[init]
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
# ~/~ end
# ~/~ begin <<docs/day05.md#docs/day05.md::binary-search>>[init]
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
# ~/~ end
# ~/~ begin <<docs/day05.md#docs/day05.md::binary-search>>[1]
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
# ~/~ end
# ~/~ begin <<docs/day05.md#docs/day05.md::read-input>>[init]
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
# ~/~ end

if __name__ == "__main__":
    print("# Day 05")
    rset, nums = read_input()
    print("Part 1:", sum(n in rset for n in nums))
    print("Part 2:", sum(len(r) for r in rset.ranges))
# ~/~ end
