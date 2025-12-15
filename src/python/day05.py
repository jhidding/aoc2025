# ~/~ begin <<docs/day05.md#docs/day05.md::src/python/day05.py>>[init]
from dataclasses import dataclass
from collections.abc import Generator

<<docs/day05.md::range-set>>
<<docs/day05.md::binary-search>>
<<docs/day05.md::read-input>>

if __name__ == "__main__":
    print("# Day 05")
    rset, nums = read_input()
    print("Part 1:", sum(n in rset for n in nums))
    print("Part 2:", sum(len(r) for r in rset.ranges))
# ~/~ end
