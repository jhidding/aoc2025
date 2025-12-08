# ~/~ begin <<docs/day08.md#docs/day08.md::src/python/day08.py>>[init]
from collections.abc import Generator, MutableSequence
from dataclasses import dataclass
import operator

def readlines() -> Generator[str]:
    while True:
        try:
            yield input()
        except EOFError:
            return

type Triple = tuple[int, int, int]

def distance(a: Triple, b: Triple) -> Triple:
    return tuple(map(operator.sub, b, a))

def argmax[T](p: Sequence[T]) -> int:
    return max(enumerate(p), key=lambda x: x[1])[0]

def triple_with(a: Triple, i: int, v: int) -> Triple:
    return tuple(a[j] if j != i else v for j in range(3))

def string_to_triple(line: str) -> Triple:
    x = tuple(map(int, line.split(',')))
    assert len(x) == 3
    return x

def read_input() -> Generator[Triple]:
    return map(string_to_triple, readlines())

def partition[T](pred: Callable[[T], bool], seq: MutableSequence[T]) -> int:
    n = len(seq)
    if n == 0:
        return 0
        
    a = 0
    b = n - 1
    while a < b:
        while a < n and pred(seq[a]):
            a += 1
        while b >= 0 and not pred(seq[b]):
            b -= 1

        if a < b:
            seq[a], seq[b] = seq[b], seq[a]
            a += 1
            b -= 1
        
    return a

def test_partition():
    import random

    a = [1, 2, 3, 4]
    assert partition(lambda x: x < 3, a) == 2
    assert a == [1, 2, 3, 4]
    b = [random.randint(0, 10) for _ in range(20)]
    s = partition(lambda x: x < 5, b)
    assert all(x < 5 for x in b[:s])
    assert all(x >= 5 for x in b[s:])

class View[T, C: MutableSequence[T]]:
    data: C
    range: range

    def __getitem__(self, i: int) -> T:
        return self.data[self.range[i]]

    def __setitem__(self, i: int, v: T):
        return self.data[self.range[i]] = v

    def __len__(self):
        return len(self.range)

    def __delitem__(self, i: int):
        raise TypeError("Cannot delete from a View object")

    def insert(self, i: int, v: T):
        raise TypeError("Cannot insert into a View object")
    
@dataclass
class BVTree:
    vertices: list[Triple]
    min: Triple
    max: Triple
    slice: slice
    leaves: tuple[BVTree, BVTree] | None = None

    def split(self):
        assert self.leaves == None
        if len(self.slice) < 8:
            return
            
        d = distance(self.max, self.min)
        m = argmax(d)
        x = self.min[m] + d[m] // 2
        s = partition(lambda t: t[m] < x, View(self.vertices, self.slice))

        self.leaves = (
            BVTree(self.vertices, self.min, triple_with(self.max, m, x), slice(self.slice.start, self.slice.start + s)),
            BVTree(self.vertices, triple_with(self.min, m, x), self.max, slice(self.slice.start + s, self.slice.stop))
        )
        self.leaves[0].split()
        self.leaves[1].split()

    def __len__(self):
        return len(self.slice)
# ~/~ end
