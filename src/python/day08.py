# ~/~ begin <<docs/day08.md#docs/day08.md::src/python/day08.py>>[init]
from collections.abc import Generator, MutableSequence
from collections import defaultdict
from dataclasses import dataclass, field
import operator
from functools import partial, reduce
from itertools import islice

def readlines() -> Generator[str]:
    while True:
        try:
            yield input()
        except EOFError:
            return

type Triple = tuple[int, int, int]

def distance(a: Triple, b: Triple) -> Triple:
    return tuple(map(lambda a, b: abs(b - a), a, b))

def dist2(a: Triple) -> int:
    return sum(map(operator.mul, a, a))

def euclidean_distance_2(a: Triple, b: Triple) -> int:
    return dist2(distance(a, b))

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

@dataclass
class BVTree:
    vertices: list[Triple]
    min: Triple
    max: Triple
    slice: range
    leaves: tuple[BVTree, BVTree] | None = None

    @staticmethod
    def new(vertices: list[Triple]):
        if not vertices:
            raise ValueError()
        dim = len(vertices[0])
        minv = tuple(map(lambda i: min(map(lambda v: v[i], vertices)), range(dim)))
        maxv = tuple(map(lambda i: max(map(lambda v: v[i], vertices)), range(dim)))
        slice = range(0, len(vertices))
        return BVTree(vertices, minv, maxv, slice).split()

    def split(self) -> Self:
        assert self.leaves == None
        if len(self.slice) < 4:
            return

        d = distance(self.max, self.min)
        m = argmax(d)
        ssl = sorted(self.vertices[self.slice.start:self.slice.stop], key=lambda v: v[m])
        vertices[self.slice.start:self.slice.stop] = ssl
        # x = self.min[m] + d[m] // 2
        # s = partition(lambda t: t[m] < x, View(self.vertices, self.slice))
        s = self.slice.start + len(self.slice) // 2
        self.leaves = (
            BVTree(self.vertices, self.min,
                   triple_with(self.max, m, self[s-1][m]),
                   range(self.slice.start, s)),
            BVTree(self.vertices,
                   triple_with(self.min, m, self[s][m]),
                   self.max, range(s, self.slice.stop))
        )
        self.leaves[0].split()
        self.leaves[1].split()
        return self

    def __len__(self) -> int:
        return len(self.slice)

    def __getitem__(self, i: int) -> Triple:
        return self.vertices[i]

    def min_distance(self, p: Triple) -> int:
        def dx(k: int):
            if self.min[k] <= p[k] and p[k] < self.max[k]:
                return 0
            else:
                return min(abs(self.min[k] - p[k]), abs(self.max[k] - p[k]))
        return dist2(tuple(map(dx, range(3))))

    def distance(self, i: int, j: int) -> int:
        return euclidean_distance_2(self[i], self[j])

    def __contains__(self, i: int) -> bool:
        return i in self.slice

    @property
    def leaf_node(self) -> bool:
        return self.leaves is None

    def nearest_neighbour(self, i: int, excluding: set[int] | None = None) -> int | None:
        excluding = excluding or set()
        if self.leaf_node:
            candidates = set(self.slice) - {i} - excluding
            if not candidates:
                return None
            return min(candidates, key=partial(self.distance, i))

        if i not in self:
            n_a = self.leaves[0].nearest_neighbour(i, excluding)
            n_b = self.leaves[1].nearest_neighbour(i, excluding)
            if n_a is None:
                return n_b
            if n_b is None:
                return n_a
            return min((n_a, n_b), key=partial(self.distance, i))

        home, other = self.leaves if i in self.leaves[0] else (self.leaves[1], self.leaves[0])
        assert i in home
        assert i not in other

        nn_home = home.nearest_neighbour(i, excluding)
        if nn_home is None:
            return other.nearest_neighbour(i, excluding)

        min_dist_other = other.min_distance(self[i])
        min_dist_home = self.distance(i, nn_home)
        if min_dist_home < min_dist_other:
            return nn_home
        else:
            nn_other = other.nearest_neighbour(i, excluding)
            if nn_other is None:
                return nn_home
            min_dist_other = self.distance(i, nn_other)
            if min_dist_other < min_dist_home:
                return nn_other
            else:
                return nn_home


def shortest_connections(bvt: BVTree) -> Generator[tuple(int, int)]:
    nn: dict[int, tuple[int, int]] = dict()
    visited: dict[int, set[int]] = defaultdict(set)

    def add_nearest_to(i: int):
        j = bvt.nearest_neighbour(i, visited[i])

        if j is None:
            if i in nn:
                del nn[i]
            return

        visited[i].add(j)
        d = bvt.distance(i, j)    
        nn[i] = (j, d)

    for i in bvt.slice:
        add_nearest_to(i)

    while True:
        i, (j, d) = min(nn.items(), key=lambda kv: kv[1][1])
        yield (i, j)

        assert nn[i] == (j, d)
        assert nn[j] == (i, d), f"nearest to {j} is {bvt.nearest_neighbour(j)}, expected {(i, d)}"

        add_nearest_to(i)
        add_nearest_to(j)


@dataclass
class ConnectionTracker:
    sets: dict[int, set[int]] = field(default_factory=dict)
    setmap: dict[int, int] = field(default_factory=dict)

    @staticmethod
    def disconnected(n: int) -> Self:
        sets = { i: {i} for i in range(n) }
        setmap = { i: i for i in range(n) }
        return ConnectionTracker(sets, setmap)

    def add(self, edge: tuple[int, int]):
        i, j = edge

        if self.setmap[i] == self.setmap[j]:
            return

        self.sets[self.setmap[i]] |= self.sets[self.setmap[j]]
        del self.sets[self.setmap[j]]

        for n in self.sets[self.setmap[i]]:
            self.setmap[n] = self.setmap[i]


def connected_sets(n_vertices: int, edges: Iterable[tuple[int, int]]) -> list[set[int]]:
    ct = ConnectionTracker.disconnected(n_vertices)
    for edge in edges:
        ct.add(edge)
    return list(ct.sets.values())


def find_last_connection(n_vertices: int, edges: Iterable[tuple[int, int]]) -> tuple[int, int]:
    ct = ConnectionTracker.disconnected(n_vertices)
    for edge in edges:
        ct.add(edge)
        if len(ct.sets) == 1:
            return edge


def part_1(bvt: BVTree):
    sets = connected_sets(len(bvt), islice(shortest_connections(bvt), 1000))
    print("Part 1:", reduce(operator.mul, sorted(map(len, sets), reverse=True)[:3]))


def part_2(bvt: BVTree):
    i, j = find_last_connection(len(bvt), shortest_connections(bvt))
    print("Part 2:", bvt[i][0] * bvt[j][0])


if __name__ == "__main__":
    print("# Day 08")
    vertices = list(read_input())
    bvt = BVTree.new(vertices)
    print("Constructed tree")
    part_1(bvt)
    part_2(bvt)
# ~/~ end
