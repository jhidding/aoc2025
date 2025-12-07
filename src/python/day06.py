# ~/~ begin <<docs/day06.md#docs/day06.md::src/python/day06.py>>[init]
import operator
from functools import reduce, partial
from itertools import zip_longest, takewhile

def readlines():
    while True:
        try:
            yield input()
        except:
            return

operator_syms = {
    "+": operator.add,
    "*": operator.mul
}

def transpose(text: list[str]) -> list[str]:
    return map("".join, zip_longest(*text, fillvalue=' '))

def split_on[T](pred: Callable[[T], bool], lst: Iterable[T]) -> Generator[Iterable[T]]:
    buffer = []
    stop = False
    it = iter(lst)
    
    def take_until():
        nonlocal buffer
        nonlocal stop
        yield from iter(buffer)
        buffer = []
        for item in it:
            if pred(item):
                return
            else:
                yield item
        stop = True

    def skip():
        nonlocal buffer
        nonlocal stop
        for item in it:
            if pred(item):
                continue
            else:
                buffer = [item]
                return
        stop = True

    while not stop:
        skip()
        yield take_until()
        
if __name__ == "__main__":
    text = list(readlines())
    operators = list(map(operator_syms.get, text[-1].split()))
    numbers_1 = zip(*map(lambda l: list(map(int, l.split())), text[:-1]))
    print("Part 1:", sum(map(reduce, operators, numbers_1)))
    numbers_2 = map(partial(map, int), split_on(lambda l: not l.strip(), transpose(text[:-1])))
    print("Part 2:", sum(map(reduce, operators, numbers_2)))
# ~/~ end
