Day 03: Lobby
=============

We get a string of digits and need to find the maximum n-digit number that we can construct by selecting digits from the string, but keeping them in order.

```python
#| file: src/python/day03.py
from collections.abc import Generator
from functools import partial

<<read-input>>
<<joltage>>
<<main>>
```

Input
-----

We just need to read the lines.

```python
#| id: read-input
def readlines() -> Generator[str]:
    while True:
        try:
            yield input()
        except EOFError:
            return
```

Joltage
-------

The first digit always needs to be the maximum that we can get, except that we should still be able to complete the number, so we search the first `len(s) - (n - 1)` digits for the first number, then starting from that number until `len(s) - (n - 2)` etc.

```python
#| id: joltage
def joltage(n: int, bank: str) -> int:
    num = ""
    pos = 0

    for i in reversed(range(1, n)):
        delta, d = max(enumerate(bank[pos:-i]), key=lambda t: t[1])
        pos += delta + 1
        num += d

    num += max(*bank[pos:])

    return int(num)
```

Main
----

It almost feels bad to be storing the input in a list.

```python
#| id: main
if __name__ == "__main__":
    print("# Day 03")
    inp: list[str] = list(readlines())
    print("Part 1:", sum(map(partial(joltage, 2), inp)))
    print("Part 2:", sum(map(partial(joltage, 12), inp)))
```

Transducers
-----------

Can we splice the iterator over multiple streams and get two results? Theres the `itertools.tee` function, but I don't see how that would work on stateful iterators like `readlines()`, other than buffering the entire iterator. Sadly, these Python functions and iterators do not compose so nicely. We could have a `splice_map` function, that feeds the input to a tuple of functions and yields tuples of results.

```python
def splice(fns, it):
    for item in it:
        yield tuple(map(lambda f: f(item), fns))
```

Then reducing becomes more tricky. Now we need to have a step function that reduces tuples of results:

```python
def splice_step(fns):
    def stepper(state, inp):
        return tuple(f(s, x) for f, s, x in zip(fns, state, inp))
    return stepper
```

Now we need to use `operator.add` to indicate we are computing a sum. All of this can be written much neater when we use [**transducers**](https://www.youtube.com/watch?v=6mTbuzafcII). I'm leaving out some details here (early termination for one), and the simplified type annotations severely limit the applicability of these constructs, but for now this is enough.

```python
#| file: src/python/day03b.py
from abc import ABCMeta, abstractmethod
from dataclasses import dataclass
from collections.abc import Callable, Iterable, Generator
from functools import partial, reduce

<<read-input>>
<<joltage>>

<<duct>>
<<sum>>
<<map>>
<<tee>>
<<transmain>>
```

First, the `Duct`. This is an object that represents an element in a pipeline. It should have an `init`, `fini` and `step` method. The precise meaning of these methods is given when we implement the `reduce` method.

```python
#| id: duct
class Duct[R, X](metaclass=ABCMeta):
    @abstractmethod
    def init(self) -> R:
        """Initializes the value into which the stream is reduced."""
        ...

    def fini(self, r: R) -> R:
        return r

    @abstractmethod
    def step(self, r: R, x: X) -> R:
        ...

    def reduce(self, it: Iterable[X]):
        return self.fini(reduce(self.step, it, initial=self.init()))
```

The first `Duct` we need to implement is the `Sum`. We don't need to implement any loop or anything: just the function that, when reduced over, produces a sum.

```python
#| id: sum
class Sum(Duct[int, int]):
    def init(self):
        return 0

    def step(self, r: int, x: int) -> int:
        return r + x
```

Next, the `Map` duct also takes another `Duct` as input argument. We don't want to specify what we're mapping into, just the fact that we are transforming the elements. In fact, `Map` is our first full **transducer**: `partial(Map, f)` is a function with signature `Duct -> Duct`, making this system fully composable.

```python
#| id: map
@dataclass
class Map[R, A, B](Duct[R, A]):
    foo: Callable[[A], B]
    sink: Duct[R, B]

    def init(self):
        return self.sink.init()

    def step(self, r: R, x: A) -> R:
        return self.sink.step(r, self.foo(x))
```

Finally, we can define the `Tee` duct. This takes any amount of `Duct` sinks as argument and splices the input iterator into them. The typing here assumes that all these sinks have the same type, but that needn't be the case. I don't know if the Python type system is able to express that we have a type list `*Rs` and then a member `tuple[*(Duct[R, A] for R in Rs)]`, but this is not allowed.

```python
#| id: tee
class Tee[*Rs, A](Duct[tuple[*Rs], A]):
    sinks: tuple[Duct[Any, A],...]

    def __init__(self, *sinks):
        self.sinks = sinks

    def init(self):
        return tuple(s.init() for s in self.sinks)

    def step(self, r: tuple[*Rs], x: A) -> tuple[*Rs]:
        return tuple(s.step(rn, x) for s, rn in zip(self.sinks, r))
```

Anyway, the moment you've all been waiting for:

```python
#| id: transmain
if __name__ == "__main__":
    print("# Day 03")

    result = Tee(
        Map(partial(joltage, 2), Sum()),
        Map(partial(joltage, 12), Sum())
    ).reduce(readlines())

    for i, r in enumerate(result, start=1):
        print(f"Part {i}: {r}")
```

This now computes both answers without storing any intermediate results. Moreover, there is only one loop, namely the one in the call to the `reduce` method. An optimizing compiler should be able to make this code quite efficient.

C3
--

In C3 this gives us the opportunity to implement a generic function `arg_max` with a constraint that its input should be non-empty. Template arguments apply to an entire module.

```c3
//| file: src/c3/day03.c3
module arg_max { Tx };

<*
 @require xs.len > 0 : "array should be non-empty"
 *>
fn usz arg_max(Tx[] xs) {
	usz loc = 0;
	Tx x_max = xs[0];

	foreach (i, x : xs[1..]) {
		if (x > x_max) {
			x_max = x;
			loc = i+1;
		}	
	}

	return loc;
}

module lobby;

import std::io;
import arg_max;

fn usz joltage(int n, char[] bank) {
	usz num = 0;
	usz pos = 0;

	for (int i = n-1; i > 0; i--) {
		usz delta = arg_max { char } (bank[pos..(bank.len-1-i)]);
		num = num * 10 + (long)(bank[pos+delta] - '0');
		pos += delta + 1;
	}

	usz delta = arg_max{char}(bank[pos..]);
	num = num * 10 + (long)(bank[pos + delta] - '0');
	return num;
}

fn void main() => @pool() {
    usz part_1 = 0;
    usz part_2 = 0;
    while (try line = io::readline(tmem)) {
        part_1 += joltage(2, line);
        part_2 += joltage(12, line);
    }
    io::printn("# Day 03");
    io::printfn("Part 1: %d", part_1);
    io::printfn("Part 2: %d", part_2);
}
```
