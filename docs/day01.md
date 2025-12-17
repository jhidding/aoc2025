---
entangled:
  version: "2.4"
  namespace: day01
---

# Day 01: Secret Entrance

We have a dial from 0 to 99 that we rotate left and right.

```python
#| file: src/python/day01.py
from dataclasses import dataclass
from enum import StrEnum
from functools import reduce
from itertools import accumulate
from typing import Callable, Iterable


class Direction(StrEnum):
    LEFT = "left"
    RIGHT = "right"


@dataclass
class Instruction:
    direction: Direction
    amount: int

    def __str__(self):
        return f"{self.direction}-{self.amount}"


def readlines():
    try:
        while True:
            yield input()
    except EOFError:
        return


def parse_instruction(expr: str) -> Instruction:
    if expr[0] == "L":
        direction = Direction.LEFT
    elif expr[0] == "R":
        direction = Direction.RIGHT
    else:
        raise ValueError(f"Expression should start with L or R, got: {expr}")

    amount = int(expr[1:])

    return Instruction(direction, amount)


@dataclass
class Dial:
    position: int
    clicks: int = 0

    def __bool__(self):
        return self.position == 0


def rotate_check(dial: Dial, instruction: Instruction) -> Dial:
    match instruction.direction:
        case Direction.LEFT:
            newpos = dial.position - instruction.amount
        case Direction.RIGHT:
            newpos = dial.position + instruction.amount

    if newpos > 0:
        clicks = newpos // 100
    elif dial.position == 0:
        clicks = - (newpos // 100 + 1)
    else:
        clicks = - ((newpos - 1) // 100)

    return Dial(newpos % 100, dial.clicks + clicks)


if __name__ == "__main__":
    print("# Day 01")
    out = list(accumulate(map(parse_instruction, readlines()), rotate_check, initial=Dial(50)))
    print(sum(map(bool, out)))
    print(out[-1].clicks)
```

C3
--

C3 is a nice new variant of C. It has modules, namespaces and much saner ways to deal with memory.

```c3
//| file: src/c3/day01.c3
module day01;
import std::io;
import std::math;

<<day01::c3-dial>>
<<day01::c3-reading>>
<<day01::c3-main>>
```

We'll define a struct that keeps all state, and update while reading the input. The logic changes slightly from the Python version, because both division and modulo in C/C3 are different when it comes to negative numbers. Again I spent some time covering all the corner cases.

```c3
//| id: c3-dial
struct Dial
{
    int position;
    int n_stops_zero;
    int n_passes_zero;
}

fn int modulo(int a, int b) @inline {
    int c = a % b;
    return c < 0 ? b + c : c;
}

fn void update_dial(Dial *dial, int instr) {
    int newpos = dial.position + instr;

    dial.n_passes_zero += math::abs(newpos / 100);
    if (newpos < 0 && dial.position != 0 || newpos == 0) {
        dial.n_passes_zero += 1;
    }

    dial.position = modulo(newpos, 100);

    if (dial.position == 0) {
        dial.n_stops_zero += 1;
    }
}
```

The `read_instruction` function is most interesting because it shows some features of the C3 language. We have a new spin on the `Result` type idiom found in Rust and other languages. A result-type is suffixed with a question mark, meaning the function can throw an exception. The exclamation mark works similar to the question mark in Rust. Second, this function uses a *temporary allocator* to get the memory for reading lines. The `@pool` macro creates an arena where temporary memory is stored, which is automatically freed when the `@pool` block ends.

```c3
//| id: c3-reading
fn int? read_instruction() => @pool() {
    String line = io::readline(tmem)!;
    if (line[0] == 'L') {
        return -line[1..].to_int();
    } else {
        return line[1..].to_int();
    }
}
```

In the `main` program, we can use a nifty syntax `while (try x = ...) {}`. This reads instructions until the `read_instruction` function fails for some reason, like end-of-file.

```c3
//| id: c3-main
fn void main()
{
    io::printn("# Day 01");
    Dial dial = {50, 0, 0};
    while (try instr = read_instruction()) {
        update_dial(&dial, instr);
    }
    io::printfn("Part 1: %d\nPart 2: %d", dial.n_stops_zero, dial.n_passes_zero);
}
```

Scheme
------

First, I'll define transducers in Scheme.

```scheme
;| file: src/scheme/std/transducers.scm
(library (std transducers)
    (export make-reduced reduced? reduced-value map filter tee scanl stream-input)
    (import (except (rnrs) map filter)
            (prefix (only (rnrs) map filter) internal-))

    (define-record-type reduced (fields value))

    (define map
        (case-lambda
            ((f) (lambda (sink)
                (case-lambda
                    ; init
                    (() (sink))
                    ; fini
                    ((r) (sink r))
                    ; step
                    ((r x) (sink r (f x))))))
            ((f . args) (apply internal-map f args))))

    (define filter
        (case-lambda
            ((f) (lambda (sink)
                (case-lambda
                    ; init
                    (() (sink))
                    ; fini
                    ((r) (sink r))
                    ; step
                    ((r x) (if (f x) (sink r x) r)))))
            ((f . args) apply internal-filter f args)))

    (define scanl
        (case-lambda
            ((f init step)
                (case-lambda
                    ; init
                    (() (cons init (step)))
                    ; fini
                    ((r) (step (step (cdr r) (f (car r)))))
                    ; step
                    ((r x) (let ((r1 (f (car r) x)) (r2 (cdr r)))
                               (cons r1 (step r2 r1))))))
            ((f init) (lambda (step) (scanl f init step)))
            ((f)      (lambda (step) (scanl f (f) step)))))

    (define (tee . sinks)
        (case-lambda
            (() (map apply sinks))
            ((rs) rs)
            ((rs x) (map (lambda (sink r) (sink r x)) sinks rs))))

    (define stream-input
        (case-lambda
            ((sink) (let loop ((r (sink)))
                (if (reduced? r)
                    (reduced-value r)
                    (let ((line (get-line (current-input-port))))
                        (if (eof-object? line)
                            (sink r)
                            (loop (sink r line)))))))
            ((tr sink) (stream-input (tr sink)))))
)
```

```scheme
;| file: src/scheme/day01.scm
(import (except (rnrs) map filter)
        (std transducers)
        (std combinators)
        (monads monads)
        (parsing parsing))

(define (trace msg value)
    (display msg) (display value) (newline)
    value)

(define (trace-printer sink)
    (case-lambda
        (() (trace "init: " (sink)))
        ((r) (trace "result: " (sink r)))
        ((r x) (display "got: ") (display x) (display " -> ") (trace "" (sink r x)))))

(define print-result
    (case-lambda
        (() '())
        ((r) (display r) r)
        ((r x) x)))

(define instruction
  (seq <parsing>
      (sign <- (choice (parsing-bind (literal "L") (lambda (_) (parsing-return -1)))
                       (parsing-bind (literal "R") (lambda (_) (parsing-return 1)))))
      (amount <- integer)
      (parsing-return (* sign amount))))

(define (parse-instruction s)
    (parse-string instruction s))

(define count (compose (map (const 1)) (scanl +)))

(define main (compose (map parse-instruction)
                      (scanl + 50)
                      (filter (compose zero? (partial (swap mod) 100)))
                      count))

(display "Part 1: ") (stream-input main print-result) (newline)
```
