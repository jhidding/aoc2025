# ~/~ begin <<docs/day10.md#docs/day10.md::src/python/parsing.py>>[init]
from __future__ import annotations
from dataclasses import dataclass, field
from abc import ABCMeta, abstractmethod
from collections.abc import Callable
from functools import partial, wraps
import re
from typing import cast, Any


@dataclass
class Failure:
    msg: str = "unknown failure"
    inp: str = "unknown input"

    def __str__(self):
        return f"expected `{self.msg}`, got `{self.inp}`"


class Parser[T](metaclass=ABCMeta):
    @abstractmethod
    def parse(self, inp: str) -> tuple[T, str] | Failure:
        ...

    def bind[U](self, foo: Callable[[T], Parser[U]]) -> Parser[U]:
        @parser
        def bound(inp: str) -> tuple[U, str] | Failure:
            match self.parse(inp):
                case Failure() as f:
                    return f
                case (value, more_inp):
                    return foo(value).parse(more_inp)

        return bound

    def map[U](self, foo: Callable[[T], U]) -> Parser[U]:
        return self.bind(partial(safe_map, foo))

    def __or__[U](self, other: Parser[U]) -> Choice[T | U]:
        match (self, other):
            case (Choice(c1), Choice(c2)):
                return Choice(cast(list[Parser[T | U]], c1 + c2))
            case (Choice(c1), _):
                return Choice(cast(list[Parser[T | U]], c1 + [other]))
            case (_, Choice(c2)):
                return Choice(cast(list[Parser[T | U]], [self] + c2))
            case (_, _):
                return Choice(cast(list[Parser[T | U]], [self, other]))


@dataclass
class CompoundFailure(Failure):
    fails: list[Failure] = field(default_factory=list)


@dataclass
class Choice[T](Parser[T]):
    choices: list[Parser[T]]

    def parse(self, inp: str) -> tuple[T, str] | Failure:
        f = CompoundFailure("one of", inp)
        for p in self.choices:
            match x := p.parse(inp):
                case CompoundFailure(_, _, cf):
                    f.fails.extend(cf)
                case Failure():
                    f.fails.append(x)
                case _:
                    return x
        return f

def safe_map[T, U](foo: Callable[[T], U], t: T) -> Parser[U]:
    try:
        return pure(foo(t))
    except Exception as e:
        return fail(str(e))


@dataclass
class FnParser[T](Parser[T]):
    foo: Callable[[str], tuple[T, str] | Failure]

    def parse(self, inp: str) -> tuple[T, str] | Failure:
        return self.foo(inp)


def parser[T](foo: Callable[[str], tuple[T, str] | Failure]) -> FnParser[T]:
    return FnParser(foo)


def fail[T](msg: str) -> Parser[T]:
    return parser(lambda inp: Failure(msg, inp))


def pure[T](value: T) -> Parser[T]:
    return parser(lambda inp: (value, inp))


def match(expr: str) -> Parser[re.Match[str]]:
    c = re.compile(expr)

    @parser
    def matching(inp: str) -> tuple[re.Match[str], str] | Failure:
         if m := re.match(c, inp):
             return m, inp[m.end():]
         else:
             return Failure(expr, inp)

    return matching


def integer() -> Parser[int]:
    return match(r"-?\d+").map(lambda m: int(m.group()))


def many[T](p: Parser[T], init: list[T] | None = None) -> Parser[list[T]]:
    @parser
    def many_p(inp: str) -> tuple[list[T], str] | Failure:
        result = init or []
        while True:
            match p.parse(inp):
                case Failure():
                    return (result, inp)
                case (item, next_inp):
                    result.append(item)
                    inp = next_inp

    return many_p


def singleton[T](v: T) -> list[T]:
    return [v]


def some[T](p: Parser[T]) -> Parser[list[T]]:
    return p.map(singleton).bind(partial(many, p))


def struct[T,**Args](t: type[T], **kwargs: Parser[Any]) -> Parser[T]:
    @parser
    def struct_p(inp: str) -> tuple[T, str] | Failure:
        args = {}
        for k, p in kwargs.items():
            match p.parse(inp):
                case Failure() as f:
                    return f
                case (value, next_inp):
                    if k[0] != '_':
                        args[k] = value
                    inp = next_inp
        return t(**args), inp

    return struct_p


def literal(x: str) -> Parser[str]:
    @parser
    def literal_p(inp: str) -> tuple[str, str] | Failure:
        if inp.startswith(x):
            return x, inp.removeprefix(x)
        else:
            return Failure(x, inp)

    return literal_p


def tokenize[T](p: Parser[T], whitespace=match(r"\s*")) -> Parser[T]:
    return p.bind(lambda x: whitespace.bind(lambda _: pure(x)))


def delimited[T](open: Parser[Any], close: Parser[Any], p: Parser[T]) -> Parser[T]:
    return open.bind(lambda _: p.bind(lambda x: close.bind(lambda _: pure(x))))


def separated_by[T](sep: Parser[Any], p: Parser[T]) -> Parser[list[T]]:
    return p.bind(lambda x: many(sep.bind(lambda _: p), [x]))
# ~/~ end
