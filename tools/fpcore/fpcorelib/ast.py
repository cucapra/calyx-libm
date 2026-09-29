from abc import ABC, abstractmethod
from collections.abc import Callable, Iterable
from dataclasses import dataclass
from itertools import chain
from typing import Generic, Optional, SupportsFloat, TypeVar, Union

Num = TypeVar('Num', bound=SupportsFloat)

Ctx = dict[str, Num]
Lib = dict[str, Callable[..., Num]]


class Expr(ABC, Generic[Num]):
    @abstractmethod
    def interp(self, ctx: Ctx[Num], lib: Lib[Num]) -> Num: ...

    @abstractmethod
    def __str__(self) -> str: ...


@dataclass(eq=False)
class Symbol(Expr[Num]):
    id: str

    def interp(self, ctx: Ctx[Num], lib: Lib[Num]) -> Num:
        return ctx[self.id]

    def __str__(self) -> str:
        return self.id


@dataclass(eq=False)
class Number(Expr[Num]):
    val: Num

    def interp(self, ctx: Ctx[Num], lib: Lib[Num]) -> Num:
        return lib['cast'](self.val)

    def __str__(self) -> str:
        val = float(self.val)

        return f'{val:.0f}' if val.is_integer() else val.hex()


@dataclass(eq=False)
class Operation(Expr[Num]):
    op: str
    args: list[Expr[Num]]

    def interp(self, ctx: Ctx[Num], lib: Lib[Num]) -> Num:
        args = [arg.interp(ctx, lib) for arg in self.args]

        return lib[self.op](*args)

    def __str__(self) -> str:
        args = ' '.join(map(str, self.args))

        return f'({self.op} {args})'


@dataclass(eq=False)
class If(Expr[Num]):
    cond: Expr[Num]
    true: Expr[Num]
    false: Expr[Num]

    def interp(self, ctx: Ctx[Num], lib: Lib[Num]) -> Num:
        if self.cond.interp(ctx, lib):
            return self.true.interp(ctx, lib)
        else:
            return self.false.interp(ctx, lib)

    def __str__(self) -> str:
        return f'(if {self.cond} {self.true} {self.false})'


@dataclass(eq=False)
class Binder(Generic[Num]):
    var: str
    expr: Expr[Num]

    def __str__(self) -> str:
        return f'[{self.var} {self.expr}]'


@dataclass(eq=False)
class Let(Expr[Num]):
    binders: list[Binder[Num]]
    body: Expr[Num]
    seq: bool

    def interp(self, ctx: Ctx[Num], lib: Lib[Num]) -> Num:
        scope = ctx.copy()

        for binder in self.binders:
            inner = scope if self.seq else ctx

            scope[binder.var] = binder.expr.interp(inner, lib)

        return self.body.interp(scope, lib)

    def __str__(self) -> str:
        binders = ' '.join(map(str, self.binders))
        star = '*' if self.seq else ''

        return f'(let{star} ({binders}) {self.body})'


Data = Union[str, Expr[Num], list['Data[Num]']]


@dataclass(eq=False)
class Property(Generic[Num]):
    name: str
    data: Data[Num]

    def __str__(self) -> str:
        def s_expr(data: Data[Num]) -> str:
            if isinstance(data, list):
                return '({})'.format(' '.join(map(s_expr, data)))
            else:
                return str(data)

        return f'{self.name} {s_expr(self.data)}'


@dataclass(eq=False)
class Annotation(Expr[Num]):
    props: list[Property[Num]]
    body: Expr[Num]

    def interp(self, ctx: Ctx[Num], lib: Lib[Num]) -> Num:
        return self.body.interp(ctx, lib)

    def __str__(self) -> str:
        props = ' '.join(map(str, self.props))

        return f'(! {props} {self.body})'


@dataclass(eq=False)
class Argument(Generic[Num]):
    var: str
    props: list[Property[Num]]

    def __str__(self) -> str:
        if len(self.props) > 0:
            props = ' '.join(map(str, self.props))

            return f'(! {props} {self.var})'
        else:
            return self.var


@dataclass(eq=False)
class FPCore(Generic[Num]):
    name: Optional[str]
    args: list[Argument[Num]]
    props: list[Property[Num]]
    body: Expr[Num]

    def interp(
        self,
        args: Iterable[Num],
        lib: Lib[Num],
        env: Ctx[Num] = {},  # noqa: B006
    ) -> Num:
        ctx = {arg.var: lib['cast'](val) for arg, val in zip(self.args, args)}

        return self.body.interp(env | ctx, lib)

    def __str__(self) -> str:
        name = self.name or ''
        args = ' '.join(map(str, self.args))
        props = ' '.join(chain(map(str, self.props), ('',)))

        return f'(FPCore {name}({args}) {props}{self.body})'
