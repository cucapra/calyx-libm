import random
from collections.abc import Callable
from typing import Any, Optional

from fpcorelib import ast
from fpcorelib.ast import Expr, FPCore, Num, Number, Operation, Symbol

Dist = Callable[[], Num]


def random_sym(vars: list[str]) -> Symbol[Any]:
    return Symbol(random.choice(vars))


def random_num(dist: Dist[Num]) -> Number[Num]:
    return Number(dist())


def random_op(vars: list[str], lpi: float, dist: Dist[Num]) -> Operation[Num]:
    op, arity = random.choice((('+', 2), ('-', 2), ('*', 2), ('sqrt', 1)))
    args = [random_expr(vars, lpi / arity, dist) for _ in range(arity)]

    return Operation(op, args)


def random_expr(vars: list[str], lpi: float, dist: Dist[Num]) -> Expr[Num]:
    if random.random() < 1 / lpi:
        if random.getrandbits(1):
            return random_sym(vars)
        else:
            return random_num(dist)
    else:
        return random_op(vars, lpi, dist)


def random_fpcore(
    name: Optional[str], args: list[str], lpi: float, dist: Dist[Num]
) -> FPCore[Num]:
    argv = [ast.Argument[Num](arg, []) for arg in args]
    props = [ast.Property(':calyx-impl', Symbol[Num]('iterative'))]
    body = random_expr(args, lpi, dist)

    return FPCore(name, argv, props, body)
