import re
from dataclasses import dataclass
from typing import TypedDict, Union

from fixedpoint import FixedPoint

SupportsFixedPoint = Union[int, float, str]


class RoundingMode(TypedDict, total=False):
    overflow: str
    rounding: str
    overflow_alert: str


@dataclass
class QFormat:
    int_width: int
    frac_width: int
    is_signed: bool

    @classmethod
    def parse(cls, format: str):
        if match := re.match(r'^(U)?Q(\d+).(\d+)$', format):
            u, m, n = match.groups()

            return cls(int(m), int(n), u is None)
        else:
            raise ValueError('invalid format')

    @property
    def width(self) -> int:
        return self.int_width + self.frac_width

    def cast(self, x: SupportsFixedPoint, mode: RoundingMode) -> FixedPoint:
        return FixedPoint(
            x,
            signed=self.is_signed,
            m=self.int_width,
            n=self.frac_width,
            implicit_cast_alert='error',
            mismatch_alert='error',
            **mode,
        )

    def decode(
        self,
        bits: int,
        mode: RoundingMode = {},  # noqa: B006
    ) -> FixedPoint:
        return self.cast(hex(bits), mode)

    def __str__(self) -> str:
        prefix = 'UQ'[self.is_signed :]

        return f'{prefix}{self.int_width}.{self.frac_width}'
