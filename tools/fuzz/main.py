import json
import random
import subprocess
from argparse import ArgumentParser
from enum import Enum
from pathlib import Path
from tempfile import TemporaryDirectory

from fixedpoint import FixedPoint
from fpcorelib import FPCore

from interp import LIB_FIXED_POINT
from qformat import QFormat, RoundingMode
from rand import random_fpcore


class Simulator(Enum):
    IVERILOG = 'fpcore-icarus'
    VERILATOR = 'fpcore-verilator'

    def __str__(self) -> str:
        return self.value


def run(
    benchmark: FPCore[FixedPoint],
    vals: list[FixedPoint],
    fmt: QFormat,
    sim: Simulator,
) -> int:
    with TemporaryDirectory() as tmp:
        file = Path(tmp, 'data.dat')

        with file.open('w') as f:
            print(*(format(val.bits, 'x') for val in vals), file=f)

        cmd = [
            'fud2',
            '--from', 'fpcore',
            '--through', sim.value,
            '--to', 'fpcore-dat',
            '-s', f'calyx-libm.args=--format {fmt}',
            '-s', f'sim.data={file}',
            '--quiet',
        ]  # fmt: skip

        proc = subprocess.run(
            cmd,
            check=True,
            text=True,
            input=str(benchmark),
            stdout=subprocess.PIPE,
        )

    result = json.loads(proc.stdout)

    return result['output'][0]


def format_diagnostic(
    benchmark: FPCore[FixedPoint],
    vals: list[FixedPoint],
    fmt: QFormat,
    got: FixedPoint,
    expected: FixedPoint,
) -> str:
    args = ', '.join(map(hex, vals))

    return (
        '  ---\n'
        '  FPCore: {core}\n'
        '  args: [{args}]\n'
        '  format: {fmt}\n'
        '  got: {got:#x}\n'
        '  expected: {expected:#x}\n'
        '  ...'
    ).format(core=benchmark, args=args, fmt=fmt, got=got, expected=expected)


def main():
    parser = ArgumentParser(description='FPCore fuzzer.')

    parser.add_argument(
        '-n',
        '--trials',
        type=int,
        default=10,
        help='number of benchmarks to generate',
    )
    parser.add_argument(
        '-f',
        '--format',
        default='UQ32.0',
        help='numeric format',
    )
    parser.add_argument(
        '--argc',
        type=int,
        default=5,
        help='number of arguments in the generated benchmarks',
    )
    parser.add_argument(
        '--size',
        type=float,
        default=25,
        help='target size for the generated benchmarks',
    )
    parser.add_argument(
        '--sim',
        type=Simulator,
        choices=tuple(Simulator),
        default=Simulator.IVERILOG,
        help='simulator',
    )
    parser.add_argument(
        '-v',
        '--verbose',
        action='store_true',
        help='show metadata for passing tests',
    )

    opts = parser.parse_args()

    fmt = QFormat.parse(opts.format)

    mode: RoundingMode = {
        'overflow': 'wrap',
        'rounding': 'down',
        'overflow_alert': 'ignore',
    }

    def dist():
        return fmt.decode(random.getrandbits(fmt.width), mode)

    print('TAP version 14')
    print(f'1..{opts.trials}')

    try:
        for _ in range(opts.trials):
            args = [f'x{i}' for i in range(opts.argc)]
            vals = [dist() for _ in range(opts.argc)]

            bench = random_fpcore(None, args, opts.size, dist)

            got = fmt.decode(run(bench, vals, fmt, opts.sim))
            expected = bench.interp(vals, LIB_FIXED_POINT)

            if got == expected and not opts.verbose:
                print(f'ok - {got:#x}')
            else:
                print('ok' if got == expected else 'not ok')
                print(format_diagnostic(bench, vals, fmt, got, expected))
    except Exception as e:
        print('Bail out!', e)


if __name__ == '__main__':
    main()
