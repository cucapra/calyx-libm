import re
from pathlib import Path

import simplejson as json
from fud import errors
from fud.stages import SourceType, Stage
from fud.utils import TmpDir, shell


class CalyxLibmStage(Stage):
    name = 'calyx-libm'

    def __init__(self):
        super().__init__(
            src_state='fpcore',
            target_state='calyx',
            input_type=SourceType.Stream,
            output_type=SourceType.Stream,
            description='Compile FPCore to Calyx',
        )

    @staticmethod
    def pre_install():
        pass

    @staticmethod
    def defaults():
        root = Path(__file__).parent.parent
        exe = root / 'target' / 'debug' / 'calyx-libm'

        return {'exec': str(exe.resolve()), 'file_extensions': ['.fpcore']}

    def known_opts(self):
        return ['exec', 'file_extensions', 'flags']

    def _define_steps(self, input, builder, config):
        exe = config['stages', self.name, 'exec']
        flags = config.get(['stages', self.name, 'flags']) or ''

        cmd = ' '.join([exe, flags])

        @builder.step(description=cmd)
        def run_calyx_libm(stream: SourceType.Stream) -> SourceType.Stream:
            return shell(cmd, stdin=stream)

        return run_calyx_libm(input)


class SimulationBaseStage(Stage):
    def __init__(
        self,
        source: str,
        description: str,
        compiled_name: str,
    ):
        super().__init__(
            src_state=source,
            target_state='libm-dat',
            input_type=SourceType.Path,
            output_type=SourceType.Stream,
            description=description,
        )

        self.compiled_name = compiled_name

    def compiler_exec(self, config) -> str:
        raise NotImplementedError

    def compiler_flags(self, tmpdir: str) -> list[str]:
        raise NotImplementedError

    @staticmethod
    def pre_install():
        pass

    @staticmethod
    def defaults():
        return {}

    def _define_steps(self, input, builder, config):
        tb = Path(__file__).parent.resolve() / 'sim' / 'tb.sv'

        @builder.step()
        def mktmp() -> SourceType.Directory:
            """
            Make a temporary directory to store build files.
            """

            return TmpDir()

        @builder.step()
        def get_data() -> SourceType.Path:
            """
            Dynamically retrieve the value of `stages.verilog.data'.
            """

            if path := config.get(['stages', 'verilog', 'data']):
                return Path(path)
            else:
                raise errors.MissingDynamicConfiguration('verilog.data')

        @builder.step()
        def compile(
            input: SourceType.Path,
            data: SourceType.Path,
            tmp: SourceType.Directory,
        ) -> SourceType.Stream:
            """
            Compile Verilog.
            """

            macros = TestBenchMacros(input.read_text())

            with open(data) as f:
                stimuli = sum(1 for line in f if not line.isspace())

            cmd = [
                self.compiler_exec(config),
                *self.compiler_flags(tmp.name),
                '-o',
                f'{tmp.name}/{self.compiled_name}',
                f"-DDUT='{macros.dut}'",
                f"-DDIM='{macros.dim}'",
                f'-DARGC={macros.argc}',
                f'-DSTIMULI={stimuli}',
                '-DCOMB' if macros.comb else '-DSEQ',
                str(tb),
                str(input),
            ]

            return shell(cmd, stdout_as_debug=True)

        @builder.step()
        def simulate(
            data: SourceType.Path, tmp: SourceType.Directory
        ) -> SourceType.Stream:
            """
            Simulate the compiled program.
            """

            cmd = [
                f'{tmp.name}/{self.compiled_name}',
                f'+INPUT={data}',
                f'+OUTPUT={tmp.name}/results.out',
            ]

            return shell(cmd)

        @builder.step()
        def output_json(
            stdout: SourceType.String, tmp: SourceType.Directory
        ) -> SourceType.Stream:
            """
            Output data file.
            """

            with open(f'{tmp.name}/results.out', 'r') as f:
                results = [
                    int(line, 16) for line in f if not line.startswith('//')
                ]

            if match := re.search(r'min=(\d+) max=(\d+) total=(\d+)', stdout):
                best, worst, total = match.groups()

                data = {
                    'output': results,
                    'best_latency': int(best),
                    'worst_latency': int(worst),
                    'total_latency': int(total),
                }
            else:
                raise errors.Malformed(
                    'simulation output', "Couldn't parse latency"
                )

            output = f'{tmp.name}/output.json'

            with open(output, 'w') as f:
                json.dump(data, f)

            return open(output, 'rb')

        @builder.step()
        def cleanup(tmp: SourceType.Directory):
            """
            Clean up build files.
            """

            tmp.remove()

        tmp = mktmp()
        data = get_data()
        compile(input, data, tmp)
        stdout = simulate(data, tmp)
        result = output_json(stdout, tmp)
        cleanup(tmp)

        return result


class TestBenchMacros:
    def __init__(self, verilog: str):
        inputs: list[str] = []

        if match := re.search(r'module main\(([^)]*)\)', verilog):
            ports = match.group(1)

            for match in re.finditer(r'input logic (\[[^]]*\])? ?(\w+)', ports):
                dim, ident = match.groups(default='')

                if ident not in ('go', 'clk', 'reset'):
                    inputs.append(ident)
                    self.dim = dim
        else:
            raise errors.Malformed('Verilog', "Couldn't find main module")

        self.dut = self.instantiate_dut(inputs)
        self.argc = len(inputs)
        self.comb = 'output logic done' not in ports

    @staticmethod
    def instantiate_dut(inputs: list[str]):
        connections = ', '.join(
            f'.{port}(args[{i}])' for i, port in enumerate(inputs)
        )

        return f'main dut ({connections}, .*);'


class CalyxLibmVerilatorStage(SimulationBaseStage):
    name = 'libm-verilog'

    def __init__(self):
        super().__init__(
            source='verilog',
            description='Simulate numeric programs with Verilator',
            compiled_name='Vtb',
        )

    def compiler_exec(self, config):
        return config['stages', 'verilog', 'exec']

    def compiler_flags(self, tmpdir: str) -> list[str]:
        return [
            '--binary',
            '--top-module',
            'tb',
            '--Mdir',
            f'{tmpdir}/obj_dir',
        ]


class CalyxLibmIcarusStage(SimulationBaseStage):
    name = 'libm-icarus-verilog'

    def __init__(self):
        super().__init__(
            source='icarus-verilog',
            description='Simulate numeric programs with Icarus Verilog',
            compiled_name='main.vvp',
        )

    def compiler_exec(self, config):
        return config['stages', 'icarus-verilog', 'exec']

    def compiler_flags(self, tmpdir: str) -> list[str]:
        return ['-g2012']


__STAGES__ = [
    CalyxLibmStage,
    CalyxLibmVerilatorStage,
    CalyxLibmIcarusStage,
]
