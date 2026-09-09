import os
from pathlib import Path

from fud.errors import MissingFile
from fud.stages import SourceType, Stage
from fud.stages.vivado.stage import VivadoBaseStage
from fud.utils import shell


class FPBenchStage(Stage):
    name = 'fpbench-tools'

    def __init__(self):
        super().__init__(
            src_state='fpcore',
            target_state='vivado-hls',
            input_type=SourceType.Stream,
            output_type=SourceType.Stream,
            description='Compile FPCore to Vivado C++',
        )

    @staticmethod
    def pre_install():
        pass

    @staticmethod
    def defaults():
        return {'exec': 'racket'}

    def known_opts(self):
        return ['exec', 'exporter', 'lang']

    def _define_steps(self, input, builder, config):
        racket = config['stages', self.name, 'exec']
        exporter = config['stages', self.name, 'exporter']
        lang = config.get(['stages', self.name, 'lang']) or 'vivado'

        cmd = f'{racket} {exporter} --lang {lang} - -'

        @builder.step(description=cmd)
        def run_exporter(stream: SourceType.Stream) -> SourceType.Stream:
            return shell(cmd, stdin=stream)

        return run_exporter(input)


class VivadoCoSimulationStage(VivadoBaseStage):
    name = 'vivado-hls'

    def __init__(self):
        super().__init__(
            source='vivado-hls',
            destination='cosim-files',
            description='Run C/RTL Co-Simulation',
            target_name='kernel.cpp',
            remote_exec='vivado_hls',
            flags='-f cosim.tcl',
        )

    @staticmethod
    def pre_install():
        pass

    @staticmethod
    def defaults():
        return {}

    def device_files(self, config):
        cosim = Path(__file__).parent.resolve() / 'cosim'

        header = config['stages', self.name, 'header']
        data = config['stages', self.name, 'data']

        return [
            cosim / 'cosim.tcl',
            cosim / 'bench.cpp',
            Path(header),
            Path(data),
        ]


class VivadoCoSimulationExtractStage(Stage):
    name = 'cosim-files'

    def __init__(self):
        super().__init__(
            src_state='cosim-files',
            target_state='cosim-dat',
            input_type=SourceType.Directory,
            output_type=SourceType.String,
            description='Extract co-simulation results',
        )

    @staticmethod
    def pre_install():
        pass

    @staticmethod
    def defaults():
        return {}

    def _define_steps(self, input, builder, config):
        @builder.step()
        def extract(directory: SourceType.Directory) -> SourceType.String:
            """
            Extract the data file produced during co-simulation.
            """

            for root, dirs, _ in os.walk(directory.name):
                if 'project' in dirs:
                    project = Path(os.path.join(root, 'project'))

                    break
            else:
                raise MissingFile('project')

            dat = project / 'solution1' / 'sim' / 'wrapc' / 'result.dat'

            return dat.read_text()

        return extract(input)


__STAGES__ = [
    FPBenchStage,
    VivadoCoSimulationStage,
    VivadoCoSimulationExtractStage,
]
