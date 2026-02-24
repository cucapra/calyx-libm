import "fpcore" as fp;
import "hls" as hls;

export const cosim_dat = state("cosim-dat", ["dat"]);

private fn cosim_setup(e) {
    e.var_("bench", "$calyx-libm-base/tools/fud/hls/cosim/bench.cpp");
    e.var_("cosim", "$calyx-libm-base/tools/fud/hls/cosim/cosim.tcl");
    e.var_("extract", "$calyx-libm-base/tools/fud/hls/cosim/extract.py");

    e.var_("cosim-header", e.external_path(e.config_val("cosim.header")));
    e.var_("cosim-data", e.external_path(e.config_val("cosim.data")));

    e.rule("copy", "cp $in $out");

    e.build_cmd(["bench.cpp"], "copy", ["$bench"], []);
    e.build_cmd(["cosim.tcl"], "copy", ["$cosim"], []);
    e.build_cmd(["kernel.hpp"], "copy", ["$cosim-header"], []);
    e.build_cmd(["sample.dat"], "copy", ["$cosim-data"], []);

    e.var_("result", "project/solution1/sim/wrapc/result.dat");
    e.var_("report", "project/solution1/sim/report/verilog/lat.rpt");

    e.rule("vitis-cosim", "vitis-run --tcl cosim.tcl > /dev/null");

    e.build_cmd(
        ["$result", "$report"],
        "vitis-cosim",
        ["cosim.tcl"],
        ["kernel.cpp", "kernel.hpp", "bench.cpp", "sample.dat"],
    );

    e.rule("extract-latency", "python3 $extract $in $out");
}

op(
    "vitis-cosim",
    [fp::calyx_libm_setup, cosim_setup],
    hls::cpp_state,
    cosim_dat,
    |e, input, output| {
        e.build_cmd(["kernel.cpp"], "copy", [input], []);

        let latency = e.config_or("cosim.latency", "");

        let deps = if !latency.is_empty() {
            e.var_("cosim-latency", e.external_path(latency));

            e.build_cmd(
                ["$cosim-latency"],
                "extract-latency",
                ["$report"],
                [],
            );

            ["$cosim-latency"]
        } else {
            []
        };

        e.build_cmd([output], "copy", ["$result"], deps);
    },
);
