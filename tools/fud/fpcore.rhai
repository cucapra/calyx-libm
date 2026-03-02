import "calyx" as c;

export const fpcore_state = state("fpcore", ["fpcore"]);

export const calyx_libm_setup = calyx_libm_setup;

fn calyx_libm_setup(e) {
    e.config_var("calyx-libm-base", "calyx-libm.base");
    e.config_var_or(
        "calyx-libm-exe",
        "calyx-libm.exe",
        "$calyx-libm-base/target/debug/calyx-libm",
    );
    e.config_var_or("calyx-libm-args", "calyx-libm.args", "");

    e.rule(
        "calyx-libm",
        "$calyx-libm-exe $calyx-libm-args $in > $out",
    );
}

op(
    "fpcore-to-calyx",
    [c::calyx_setup, calyx_libm_setup],
    fpcore_state,
    c::calyx_state,
    |e, input, output| {
        e.build_cmd([output], "calyx-libm", [input], []);
    },
);
