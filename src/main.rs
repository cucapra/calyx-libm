mod opts;

use std::borrow::Cow;
use std::fs::{self, File};
use std::io;
use std::path::PathBuf;
use std::process::ExitCode;

use calyx_libm_ast::{FPCoreParser, Span};
use calyx_libm_ast_lowering::lower_ast;
use calyx_libm_backend::{Program, compile_hir};
use calyx_libm_hir_passes::run_passes;
use calyx_libm_utils::{Diagnostic, Reporter};

use opts::Opts;

fn read_input(file: &Option<PathBuf>) -> io::Result<(Cow<'_, str>, String)> {
    if let Some(file) = file {
        let filename = file.to_string_lossy();
        let src = fs::read_to_string(file)?;

        Ok((filename, src))
    } else {
        let filename = Cow::from("<stdin>");
        let src = io::read_to_string(io::stdin())?;

        Ok((filename, src))
    }
}

fn write_output(program: &Program, file: &Option<PathBuf>) -> io::Result<()> {
    let mut out: Box<dyn io::Write> = if let Some(path) = file {
        Box::new(File::create(path)?)
    } else {
        Box::new(io::stdout())
    };

    program.write(&mut out)
}

fn main() -> ExitCode {
    let opts = Opts::parse();
    let config = opts.config();

    let (filename, src) = match read_input(&opts.file) {
        Ok(result) => result,
        Err(err) => {
            Reporter::new("", "").emit(&Diagnostic::from(err));

            return ExitCode::FAILURE;
        }
    };

    let mut reporter = Reporter::new(&filename, &src);

    let defs = match FPCoreParser::parse_file(&src) {
        Ok(result) => result,
        Err(err) => {
            reporter.emit(
                &Diagnostic::error()
                    .with_message("syntax error")
                    .with_primary(
                        Span::from(err.location),
                        err.variant.message(),
                    ),
            );

            return ExitCode::FAILURE;
        }
    };

    let Some(mut ctx) = lower_ast(defs, &config, &mut reporter) else {
        return ExitCode::FAILURE;
    };

    if run_passes(&mut ctx, &config, &mut reporter).is_err() {
        return ExitCode::FAILURE;
    }

    let Some(program) = compile_hir(&ctx, &config, &mut reporter) else {
        return ExitCode::FAILURE;
    };

    if let Err(err) = write_output(&program, &opts.output) {
        reporter.emit(&Diagnostic::from(err));

        return ExitCode::FAILURE;
    }

    ExitCode::SUCCESS
}
