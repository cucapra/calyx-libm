//! Error reporting.

use std::io::{self, IsTerminal};
use std::ops::Range;

use codespan_reporting::diagnostic::{Diagnostic as InnerDiagnostic, Label};
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use codespan_reporting::term::{self, Config};

pub struct Diagnostic(InnerDiagnostic<()>);

impl Diagnostic {
    pub fn bug() -> Diagnostic {
        Self(InnerDiagnostic::bug())
    }

    pub fn error() -> Diagnostic {
        Self(InnerDiagnostic::error())
    }

    pub fn warning() -> Diagnostic {
        Self(InnerDiagnostic::warning())
    }

    pub fn with_message<M: Into<String>>(mut self, message: M) -> Diagnostic {
        self.0.message = message.into();
        self
    }

    pub fn with_primary<S, L>(mut self, span: S, label: L) -> Diagnostic
    where
        S: Into<Range<usize>>,
        L: Into<String>,
    {
        self.0
            .labels
            .push(Label::primary((), span).with_message(label));

        self
    }

    pub fn with_secondary<S, L>(mut self, span: S, label: L) -> Diagnostic
    where
        S: Into<Range<usize>>,
        L: Into<String>,
    {
        self.0
            .labels
            .push(Label::secondary((), span).with_message(label));

        self
    }

    pub fn with_note<N: Into<String>>(mut self, note: N) -> Diagnostic {
        self.0.notes.push(note.into());
        self
    }
}

impl From<io::Error> for Diagnostic {
    fn from(err: io::Error) -> Self {
        Diagnostic::error().with_message(err.to_string())
    }
}

pub struct Reporter<'src> {
    file: SimpleFile<&'src str, &'src str>,
    writer: StandardStream,
}

impl Reporter<'_> {
    pub fn new<'src>(filename: &'src str, source: &'src str) -> Reporter<'src> {
        let choice = if std::io::stderr().is_terminal() {
            ColorChoice::Auto
        } else {
            ColorChoice::Never
        };

        Reporter {
            file: SimpleFile::new(filename, source),
            writer: StandardStream::stderr(choice),
        }
    }

    pub fn emit(&mut self, diagnostic: &Diagnostic) {
        term::emit(
            &mut self.writer,
            &Config::default(),
            &self.file,
            &diagnostic.0,
        )
        .unwrap();
    }
}
