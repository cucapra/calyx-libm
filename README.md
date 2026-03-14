# The FPCore Frontend for Calyx

Compile [FPCore][fpcore] to fixed-point hardware via [Calyx][calyx].

## Installation

You'll need [Rust][rust] (1.88+) and the [Sollya][sollya] tool. On macOS, you
can install the latter via [Homebrew][brew] with `brew install sollya`. On
Ubuntu or Debian, use `apt install sollya`.

Finally, build the compiler with `cargo build`.

### `fud2` Support

Optionally, you can register the frontend with the [Calyx driver][fud2], `fud2`.
To make the compiler accessible from `fud2`, add it to the plugins list in your
`fud2.toml` file:

```toml
plugins = ["<repository root>/tools/fud/fpcore.rhai"]

[calyx-libm]
base = "<repository root>"
```

### Running Tests

The frontend is tested with [runt][runt]. Install it with `cargo install runt`,
then run the tests with `runt`.

## Usage

Input programs are written using a subset of the [FPCore 2.0][fpcore] format.
FPCore is a simple S-expression language designed to express numerical
computations. For example, `sinc` might be written as:

```lisp
(FPCore sinc (x)
 (let ([pi 3.14159])
   (if (!= x 0)
       (/ (sin (* pi x)) (* pi x))
       1.0)))
```

Calyx's FPCore frontend can be used to compile these programs into hardware. In
particular, it can generate fixed-point implementations of transcendental
functions such as the `sin` used in the preceding example.

### The FPCore Rounding Context

FPCore programs control the behavior of operations via a *rounding context*. The
frontend uses the rounding context to decide how each operator should be
implemented.

For instance, the following annotation updates the context so that any enclosed
operators are implemented using a 64-entry lookup table:

```lisp
(! :calyx-impl (lut 64)
  <body>)
```

#### Numeric Format

Currently, the numeric format is set globally for the entire program. The format
is specified on the command line in ARM-style
[Q notation](https://en.wikipedia.org/wiki/Q_(number_format)):

```sh
fud2 -s calyx-libm.args='--format Q16.16' <args...>
```

Any `:precision` annotations in the input program are ignored.

### Generating Mathematical Operators

While arithmetic operators are mapped directly to primitives in Calyx's standard
library, transcendental functions such as `sin` or `log` require that a bespoke
implementation be generated for each use.

The frontend recognizes two metadata properties that control the generated
implementations:

- `:calyx-domain (<left> <right>)` specifies that the function should be
  implemented on the half-open interval `[left, right)`.
- `:calyx-impl <method>` specifies the technique that should be used to
  implement the function.

Two implementation methods are currently supported:

- `:calyx-impl (lut <size>)` generates a lookup table with `size` entries.
- `:calyx-impl (poly <degree> <error>?)` generates a piecewise polynomial
  approximation with the given degree. By default, the operator will be
  faithfully rounded. If `error` is given, it specifies a relaxed error budget
  of at least 1 ulp.

#### Computing Domains Automatically

The frontend implements range analysis that can automatically infer the domain
of each operator given a precondition on the program inputs. Preconditions are
specified using the `:pre` property on the FPCore program:

```lisp
; an implementation of sigmoid on the interval (-4, 4)
(FPCore (x)
 :pre (< -4 x 4)
 :calyx-impl (poly 3)
 (/ 1.0 (+ 1.0 (exp (- x)))))
```

Currently, the only supported analysis is interval arithmetic. Enable it by
specifying `--range-analysis interval` on the command line.

#### Experimental: Compound Operators

The frontend's implementation techniques are not limited to only named functions
like `exp`; the entire sigmoid function from the previous example can be
implemented as a single polynomial approximation. Enable this feature with
`-e operator-coalescing`.

[brew]: https://brew.sh/
[calyx]: https://calyxir.org/
[fpcore]: https://fpbench.org/spec/fpcore-2.0.html
[fud2]: https://docs.calyxir.org/running-calyx/fud2/index.html
[runt]: https://github.com/rachitnigam/runt
[rust]: https://doc.rust-lang.org/cargo/getting-started/installation.html
[sollya]: https://www.sollya.org/
