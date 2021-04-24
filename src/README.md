# Polygen: build instructions

## Table of contents

* [Requirements](#requirements)
  + [Portability](#portability)
* [Building](#building)
  + [Installing opam](#installing-opam)
  + [Building Polygen](#building-polygen)
* [Using Polygen](#using-polygen)
  + [Command line program](#command-line-program)
  + [Polygen as a library](#polygen-as-a-library)
  + [Polygen inside the browser](#polygen-inside-the-browser)

## Requirements

Polygen is written in OCaml, a highly efficient dialect of the functional language ML - a serious language with a serious paradigm for serious programmers.

Thus, in order to compile it by yourself, you need the following:

 - OCaml bytecode (ocamlc) or native code (ocamlopt) compiler
 - OCaml lexer (ocamllex) and parser (ocamlyacc) generators
 - [`dune`](https://dune.readthedocs.io/en/stable/), the _de facto_ standard build system of the OCaml ecosystem

For instructions to install all of these components see [building](#building) below.


Yuo can learn more about OCaml browsing the [official site](https://ocaml.org/).


### Portability

OCaml is a platform-independent language, since its standard library abstracts from the underlying operating system.

Polygen uses just few I/O functions, all the rest being pure owner code: it is therefore 100% portable to any platform supported by an (even poor) OCaml implementation.

## Building

To build Polygen you have to install the OCaml standard toolchain (compiler, build system, standard libraries, etc.). There are many ways to achive this task, but we think the easiest way is using the [opam](https://opam.ocaml.org/) package manager.

### Installing opam

First of all you need opam to be installed on your computer. Follow the official documentation about [How to install opam](https://opam.ocaml.org/doc/Install.html). Most probably opam is available in your Linux distrubution using the [distro package manager](https://opam.ocaml.org/doc/Install.html#Using-your-distribution-39-s-package-system).

### Building Polygen

Having opam installed it's easy to build and install Polygen.

```bash
$ opam init # answer YES to both the questions
$ eval $(opam env)
$ git clone --recursive https://github.com/alvisespano/Polygen.git
$ cd Polygen
$ opam install .
$ polygen
Polygen (type 2) v1.0.6 build 20180122 - http://www.polygen.org
Manta/Spinning Kids alias Alvise Spano' anno MMII ac insequenti fecit.

usage: polygen [OPTION]... SOURCES...

 SOURCE     source file(s) providing grammar definition

 OPTION
  -eof STR  use string STR as end-of-file (default: STR = "\n")
  -help     display this help message
  -info     alias for '-S I'
  -l LABEL  add LABEL to initial active label environment
  -o DEST   output to DEST file
  -pedantic set warning level to maximum
  -pre      output preprocessed source grammar
  -seed N   pass unsigned integer N as random seed
  -S SYM    use SYM as starting non-terminal symbol (default: SYM = S)
  -t        check source grammar and output inferred global label groups
  -v        be verbose
  -X N      iterate generation for N times (default: N = 1)
  -W N      set warning pedantry at level N (default: N = 1)
  --help  Display this list of options
```

Done!

The `polygen` executable is installed in `$HOME/.opam/default/bin/polygen`: since it's a statically linked program you can move it in the location you prefer in your `$PATH` and remove all the OCaml environment, which is as simple as:

```bash
$ rm -Rf $HOME/.opam
```

## Using Polygen

### Command line program

Polygen first use is via command line: `polygen --help` gives you the complete set of available options.
### Polygen as a library

Polygen is also a library: if you want to include functions from Polygen in your OCaml program the complete library is exposed in the current [opam switch](https://opam.ocaml.org/doc/Usage.html#opam-switch) as `polygen`. If you use `dune` as a build system you have to include the library in the list of used libraries. This is an example of `dune` file:

```
(executable
  (name my_program)
  (public_name my_program)
  (libraries polygen another_library)
  (modules my_program another_module_in_my_source)
)
```

In your program the library is contained in the `Polygen_lib` module.

### Polygen inside the browser

The generative subset of the Polygen library is also available in the browser because the whole library is also transcompiled into Javascript.

The relevant file is installed in `HOME/.opam/default/lib/polygen/polygen.js`: copy that file where you prefer and include this line into your HTML:

```html
<script src="polygen.js"></script>
```

From any Javascript program you can now access the global `Polygen` object in which a single function is present: `generate`.

A minimal working example is present in the `html` filder.
