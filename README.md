# mao-lang

Web interpreter for the *MAO* (acronym for ***M**odello **A**stratto **O**perazionale* in Italian) language, a simple didactic imperative language taught in the course of *Programmazione & Algoritmica* at the 1st year of the Computer Science degree at the University of Pisa.

The core interpreter is written in OCaml and can be run from the command line, but there's also a web version which uses the OCaml to WebAssembly compiler to run the interpreter in the browser; it is available at [https://francescoborri.github.io/mao-lang](https://francescoborri.github.io/mao-lang) and includes also a convenient trace feature to inspect the execution and the state of the program step-by-step.

## Building and running

The project uses [OCaml](https://ocaml.org/) and [Dune](https://dune.build/) as build system, and [OPAM](https://opam.ocaml.org/) to manage dependencies. You can install them on your system following the instructions on their respective websites.
Clone the repository and create a local OPAM switch with the project dependencies installed:

```bash
git clone https://github.com/francescoborri/mao-lang
cd mao-lang
opam switch create . --deps-only --with-dev-setup
eval $(opam env)
```

Build the project, run the tests and try it on a sample `.mao` program:

```bash
dune build @runtest
cat sample/fib.mao | dune exec mao_run
```

The previous command will print a limited trace of the execution of the program in the command line. You can also format a `.mao` program with the `mao_fmt` command:

```bash
cat sample/fib.mao | dune exec mao_fmt
```

This will print a formatted version of the program to the command line.

To build and run the web version, just run the script at `scripts/build_site.sh`: this will create the static site in the `_site` folder. You can then run a local web server to serve the static site, for example with Python:

```bash
python3 -m http.server -d _site 8000
```

## Future improvements

- [ ] Closures for functions and procedures.
- [ ] Syntactic sugar for convenient constructs (`else if`, `for` loops, etc.).
- [ ] Better comments handling: up to now they are discarded by the lexer, which causes the formatter to remove them from the formatted program.

## Authors

- Francesco Borri ([francescoborri](https://github.com/francescoborri))
- Tommaso Crocetti ([Tommaso-Crocetti](https://github.com/Tommaso-Crocetti))

## License

LGPL 3.0 or later.
