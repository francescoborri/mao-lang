# mao-lang

Interpreter for a simple, imperative didactical programming language.

## Building and running

Clone the repository and create a local OPAM switch with the project dependencies:

```bash
git clone https://github.com/francescoborri/mao-lang
cd mao-lang
opam switch create . --deps-only --with-dev-setup
eval $(opam env)
```

Build the project and run it on a sample *mao* program:

```bash
dune build
cat sample/dummy.mao | dune exec mao
```

To build and run the web version, run:

```bash
./scripts/build_site.sh
python3 -m http.server -d _site 8000
```

The web UI will be available at `http://localhost:8000`.

## License

LGPL 3.0 or later.
