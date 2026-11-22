(lang dune 3.24)

(name mao-lang)

(generate_opam_files true)

(source
 (github francescoborri/mao-lang))

(authors "Francesco Borri" "Tommaso Crocetti")

(maintainers "Francesco Borri" "Tommaso Crocetti")

(license LGPL-3.0-or-later)

(documentation https://github.com/francescoborri/mao-lang)

(using menhir 3.0)

(package
 (name mao-lang)
 (synopsis "A web interpreter for the didactic programming language MAO")
 (description "A web interpreter for the didactic programming language MAO")
 (depends
  (ocaml (>= 5.4))
  menhir
  menhirLib
  js_of_ocaml
  wasm_of_ocaml-compiler
  fmt
  (alcotest :with-test)
  (ocamlformat
   (and
    :with-dev-setup
    (= 0.29.0)))
  (ocaml-lsp-server :with-dev-setup)
  (menhir-lsp :with-dev-setup)))
