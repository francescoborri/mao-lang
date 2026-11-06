(lang dune 3.24)

(name mao-lang)

(generate_opam_files true)

(source
 (github francescoborri/mao-lang))

(authors "Francesco Borri")

(maintainers "Francesco Borri")

(license LGPL-3.0-or-later)

(documentation https://github.com/francescoborri/mao-lang)

(using menhir 3.0)

(package
 (name mao-lang)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  ocaml
  fmt
  menhir
  js_of_ocaml
  wasm_of_ocaml-compiler
  (ocamlformat
   (and
    :with-dev-setup
    (= 0.29.0)))
  (ocaml-lsp-server :with-dev-setup)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
