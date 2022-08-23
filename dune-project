(lang dune 3.3)

(name byrgenwerth)

(generate_opam_files true)

(source
 (github lmdexpr/byrgenwerth))

(authors "Yuki Tajiri")

(maintainers "Yuki Tajiri <lmdexpr@gmail.com>")

(license MIT)

(package
 (name byrgenwerth)
 (synopsis "web app in a dream")
 (depends
   ocaml
   ocamlformat
   ocamlformat-rpc
   ocaml-lsp-server
   dune
   dream))
