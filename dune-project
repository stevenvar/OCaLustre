(lang dune 3.0)
(name ocalustre)
(authors "Steven Varoumas")
(maintainers "Steven Varoumas")
(source (github stevenvar/OCaLustre))

(package
 (name ocalustre)
 (synopsis "Lustre-like synchronous extension of OCaml")
 (depends ppx_tools ocaml-compiler-libs ppxfind))

(generate_opam_files true)