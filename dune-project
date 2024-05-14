(lang dune 3.8)

(using melange 0.1)

(generate_opam_files)

(name graphql_ppx)

(source
 (github mlms13/bs-decode))

(license MIT)

(maintainers
 "Jaap Frolich <jfrolich@gmail.com>, Tomasz Cichocinski <tomaszcichocinski@gmail.com>")

(authors
 "Jaap Frolich <jfrolich@gmail.com>, Tomasz Cichocinski <tomaszcichocinski@gmail.com>")

(homepage https://github.com/reasonml-community/graphql-ppx)

(bug_reports https://github.com/reasonml-community/graphql-ppx/issues)

(package
 (allow_empty)
 (name graphql_ppx)
 (synopsis "GraphQL PPX rewriter for ReScript/ReasonML")
 (description
  "GraphQL language primitives for ReScript/ReasonML written in ReasonML")
 (depends
  alcotest
  (ppxlib
   (>= 0.21.0))
  yojson
  dot-merlin-reader
  (melange
   (and
    (>= 3.0.0)
    (< 4.0.0)))
  (ocaml
   (>= 5.1.0))
  (reason
   (>= 3.10.0))
  ocaml-lsp-server
  ocamlformat
  opam-check-npm-deps
  result))
