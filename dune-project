(lang dune 1.10)
(generate_opam_files true)

(name sedlying)
(version 0.1.0)
(source (github ifazk/sedlying))
(license MIT)
(authors "Ifaz Kabir")
(maintainers "ifazk@users.noreply.github.com")
(documentation https://ifazk.github.io/sedlying/)

(package
 (name sedlying)
 (synopsis "Sedlex buffers for marking at boundary codepoints")
 (description "Sedlex buffers which ignore mark requests at non-boundary codepoints")
 (depends
  (dune (and :build (> 1.10)))
  uutf
  bitv
  (ocaml (>= 4.02.3))))
