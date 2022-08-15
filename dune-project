(lang dune 3.4)

(name fetch)

(generate_opam_files true)

(source
 (github mirryi/fetch))

(authors "Eric Zhao <21zhaoe@protonmail.com>")
(maintainers "Eric Zhao <21zhaoe@protonmail.com>")
(license MIT)

(package
 (name fetch)
 (synopsis "Query-based compiler architecture library inspired by Rock")
 (depends ocaml dune dmap (odoc :with-doc)))
