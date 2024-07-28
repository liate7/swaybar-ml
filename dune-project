(lang dune 3.15)

(name swaybar_ml)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")


(documentation https://url/to/documentation)

(package
 (name swaybar_ml)
 (synopsis "A short synopsis")
 (license MIT)
 (description "A longer description")
 (depends ocaml dune
          eio eio_linux eio_main lwt_eio
          containers cmdliner yojson
          obus timedesc cohttp cohttp-eio
          ppx_inline_test ppx_expect ppx_string ppx_compare ppx_yojson_conv
          tls-eio mirage-crypto-rng-eio
          (ocamlformat :dev) (merlin :dev) (utop :dev) (ocaml-print-intf :dev))
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
