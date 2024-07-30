(lang dune 3.15)

(name swaybar_ml)

(generate_opam_files true)

; (source
;  (github username/reponame))

(authors "Andrew Patterson")

(package
 (name swaybar_ml)
 (synopsis "A library for writing a swaybar status_command")
 (license MIT)
 (depends ocaml dune
          eio eio_linux eio_main lwt_eio
          containers cmdliner yojson
          stringext
          obus timedesc cohttp cohttp-eio
          ppx_inline_test ppx_expect ppx_string ppx_compare ppx_yojson_conv
          tls-eio mirage-crypto-rng-eio
          (ocamlformat :dev) (merlin :dev) (utop :dev) (ocaml-print-intf :dev))
 ; (tags
 ;  (topics "to describe" your project))
)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
