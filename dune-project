(lang dune 3.20)

(name sakura-mail)

(generate_opam_files true)

(source
 (github valerius21/sakura-mail))

(authors "Valerius Mattfeld <sakuramail@valerius.me>")

(maintainers "Valerius Mattfeld <sakuramail@valerius.me>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name sakura-mail)
 (synopsis "Email API powered by mbsync")
 (description "HTTP email API that wraps mbsync for IMAP synchronization")
 (depends
  ocaml
  dream
  yojson
  ppx_deriving_yojson
  mrmime
  uuidm
  lwt
  lwt_ppx
  base64
  (alcotest :with-test)))
