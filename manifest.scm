(use-modules 
 (srfi srfi-1)
 (guix packages)
 (guix gexp)
 (guix build utils)
 (gnu packages commencement)
 ((guix licenses) #:prefix l:)
 (guix build-system trivial))

(define cc
  (package
    (name "cc")
    (version (package-version gcc-toolchain))
    (source #f)
    (build-system trivial-build-system)
    (inputs (list gcc-toolchain))
    (arguments
     (list
      #:builder
      #~(begin
          (mkdir #$output)
          (chdir #$output)
          (mkdir "bin")
          (symlink (string-append #$gcc-toolchain "/bin/gcc")
                   "bin/cc"))))
    (home-page "just give me a cc")
    (synopsis "cc, for if you need to run non-portable build scripts you can't edit")
    (description "see synopsis")
    (license l:public-domain) ;; literally just a symlink
    ))

(packages->manifest
 (xcons (map specification->package
             '("gcc-toolchain" "autoconf" "rlwrap"
               "gmp"))
        ;; for zarith
        cc))
