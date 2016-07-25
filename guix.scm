(use-modules (guix packages)
             (guix licenses)
             (guix build-system gnu)
             (guix download)
             (gnu packages guile))

(package
  (name "guile-commonmark")
  (version "0.1")
  (source
   (origin
     (method url-fetch)
     (uri 
       (string-append "https://github.com/OrangeShark/guile-commonmark/releases/download/v"
                      version "/guile-commonmark-" version ".tar.gz"))
     (sha256
      (base32
        "12cb5fqvvgc87f5xp0ih5az305wnjia89l5jba83d0r2p8bfy0b0"))))
  (build-system gnu-build-system)
  (inputs
   `(("guile" ,guile-2.0)))
  (synopsis "CommonMark parser for GNU Guile")
  (description
   "guile-commonmark is a library for parsing CommonMark, a fully specified
variant of Markdown.")
  (home-page "https://github.com/OrangeShark/guile-commonmark")
  (license lgpl3+))
