(use-modules (guix packages)
             (guix licenses)
             (guix build-system gnu)
             (guix git-download)
             (gnu packages guile)
             (gnu packages autotools)
             (gnu packages pkg-config)
             (gnu packages texinfo))

(package
  (name "guile-commonmark")
  (version "6a1f15f")
  (source
   (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/OrangeShark/guile-commonmark.git")
           (commit version)))
     (sha256
      (base32
        "04ms67pphlnk8fm1n5p7nkkz4hcnhyqdkrcg3fb1m6s2ynmca780"))))
  (build-system gnu-build-system)
  (arguments
   '(#:phases
     (modify-phases %standard-phases
       (add-after 'unpack 'bootstrap
         (lambda _ (zero? (system* "sh" "bootstrap")))))))
  (native-inputs
   `(("autoconf" ,autoconf)
     ("automake" ,automake)
     ("texinfo" ,texinfo)
     ("pkg-config" ,pkg-config)))
  (inputs
   `(("guile" ,guile-2.2)))
  (synopsis "CommonMark parser for Guile")
  (description
   "guile-commonmark is a library for parsing CommonMark, a fully specified
+variant of Markdown.  The library is written in Guile Scheme and is designed
+to transform a CommonMark document to SXML.  guile-commonmark tries to closely
+follow the @uref{http://commonmark.org/, CommonMark spec}, the main difference
+is no support for parsing block and inline level HTML.")
  (home-page "https://github.com/OrangeShark/guile-commonmark")
  (license lgpl3+))
