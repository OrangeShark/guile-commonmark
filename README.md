guile-commonmark
================
guile-commonmark is a library for parsing [CommonMark](http://commonmark.org/),
a fully specified variant of Markdown.


Example
-------
```scheme
(use-modules (commonmark)
             (sxml simple))

(define doc
  "A CommonMark document
=============

1. here is a list
2. with another item

    this is some code

A regular paragraph"

;; Parses the CommonMark.
(define doc-sxml (commonmark->sxml doc))

;; Writes to the current output port
(sxml->xml doc-sxml)
```

Requirements
------------

 - [GNU Guile](https://www.gnu.org/software/guile/) >= 2.0.11
 - [pkg-config](http://www.freedesktop.org/wiki/Software/pkg-config)

Installation
------------

Download the latest tarball and run:
```sh
./configure --prefix=<guile-prefix>
make
sudo make install
```

To build from git requires Autoconf and Automake.
```sh
./bootstrap
./configure
make
make install
```

License
-------
LGPLv3 or later. See COPYING.LESSER and COPYING
