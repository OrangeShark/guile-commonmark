;; Copyright (C) 2016, 2018  Erik Edrosa <erik.edrosa@gmail.com>
;;
;; This file is part of guile-commonmark
;;
;; guile-commonmark is free software: you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.
;;
;; guile-commonmark is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with guile-commonmark.  If not, see <http://www.gnu.org/licenses/>.

(define-module (test-inlines emphasis)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match)
  #:use-module (commonmark inlines)
  #:use-module (commonmark node))

(test-begin "inlines emphasis")

(define (make-paragraph text)
  (make-node 'document #f
             (list (make-node 'paragraph #f
                              (list (make-node 'text #f (list text)))))))

(define (em? node-data)
  (eq? 'em (assq-ref node-data 'type)))

(test-assert "parse-inlines, simple emphasis"
  (match (parse-inlines (make-paragraph "*foo bar*"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data
                                       ('text text-data "foo bar"))))
     (em? emphasis-data))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, not emphasis because whitespace after *"
  (match (parse-inlines (make-paragraph "a * foo bar*"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data1 "*")
                            ('text text-data2 " foo bar")
                            ('text text-data3 "*")
                            ('text text-data4 "a ")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, not emphasis because punctuation after alphanumeric before"
  (match (parse-inlines (make-paragraph "a*\"foo\"*"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data1 "*")
                            ('text text-data2 "\"foo\"")
                            ('text text-data3 "*")
                            ('text text-data4 "a")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, not emphasis because unicode nonbreaking spaces count as whitespace"
  (match (parse-inlines (make-paragraph "*\xa0a\xa0*"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data1 "*")
                            ('text text-data2 "\xa0a\xa0")
                            ('text text-data3 "*")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis with intraword emphasis"
  (match (parse-inlines (make-paragraph "foo*bar*"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data
                                       ('text text-data "bar"))
                            ('text text-data "foo")))
     (em? emphasis-data))
    (x (pk 'fail x #f))))


(test-assert "parse-inlines, emphasis with intraword emphasis"
  (match (parse-inlines (make-paragraph "5*6*78"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data "78")
                            ('emphasis emphasis-data
                                       ('text text-data "6"))
                            ('text text-data "5")))
     (em? emphasis-data))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, simple _ emphasis"
  (match (parse-inlines (make-paragraph "_foo bar_"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data
                                       ('text text-data "foo bar"))))
     (em? emphasis-data))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, not emphasis because punctuation after alphanumeric before"
  (match (parse-inlines (make-paragraph "a_\"foo\"_"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data1 "_")
                            ('text text-data2 "\"foo\"")
                            ('text text-data3 "_")
                            ('text text-data4 "a")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis with _ is not allowed inside words"
  (match (parse-inlines (make-paragraph "foo_bar_"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data1 "_")
                            ('text text-data2 "bar")
                            ('text text-data3 "_")
                            ('text text-data4 "foo")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis with _ is not allowed inside words"
  (match (parse-inlines (make-paragraph "5_6_78"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data1 "78")
                            ('text text-data2 "_")
                            ('text text-data3 "6")
                            ('text text-data4 "_")
                            ('text text-data5 "5")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis with _ is not allowed inside words"
  (match (parse-inlines (make-paragraph "пристаням_стремятся_"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data1 "_")
                            ('text text-data2 "стремятся")
                            ('text text-data3 "_")
                            ('text text-data4 "пристаням")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis with _ is not allowed to have delimiters to be both flanking"
  (match (parse-inlines (make-paragraph "aa_\"bb\"_cc"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data1 "cc")
                            ('text text-data2 "_")
                            ('text text-data3 "\"bb\"")
                            ('text text-data4 "_")
                            ('text text-data5 "aa")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis with _ is allowed when preceded by punctuation"
  (match (parse-inlines (make-paragraph "foo-_(bar)_"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data
                                       ('text text-data1 "(bar)"))
                            ('text text-data2 "foo-")))
     (em? emphasis-data))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis delimiters must match"
  (match (parse-inlines (make-paragraph "_foo*"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data1 "*")
                            ('text text-data2 "foo")
                            ('text text-data3 "_")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis closing delimiter must not be preceded by whitespace"
  (match (parse-inlines (make-paragraph "*foo bar *"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data1 "*")
                            ('text text-data2 "foo bar ")
                            ('text text-data3 "*")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, not emphasis because second * is not right-flanking delimiter run"
  (match (parse-inlines (make-paragraph "*(*foo)"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data1 "foo)")
                            ('text text-data2 "*")
                            ('text text-data3 "(")
                            ('text text-data4 "*")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis example showing previous restriction benefits"
  (match (parse-inlines (make-paragraph "*(*foo*)*"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data1
                                       ('text text-data1 ")")
                                       ('emphasis emphasis-data2
                                                  ('text text-data2 "foo"))
                                       ('text text-data3 "("))
                           ))
     (and (em? emphasis-data1)
          (em? emphasis-data2)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis intraword emphasis with * is allowed"
  (match (parse-inlines (make-paragraph "*foo*bar"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data2 "bar")
                            ('emphasis emphasis-data
                                       ('text text-data1 "foo"))))
     (em? emphasis-data))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, not emphasis because the closing _ is preceded by whitespace"
  (match (parse-inlines (make-paragraph "_foo bar _"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data1 "_")
                            ('text text-data2 "foo bar ")
                            ('text text-data3 "_")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, not emphasis because the second _ is preceded by punctuation and
followed by an alphanumeric"
  (match (parse-inlines (make-paragraph "_(_foo)"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data1 "foo)")
                            ('text text-data2 "_")
                            ('text text-data3 "(")
                            ('text text-data4 "_")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis within emphasis"
  (match (parse-inlines (make-paragraph "_(_foo_)_"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data1
                                       ('text text-data1 ")")
                                       ('emphasis emphasis-data2
                                                  ('text text-data2 "foo"))
                                       ('text text-data3 "("))
                           ))
     (and (em? emphasis-data1)
          (em? emphasis-data2)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, intraword emphasis is disallowed for _"
  (match (parse-inlines (make-paragraph "_foo_bar"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data1 "bar")
                            ('text text-data2 "_")
                            ('text text-data3 "foo")
                            ('text text-data4 "_")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, intraword emphasis is disallowed for _"
  (match (parse-inlines (make-paragraph "_пристаням_стремятся"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data1 "стремятся")
                            ('text text-data2 "_")
                            ('text text-data3 "пристаням")
                            ('text text-data4 "_")))
     #t)
    (x (pk 'fail x #f))))


(test-assert "parse-inlines, intraword emphasis is disallowed for _"
  (match (parse-inlines (make-paragraph "_foo_bar_baz_"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data
                                       ('text text-data1 "baz")
                                       ('text text-data2 "_")
                                       ('text text-data3 "bar")
                                       ('text text-data4 "_")
                                       ('text text-data5 "foo"))))
     (em? emphasis-data))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis closing delimiter is followed by punctuation"
  (match (parse-inlines (make-paragraph "_(bar)_."))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data1 ".")
                            ('emphasis emphasis-data
                                       ('text text-data2 "(bar)"))))
     (em? emphasis-data))
    (x (pk 'fail x #f))))

(define (strong? node-data)
  (eq? 'strong (assq-ref node-data 'type)))


(test-assert "parse-inlines, emphasis rule 5"
  (match (parse-inlines (make-paragraph "**foo bar**"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data
                                       ('text text-data "foo bar"))))
     (strong? emphasis-data))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis not strong emphasis because opening delimiter
is followed by whitespace"
  (match (parse-inlines (make-paragraph "** foo bar**"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data1 "**")
                            ('text text-data2 " foo bar")
                            ('text text-data3 "**")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis not strong emphasis because opening ** is preceded
by an alphanumeric and followed by punctuation"
  (match (parse-inlines (make-paragraph "a**\"foo\"**"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data1 "**")
                            ('text text-data2 "\"foo\"")
                            ('text text-data3 "**")
                            ('text text-data4 "a") ))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis intraword strong emphasis with ** is permitted"
  (match (parse-inlines (make-paragraph "foo**bar**"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data
                                       ('text text-data1 "bar"))
                            ('text text-data2 "foo")))
     (strong? emphasis-data))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis rule 6"
  (match (parse-inlines (make-paragraph "__foo bar__"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data
                                       ('text text-data "foo bar"))))
     (strong? emphasis-data))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis not strong emphasis because opening delimiter
is followed by whitespace"
  (match (parse-inlines (make-paragraph "__ foo bar__"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data1 "__")
                            ('text text-data2 " foo bar")
                            ('text text-data3 "__")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis not strong emphasis because newline counts as whitespace"
  (match (parse-inlines (make-paragraph "__\nfoo bar__"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data1 "__")
                            ('text text-data2 "foo bar")
                            ('softbreak break-data)
                            ('text text-data3 "__")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis not strong emphasis because the opening __ is preceded
by an alphanumeric and followed by punctuation"
  (match (parse-inlines (make-paragraph "a__\"foo\"__"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data1 "__")
                            ('text text-data2 "\"foo\"")
                            ('text text-data3 "__")
                            ('text text-data4 "a")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis intraword strong emphasis is forbidden with __"
  (match (parse-inlines (make-paragraph "foo__bar__"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data1 "__")
                            ('text text-data2 "bar")
                            ('text text-data3 "__")
                            ('text text-data4 "foo")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis intraword strong emphasis is forbidden with __"
  (match (parse-inlines (make-paragraph "5__6__78"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data1 "78")
                            ('text text-data2 "__")
                            ('text text-data3 "6")
                            ('text text-data4 "__")
                            ('text text-data4 "5")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis intraword strong emphasis is forbidden with __"
  (match (parse-inlines (make-paragraph "пристаням__стремятся__"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data1 "__")
                            ('text text-data2 "стремятся")
                            ('text text-data3 "__")
                            ('text text-data4 "пристаням")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis intraword strong emphasis is forbidden with __"
  (match (parse-inlines (make-paragraph "__foo, __bar__, baz__"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data1
                                       ('text text-data1 ", baz")
                                       ('emphasis emphasis-data2
                                                  ('text text-data2 "bar"))
                                       ('text text-data3 "foo, "))))
     (and (strong? emphasis-data1)
          (strong? emphasis-data2)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis is strong emphais because it is preceded by punctuation"
  (match (parse-inlines (make-paragraph "foo-__(bar)__"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data
                                       ('text text-data1 "(bar)"))
                             ('text text-data2 "foo-")))
     (strong? emphasis-data))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis not strong emphasis because closing delimiter
is preceded by whitespace"
  (match (parse-inlines (make-paragraph "**foo bar **"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data1 "**")
                            ('text text-data2 "foo bar ")
                            ('text text-data3 "**")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis not strong emphasis because the second ** is preceded
by punctuation and followed by an alphanumeric"
  (match (parse-inlines (make-paragraph "**(**foo)"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data1 "foo)")
                            ('text text-data2 "**")
                            ('text text-data3 "(")
                            ('text text-data3 "**")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis restriction is appreciated with these examples"
  (match (parse-inlines (make-paragraph "**Gomphocarpus (*Gomphocarpus physocarpus*, syn.
*Asclepias physocarpa*)**"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data1
                                       ('text text-data1 ")")
                                       ('emphasis emphasis-data2
                                                  ('text text-data2 "Asclepias physocarpa"))
                                       ('softbreak break-data)
                                       ('text text-data3 ", syn.")
                                       ('emphasis emphasis-data3
                                                  ('text text-data4 "Gomphocarpus physocarpus"))
                                       ('text text-data5 "Gomphocarpus ("))))
     (and (strong? emphasis-data1)
          (em? emphasis-data2)
          (em? emphasis-data3)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis restriction is appreciated with these examples"
  (match (parse-inlines (make-paragraph "**foo \"*bar*\" foo**"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data1
                                       ('text text-data1 "\" foo")
                                       ('emphasis emphasis-data2
                                                  ('text text-data2 "bar"))
                                       ('text text-data3 "foo \""))))
     (and (strong? emphasis-data1)
          (em? emphasis-data2)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis intraword emphasis"
  (match (parse-inlines (make-paragraph "**foo**bar"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data2 "bar")
                            ('emphasis emphasis-data1
                                       ('text text-data1 "foo"))))
     (strong? emphasis-data1))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, not strong emphasis because the closing delimiter is preceded
by whitespace"
  (match (parse-inlines (make-paragraph "__foo bar __"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data1 "__")
                            ('text text-data2 "foo bar ")
                            ('text text-data3 "__")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, not strong emphasis because the second __ is preceded
by punctuation and followed by an alphanumeric"
  (match (parse-inlines (make-paragraph "__(__foo)"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data1 "foo)")
                            ('text text-data2 "__")
                            ('text text-data3 "(")
                            ('text text-data3 "__")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis restriction is appreciated with this examples"
  (match (parse-inlines (make-paragraph "_(__foo__)_"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data1
                                       ('text text-data1 ")")
                                       ('emphasis emphasis-data2
                                                  ('text text-data2 "foo"))
                                       ('text text-data3 "("))))
     (and (em? emphasis-data1)
          (strong? emphasis-data2)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis intraword strong emphasis is forbidden with __"
  (match (parse-inlines (make-paragraph "__foo__bar"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data1 "bar")
                            ('text text-data2 "__")
                            ('text text-data3 "foo")
                            ('text text-data4 "__")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis intraword strong emphasis is forbidden with __"
  (match (parse-inlines (make-paragraph "__пристаням__стремятся"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data1 "стремятся")
                            ('text text-data2 "__")
                            ('text text-data3 "пристаням")
                            ('text text-data4 "__")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis intraword strong emphasis is forbidden with __"
  (match (parse-inlines (make-paragraph "__foo__bar__baz__"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data
                                       ('text text-data1 "baz")
                                       ('text text-data2 "__")
                                       ('text text-data3 "bar")
                                       ('text text-data4 "__")
                                       ('text text-data5 "foo"))))
     (strong? emphasis-data))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, strong emphasis, even though the closing delimiter is both
left- and right-flanking, because it is followed by punctuation"
  (match (parse-inlines (make-paragraph "__(bar)__."))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data1 ".")
                            ('emphasis emphasis-data
                                       ('text text-data2 "(bar)"))))
     (strong? emphasis-data))
    (x (pk 'fail x #f))))

(define (destination=? node-data destination)
  (equal? (assq-ref node-data 'destination) destination))

(test-assert "parse-inlines, emphasis any nonempty sequence of inline elements can
be the contents of an emphasized span."
  (match (parse-inlines (make-paragraph "*foo [bar](/url)*"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data
                                       ('link link-data
                                              ('text text-data "bar"))
                                       ('text text-data2 "foo "))))
     (and (em? emphasis-data)
          (destination=? link-data "/url")))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis any nonempty sequence of inline elements can
be the contents of an emphasized span."
  (match (parse-inlines (make-paragraph "*foo\nbar*"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data
                                       ('text text-data "bar")
                                       ('softbreak break-data)
                                       ('text text-data "foo"))))
     (em? emphasis-data))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, in particular, emphasis and strong emphasis can be nested
inside emphasis"
  (match (parse-inlines (make-paragraph "_foo __bar__ baz_"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data1
                                       ('text text-data1 " baz")
                                       ('emphasis emphasis-data2
                                                  ('text text-data3 "bar"))
                                       ('text text-data2 "foo "))))
     (and (em? emphasis-data1)
          (strong? emphasis-data2)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, in particular, emphasis and strong emphasis can be nested
inside emphasis"
  (match (parse-inlines (make-paragraph "_foo _bar_ baz_"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data1
                                       ('text text-data1 " baz")
                                       ('emphasis emphasis-data2
                                                  ('text text-data2 "bar"))
                                       ('text text-data2 "foo "))))
     (and (em? emphasis-data1)
          (em? emphasis-data2)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, in particular, emphasis and strong emphasis can be nested
inside emphasis"
  (match (parse-inlines (make-paragraph "__foo_ bar_"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data1
                                       ('text text-data1 " bar")
                                       ('emphasis emphasis-data2
                                                  ('text text-data2 "foo")))))
     (and (em? emphasis-data1)
          (em? emphasis-data2)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, in particular, emphasis and strong emphasis can be nested
inside emphasis"
  (match (parse-inlines (make-paragraph "*foo *bar**"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data1
                                       ('emphasis emphasis-data2
                                                  ('text text-data2 "bar"))
                                       ('text text-data1 "foo "))))
     (and (em? emphasis-data1)
          (em? emphasis-data2)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, in particular, emphasis and strong emphasis can be nested
inside emphasis"
  (match (parse-inlines (make-paragraph "*foo **bar** baz*"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data1
                                       ('text text-data1 " baz")
                                       ('emphasis emphasis-data2
                                                  ('text text-data2 "bar"))
                                       ('text text-data3 "foo "))))
     (and (em? emphasis-data1)
          (strong? emphasis-data2)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, internal delimiters can close emphasis"
  (match (parse-inlines (make-paragraph "*foo**bar**baz*"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data1
                                       ('text text-data1 "baz"))
                            ('emphasis emphasis-data2
                                       ('text text-data2 "bar"))
                            ('emphasis emphasis-data3
                                       ('text text-data3 "foo"))))
     (and (em? emphasis-data1)
          (em? emphasis-data2)
          (em? emphasis-data3)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis in cases with spaces, they cannot"
  (match (parse-inlines (make-paragraph "***foo** bar*"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data1
                                       ('text text-data1 " bar")
                                       ('emphasis emphasis-data2
                                                  ('text text-data2 "foo")))))
     (and (em? emphasis-data1)
          (strong? emphasis-data2)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis in cases with spaces, they cannot"
  (match (parse-inlines (make-paragraph "*foo **bar***"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data1
                                       ('emphasis emphasis-data2
                                                  ('text text-data2 "bar"))
                                       ('text text-data1 "foo "))))
     (and (em? emphasis-data1)
          (strong? emphasis-data2)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis no strong emphasis because the opening
delimiter is closed by the first * before bar"
  (match (parse-inlines (make-paragraph "*foo**bar***"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data3 "**")
                            ('emphasis emphasis-data2
                                       ('text text-data2 "bar"))
                            ('emphasis emphasis-data1
                                       ('text text-data1 "foo"))))
     (and (em? emphasis-data1)
          (em? emphasis-data2)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis indefinite levels of nesting are possible"
  (match (parse-inlines (make-paragraph "*foo **bar *baz* bim** bop*"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data1
                                       ('text text-data1 " bop")
                                       ('emphasis emphasis-data2
                                                  ('text text-data2 " bim")
                                                  ('emphasis emphasis-data3
                                                             ('text text-data4 "baz"))
                                                  ('text text-data5 "bar "))
                                       ('text text-data6 "foo "))))
     (and (em? emphasis-data1)
          (strong? emphasis-data2)
          (em? emphasis-data3)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis indefinite levels of nesting are possible"
  (match (parse-inlines (make-paragraph "*foo [*bar*](/url)*"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data1
                                       ('link link-data
                                              ('emphasis emphasis-data2
                                                         ('text text-data5 "bar")))
                                       ('text text-data6 "foo "))))
     (and (em? emphasis-data1)
          (em? emphasis-data2)
          (destination=? link-data "/url")))
    (x (pk 'fail x #f))))


(test-assert "parse-inlines, there can be no empty emphasis or strong emphasis"
  (match (parse-inlines (make-paragraph "** is not an empty emphasis"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data1 " is not an empty emphasis")
                            ('text text-data2 "**")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, there can be no empty emphasis or strong emphasis"
  (match (parse-inlines (make-paragraph "**** is not an empty strong emphasis"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data1 " is not an empty strong emphasis")
                            ('text text-data2 "****")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, any nonempty sequence of inline elements can be the
contents of a strongly emphasized span"
  (match (parse-inlines (make-paragraph "**foo [bar](/url)**"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data1
                                       ('link link-data
                                              ('text text-data "bar"))
                                       ('text text-data "foo "))))
     (and (strong? emphasis-data1)
          (destination=? link-data "/url")))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, any nonempty sequence of inline elements can be the
contents of a strongly emphasized span"
  (match (parse-inlines (make-paragraph "**foo\nbar**"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data
                                       ('text text-data "bar")
                                       ('softbreak break-data)
                                       ('text text-data "foo"))))
     (strong? emphasis-data))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis and strong emphasis can be nested inside strong
emphasis"
  (match (parse-inlines (make-paragraph "__foo _bar_ baz__"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data1
                                       ('text text-data1 " baz")
                                       ('emphasis emphasis-data2
                                                  ('text text-data2 "bar"))
                                       ('text text-data3 "foo "))))
     (and (strong? emphasis-data1)
          (em? emphasis-data2)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis and strong emphasis can be nested inside strong
emphasis"
  (match (parse-inlines (make-paragraph "__foo __bar__ baz__"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data1
                                       ('text text-data1 " baz")
                                       ('emphasis emphasis-data2
                                                  ('text text-data2 "bar"))
                                       ('text text-data3 "foo "))))
     (and (strong? emphasis-data1)
          (strong? emphasis-data2)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis and strong emphasis can be nested inside strong
emphasis"
  (match (parse-inlines (make-paragraph "____foo__ bar__"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data1
                                       ('text text-data1 " bar")
                                       ('emphasis emphasis-data2
                                                  ('text text-data2 "foo")))))
     (and (strong? emphasis-data1)
          (strong? emphasis-data2)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis and strong emphasis can be nested inside strong
emphasis"
  (match (parse-inlines (make-paragraph "**foo **bar****"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data1
                                       ('emphasis emphasis-data2
                                                  ('text text-data2 "bar"))
                                       ('text text-data1 "foo "))))
     (and (strong? emphasis-data1)
          (strong? emphasis-data2)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis and strong emphasis can be nested inside strong
emphasis"
  (match (parse-inlines (make-paragraph "**foo *bar* baz**"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data1
                                       ('text text-data1 " baz")
                                       ('emphasis emphasis-data2
                                                  ('text text-data2 "bar"))
                                       ('text text-data3 "foo "))))
     (and (strong? emphasis-data1)
          (em? emphasis-data2)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, internal delimiters can close emphasis"
  (match (parse-inlines (make-paragraph "**foo*bar*baz**"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data "**")
                            ('text text-data1 "baz")
                            ('emphasis emphasis-data1
                                       ('text text-data2 "bar")
                                       ('emphasis emphasis-data2
                                                  ('text text-data3 "foo")))))
     (and (em? emphasis-data1)
          (em? emphasis-data2)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, delimiters with spaces cannot close emphasis"
  (match (parse-inlines (make-paragraph "***foo* bar**"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data1
                                       ('text text-data1 " bar")
                                       ('emphasis emphasis-data2
                                                  ('text text-data2 "foo")))))
     (and (strong? emphasis-data1)
          (em? emphasis-data2)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, delimiters with spaces cannot close emphasis"
  (match (parse-inlines (make-paragraph "**foo *bar***"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data1
                                       ('emphasis emphasis-data2
                                                  ('text text-data1 "bar"))
                                       ('text text-data2 "foo "))))
     (and (strong? emphasis-data1)
          (em? emphasis-data2)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis indefinite levels of nesting are possible"
  (match (parse-inlines (make-paragraph "**foo *bar **baz**\nbim* bop**"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data1
                                       ('text text-data5 " bop")
                                       ('emphasis emphasis-data2
                                                  ('text text-data4 "bim")
                                                  ('softbreak break-data)
                                                  ('emphasis emphasis-data3
                                                             ('text text-data3 "baz"))
                                                  ('text text-data1 "bar "))
                                       ('text text-data2 "foo "))))
     (and (strong? emphasis-data1)
          (em? emphasis-data2)
          (strong? emphasis-data3)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis indefinite levels of nesting are possible"
  (match (parse-inlines (make-paragraph "**foo [*bar*](/url)**"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data1
                                       ('link link-data
                                              ('emphasis emphasis-data2
                                                         ('text text-data1 "bar")))
                                       ('text text-data2 "foo "))))
     (and (strong? emphasis-data1)
          (em? emphasis-data2)
          (destination=? link-data "/url")))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, there can be no empty emphasis or strong emphasis"
  (match (parse-inlines (make-paragraph "__ is not an empty emphasis"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data1 " is not an empty emphasis")
                            ('text text-data2 "__")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, there can be no empty emphasis or strong emphasis"
  (match (parse-inlines (make-paragraph "____ is not an empty strong emphasis"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data1 " is not an empty strong emphasis")
                            ('text text-data2 "____")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis rule 11"
  (match (parse-inlines (make-paragraph "foo ***"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data1 "***")
                            ('text text-data2 "foo ")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis rule 11"
  (match (parse-inlines (make-paragraph "foo *\\**"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data
                                       ('text text-data1 "*"))
                            ('text text-data2 "foo ")))
     (em? emphasis-data))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis rule 11"
  (match (parse-inlines (make-paragraph "foo *_*"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data
                                       ('text text-data1 "_"))
                            ('text text-data2 "foo ")))
     (em? emphasis-data))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis rule 11"
  (match (parse-inlines (make-paragraph "foo *****"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data1 "*****")
                            ('text text-data2 "foo ")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis rule 11"
  (match (parse-inlines (make-paragraph "foo **\\***"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data
                                       ('text text-data1 "*"))
                            ('text text-data2 "foo ")))
     (strong? emphasis-data))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis rule 11"
  (match (parse-inlines (make-paragraph "foo **_**"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data
                                       ('text text-data1 "_"))
                            ('text text-data2 "foo ")))
     (strong? emphasis-data))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis rule 11 excess literal * characters
will appear outside of the emphasis"
  (match (parse-inlines (make-paragraph "**foo*"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data
                                       ('text text-data1 "foo"))
                            ('text text-data2 "*")))
     (em? emphasis-data))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis rule 11 excess literal * characters
will appear outside of the emphasis"
  (match (parse-inlines (make-paragraph "*foo**"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data2 "*")
                            ('emphasis emphasis-data
                                       ('text text-data1 "foo"))))
     (em? emphasis-data))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis rule 11 excess literal * characters
will appear outside of the emphasis"
  (match (parse-inlines (make-paragraph "***foo**"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data
                                       ('text text-data1 "foo"))
                            ('text text-data2 "*")))
     (strong? emphasis-data))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis rule 11 excess literal * characters
will appear outside of the emphasis"
  (match (parse-inlines (make-paragraph "****foo*"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data
                                       ('text text-data1 "foo"))
                            ('text text-data2 "***")))
     (em? emphasis-data))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis rule 11 excess literal * characters
will appear outside of the emphasis"
  (match (parse-inlines (make-paragraph "**foo***"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data2 "*")
                            ('emphasis emphasis-data
                                       ('text text-data1 "foo"))))
     (strong? emphasis-data))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis rule 11 excess literal * characters
will appear outside of the emphasis"
  (match (parse-inlines (make-paragraph "*foo****"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data2 "***")
                            ('emphasis emphasis-data
                                       ('text text-data1 "foo"))))
     (em? emphasis-data))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis rule 12"
  (match (parse-inlines (make-paragraph "foo ___"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data1 "___")
                            ('text text-data2 "foo ")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis rule 12"
  (match (parse-inlines (make-paragraph "foo _\\__"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data
                                       ('text text-data1 "_"))
                            ('text text-data2 "foo ")))
     (em? emphasis-data))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis rule 12"
  (match (parse-inlines (make-paragraph "foo _*_"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data
                                       ('text text-data1 "*"))
                            ('text text-data2 "foo ")))
     (em? emphasis-data))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis rule 12"
  (match (parse-inlines (make-paragraph "foo _____"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data1 "_____")
                            ('text text-data2 "foo ")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis rule 12"
  (match (parse-inlines (make-paragraph "foo __\\___"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data
                                       ('text text-data1 "_"))
                            ('text text-data2 "foo ")))
     (strong? emphasis-data))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis rule 12"
  (match (parse-inlines (make-paragraph "foo __*__"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data
                                       ('text text-data1 "*"))
                            ('text text-data2 "foo ")))
     (strong? emphasis-data))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis rule 12 excess literal _ characters
will appear outside of the emphasis"
  (match (parse-inlines (make-paragraph "__foo_"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data
                                       ('text text-data1 "foo"))
                            ('text text-data2 "_")))
     (em? emphasis-data))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis rule 12 excess literal _ characters
will appear outside of the emphasis"
  (match (parse-inlines (make-paragraph "_foo__"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data2 "_")
                            ('emphasis emphasis-data
                                       ('text text-data1 "foo"))))
     (em? emphasis-data))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis rule 12 excess literal _ characters
will appear outside of the emphasis"
  (match (parse-inlines (make-paragraph "___foo__"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data
                                       ('text text-data1 "foo"))
                            ('text text-data2 "_")))
     (strong? emphasis-data))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis rule 12 excess literal _ characters
will appear outside of the emphasis"
  (match (parse-inlines (make-paragraph "____foo_"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data
                                       ('text text-data1 "foo"))
                            ('text text-data2 "___")))
     (em? emphasis-data))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis rule 12 excess literal _ characters
will appear outside of the emphasis"
  (match (parse-inlines (make-paragraph "__foo___"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data2 "_")
                            ('emphasis emphasis-data
                                       ('text text-data1 "foo"))))
     (strong? emphasis-data))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis rule 12 excess literal _ characters
will appear outside of the emphasis"
  (match (parse-inlines (make-paragraph "_foo____"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data2 "___")
                            ('emphasis emphasis-data
                                       ('text text-data1 "foo"))))
     (em? emphasis-data))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis rule 13 can be applied to arbitrarily
long sequences of delimiters"
  (match (parse-inlines (make-paragraph "******foo******"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data1
                                       ('emphasis emphasis-data2
                                                  ('emphasis emphasis-data3
                                                             ('text text-data1 "foo"))))))
     (and (strong? emphasis-data1)
          (strong? emphasis-data2)
          (strong? emphasis-data3)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis rule 14"
  (match (parse-inlines (make-paragraph "***foo***"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data1
                                       ('emphasis emphasis-data2
                                                  ('text text-data1 "foo")))))
     (and (strong? emphasis-data1)
          (em? emphasis-data2)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis rule 14"
  (match (parse-inlines (make-paragraph "_____foo_____"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data1
                                       ('emphasis emphasis-data2
                                                  ('emphasis emphasis-data3
                                                             ('text text-data1 "foo"))))))
     (and (strong? emphasis-data1)
          (strong? emphasis-data2)
          (em? emphasis-data3)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis rule 15"
  (match (parse-inlines (make-paragraph "*foo _bar* baz_"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data1 "_")
                            ('text text-data2 " baz")
                            ('emphasis emphasis-data1
                                       ('text text-data3 "bar")
                                       ('text text-data4 "_")
                                       ('text text-data5 "foo "))))
     (em? emphasis-data1))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis rule 15"
  (match (parse-inlines (make-paragraph "**foo*bar**"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data1 "*")
                            ('emphasis emphasis-data1
                                       ('text text-data2 "bar")
                                       ('emphasis emphasis-data2
                                                  ('text text-data3 "foo")))))
     (and (em? emphasis-data1)
          (em? emphasis-data2)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis rule 15"
  (match (parse-inlines (make-paragraph "*foo __bar *baz bim__ bam*"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data1
                                       ('text text-data1 " bam")
                                       ('emphasis emphasis-data2
                                                  ('text text-data3 "baz bim")
                                                  ('text text-data4 "*")
                                                  ('text text-data5 "bar "))
                                       ('text text-data2 "foo "))))
     (and (em? emphasis-data1)
          (strong? emphasis-data2)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis rule 16"
  (match (parse-inlines (make-paragraph "**foo **bar baz**"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data1
                                       ('text text-data1 "bar baz"))
                            ('text text-data2 "foo ")
                            ('text text-data3 "**")))
     (strong? emphasis-data1))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis rule 16"
  (match (parse-inlines (make-paragraph "*foo *bar baz*"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data1
                                       ('text text-data1 "bar baz"))
                            ('text text-data2 "foo ")
                            ('text text-data3 "*")))
     (em? emphasis-data1))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis rule 17"
  (match (parse-inlines (make-paragraph "*[bar*](/url)"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                   ('text text-data "*")
                                   ('text text-data2 "bar"))
                            ('text text-data "*")))
     (destination=? link-data "/url"))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis rule 17"
  (match (parse-inlines (make-paragraph "_foo [bar_](/url)"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                   ('text text-data "_")
                                   ('text text-data2 "bar"))
                            ('text text-data "foo ")
                            ('text text-data "_")))
     (destination=? link-data "/url"))
    (x (pk 'fail x #f))))

(test-expect-fail 3)
(test-assert "parse-inlines, emphasis rule 17"
  (match (parse-inlines (make-paragraph "*<img src=\"foo\" title=\"*\"/>"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data1
                                       ('text text-data1 "bar baz"))
                            ('text text-data2 "foo ")
                            ('text text-data3 "*")))
     (em? emphasis-data1))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis rule 17"
  (match (parse-inlines (make-paragraph "**<a href=\"**\">"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data1
                                       ('text text-data1 "bar baz"))
                            ('text text-data2 "foo ")
                            ('text text-data3 "*")))
     (em? emphasis-data1))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis rule 17"
  (match (parse-inlines (make-paragraph "__<a href=\"__\">"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data1
                                       ('text text-data1 "bar baz"))
                            ('text text-data2 "foo ")
                            ('text text-data3 "*")))
     (em? emphasis-data1))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis rule 17"
  (match (parse-inlines (make-paragraph "*a `*`*"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data1
                                       ('code-span code-data1 "*")
                                       ('text text-data1 "a "))))
     (em? emphasis-data1))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis rule 17"
  (match (parse-inlines (make-paragraph "_a `_`_"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data1
                                       ('code-span code-data1 "_")
                                       ('text text-data1 "a "))))
     (em? emphasis-data1))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis rule 17"
  (match (parse-inlines (make-paragraph "**a<http://foo.bar/?q**>"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                   ('text text-data1 "http://foo.bar/?q**"))
                            ('text text-data "a")
                            ('text text-data "**")))
     (destination=? link-data "http://foo.bar/?q**"))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, emphasis rule 17"
  (match (parse-inlines (make-paragraph "__a<http://foo.bar/?q=__>"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                   ('text text-data1 "http://foo.bar/?q=__"))
                            ('text text-data "a")
                            ('text text-data "__")))
     (destination=? link-data "http://foo.bar/?q=__"))
    (x (pk 'fail x #f))))

(test-end)
