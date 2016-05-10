;; Copyright (C) 2016  Erik Edrosa <erik.edrosa@gmail.com>
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

(test-assert "parse-inlines, emphasis not strong emphasis because newline counts as whitespace 
is followed by whitespace"
  (match (parse-inlines (make-paragraph "__\nfoo bar__"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data1 "__")
                            ('text text-data2 "\nfoo bar")
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
                                       ('text text-data3 ", syn.\n")
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

(test-end)

(exit (= (test-runner-fail-count (test-runner-current)) 0))
