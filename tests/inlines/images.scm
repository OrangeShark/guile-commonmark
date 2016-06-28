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

(define-module (test-inlines images)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match)
  #:use-module (commonmark inlines)
  #:use-module (commonmark node))

(test-begin "inlines images")

(define (make-paragraph text)
  (make-node 'document #f
             (list (make-node 'paragraph #f
                              (list (make-node 'text #f (list text)))))))

(define (destination=? node-data destination)
  (equal? (assq-ref node-data 'destination) destination))

(define (title=? node-data title)
  (equal? (assq-ref node-data 'title) title))

(test-assert "parse-inlines, simple inline image"
  (match (parse-inlines (make-paragraph "![foo](/url \"title\")"))
    (('document doc-data
                ('paragraph para-data
                            ('image image-data
                                    ('text text-data "foo"))))
     (and (destination=? image-data "/url")
          (title=? image-data "title")))
    (x (pk 'fail x #f))))

(define (make-document text references)
  (node-add-data
   (make-node 'document '()
              (list (make-node 'paragraph #f
                               (list (make-node 'text #f (list text))))))
   'link-references references))


(test-assert "parse-inlines, full reference image"
  (match (parse-inlines (make-document "![foo *bar*]"
                                       '(("foo *bar*" "train.jpg" "\"train & tracks\""))))
    (('document doc-data
                ('paragraph para-data
                            ('image image-data
                                   ('emphasis em-data
                                              ('text text-data "bar"))
                                   ('text text-data "foo "))))
     (and (destination=? image-data "train.jpg")
          (title=? image-data "train & tracks")))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, image can have inline images in image description"
  (match (parse-inlines (make-paragraph "![foo ![bar](/url1)](/url2)"))
    (('document doc-data
                ('paragraph para-data
                            ('image image-data1
                                   ('image image-data2
                                              ('text text-data "bar"))
                                   ('text text-data "foo "))))
     (and (destination=? image-data1 "/url2")
          (title=? image-data1 #f)
          (destination=? image-data2 "/url1")
          (title=? image-data2 #f)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, image can have inline links in image description"
  (match (parse-inlines (make-paragraph "![foo [bar](/url1)](/url2)"))
    (('document doc-data
                ('paragraph para-data
                            ('image image-data
                                   ('link link-data
                                              ('text text-data "bar"))
                                   ('text text-data "foo "))))
     (and (destination=? image-data "/url2")
          (title=? image-data #f)
          (destination=? link-data "/url1")
          (title=? link-data #f)))
    (x (pk 'fail x #f))))

(test-end)

(exit (= (test-runner-fail-count (test-runner-current)) 0))
