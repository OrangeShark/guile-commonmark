
(define-module (test-node)
  #:use-module (srfi srfi-64)
  #:use-module (commonmark node))

(test-begin "node")

(test-assert "no-children, true when no children"
             (no-children? (make-document-node)))

(test-end)

(exit (= (test-runner-fail-count (test-runner-current)) 0))
