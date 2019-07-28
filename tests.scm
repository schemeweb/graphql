(require-library simple-tests)
(import simple-tests)
(import (srfi 1))

(import (net graphql read))
(import (net graphql write))

(define (deb s x)
  (display "deb: ") (display s) (display " ") (write x) (newline) x)

(define (lines . lines_)
  (fold (lambda (line so-far) (string-append so-far line "\n"))
        "" lines_))

(define-test (hello-world)
  (equal? (graphql->string '(query query-name field-1 field-2))
          (lines
           "{"
           "  query_name {"
           "    field_1"
           "    field_2"
           "  }"
           "}")))

(define-test (read-simple)
  (equal? (string->graphql "{ abc def ghi }")
          '((query #f abc def ghi)))
  (equal? (string->graphql "query { abc def ghi }")
          '((query #f abc def ghi)))
  (equal? (string->graphql "query foo { abc def ghi }")
          '((query foo abc def ghi)))
  (equal? (string->graphql
           "query foo @deprecated(reason: $foo) { abc { a b c } def ghi }")
          '((query (foo (@ deprecated (reason ($ foo))))
              (field abc a b c)
              def
              ghi))))

(compound-test (simple-tests)
  (hello-world)
  (read-simple))
