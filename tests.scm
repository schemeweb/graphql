(require-library simple-tests)
(import simple-tests)
(import (srfi 1))

(import (graphql-read))
(import (graphql-write))

(define (deb x)
  (write x)
  (newline)
  x)

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
  (equal? (deb (string->graphql "query foo { abc def ghi }"))
          '((query foo abc def ghi))))

(compound-test (simple-tests)
  (hello-world)
  (read-simple))
