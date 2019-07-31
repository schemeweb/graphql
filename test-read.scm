(include "net.graphql.read.import.scm")

(import (scheme process-context)
        (net graphql read))

(define (writelnln x)
  (write x)
  (newline)
  (newline))

(define (dump graphql-file-name)
  (for-each writelnln (with-input-from-file graphql-file-name read-graphql)))

(for-each dump (cdr (command-line)))
