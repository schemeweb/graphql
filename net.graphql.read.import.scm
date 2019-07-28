(import (r7rs))

(define-library (net graphql read)
  (export read-graphql
          read-graphql-tokens
          string->graphql)
  (import (scheme base)
          (scheme char)
          (scheme file)
          (scheme write)
          (matchable)
          (srfi 1)
          (read-char-if)
          (packrat))
  (include "graphql-read.scm"))
