(import (r7rs))

(define-library (graphql-write)
  (export graphql->string)
  (import (scheme base)
          (scheme char)
          (scheme file)
          (matchable)
          (srfi 1))
  (include "graphql-write.scm"))
