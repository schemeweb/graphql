(import (r7rs))
(define-library (read-char-if)
  (export read-char? read-char*)
  (import (scheme base)
          (scheme char)
          (scheme file)
          (srfi 13))
  (include "read-char-if.scm"))
