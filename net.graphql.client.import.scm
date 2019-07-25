(import (r7rs))

(define-library (net graphql client)
  (export graphql-query)
  (import (scheme base)
          (scheme char)
          (scheme file)
          (matchable)
          (srfi 1)
          (json)
          (openssl)
          (uri-common)
          (intarweb)
          (http-client)
          (net graphql write))
  (include "graphql-client.scm"))
