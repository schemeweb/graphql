(import (net graphql client))

(define endpoint "https://api.staging.scheme.fi/graphql")

(define document '(query reports
                         id
                         full-title
                         (query documents
                                id
                                original-pdf-url
                                errata-corrected-pdf-url)))

(write (graphql-query endpoint document))
