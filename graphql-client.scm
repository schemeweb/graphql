(define (graphql-query endpoint document)
  (let* ((doc (graphql->string document))
         (req (make-request
               method: 'POST
               uri: (uri-reference endpoint)
               headers: (headers '((content-type application/graphql))))))
    (with-input-from-request req doc json-read)))
