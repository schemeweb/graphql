(define (graphql-symbol->string symbol)
  (string-map (lambda (ch) (if (char=? ch #\-) #\_ ch))
              (symbol->string symbol)))

(define (graphql->string-inner document indent)
  (let ((ind (make-string (* 2 indent) #\space)))
    (match document
      ((query name . body)
       (string-append
        ind (graphql-symbol->string name) " {\n"
        (fold (lambda (body-part so-far)
                (string-append
                 so-far
                 (graphql->string-inner body-part (+ indent 1))))
              "" body)
        ind "}\n"))
      (name
       (string-append ind (graphql-symbol->string name) "\n")))))

(define (graphql->string document)
  (match document
    ((query name . body)
     (string-append "{\n" (graphql->string-inner document 1) "}\n"))))
