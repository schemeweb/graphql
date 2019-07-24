(define ascii-digit "0123456789")
(define name-leader "_ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")
(define name-subseq (string-append name-leader ascii-digit))

(define (graphql-string->symbol string)
  (string->symbol (string-map (lambda (ch) (if (char=? ch #\_) #\- ch))
                              string)))

(define (punctuator-char? c)
  (case c
    ((#\! #\$ #\( #\) #\: #\= #\@ #\[ #\] #\{ #\| #\}) #t)
    (else #f)))

(define (read-punctuator?)
  (cond ((read-char? punctuator-char?))
        ((read-char? #\.)
         (unless (and (read-char? #\.) (read-char? #\.))
           (error "Saw dot, expected ellipsis")))
        (else #f)))

(define (skip-comment)
  (unless (or (read-char? eof-object?)
              (read-char? #\newline)
              (read-char? #\return))
    (skip-comment)))

(define (skip-whitespace-and-comments)
  (cond ((read-char? #\#)
         (skip-comment)
         (skip-whitespace-and-comments))
        ((or (read-char? #\space) (read-char? #\tab))
         (skip-whitespace-and-comments))))

(define (read-name?)
  (let ((leader (read-char? name-leader)))
    (and leader
         (graphql-string->symbol
          (string-append (string leader) (read-char* name-subseq))))))

(define (read-string?)
  (and (read-char? #\")))

(define (read-number?)
  (let ((negative? (not (not (read-char? #\-)))))
    (if (read-char? #\0)
        (if (read-char? ascii-digit)
            (error "GraphQL syntax does not permit leading zeros")
            0)
        (let ((digits (read-char* ascii-digit)))
          (cond (digits (* (if negative? -1 1) (string->number digits)))
                (negative? (error "Minus sign not followed by number"))
                (else #f))))))

(define (graphql-read-token)
  (skip-whitespace-and-comments)
  (if (eof-object? (peek-char))
      (eof-object)
      (or (read-punctuator?)
          (read-string?)
          (read-number?)
          (read-name?)
          (error "Syntax error"))))

(define (graphql-read-document)
  (let loop ((tokens '()))
    (let ((token (graphql-read-token)))
      (if (eof-object? token)
          tokens
          (loop (append tokens (list token)))))))

(define (string->graphql document-as-string)
  (parameterize ((current-input-port (open-input-string document-as-string)))
    (graphql-read-document)))
