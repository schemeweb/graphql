(define (deb s x)
  (display "deb: ") (display s) (display " ") (write x) (newline) x)

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
  (let ((punct (read-char? punctuator-char?)))
    (cond (punct (cons (string->symbol (string punct)) #f))
          ((read-char? #\.)
           (if (and (read-char? #\.) (read-char? #\.))
               '|...|
               (error "Saw dot, expected ellipsis")))
          (else #f))))

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
         (cons 'name
               (graphql-string->symbol
                (string-append (string leader) (read-char* name-subseq)))))))

(define (read-string-char)
  (cond ((or (read-char? eof-object?)
             (read-char? #\newline)
             (read-char? #\return))
         (error "Newline inside quoted string"))
        ((read-char? #\\)
         (or (and (read-char #\u)
                  (let ((char (read-char "bfnrt\"\\/")))
                    (case char
                      ((#\b) #\x08)
                      ((#\f) #\x0c)
                      ((#\n) #\newline)
                      ((#\r) #\return)
                      ((#\t) #\tab)
                      (else char))))))
        (else (read-char? (lambda (_) #t)))))

(define (read-double-quoted-string)
  (let loop ((chars '()))
    (if (read-char? #\")
        (list->string chars)
        (loop (append chars (list (read-string-char)))))))

(define (read-triple-quoted-string)
  (let loop ((chars '()))
    (if (read-char? #\")
        (if (read-char? #\")
            (if (read-char? #\")
                (list->string chars)
                (loop (append chars '(#\" #\"))))
            (loop (append chars '(#\"))))
        (loop (append chars (list (read-string-char)))))))

(define (read-string?)
  (and (read-char? #\")
       (cons 'string
             (if (read-char? #\")
                 (if (read-char? #\")
                     (read-triple-quoted-string)
                     "")
                 (read-double-quoted-string)))))

(define (read-number?)
  (let ((negative? (not (not (read-char? #\-)))))
    (if (read-char? #\0)
        (if (read-char? ascii-digit)
            (error "GraphQL syntax does not permit leading zeros")
            (cons 'nat 0))
        (let ((digits (read-char* ascii-digit)))
          (cond ((and digits negative?)
                 (cons 'neg (- (string->number digits))))
                (digits
                 (cons 'nat (string->number digits)))
                (negative? (error "Minus sign not followed by number"))
                (else #f))))))

(define (read-token)
  (skip-whitespace-and-comments)
  (if (eof-object? (peek-char))
      (eof-object)
      (or (read-punctuator?)
          (read-string?)
          (read-number?)
          (read-name?)
          (error "Syntax error"))))

(define (token-generator)
  (lambda ()
    (let ((token (read-token)))
      (if (eof-object? token)
          (values #f #f)
          (values #f token)))))

(define (one-of-the-symbols symbols)
  (lambda (results)
    (let ((token (parse-results-token-value results)))
      (if (memv token symbols)
          (make-result token (parse-results-next results))
          (make-expected-result (parse-results-position results) token)))))

(define parse-graphql-document
  (packrat-parser

   (begin

     (define operation-type
       (one-of-the-symbols '(query mutation subscription)))

     document)

   (document
    ((list <- definition-list*) list))

   (definition-list*
     ((list <- definition-list+) list)
     (() '()))
   (definition-list+
     ((first <- definition rest <- definition-list*) (cons first rest)))
   (definition
     ((a <- operation-definition) a)
     ((a <- fragment-definition) a)
     ;;((a <- type-system-definition) a)
     ;;((a <- type-system-extension) a)
     )

   (operation-definition
    ((operation-type <- operation-type
                     name <- name?
                     ;;variables <- variable-definitions?
                     ;;directives <- directives?
                     selection-set <- selection-set)
     `(,operation-type ,name ,@selection-set))
    ((selection-set <- selection-set)
     `(query #f ,@selection-set)))

   (variable-definitions?
    (('|(| list <- variable-definition-list+ '|)|) list)
    (() '()))
   (variable-definition-list*
    ((list <- variable-definition-list+) list)
    (() '()))
   (variable-definition-list+
    ((first <- variable-definition rest <- variable-definition-list*)
     (cons first rest)))
   (variable-definition
    ((name <- variable-name '|:| type <- type default <- value?)
     (list name type default)))
   (type
    ((ty <- non-null-type) ty)
    ((ty <- list-type) ty)
    ((ty <- 'name) ty))
   (non-null-type
    ((ty <- list-type '|!|) `(non-null ,ty))
    ((ty <- 'name     '|!|) `(non-null ,ty)))
   (list-type
    (('|[| ty <- type '|]|) `(list ,ty)))

   (fragment-definition
    (('|fragment| fragment-name #|type-condition directives?|# selection-set)
     `(fragment ,fragment-name ,selection-set)))
   (fragment-name
    (('|on|) (error ""))
    ((name <- 'name) name))

   #|
   (type-system-definition)

   (type-system-extension)
   |#

   (selection-set
    (('|{| list <- selection-list+ '|}|) list))
   (selection-list+
    ((first <- selection rest <- selection-list*) (cons first rest)))
   (selection-list*
    ((list <- selection-list+) list)
    (() '()))
   (selection
    ((a <- field) a)
    ;;fragment-spread
    ;;inline-fragment
    )
   (field
    ((alias <- alias?  name <- 'name  arguments <- arguments)
     (let ((x (if (null? arguments) name (list name arguments))))
       (if alias `(alias ,alias ,x) x))))
   (alias?
    ((name <- 'name '|:|)  name)
    (()                    #f))
   (arguments
    (('|(| args <- name-value-pair-list+ '|)|)  args)
    (()                                         '()))
   (name-value-pair-list+
    ((first <- name-value-pair rest <- name-value-pair-list*)
     (cons first rest)))
   (name-value-pair-list*
    ((list <- name-value-pair-list+)  list)
    (()                               '()))
   (name-value-pair
    ((name <- 'name  '|:|  value <- value)  (cons name value)))
   (value?
    ((v <- value) v)
    (() #f))
   (value
    ((name <- variable-name) `(variable ,name))
    ((v <- 'integer) v)
    ((v <- 'float) v)
    ((v <- 'string) v)
    ((v <- 'true) #t)
    ((v <- 'false) #f)
    ((v <- 'null) 'null)
    ((v <- 'name) (cons 'enum v))
    ((v <- list-value) v)
    ((v <- object-value) v))
   (value-list*
    ((first <- value rest <- value-list*) (cons first rest))
    (()                                   '()))
   (list-value
    (('|[| v <- value-list* '|]|) (list->vector v)))
   (object-value
    (('|{| pairs <- name-value-pair-list* '|}|) (cons 'object pairs)))
   (variable-name
    (('|$| name <- 'name) name))

   (name?
    ((name <- 'name) name)
    (() #f))))

(define (string->graphql document-as-string)
  (parameterize ((current-input-port (open-input-string document-as-string)))
    (let ((result (parse-graphql-document
                   (base-generator->results (token-generator)))))
      (if (parse-result-successful? result)
          (parse-result-semantic-value result)
          (error "generate")))))
