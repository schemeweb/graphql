(define (deb s x)
  (display "deb: ") (display s) (display " ") (write x) (newline) x)

;;;; Lexer

(define (ascii-digit c)
  (let ((cc (char->integer c)))
    (<= #x30 cc #x39)))

(define (name-leader c)
  (let ((cc (char->integer c)))
    (or (<= #x41 cc #x5a)  ; A..Z
        (<= #x61 cc #x7a)  ; a..z
        (= #x5f cc))))     ; _

(define (name-subseq c)
  (let ((cc (char->integer c)))
    (or (<= #x41 cc #x5a)     ; A..Z
        (<= #x61 cc #x7a)     ; a..z
        (= #x5f cc)           ; _
        (<= #x30 cc #x39))))  ; 0..9

(define (graphql-string->symbol string)
  (string->symbol (string-map (lambda (ch) (if (char=? ch #\_) #\- ch))
                              string)))

(define (punctuator-char? c)
  (case c
    ((#\! #\$ #\( #\) #\: #\= #\@ #\[ #\] #\{ #\| #\}) #t)
    ((#\&) #t)  ;; TODO: not classified as a punctuator in the spec
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
        ((read-char? #\,)
         (skip-whitespace-and-comments))
        ((or (read-char? #\space)
             (read-char? #\tab)
             (read-char? #\newline)
             (read-char? #\return))
         (skip-whitespace-and-comments))))

(define (read-name?)
  (let ((leader (read-char? name-leader)))
    (and leader
         (cons 'name
               (graphql-string->symbol
                (string-append (string leader)
                               (or (read-char* name-subseq) "")))))))

(define (read-string-char allow-newline?)
  (cond ((read-char? eof-object?)
         (error "End of stream inside quoted string"))
        ((and (not allow-newline?)
              (or (read-char? #\newline)
                  (read-char? #\return)))
         (error "Newline inside quoted string"))
        ((read-char? #\\)
         (or (and (read-char? #\u)
                  (let ((char (read-char? "bfnrt\"\\/")))
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
        (loop (append chars (list (read-string-char #f)))))))

(define (read-triple-quoted-string)
  (let loop ((chars '()))
    (if (read-char? #\")
        (if (read-char? #\")
            (if (read-char? #\")
                (list->string chars)
                (loop (append chars '(#\" #\"))))
            (loop (append chars '(#\"))))
        (loop (append chars (list (read-string-char #t)))))))

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
          (error
           (string-append "Syntax error: "
                          (number->string (char->integer (peek-char))))))))

(define (token-generator)
  (lambda ()
    (let ((token (read-token)))
      (if (eof-object? token)
          (values #f #f)
          (values #f token)))))

(define (read-graphql-tokens)
  (let loop ((tokens '()))
    (let ((token (read-token)))
      (if (eof-object? token)
          tokens
          (loop (append tokens (list token)))))))

(define (one-of-the-tokens symbols)
  (lambda (results)
    (let ((token (parse-results-token-value results)))
      (if (memv token symbols)
          (make-result token (parse-results-next results))
          (make-expected-result (parse-results-position results) token)))))

(define (the-token goal)
  (lambda (results)
    (let ((token (parse-results-token-value results)))
      (if (eqv? goal token)
          (make-result token (parse-results-next results))
          (make-expected-result (parse-results-position results) token)))))

;;;; Parser

(define parse-graphql-document

  (let ((empty (lambda (results) (make-result #f results)))
        (eof (the-token #f))
        (operation-type (one-of-the-tokens '(query mutation subscription)))
        (fragment-keyword (the-token 'fragment))
        (directive-keyword (the-token 'directive))
        (on-keyword (the-token 'on))
        (input-keyword (the-token 'input))
        (union-keyword (the-token 'union))
        (enum-keyword (the-token 'enum))
        (scalar-keyword (the-token 'scalar))
        (type-keyword (the-token 'type))
        (implements-keyword (the-token 'implements))
        (interface-keyword (the-token 'interface))
        (executable-directive-location
         (one-of-the-tokens
          '(QUERY
            MUTATION
            SUBSCRIPTION
            FIELD
            FRAGMENT-DEFINITION
            FRAGMENT-SPREAD
            INLINE-FRAGMENT)))
        (type-system-directive-location
         (one-of-the-tokens
          '(SCHEMA
            SCALAR
            OBJECT
            FIELD-DEFINITION
            ARGUMENT-DEFINITION
            INTERFACE
            UNION
            ENUM
            ENUM-VALUE
            INPUT-OBJECT
            INPUT-FIELD-DEFINITION))))

    (packrat-parser
     document

   (document
    ((lst <- definition-list+) lst))

   (definition-list*
     ((lst <- definition-list+) lst)
     ((eof) '()))
   (definition-list+
     ((first <- definition rest <- definition-list*) (cons first rest)))
   (definition
     ((a <- operation-definition) a)
     ((a <- fragment-definition) a)
     ((a <- interface-type-definition) a)
     ((a <- input-object-type-definition) a)
     ((a <- union-type-definition) a)
     ((a <- enum-type-definition) a)
     ((a <- scalar-type-definition) a)
     ((a <- object-type-definition) a)
     ((a <- directive-definition) a))

   (operation-definition
    ((operation-type <- operation-type
                     name <- name?
                     variables <- variable-definitions?
                     directives <- directive-list*
                     selection-set <- selection-set)
     (if (and (null? variables) (null? directives))
         `(,operation-type ,name ,@selection-set)
         `(,operation-type (,name ,@variables ,@directives) ,@selection-set)))
    ((selection-set <- selection-set)
     `(query #f ,@selection-set)))

   (interface-type-definition
    ((description <- string-value?
                  interface-keyword
                  name <- 'name
                  directives <- directive-list*
                  field <- fields-definition?)
     `(interface ,name ,description ,directives ,field)))

   (input-object-type-definition
    ((description <- string-value?
                  input-keyword
                  name <- 'name
                  directives <- directive-list*
                  field <- input-fields-definition?)
     `(input ,name ,description ,directives ,field)))

   (union-type-definition
    ((description <- string-value?
                  union-keyword
                  name <- 'name
                  directives <- directive-list*
                  types <- union-member-types?)
     `(union ,name ,description ,directives ,types)))

   (enum-type-definition
    ((description <- string-value?
                  enum-keyword
                  name <- 'name
                  directives <- directive-list*
                  values <- enum-values-definition?)
     `(enum ,name ,description ,directives ,values)))

   (scalar-type-definition
    ((description <- string-value?
                  scalar-keyword
                  name <- 'name
                  directives <- directive-list*)
     `(scalar ,name ,description ,directives)))

   (object-type-definition
    ((description <- string-value?
                  type-keyword
                  name <- 'name
                  implements <- implements-interfaces?
                  directives <- directive-list*
                  fields <- fields-definition?)
     `(type (,name (implements ,@implements))
            ,description
            ,@fields)))

   (implements-interfaces?
    ((implements-keyword '|&| lst <- implements-interfaces-list+) lst)
    ((implements-keyword      lst <- implements-interfaces-list+) lst)
    ((_ <- empty) '()))

   (implements-interfaces-list+
    ((first <- 'name rest <- implements-interfaces-list-cont)
     (cons first rest)))

   (implements-interfaces-list-cont
    ((_ <- '|&| lst <- implements-interfaces-list+) lst)
    ((_ <- empty) '()))

   (directive-locations
    ((_ <- '|\|| lst <- directive-location-list+) lst)
    ((lst <- directive-location-list+) lst))

   (directive-location-list+
    ((first <- directive-location '|\|| rest <- directive-location-list+)
     (cons first rest))
    ((first <- directive-location)
     (list first)))

   (directive-location
    ((a <- executable-directive-location) a)
    ((a <- type-system-directive-location) a))

   (union-member-types?
    ((_ <- '|=| '|\|| lst <- union-member-type-list+) lst)
    ((_ <- '|=| lst <- union-member-type-list+) lst)
    ((_ <- empty) '()))

   (union-member-type-list+
    ((first <- 'name '|\|| rest <- union-member-type-list+)
     (cons first rest))
    ((first <- 'name)
     (list first)))

   (enum-values-definition?
    ((_ <- '|{| lst <- enum-value-definition-list* '|}|) lst)
    ((_ <- empty) '()))

   (enum-value-definition-list*
    ((first <- enum-value-definition rest <- enum-value-definition-list*)
     (cons first rest))
    ((_ <- empty) '()))

   (enum-value-definition
    ((description <- string-value?
                  value <- enum-value
                  directives <- directive-list*)
     `(value ,value ,description ,directives)))

   (enum-value
    ((name <- 'name) name)) ;; TODO: but not true or false or null

   (fields-definition?
    ((def <- fields-definition) def)
    ((_ <- empty) #f))

   (fields-definition
    ((_ <- '|{| fields <- field-definition-list+ '|}|)
     fields))

   (field-definition-list*
    ((lst <- field-definition-list+) lst)
    ((_ <- empty) '()))

   (field-definition-list+
    ((first <- field-definition rest <- field-definition-list*)
     (cons first rest)))

   (field-definition
    ((description <- string-value?
                  name <- 'name
                  arguments <- arguments-definition?
                  '|:|
                  type <- type
                  directives <- directive-list*)
     `(field ,name ,description ,arguments ,type ,directives)))

   (arguments-definition?
    ((a <- arguments-definition) a)
    ((_ <- empty) #f))

   (arguments-definition
    ((_ <- '|(| lst <- input-value-definition-list* '|)|)
     lst))

   (input-fields-definition?
    ((_ <- '|{| lst <- input-value-definition-list+ '|}|) lst)
    ((_ <- empty) '()))

   (input-value-definition-list*
    ((lst <- input-value-definition-list+) lst)
    ((_ <- empty) '()))

   (input-value-definition-list+
    ((first <- input-value-definition rest <- input-value-definition-list*)
     (cons first rest)))

   (input-value-definition
    ((description <- string-value?
                  name <- 'name
                  '|:|
                  type <- type
                  default <- default-value?
                  directives <- directive-list*)
     `(input-value
       ,name
       ,description
       ,type
       ,default
       ,@directives)))

   (default-value?
     ((_ <- '|=| value <- value) `(default ,value))
     ((_ <- empty) #f))

   (variable-definitions?
    ((_ <- '|(| lst <- variable-definition-list+ '|)|) lst)
    ((_ <- empty) '()))
   (variable-definition-list*
    ((lst <- variable-definition-list+) lst)
    ((_ <- empty) '()))
   (variable-definition-list+
    ((first <- variable-definition rest <- variable-definition-list*)
     (cons first rest)))
   (variable-definition
    ((name <- variable-name '|:| type <- type default <- value?)
     (list name type default)))

   (directive-list+
    ((first <- directive rest <- directive-list*) (cons first rest)))
   (directive-list*
    ((lst <- directive-list+) lst)
    ((_ <- empty) '()))
   (directive
    ((_ <- '|@| name <- 'name arguments <- arguments?)
     `(@ ,name ,@arguments)))

   (type
    ((ty <- non-null-type) ty)
    ((ty <- list-type) ty)
    ((ty <- 'name) ty))
   (non-null-type
    ((ty <- list-type '|!|) `(non-null ,ty))
    ((ty <- 'name     '|!|) `(non-null ,ty)))
   (list-type
    ((_ <- '|[| ty <- type '|]|) `(list ,ty)))

   (fragment-definition
    ((fragment-keyword
      name <- fragment-name
      type-cond <- type-condition
      directives <- directive-list*
      selection-set <- selection-set)
     `(fragment
       ,name
       ,type-cond
       ,directives
       ,@selection-set)))

   (fragment-name
    ((on-keyword) (error ""))
    ((name <- 'name) name))

   (directive-definition
    ((description <- string-value?
                  directive-keyword
                  '|@| name <- 'name
                  arguments <- arguments-definition?
                  on-keyword
                  directive-locations <- directive-locations)
     `(directive
       ,name
       ,description
       ,arguments
       ,directive-locations
       )))

   (type-condition?
    ((a <- type-condition) a)
    ((_ <- empty) #f))

   (type-condition
    ((on-keyword name <- 'name)
     `(on ,name)))

   #|
   (type-system-extension)
   |#

   (selection-set?
    ((selection-set <- selection-set) selection-set)
    ((_ <- empty) '()))
   (selection-set
    ((_ <- '|{| lst <- selection-list+ '|}|) lst))
   (selection-list+
    ((first <- selection rest <- selection-list*) (cons first rest)))
   (selection-list*
    ((lst <- selection-list+) lst)
    ((_ <- empty) '()))
   (selection
    ((a <- field) a)
    ;;fragment-spread
    ;;inline-fragment
    )
   (field
    ((alias <- alias?
            name <- 'name
            arguments <- arguments?
            selection-set <- selection-set?)
     (let ((x (cond ((and (null? arguments) (null? selection-set))
                     name)
                    ((null? arguments)
                     `(field ,name ,@selection-set))
                    (else
                     `(field (,name ,@arguments) ,@selection-set)))))
       (if alias `(alias ,alias ,x) x))))
   (alias?
    ((name <- 'name '|:|)  name)
    ((_ <- empty) #f))
   (arguments?
    ((_ <- '|(| args <- name-value-pair-list+ '|)|)  args)
    ((_ <- empty) '()))
   (name-value-pair-list+
    ((first <- name-value-pair rest <- name-value-pair-list*)
     (cons first rest)))
   (name-value-pair-list*
    ((lst <- name-value-pair-list+) lst)
    ((_ <- empty) '()))
   (name-value-pair
    ((name <- 'name  '|:|  value <- value)  (list name value)))
   (value?
    ((v <- value) v)
    ((_ <- empty) #f))
   (value
    ((name <- variable-name) `($ ,name))
    ((v <- 'string) v)
    ((v <- 'nat) v)
    ((v <- 'neg) v)
    ((v <- 'float) v)
    ((v <- 'true) #t)
    ((v <- 'false) #f)
    ((v <- 'null) 'null)
    ((v <- 'name) (list 'enum v))
    ((v <- list-value) v)
    ((v <- object-value) v))
   (string-value?
    ((v <- 'string) v)
    ((_ <- empty) #f))
   (value-list*
    ((first <- value rest <- value-list*) (cons first rest))
    ((_ <- empty) '()))
   (list-value
    ((_ <- '|[| v <- value-list* '|]|) (list->vector v)))
   (object-value
    ((_ <- '|{| pairs <- name-value-pair-list* '|}|) (cons 'object pairs)))
   (variable-name
    ((_ <- '|$| name <- 'name) name))

   (name?
    ((name <- 'name) name)
    ((_ <- empty) #f)))))

;;;; API

(define (read-graphql)
  (let ((result (parse-graphql-document
                 (base-generator->results (token-generator)))))
    (if (parse-result-successful? result)
        (parse-result-semantic-value result)
        (error (parse-error-expected
                (parse-result-error result))))))

(define (string->graphql document-as-string)
  (parameterize ((current-input-port (open-input-string document-as-string)))
    (read-graphql)))
