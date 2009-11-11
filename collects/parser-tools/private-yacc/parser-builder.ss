#lang scheme/base
(require scheme/list
         scheme/class
         scheme/contract
         scheme/dict
         syntax/parse
         (only-in unstable/syntax
                  define-pattern-variable)
         "grammar.ss"
         "table.ss"
         "parser-actions.ss"
         "../private-lex/token-syntax.ss"
         (for-template scheme/base))

(provide/contract
 [build-parser
  (-> string? any/c any/c (listof identifier?) (listof identifier?)
      (listof identifier?) (or/c syntax? false/c) syntax?
      (values any/c any/c any/c any/c))])

(provide p1-grammar-clause
         p1-token-clause
         p1-end-clause
         grammar-clause
         ntprod
         maybe-prec
         token-clause
         start-clause
         end-clause
         error-clause
         precs-clause
         associativity
         srcpos-clause
         suppress-clause
         debug-clause
         yacc-output-clause)

#|
;; routines for parsing the input to the parser generator and producing a
;; grammar (See grammar.ss)

(provide/contract
 [parse-input
  (-> (listof identifier?) (listof identifier?) (listof identifier?)
      (or/c false/c syntax?) syntax? any/c
      (is-a?/c grammar%))]
 [get-term-list
  (-> (listof identifier?)
      (listof identifier?))])
|#

(define stx-for-original-property
  (read-syntax #f (open-input-string "original")))

;; build-parser
(define (build-parser filename src-pos suppress input-terms start end assocs prods)
  (let* ([grammar (parse-input input-terms start end assocs prods src-pos)]
         [table (build-table grammar filename suppress)]
         [all-tokens (make-hasheq)]
         [actions-code
          `(vector ,@(map prod-action (send grammar get-prods)))])
    (for ([term (in-list (send grammar get-terms))])
      (hash-set! all-tokens (gram-sym-symbol term) #t))
    (when #f ;; DISABLED
      (print-grammar+table-info grammar table))
    (values table
            all-tokens
            actions-code
            (fix-check-syntax input-terms start end assocs prods))))

;; print-grammar+table-info : Grammar Table -> void
(define (print-grammar+table-info grammar table)
  (let ([num-states (vector-length table)]
        [num-gram-syms (+ (send grammar get-num-terms)
                          (send grammar get-num-non-terms))]
        [num-ht-entries (apply + (map length (vector->list table)))]
        [num-reduces
         (let ((ht (make-hasheq)))
           (for ([x (in-list (map cdr (apply append (vector->list table))))])
             (when (reduce? x)
               (hash-set! ht x #t)))
           (length (hash-map ht void)))])
    (printf "~a states, ~a grammar symbols, ~a hash-table entries, ~a reduces~n"
            num-states num-gram-syms num-ht-entries num-reduces)
    (printf "~a -- ~aKB, previously ~aKB~n"
            (/ (+ 2 num-states
                  (* 4 num-states) (* 2 1.5 num-ht-entries)
                  (* 5 num-reduces)) 256.0)
            (/ (+ 2 num-states
                  (* 4 num-states) (* 2 2.3 num-ht-entries)
                  (* 5 num-reduces)) 256.0)
            (/ (+ 2 (* num-states num-gram-syms) (* 5 num-reduces)) 256.0))))

;; fix-check-syntax : (listof identifier?) (listof identifier?) (listof identifier?)
;;                    (union syntax? false/c) syntax?) -> syntax?
(define (fix-check-syntax input-terms start ends assocs prods)
  (let* ([term-binders (get-term-list input-terms)]
         [get-term-binder
          (let ((t (make-hasheq)))
            (for ([term (in-list term-binders)])
              (hash-set! t (syntax-e term) term))
            (lambda (x)
              (let ([r (hash-ref t (syntax-e x) #f)])
                (if r
                    (syntax-local-introduce (datum->syntax r (syntax-e x) x x))
                    x))))]
         [rhs-list
          (syntax-case prods ()
            [((_ rhs ...) ...)
             (syntax->list (syntax (rhs ... ...)))])])
    (with-syntax ([(tmp ...) (map syntax-local-introduce term-binders)]
                  [(term-group ...)
                   (map (lambda (tg)
                          (syntax-property (datum->syntax tg #f)
                                           'disappeared-use tg))
                        input-terms)]
                  [(end ...)
                   (map get-term-binder ends)]
                  [(start ...)
                   (map get-term-binder start)]
                  [(bind ...)
                   (syntax-case prods ()
                     [((bind _ ...) ...)
                      (syntax->list (syntax (bind ...)))])]
                  [((bound ...) ...)
                   (map
                    (lambda (rhs)
                      (syntax-case rhs ()
                        [((bound ...) (_ pbound) __)
                         (map get-term-binder
                              (cons (syntax pbound)
                                    (syntax->list (syntax (bound ...)))))]
                        [((bound ...) _)
                         (map get-term-binder
                              (syntax->list (syntax (bound ...))))]))
                    rhs-list)]
                  [(prec ...)
                   (if assocs
                       (map get-term-binder
                            (syntax-case assocs ()
                              [((__ term ...) ...)
                               (syntax->list (syntax (term ... ...)))]))
                       null)])
      #`(when #f
          (let ([bind void] ... [tmp void] ...)
            (void bound ... ... term-group ... start ... end ... prec ...))))))


;; ----


;; check-duplicate : (listof X)
;;                   #:key (X -> K)
;;                   #:equal? (K K -> bool)
;;                   #:make-dict (-> (dictof K bool))
;;                -> X or #f
(define (check-duplicate items
                        #:key [f values]
                        #:equal? [same? #f]
                        #:make-dict [make-dict #f])
  (cond [(and same? make-dict)
         (error 'check-duplicate "cannot supply both #:equal? and #:make-dict")]
        [make-dict
         (let ([dict (make-dict)])
           (if (dict-mutable? dict)
               (check-duplicate/t items f dict #t)
               (check-duplicate/t items f dict #f)))]
        [else
         (let ([same? (or same? equal?)])
           (cond [(eq? same? equal?)
                  (check-duplicate/t items f (make-hash) #t)]
                 [(eq? same? eq?)
                  (check-duplicate/t items f (make-hasheq) #t)]
                 [else
                  (check-duplicate/list items f same?)]))]))

(define (check-duplicate/t items f table mutating?)
  (let loop ([items items] [table table])
    (and (pair? items)
         (let ([f-item (f (car items))])
           (if (dict-ref table f-item #f)
               (car items)
               (loop (cdr items) (if mutating?
                                     (begin (dict-set! table f-item #t) table)
                                     (dict-set table f-item #t))))))))

(define (check-duplicate/list items f same?)
  (let loop ([items items] [sofar null])
    (and (pair? items)
         (let ([f-item (f (car items))])
           (if (for/or ([prev (in-list sofar)])
                 (same? prev f-item))
               (car items)
               (loop (cdr items) (cons f-item sofar)))))))


;; Syntax Classes

;; pass1
;; Gather terminal names, nonterminal names, and end token names.

(define-pattern-variable the-error-token
  (datum->syntax #f 'error))

(define-syntax-class p1-grammar-clause
  (pattern ((~datum grammar) [nt:id . _] ...)
           #:fail-when (check-duplicate (syntax->list #'(nt ...))
                                        #:key syntax->datum)
                       "duplicate nonterminal definition"
           #:attr nts (let ([ht (make-hasheq)])
                        (for ([nt (syntax->datum #'(nt ...))]) (hash-set! ht nt #t))
                        ht)))

(define-syntax-class p1-token-clause
  (pattern ((~datum tokens) group:token-group ...)
           #:with (token ...)
                  (remove-duplicates
                   (syntax->list #'(the-error-token group.token ... ...))
                   #:key syntax->datum)
           #:attr ts (let ([ht (make-hasheq)])
                       (for ([t (syntax->datum #'(the-error-token group.token ... ...))])
                         (hash-set! ht t #t))
                       ht)))

(define-syntax-class token-group #:attributes ([token 1])
  #:opaque
  #:description "token group name"
  (pattern (~var group (static terminals-def? "terminal group"))
           #:with (token ...) (terminals-def-t (attribute group.value)))
  (pattern (~var group (static e-terminals-def? "empty terminal group"))
           #:with (token ...) (e-terminals-def-t (attribute group.value))))

(define-syntax-class p1-end-clause
  (pattern ((~datum end) token:id ...)
           #:attr ends (let ([ht (make-hasheq)])
                         (for ([t (syntax->datum #'(token ...))])
                           (hash-set! ht t #t))
                         ht)))

(define-syntax-class (declared-nonterminal nts)
  (pattern nonterminal:id
           #:fail-unless (hash-ref nts (syntax-e #'nonterminal) #f)
                         "not defined as a nonterminal"))

(define-syntax-class (declared-terminal ts)
  (pattern terminal:id
           #:fail-unless (hash-ref ts (syntax-e #'terminal) #f)
                         "not declared as a terminal"))

;; pass2

(define-syntax-class (grammar-clause nts ts ends)
  (pattern ((~datum grammar) (~var prod (ntprod nts ts ends)) ...)))

(define-syntax-class (ntprod nts ts ends)
  #:attributes (nt [i 2])
  (pattern (nt:id (((~var i (item nts ts ends)) ...) prec:maybe-prec rhs:expr) ...)
           #:fail-when (and (hash-ref ts (syntax-e #'nt) #f) #'nt)
                       "already declared as a terminal"))

(define-syntax-class (item nts ts ends)
  #:description "terminal or nonterminal name"
  (pattern i:id
           #:when (or (hash-ref nts (syntax-e #'i) #f) (hash-ref ts (syntax-e #'i) #f))
           #:fail-when (hash-ref ends (syntax-e #'i) #f)
                       "end token cannot be used in a production"))

(define-splicing-syntax-class maybe-prec
  (pattern (~optional ((~datum prec) token:id))))

(define-syntax-class token-clause
  (pattern ((~datum tokens) . _)))

(define-syntax-class (start-clause nts)
  (pattern ((~datum start) (~var nonterminal (declared-nonterminal nts)) ...)
           #:fail-unless (pair? (syntax->list #'(nonterminal ...)))
                         "missing start symbol"))

(define-syntax-class (end-clause ts)
  (pattern ((~datum end) (~var token (declared-terminal ts)) ...)
           #:fail-when (check-duplicate (syntax->list #'(token ...))
                                        #:key syntax->datum)
                       "duplicate end symbol"
           #:fail-unless (pair? (syntax->list #'(token ...)))
                         "end clause must contain at least 1 token"))

(define-syntax-class error-clause
  (pattern ((~datum error) handler:expr)))

(define-syntax-class (precs-clause ts)
  (pattern ((~datum precs) (~var decl (precs-decl ts)) ...)
           #:fail-when (check-duplicate (syntax->list #'(decl.token ... ...))
                                        #:key syntax->datum)
                       "duplicate precedence declaration"))

(define-syntax-class (precs-decl ts)
  #:description "precedence declaration"
  (pattern (a:associativity (~var token (declared-terminal ts)) ...)))

(define-syntax-class associativity
  (pattern (~datum left))
  (pattern (~datum right))
  (pattern (~datum nonassoc)))

(define-syntax-class srcpos-clause
  (pattern ((~datum src-pos))))

(define-syntax-class suppress-clause
  (pattern ((~datum suppress))))

(define-syntax-class debug-clause
  (pattern ((~datum debug) file:str)))

(define-syntax-class yacc-output-clause
  (pattern ((~datum yacc-output) file:str)))


;; parse-input : ?? -> ??
(define (parse-input term-defs start ends prec-decls prods src-pos)
  (let* ([start-syms (map syntax-e start)]
         [list-of-terms (map syntax-e (get-term-list term-defs))]
         [end-terms (map syntax-e ends)]
         ;; Get the list of terminals out of input-terms
         [list-of-non-terms
          (syntax-case prods ()
            [((non-term production ...) ...)
             (syntax->datum #'(non-term ...))])]
         ;; Check the precedence declarations for errors and turn them into data
         [precs
          (syntax-case prec-decls ()
            [((type term ...) ...)
             (syntax->datum prec-decls)]
            [#f null])]
         [terms (build-terms list-of-terms precs)]
         [non-terms
          (map (lambda (non-term) (make-non-term non-term #f))
               list-of-non-terms)]
         [term-table (make-hasheq)]
         [non-term-table (make-hasheq)])

    (for ([t (in-list terms)])
      (hash-set! term-table (gram-sym-symbol t) t))

    (for ([nt (in-list non-terms)])
      (hash-set! non-term-table (gram-sym-symbol nt) nt))

    (let* (

           )

      (for ([sstx (in-list start)] [ssym (in-list start-syms)])
        (unless (memq ssym list-of-non-terms)
          (raise-syntax-error
           'parser-start
           (format "Start symbol ~a not defined as a non-terminal" ssym)
           sstx)))

      (let* ([starts (map (lambda (x) (make-non-term (gensym) #f)) start-syms)]
             [end-non-terms (map (lambda (x) (make-non-term (gensym) #f)) start-syms)]
             [parsed-prods
              (for/list ([prods-so (syntax->list prods)])
                (parse-prods-for-nt term-table non-term-table term-defs src-pos prods-so))]
             [start-prods
              (for/list ([start (in-list starts)] [end-non-term (in-list end-non-terms)])
                (list (make-prod start (vector end-non-term) #f #f
                                 #'(lambda (x) x))))]
             [prods 
              `(,@start-prods
                ,@(for/list ([end-nt (in-list end-non-terms)]
                             [start-sym (in-list start-syms)])
                    (for/list ([end (in-list end-terms)])
                      (make-prod end-nt
                                 (vector
                                  (hash-ref non-term-table start-sym)
                                  (hash-ref term-table end))
                                 #f
                                 #f
                                 #'(lambda (x) x))))
                ,@parsed-prods)])

        (make-object grammar%
                     prods
                     (map car start-prods)
                     terms
                     (append starts (append end-non-terms non-terms))
                     (map (lambda (term-name)
                            (hash-ref term-table term-name))
                          end-terms))))))


;; parse-prods-for-nt : hash hash ?? ?? syntax -> (listof production)
(define (parse-prods-for-nt term-table non-term-table term-defs src-pos prods-so)
  (syntax-case prods-so ()
    [(nt productions ...)
     (let ((nt (hash-ref non-term-table (syntax->datum #'nt))))
       (for/list ([p (in-list (syntax->list #'(productions ...)))])
         (parse-prod+action term-table non-term-table term-defs src-pos nt p)))]))

;; parse-prod+action : hash hash ?? ?? non-term syntax -> production
;; Production syntax already validated.
;; FIXME
(define (parse-prod+action term-table non-term-table term-defs src-pos nt prod-so)
  (syntax-case prod-so ()
    [(prod-rhs action)
     (let ([p (parse-prod term-table non-term-table #'prod-rhs)])
       (make-prod 
        nt
        p
        #f
        (let loop ((i (sub1 (vector-length p))))
          (if (>= i 0)
              (let ((gs (vector-ref p i)))
                (if (term? gs)
                    (term-prec gs)
                    (loop (sub1 i))))
              #f))
        (parse-action term-defs src-pos #'prod-rhs #'action)))]
    [(prod-rhs (prec term) action)
     (identifier? #'term)
     (let ([p (parse-prod term-table non-term-table #'prod-rhs)])
       (make-prod 
        nt 
        p
        #f
        (term-prec
         (hash-ref term-table 
                   (syntax->datum #'term)
                   (lambda ()
                     (raise-syntax-error
                      'parser-production-rhs
                      (format
                       "unrecognized terminal ~a in precedence declaration"
                       (syntax->datum #'term))
                      #'term))))
        (parse-action term-defs src-pos #'prod-rhs #'action)))]))

;; parse-prod: hash hash syntax -> (vectorof gram-sym)
;; Production syntax has already been validated.
(define (parse-prod term-table non-term-table prod-so)
  (syntax-case prod-so ()
    [(prod-rhs-sym ...)
     (list->vector
      (for/list ([s (syntax->list prod-so)])
        (or (hash-ref term-table (syntax->datum s) #f)
            (hash-ref non-term-table (syntax->datum s)))))]))

;; parse-action: ?? ?? syntax syntax -> syntax
(define (parse-action term-defs src-pos rhs act)
  (let-values ([(args biggest) (get-args 1 (syntax->list rhs) src-pos term-defs)])
    (let ([act 
           (if biggest
               (with-syntax ([$n-start-pos (datum->syntax (car biggest) '$n-start-pos)]
                             [$n-end-pos (datum->syntax (cdr biggest) '$n-end-pos)])
                 #`(let ([$n-start-pos #,(car biggest)]
                         [$n-end-pos #,(cdr biggest)])
                     #,act))
               act)])
      (quasisyntax/loc act
        (lambda #,args
          #,act)))))




;; get-args: ??? -> (values (listof syntax) (or/c #f (cons integer? stx)))
(define (get-args i rhs src-pos term-defs)
  (let ([empty-table (make-hasheq)]
        [biggest-pos #f])
    (hash-set! empty-table 'error #t)
    (for ([td (in-list term-defs)])
      (let ((v (syntax-local-value td)))
        (when (e-terminals-def? v)
          (for ([s (in-list (syntax->list (e-terminals-def-t v)))])
            (hash-set! empty-table (syntax->datum s) #t)))))
    (let ([args
           (let get-args ([i i] [rhs rhs])
             (cond [(null? rhs) null]
                   [else
                    (let ([b (car rhs)]
                          [name (if (hash-ref empty-table (syntax->datum (car rhs)) #f)
                                    (gensym)
                                    (string->symbol (format "$~a" i)))])
                      (cond
                       [src-pos
                        (let ([start-pos-id
                               (datum->syntax b (string->symbol (format "$~a-start-pos" i))
                                              b stx-for-original-property)]
                              [end-pos-id
                               (datum->syntax b (string->symbol (format "$~a-end-pos" i))
                                              b stx-for-original-property)])
                          (set! biggest-pos (cons start-pos-id end-pos-id))
                          `(,(datum->syntax b name b stx-for-original-property)
                            ,start-pos-id
                            ,end-pos-id
                            ,@(get-args (add1 i) (cdr rhs))))]
                       [else
                        `(,(datum->syntax b name b stx-for-original-property)
                          ,@(get-args (add1 i) (cdr rhs)))]))]))])
      (values args biggest-pos))))


;; build-terms: symbol list * symbol list list -> term list
;; Given the list of terminal symbols and the precedence/associativity definitions,
;; builds terminal structures (See grammar.ss)
(define (build-terms term-list precs)
  (let ([counter 0]
        ;; Will map a terminal symbol to its precedence/associativity
        [prec-table (make-hasheq)])

    ;; Fill the prec table
    (for ([p-decl (in-list precs)])
      (let ((assoc (car p-decl)))
        (for ([term-sym (in-list (cdr p-decl))])
          (hash-set! prec-table term-sym (make-prec counter assoc)))
        (set! counter (add1 counter))))

    ;; Build the terminal structures
    (for/list ([term-sym (in-list term-list)])
      (make-term term-sym #f (hash-ref prec-table term-sym #f)))))


;; get-terms-from-def: identifier? -> (listof identifier?)
;; Retrieves the terminal symbols from a terminals-def (See terminal-syntax.ss)
(define (get-terms-from-def term-syn)
  (let ((t (syntax-local-value term-syn (lambda () #f))))
    (cond
     [(terminals-def? t) (syntax->list (terminals-def-t t))]
     [(e-terminals-def? t) (syntax->list (e-terminals-def-t t))]
     [else
      (raise-syntax-error 'parser-tokens
                          "undefined token group"
                          term-syn)])))

;; get-term-list : ?? -> ??
(define (get-term-list term-group-names)
  (remove-duplicates
   (cons (datum->syntax #f 'error)
         (apply append
                (map get-terms-from-def term-group-names)))
   #:key syntax->datum))
