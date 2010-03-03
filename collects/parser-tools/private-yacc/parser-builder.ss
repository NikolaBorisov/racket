#lang scheme/base
(require scheme/list
         scheme/class
         scheme/contract
         scheme/dict
         syntax/parse
         (only-in unstable/syntax
                  define-pattern-variable)
         unstable/list
         "grammar.ss"
         "table.ss"
         "parser-actions.ss"
         (only-in "yacc-helper.ss"
                  print-grammar+table-info)
         "../private-lex/token-syntax.ss"
         (for-template scheme/base))

(provide/contract
 [build-parser
  (-> (or/c string? #f) any/c any/c (listof identifier?) (listof identifier?)
      (listof identifier?) (or/c syntax? false/c) syntax?
      (values any/c any/c any/c any/c))])

(provide p1-grammar-clause
         p1-token-clause
         p1-end-clause
         grammar-clause
         token-clause
         start-clause
         end-clause
         error-clause
         precs-clause
         srcpos-clause
         suppress-clause
         debug-clause
         yacc-output-clause)

(define stx-for-original-property
  (read-syntax #f (open-input-string "original")))

;; build-parser
(define (build-parser filename src-pos suppress input-terms start end assocs prods)
  (let* ([grammar (parse-input input-terms start end assocs prods src-pos)]
         [table (build-table grammar filename suppress)]
         [all-tokens
          (for/hasheq ([term (in-list (send grammar get-terms))])
            (values (gram-sym-symbol term) #t))]
         [actions-code
          #`(vector #,@(map prod-action (send grammar get-prods)))])
    (when #f ;; DISABLED
      (print-grammar+table-info grammar table))
    (values table
            all-tokens
            actions-code
            (fix-check-syntax input-terms start end assocs prods))))

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
           #:attr nts (for/hasheq ([nt (syntax->datum #'(nt ...))])
                        (values nt #t))))

(define-syntax-class p1-token-clause
  (pattern ((~datum tokens) group:token-group ...)
           #:with (token ...)
                  (remove-duplicates
                   (syntax->list #'(the-error-token group.token ... ...))
                   #:key syntax->datum)
           #:attr ts (for/hasheq ([t (syntax->datum
                                      #'(the-error-token group.token ... ...))])
                       (values t #t))))

(define-syntax-class token-group
  #:opaque
  #:description "token group name"
  (pattern group
           #:declare group (static terminals-def? "terminal group")
           #:with (token ...) (terminals-def-t (attribute group.value)))
  (pattern group
           #:declare group (static e-terminals-def? "empty terminal group")
           #:with (token ...) (e-terminals-def-t (attribute group.value))))

(define-syntax-class p1-end-clause
  (pattern ((~datum end) token:id ...)
           #:attr ends (for/hasheq ([t (syntax->datum #'(token ...))])
                         (values t #t))))

;; ----

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
  (pattern ((~datum grammar) prod ...)
           #:declare prod (ntprod nts ts ends)))

(define-syntax-class (ntprod nts ts ends)
  #:attributes (nt [i 2])
  (pattern (nt:id ((i ...) (~optional ((~datum prec) ~! token)) rhs:expr) ...)
           #:declare i (item nts ts ends)
           #:declare token (declared-terminal ts)
           #:fail-when (and (hash-ref ts (syntax-e #'nt) #f) #'nt)
                       "already declared as a terminal"))

(define-syntax-class (item nts ts ends)
  #:description "terminal or nonterminal name"
  (pattern i:id
           #:when (or (hash-ref nts (syntax-e #'i) #f) (hash-ref ts (syntax-e #'i) #f))
           #:fail-when (hash-ref ends (syntax-e #'i) #f)
                       "end token cannot be used in a production"))

(define-syntax-class token-clause
  (pattern ((~datum tokens) . _)))

(define-syntax-class (start-clause nts)
  (pattern ((~datum start) nonterminal ...)
           #:declare nonterminal (declared-nonterminal nts)
           #:fail-unless (pair? (syntax->list #'(nonterminal ...)))
                         "missing start symbol"))

(define-syntax-class (end-clause ts)
  (pattern ((~datum end) token ...)
           #:declare token (declared-terminal ts)
           #:fail-when (check-duplicate (syntax->list #'(token ...))
                                        #:key syntax->datum)
                       "duplicate end symbol"
           #:fail-unless (pair? (syntax->list #'(token ...)))
                         "end clause must contain at least 1 token"))

(define-syntax-class error-clause
  (pattern ((~datum error) handler:expr)))

(define-syntax-class (precs-clause ts)
  (pattern ((~datum precs) decl ...)
           #:declare decl (precs-decl ts)
           #:fail-when (check-duplicate (syntax->list #'(decl.token ... ...))
                                        #:key syntax->datum)
                       "duplicate precedence declaration"))

(define-syntax-class (precs-decl ts)
  #:description "precedence declaration"
  (pattern (a:associativity token ...)
           #:declare token (declared-terminal ts)))

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
         [terms (build-terms list-of-terms (if prec-decls (syntax->datum prec-decls) null))]
         [non-terms
          (map (lambda (non-term) (make-non-term non-term #f))
               list-of-non-terms)]
         [term-table
          (for/hasheq ([t (in-list terms)])
            (values (gram-sym-symbol t) t))]
         [non-term-table
          (for/hasheq ([nt (in-list non-terms)])
            (values (gram-sym-symbol nt) nt))])

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
              ,@(for/list ([start-sym (in-list start-syms)]
                           [end-nt (in-list end-non-terms)])
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
                        end-terms)))))


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
  (syntax-parse prod-so
    [(prod-rhs (~optional ((~datum prec) term)) action)
     (let* ([p (parse-prod term-table non-term-table #'prod-rhs)]
            [prec (parse-prec term-table p (attribute term))]
            [a (parse-action term-defs src-pos #'prod-rhs #'action)])
       (make-prod nt p #f prec a))]))

;; parse-prec : hash vector id/#f -> prec?
(define (parse-prec term-table p prec-t)
  (if prec-t
      (term-prec (hash-ref term-table (syntax->datum prec-t)))
      (let loop ([i (sub1 (vector-length p))])
        (if (>= i 0)
            (let ([gs (vector-ref p i)])
              (if (term? gs)
                  (term-prec gs)
                  (loop (sub1 i))))
            #f))))

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
