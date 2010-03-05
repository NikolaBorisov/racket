#lang scheme/base

(require (for-template scheme/base)
         syntax/boundmap
         syntax/stx
         syntax/parse
         scheme/struct-info
         "patterns.ss"
         "compiler.ss")

(provide ddk? parse-literal all-vars pattern-var? match:syntax-err
         match-expander-transform trans-match parse-struct
         dd-parse parse-quote parse-id ddk literal-pattern
         null-terminated? append-pats id-pat quote-pat)

(define-syntax-class id-pat
  #:attributes (pat)
  (pattern (~datum _)
           #:attr pat (make-Dummy this-syntax))
  (pattern i:id
           #:fail-when (ddk? #'i) "incorrect use of ... in pattern"
           #:attr pat (make-Var #'i)))

;; parse x as a match variable
;; x : identifier
(define (parse-id x)
  (cond [(eq? '_ (syntax-e x))
         (make-Dummy x)]
        [(ddk? x) (raise-syntax-error 'match "incorrect use of ... in pattern"
                                      x)]
        [else (make-Var x)]))

;; stx : syntax of pattern, starting with quote
;; parse : the parse function
(define (parse-quote stx parse)
  (syntax-case stx (quote)
    [(quote ())
     (make-Null (make-Dummy stx))]
    [(quote (a . b))
     (make-Pair (parse (syntax/loc stx (quote a)))
                (parse (syntax/loc stx (quote b))))]
    [(quote vec)
     (vector? (syntax-e #'vec))
     (make-Vector (for/list ([e (syntax-e #'vec)])
                    (parse (quasisyntax/loc stx (quote #,e)))))]
    [(quote bx)
     (box? (syntax-e #'bx))
     (make-Box (parse (quasisyntax/loc 
                       stx 
                       (quote #,(unbox (syntax-e #'bx))))))]
    [(quote v)
     (or (parse-literal (syntax-e #'v))
         (raise-syntax-error 'match "non-literal in quote pattern" stx #'v))]
    [_ (raise-syntax-error 'match "syntax error in quote pattern" stx)]))

(define-syntax-class quote-pat
  #:attributes (pat)
  (pattern ()
           #:attr pat (make-Null (make-Dummy this-syntax)))
  [pattern (a:quote-pat . b:quote-pat)
           #:attr pat (make-Pair (attribute a.pat) (attribute b.pat))]
  [pattern #(p:quote-pat ...)
   #:attr pat (make-Vector (attribute p.pat))]
  [pattern #&p:quote-pat
   #:attr pat (make-Box (attribute p.pat))]
  [pattern v:literal-pattern
   #:attr pat (attribute v.pat)])


;; parse : the parse fn
;; p : the parsed repeated pattern
;; dd : the ... stx
;; rest : the pattern for the rest
(define (dd-parse parse p dd rest #:mutable [mutable? #f])
  (let* ([count (ddk? dd)]
         [min (and (number? count) count)])
    (make-GSeq
     (list (list p))
     (list min)
     ;; no upper bound
     (list #f)
     ;; patterns in p get bound to lists
     (list #f)
     rest
     mutable?)))

;; stx : the syntax object for the whole pattern
;; cert : the certifier
;; parse : the pattern parser
;; struct-name : identifier
;; pats : syntax representing the member patterns
;; returns a pattern
(define (parse-struct stx cert parse struct-name pats)
  (let* ([fail (lambda ()
                 (raise-syntax-error
                  'match (format "~a does not refer to a structure definition"
                                 (syntax->datum struct-name))
                  stx struct-name))]
         [v (syntax-local-value (cert struct-name) fail)])
    (unless (struct-info? v) (fail))
    (let-values ([(id _1 pred acc _2 super)
                  (apply values (extract-struct-info v))])
      ;; this produces a list of all the super-types of this struct
      ;; ending when it reaches the top of the hierarchy, or a struct that we
      ;; can't access
      (define (get-lineage struct-name)
        (let ([super (list-ref (extract-struct-info (syntax-local-value
                                                     struct-name))
                               5)])
          (cond [(equal? super #t) '()] ;; no super type exists
                [(equal? super #f) '()] ;; super type is unknown
                [else (cons super (get-lineage super))])))
      (let* (;; the accessors come in reverse order
             [acc (reverse acc)]
             ;; remove the first element, if it's #f
             [acc (cond [(null? acc) acc]
                        [(not (car acc)) (cdr acc)]
                        [else acc])])
        (make-Struct id
                     (syntax-property 
                      pred 
                      'disappeared-use (list struct-name))
                     (get-lineage (cert struct-name))
                     acc
                     (cond [(eq? '_ (syntax-e pats))                            
                            (map make-Dummy acc)]
                           [(syntax->list pats)
                            =>
                            (lambda (ps)
                              (unless (= (length ps) (length acc))
                                (raise-syntax-error
                                 'match
                                 (format "~a structure ~a: expected ~a but got ~a"
                                         "wrong number for fields for"
                                         (syntax->datum struct-name) (length acc)
                                         (length ps))
                                 stx pats))
                              (map parse ps))]
                           [else (raise-syntax-error
                                  'match
                                  "improper syntax for struct pattern"
                                  stx pats)]))))))

(define (trans-match pred transformer pat)
  (make-And (list (make-Pred pred) (make-App transformer pat))))

;; transform a match-expander application
;; parse/cert : stx certifier -> pattern
;; cert : certifier
;; expander : identifier
;; stx : the syntax of the match-expander application
;; accessor : match-expander -> syntax transformer/#f
;; error-msg : string
;; produces a parsed pattern
(define (match-expander-transform parse/cert cert expander stx accessor
                                  error-msg)
  (let* ([expander (syntax-local-value (cert expander))]
         [transformer (accessor expander)])
    (unless transformer (raise-syntax-error #f error-msg expander))
    (let* ([introducer (make-syntax-introducer)]
           [certifier (match-expander-certifier expander)]
           [mstx (introducer (syntax-local-introduce stx))]
           [mresult (transformer mstx)]
           [result (syntax-local-introduce (introducer mresult))]
           [cert* (lambda (id) (certifier (cert id) #f introducer))])
      (parse/cert result cert*))))

;; raise an error, blaming stx
(define (match:syntax-err stx msg)
  (raise-syntax-error #f msg stx))

;; pattern-var? : syntax -> bool
;; is p an identifier representing a pattern variable?
(define (pattern-var? p)
  (and (identifier? p) (not (ddk? p))))

;; ddk? : syntax -> number or boolean
;; if #f is returned, was not a ddk identifier
;; if #t is returned, no minimum
;; if a number is returned, that's the minimum

(define-syntax-class ddk
  #:description "..."
  #:attributes (val min)
  (pattern (~or (~datum ...) (~datum ___))
           #:attr val #t
           #:attr min #f)
  (pattern ddd:id
           #:attr mtch
           (regexp-match #rx"^(?:\\.\\.|__)([0-9]+)$"
                         (symbol->string (syntax-e #'ddd)))
           #:when (attribute mtch)
           #:attr num (string->number (cadr (attribute mtch)))
           #:fail-unless (exact-nonnegative-integer? (attribute num)) "exact non-negative integer in ..k pattern"
           #:attr val (if (zero? (attribute num)) #t (attribute num))
           #:attr min (if (zero? (attribute num)) #f (attribute num))))
(provide ddk)

(define (ddk? s*)
  #;
  (syntax-parse s*
    [v:ddk (attribute v.val)]
    [_ #f])
  (let ([s (syntax->datum s*)])
    (and (symbol? s)
         (if (memq s '(... ___))
           #t
           (let* ([m (regexp-match #rx"^(?:\\.\\.|__)([0-9]+)$"
                                   (symbol->string s))]
                  [n (and m (string->number (cadr m)))])
             (cond [(not n) #f]
                   [(zero? n) #t]
                   [(exact-nonnegative-integer? n) n]
                   [else (raise-syntax-error
                          'match "invalid number for ..k pattern"
                          s*)]))))))

;; parse-literal : scheme-val -> pat option
;; is v is a literal, return a pattern matching it
;; otherwise, return #f
(define (parse-literal v)
  (if (or (number? v) (string? v) (keyword? v) (symbol? v) (bytes? v)
          (regexp? v) (boolean? v) (char? v))
    (make-Exact v)
    #f))

(define-syntax-class literal-pattern
  (pattern (and p (~or :number :str :keyword :id  :boolean :char))
           #:attr pat (make-Exact (syntax-e #'p)))
  (pattern p 
           #:when (or (bytes? (syntax-e #'p)) (regexp? (syntax-e #'p)) (pregexp? (syntax-e #'p)))
           #:attr pat (make-Exact (syntax-e #'p))))

;; (listof pat) syntax -> void
;; ps is never null
;; check that all the ps bind the same set of variables
(define (all-vars ps stx)  
  (let* ([first-vars (bound-vars (car ps))]
         [l (length ps)]
         [ht (make-free-identifier-mapping)])
    (for ([v first-vars]) (free-identifier-mapping-put! ht v 1))
    (for* ([p (cdr ps)]
           [v (bound-vars p)])
      (cond [(free-identifier-mapping-get ht v (lambda () #f))
             => (lambda (n)
                  (free-identifier-mapping-put! ht v (add1 n)))]
            [else (raise-syntax-error 'match
                                      "variable not bound in all or patterns"
                                      stx v)]))
    (free-identifier-mapping-for-each
     ht
     (lambda (v n)
       (unless (= n l)
         (raise-syntax-error 'match "variable not bound in all or patterns"
                             stx v))))))


;; is pat a pattern representing a list?
(define (null-terminated? pat)
  (cond [(Pair? pat) (null-terminated? (Pair-d pat))]
        [(GSeq? pat) (null-terminated? (GSeq-tail pat))]
        [(Null? pat) #t]
        [else #f]))

;; combine a null-terminated pattern with another pattern to match afterwards
(define (append-pats p1 p2)
  (cond [(Pair? p1) (make-Pair (Pair-a p1) (append-pats (Pair-d p1) p2))]
        [(GSeq? p1) (make-GSeq (GSeq-headss p1)
                               (GSeq-mins p1)
                               (GSeq-maxs p1)
                               (GSeq-onces? p1)
                               (append-pats (GSeq-tail p1) p2)
                               (GSeq-mutable? p1))]
        [(Null? p1) p2]
        [else (error 'match "illegal input to append-pats")]))