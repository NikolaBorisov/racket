#lang scheme/base

(require (for-template scheme/base)
         syntax/boundmap
         syntax/stx
         (rename-in syntax/parse [pattern sc:pattern])
         scheme/struct-info
         "patterns.ss"
         "compiler.ss"
         "parse-helper.ss")

(define-syntax-rule (define-qp-class quasi-pattern-class-name pattern-class-name)
  (...
   (define-syntax-class (quasi-pattern-class-name cert depth)
     #:literals (quasiquote quote unquote-splicing)
     #:attributes (pat)
     #:description "quasi-pattern"
     #:local-conventions ([p (pattern-class-name cert depth)]
                          [#rx"^qp" (quasi-pattern-class-name cert depth)])
     (sc:pattern (unquote p)
                 #:attr pat (attribute p.pat))
     (sc:pattern ((unquote-splicing p) . qp-rest)
                 #:fail-unless (null-terminated? (attribute p.pat))
                 "non-list pattern inside `unquote-splicing'"
                 #:attr pat (append-pats (attribute p.pat) (attribute qp-rest.pat)))
     (sc:pattern ((~var p (quasi-pattern-class-name cert (add1 depth))) dd:ddk . qp-rest)
                 #:attr pat (make-GSeq (list (list (attribute p.pat))) 
                                       (list (attribute dd.min))
                                       ;; no upper bound
                                       (list #f)
                                       ;; patterns in p get bound to lists
                                       (list #f)
                                       (attribute qp-rest.pat)
                                       #f))
     (sc:pattern (qp1 . qp2)
                 #:attr pat (make-Pair (attribute qp1.pat) (attribute qp2.pat)))
     (sc:pattern struct
                 #:attr key (prefab-struct-key (syntax-e #'struct))
                 #:when (attribute key)
                 #:with (qp ...) 
                 (cdr (vector->list (struct->vector (syntax-e #'struct))))
                 #:attr pat (make-And (list 
                                       (make-Pred 
                                        #`(struct-type-make-predicate
                                           (prefab-key->struct-type
                                            '#,(attribute key) 
                                            #,(length (syntax->list #'(qp ...))))))
                                       (make-App 
                                        #'struct->vector
                                        (make-Vector 
                                         (cons (make-Dummy #f) 
                                               (attribute qp.pat)))))))
     (sc:pattern #(qp ...)
                 #:attr pat (make-Vector (attribute qp.pat)))
     (sc:pattern () #:attr pat (make-Null (make-Dummy #f)))
     (sc:pattern #&qp
                 #:attr pat (make-Box (attribute qp.pat)))
     (sc:pattern lit:literal-pattern
                 #:attr pat (attribute lit.pat)))))

(define-qp-class legacy-quasi-pattern legacy-pattern)

(define-syntax-class (static/cert pred descr cert)
  (sc:pattern i:id
              #:with (~var si (static pred descr)) (cert #'i)
              #:attr value (attribute si.value)))

(define-syntax-class (legacy-pattern cert depth)
  #:attributes (pat)
  #:description "pattern"
  #:local-conventions ([#rx"^p" (legacy-pattern cert depth)]
                       [#rx"^qp" (legacy-quasi-pattern cert depth)])
  [sc:pattern ((~var expander (static/cert match-expander? "Named match expander" cert)) args ...)
              #:attr pat
              (match-expander-transform
               (syntax-parser [(~var p (legacy-pattern cert depth)) (attribute p.pat)])
               cert #'expander this-syntax match-expander-legacy-xform
               "This expander only works with the standard match syntax")]
  [sc:pattern ((~datum and) p ...)
              #:attr pat (make-And (attribute p.pat))]
  [sc:pattern ((~datum or))
              #:attr pat (make-Not (make-Dummy this-syntax))]
  [sc:pattern ((~datum or) p ...+)
              #:attr ps* (attribute p.pat)
              #:when (all-vars (attribute ps*) this-syntax)
              #:attr pat (make-Or (attribute ps*))]
  [sc:pattern ((~datum not) p ...)
              ;; nots are conjunctions of negations
              #:attr pat (make-And (map make-Not (attribute p.pat)))]
  [sc:pattern #&p
              #:attr pat (make-Box (attribute p.pat))]
  [sc:pattern #(es ...)
              #:when (ormap ddk? (syntax->list #'(es ...)))
              #:with p #'(es ...)
              #:attr pat (make-And (list (make-Pred #'vector?)
                                         (make-App #'vector->list (attribute p.pat))))]  
  [sc:pattern #(p ...)
              #:attr pat (make-Vector (attribute p.pat))]
  
  [sc:pattern ((~datum $) s . pats)
              #:attr pat 
              (parse-struct this-syntax cert (syntax-parser [(~var p (legacy-pattern cert depth)) (attribute p.pat)]) #'s #'pats)]
  [sc:pattern ((~datum ?) e:expr p ...+)
              #:attr pat
              (make-And (cons (make-Pred (cert #'e)) (attribute p.pat)))]
  #|
  [(? p)
   (make-Pred (cert #'p))]
  [(= f p)
   (make-App #'f (parse (cert #'p)))]
  [(quasiquote p)
   (parse-quasi #'p cert parse/legacy/cert)]
  [(quote . rest)
   (parse-quote stx parse)]
  [() (make-Null (make-Dummy #f))]
  [(..)
   (ddk? #'..)
   (raise-syntax-error 'match "incorrect use of ... in pattern" stx #'..)]
  [(p .. . rest)
   (ddk? #'..)
   (dd-parse parse #'p #'.. #'rest)]
  [(e . es)
   (make-Pair (parse #'e) (parse (syntax/loc stx es)))]
  [x
   (identifier? #'x)
   (parse-id #'x)]
  [v
   (or (parse-literal (syntax-e #'v))
       (raise-syntax-error 'match "syntax error in pattern" stx))]
|#)

(define-qp-class quasi-pattern pattern)


(define-syntax-class (pattern cert depth)
  #:attributes (pat)
  (sc:pattern (list)
              #:attr pat #f))
#;
(define-syntax-class (legacy-pattern cert depth)
  #:attributes (pat)
  (sc:pattern (list)
              #:attr pat #f))