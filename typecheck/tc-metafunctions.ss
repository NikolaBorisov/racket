#lang scheme/base

(require "../utils/utils.ss")
(require (rename-in (types subtype convenience remove-intersect union utils)                   
                    [-> -->]
                    [->* -->*]
                    [one-of/c -one-of/c])
         (rep type-rep filter-rep rep-utils) scheme/list
         scheme/contract scheme/match unstable/match
         (for-syntax scheme/base))

(provide (all-defined-out))

;; this implements the sequence invariant described on the first page relating to Bot
#;
(define (lcombine l1 l2)
  (cond [(memq (make-LBot) l1)
         (make-LFilterSet (list (make-LBot)) null)]
        [(memq (make-LBot) l2)
         (make-LFilterSet null (list (make-LBot)))]
        [else (make-LFilterSet l1 l2)]))

(define (combine l1 l2)
  (match* (l1 l2) 
          [(_ (Bot:)) (-FS -top -bot)]
          [((Bot:) _) (-FS -bot -top)]
          [(_ _) (-FS l1 l2)]))

(d/c (abstract-filters ids results)
     ((listof identifier?) tc-results? . -> . (or/c Values? ValuesDots?))
     #;(define (mk l [drest #f])
       (if drest (make-ValuesDots l (car drest) (cdr drest)) (make-Values l)))
     (match results
       [(tc-results: ts fs os dty dbound)
        (make-ValuesDots 
         (for/list ([t ts]
                    [f fs]
                    [o os])
           (make-Result t f o))
         dty dbound)]
       [(tc-results: ts fs os)
        (make-Values
         (for/list ([t ts]
                    [f fs]
                    [o os])
           (make-Result t f o)))]))

#;
(define/contract (abstract-object ids keys o)
  (-> (listof identifier?) (listof index/c) Object? LatentObject?)
  (define (lookup y)
    (for/first ([x ids] [i keys] #:when (free-identifier=? x y)) i))
  (define-match-expander lookup:
    (syntax-rules ()
      [(_ i) (app lookup (? values i))]))
  (match o    
    [(Path: p (lookup: idx)) (make-LPath p idx)]
    [_ (make-LEmpty)]))

#;
(d/c (abstract-filter ids keys fs)
  (-> (listof identifier?) (listof index/c) FilterSet/c LatentFilterSet/c)
  (match fs
    [(FilterSet: f+ f-)
     (combine
      (apply append (for/list ([f f+]) (abo ids keys f)))
      (apply append (for/list ([f f-]) (abo ids keys f))))]
    [(NoFilter:) (combine -top -top)]))

#;
(d/c (abo xs idxs f)
  ((listof identifier?) (listof index/c) Filter/c . -> . (or/c null? (list/c LatentFilter/c)))
  (define (lookup y)
    (for/first ([x xs] [i idxs] #:when (free-identifier=? x y)) i)) 
  (define-match-expander lookup:
    (syntax-rules ()
      [(_ i) (app lookup (? values i))]))
  (match f
    [(Bot:) (list (make-LBot))]
    [(TypeFilter: t p (lookup: idx)) (list (make-LTypeFilter t p idx))]
    [(NotTypeFilter: t p (lookup: idx)) (list (make-LNotTypeFilter t p idx))]
    [(ImpFilter: as cs)
     (let ([a* (apply append (for/list ([f as]) (abo xs idxs f)))]
           [c* (apply append (for/list ([f cs]) (abo xs idxs f)))])
       (cond [(< (length a*) (length as)) ;; if we removed some things, we can't be sure
              null]
             [(null? c*) ;; this clause is now useless
              null]
             [else
              (list (make-LImpFilter a* c*))]))]
    [_ null]))

(define (merge-filter-sets fs)
  (match fs
    [(list (FilterSet: f+ f-) ...)
     (make-FilterSet (make-AndFilter f+) (make-AndFilter f-))]))

(d/c (apply-filter fs ids os [polarity #t])
  (->* (FilterSet/c (listof identifier?) (listof Object?)) (boolean?) FilterSet/c)
  (match fs
    [(FilterSet: f+ f-)
     (combine (subst-filter* f+ ids os polarity) 
	      (subst-filter* f- ids os polarity))]))

(d/c (apply-type t ids os [polarity #t])
  (->* (Type/c (listof identifier?) (listof Object?)) (boolean?) FilterSet/c)
  (for/fold ([t t]) ([i (in-list ids)] [o (in-list os)])
    (subst-type t i o polarity)))

(define (subst-filter* f ids os polarity)
  (for/fold ([f f]) ([i (in-list ids)] [o (in-list os)])
    (subst-filter f i o polarity)))

(define (subst-type t id o polarity)
  (define (st t) (subst-type t id o polarity))
  (type-case (#:Type st 
	      #:Filter (lambda (f) (subst-filter f id o polarity))
	      #:Object (lambda (f) (subst-object f id o polarity)))
	      t))

(define (subst-object t id o polarity)
  (match t
    [(NoObject:) t]
    [(Empty:) t]
    [(Path: p i)
     (if (free-identifier=? i id)
	 (match o
	   [(Empty:) (make-Empty)]
	   ;; the result is not from an annotation, so it isn't a NoObject
	   [(NoObject:) (make-Empty)]
	   [(Path: p* i*) (make-Path (append p p*) i*)])
	 t)]))

;; this is the substitution metafunction 
(d/c (subst-filter f id o polarity)
  (-> Filter/c identifier? Object? boolean? Filter/c)
  (define (ap f) (subst-filter f o polarity))
  (define (tf-matcher t p i id o polarity maker)
    (match o
      [(or (Empty:) (NoObject:)) (if polarity -top -bot)]
      [(Path: p* i*)
       (cond [(free-identifier=? i id)
	      (maker
	       (subst-type t id o polarity)
	       (append p p*) 
	       i*)]
	     [(id-free-in? id t) (if polarity -top -bot)]
	     [else f])]))
  (match f
    [(ImpFilter: ant consq)
     (make-ImpFilter (subst-filter ant o (not polarity)) (ap consq))]
    [(AndFilter: fs) (make-AndFilter (map ap fs))]
    [(OrFilter: fs) (make-OrFilter (map ap fs))]
    [(Bot:) -bot]
    [(Top:) -top]
    [(TypeFilter: t p i)
     (tf-matcher t p i id o polarity make-TypeFilter)]
    [(NotTypeFilter: t p i)
     (tf-matcher t p i id o polarity make-NotTypeFilter)]))

(define (id-free-in? id type)
  (let/ec 
   return
   (define (for-object o)
     (object-case (#:Type for-type)
		  o
		  [#:Path p i
			  (if (free-identifier=? i id)
			      (return #t)
			      o)]))
   (define (for-filter o)
     (filter-case (#:Type for-type
		   #:Filter for-filter)
		  o
		  [#:NotTypeFilter t p i
				   (if (free-identifier=? i id)
				       (return #t)
				       o)]
		  [#:TypeFilter t p i
				(if (free-identifier=? i id)
				    (return #t)
				    o)]))
   (define (for-type t)
     (type-case (#:Type for-type
		 #:Filter for-filter
		 #:Object for-object)
		t))
   (for-type type)))

#|
#;
(define/contract (split-lfilters lf idx)  
  (LatentFilterSet/c index/c . -> . LatentFilterSet/c)
  (define (idx= lf)
    (match lf
      [(LBot:) #t]
      [(LNotTypeFilter: _ _ idx*) (= idx* idx)]
      [(LTypeFilter: _ _ idx*) (= idx* idx)]))
  (match lf
    [(LFilterSet: lf+ lf-)
     (make-LFilterSet (filter idx= lf+) (filter idx= lf-))]))

(define-match-expander T-FS:
  (lambda (stx) #'(FilterSet: _ (list (Bot:)))))
(define-match-expander F-FS:
  (lambda (stx) #'(FilterSet: (list (Bot:)) _)))

#;
(d/c (combine-filter f1 f2 f3 t2 t3 o2 o3)
  (FilterSet/c FilterSet/c FilterSet/c Type? Type? Object? Object? . -> . tc-results?)
  (define (mk f) (ret (Un t2 t3) f (make-Empty)))
  (match* (f1 f2 f3)
    [((T-FS:) f _) (ret t2 f o2)]
    [((F-FS:) _ f) (ret t3 f o3)]
    ;; the student expansion
    [(f (T-FS:) (F-FS:)) (mk f)]
    ;; skipping the general or/predicate rule because it's really complicated
    ;; or/predicate special case for one elem lists
    ;; note that we are relying on equal? on identifiers here
    [((FilterSet: (list (TypeFilter: t pi x)) (list (NotTypeFilter: t pi x)))
      (T-FS:)
      (FilterSet: (list (TypeFilter: s pi x)) (list (NotTypeFilter: s pi x))))
     (mk (make-FilterSet (list (make-TypeFilter (Un t s) pi x)) (list (make-NotTypeFilter (Un t s) pi x))))]
    ;; or
    [((FilterSet: f1+ f1-) (T-FS:) (FilterSet: f3+ f3-)) (mk (combine null (append f1- f3-)))]
    ;; and
    [((FilterSet: f1+ f1-) (FilterSet: f2+ f2-) (F-FS:)) 
     (mk (combine (append f1+ f2+)		  
                  (append (for/list ([f f1-]
                                     #:when (not (null? f2+)))
			    (make-ImpFilter f2+ (list f)))
			  (for/list ([f f2-]
                                     #:when (not (null? f1+)))
			    (make-ImpFilter f1+ (list f))))))]
    [(f f* f*) (mk f*)]
    [(_ _ _)
     ;; could intersect f2 and f3 here
     (mk (make-FilterSet null null))]))
|#

;; (or/c Values? ValuesDots?) listof[identifier] -> tc-results?
(d/c (values->tc-results tc formals)
  ((or/c Values? ValuesDots?) . -> . tc-results?)
  (match tc
    [(ValuesDots: (list (Result: ts fs os) ...) dty dbound)
     (ret ts fs os dty dbound)]
    [(Values: (list (Result: ts fs os) ...))
     (ret ts fs os)]))

(define (tc-results->values tc)
  (match tc
    [(tc-results: ts) (-values ts)]))

(define (combine-props new-props old-props)
  (define-values (new-imps new-atoms) (partition ImpFilter? new-props))
  (define-values (derived-imps derived-atoms)
    (for/fold 
        ([derived-imps null]
         [derived-atoms null])
      ([o old-props])
      (match o
        [(ImpFilter: as cs)
         (let ([as* (remove* new-atoms as filter-equal?)])
           (if (null? as*)
               (values derived-imps (append cs new-atoms))
               (values (cons (make-ImpFilter as* cs) derived-imps) derived-atoms)))])))
  (values (append new-imps derived-imps) (append new-atoms derived-atoms)))