
(load-relative "loadtest.rktl")

(Section 'syntax)

;; ----------------------------------------

(test 0 'with-handlers (with-handlers () 0))
(test 1 'with-handlers (with-handlers ([void void]) 1))
(test 2 'with-handlers (with-handlers ([void void]) 1 2))
(test 'zero 'zero
      (with-handlers ((zero? (lambda (x) 'zero)))
		     (raise 0)))
(test 'zero 'zero
      (with-handlers ((zero? (lambda (x) 'zero))
		      (positive? (lambda (x) 'positive)))
		     (raise 0)))
(test 'positive 'positive
      (with-handlers ((zero? (lambda (x) 'zero))
		      (positive? (lambda (x) 'positive)))
		     (raise 1)))
(test 5 'with-handlers
      (with-handlers ([void (lambda (x) 5)])
	(with-handlers ((zero? (lambda (x) 'zero)))
	  (/ 0))))

(error-test #'(with-handlers ()
	         (/ 0))
	    exn:fail:contract:divide-by-zero?)
(error-test #'(with-handlers ((zero? (lambda (x) 'zero)))
		 (/ 0))
	    exn:application:type?)
(error-test #'(with-handlers ((zero? (lambda (x) 'zero))
			     (boolean? (lambda (x) 'boolean)))
		 (/ 0))
	    exn:application:type?)

(syntax-test #'with-handlers)
(syntax-test #'(with-handlers))
(syntax-test #'(with-handlers . 1))
(syntax-test #'(with-handlers ((zero? (lambda (x) 'zero)))))
(syntax-test #'(with-handlers ((zero? (lambda (x) 'zero))) . 1))
(syntax-test #'(with-handlers (zero?) 1))
(syntax-test #'(with-handlers ((zero?)) 1))
(syntax-test #'(with-handlers ((zero? . zero?)) 1))
(syntax-test #'(with-handlers ((zero? zero?) . 2) 1))
(syntax-test #'(with-handlers ((zero? zero?) zero?) 1))
(syntax-test #'(with-handlers ((zero? zero?) (zero?)) 1))
(syntax-test #'(with-handlers ((zero? zero?) (zero?)) 1))
(syntax-test #'(with-handlers ((zero? zero? zero?)) 1))
(syntax-test #'(with-handlers ((zero? zero? . zero?)) 1))
(syntax-test #'(with-handlers ((zero? zero?)) 1 . 2))

(error-test #'(with-handlers ((0 void)) (/ 0)) 
	    exn:application:type?)
(error-test #'(with-handlers ((void 0)) (/ 0))
	    exn:application:type?)
(error-test #'(with-handlers ((unbound-variable void)) 0)
	    exn:fail:contract:variable?)
(error-test #'(with-handlers ((void unbound-variable)) 0)
	    exn:fail:contract:variable?)
(error-test #'(with-handlers (((values 1 2) void)) 0)
	    arity?)
(error-test #'(with-handlers ((void (values 1 2))) 0)
	    arity?)

(test-values '(1 2) (lambda () (with-handlers ([void void])
				 (values 1 2))))

(test '(quote a) 'quote (quote 'a))
(test '(quote a) 'quote ''a)
(syntax-test #'quote)
(syntax-test #'(quote))
(syntax-test #'(quote 1 2))

(test 12 (if #f + *) 3 4)
(syntax-test #'(+ 3 . 4))

(test 8 (lambda (x) (+ x x)) 4)
(define reverse-subtract
  (lambda (x y) (- y x)))
(test 3 reverse-subtract 7 10)
(define add4
  (let ((x 4))
    (lambda (y) (+ x y))))
(test 10 add4 6)
(test (letrec([x x]) x) 'lambda (let ([x (lambda () (define d d) d)]) (x)))
(test (letrec([x x]) x) 'lambda ((lambda () (define d d) d)))
(test '(3 4 5 6) (lambda x x) 3 4 5 6)
(test '(5 6) (lambda (x y . z) z) 3 4 5 6)
(test 'second (lambda () (cons 'first 2) 'second))
(syntax-test #'lambda)
(syntax-test #'(lambda))
(syntax-test #'(lambda x))
(syntax-test #'(lambda ()))
(syntax-test #'(lambda () (begin)))
(syntax-test #'(lambda . x))
(syntax-test #'(lambda x . x))
(syntax-test #'(lambda x . 5))
(syntax-test #'(lambda ((x)) x))
(syntax-test #'(lambda 5 x))
(syntax-test #'(lambda (5) x))
(syntax-test #'(lambda (x (y)) x))
(syntax-test #'(lambda (x . 5) x))
(syntax-test #'(lambda (x) x . 5))

(let ([f
       (case-lambda
	[() 'zero]
	[(x) (cons 1 1) 'one]
	[(x y) 'two]
	[(x y z . rest) 'three+]
	[x 'bad])]
      [g
       (case-lambda
	[(x y z) 'three]
	[(x y) (cons 2 2) 'two]
	[(x) 'one]
	[() 'zero]
	[x (cons 0 'more!) 'more])]
      [h
       (case-lambda
	[(x y) 'two]
	[(x y z w) 'four])])
  (test 'zero f)
  (test 'one f 1)
  (test 'two f 1 2)
  (test 'three+ f 1 2 3)
  (test 'three+ f 1 2 3 4)
  (test 'three+ f 1 2 3 4 5 6 7 8 9 10)

  (test 'zero g)
  (test 'one g 1)
  (test 'two g 1 2)
  (test 'three g 1 2 3)
  (test 'more g 1 2 3 4 5 6 7 8 9 10)

  (test 'two h 1 2)
  (test 'four h 1 2 3 4)
  (let ([h '(case-lambda
	     [(x y) 'two]
	     [(x y z w) 'four])])
    (error-test (datum->syntax #f (list h) #f) arity?)
    (error-test (datum->syntax #f (list* h '(1)) #f) arity?)
    (error-test (datum->syntax #f (list* h '(1 2 3)) #f) arity?)
    (error-test (datum->syntax #f (list* h '(1 2 3 4 5 6)) #f) arity?)))

(error-test #'((case-lambda)) arity?)

(syntax-test #'case-lambda)
(syntax-test #'(case-lambda . 1))
(syntax-test #'(case-lambda []))
(syntax-test #'(case-lambda 1))
(syntax-test #'(case-lambda x))
(syntax-test #'(case-lambda [x]))
(syntax-test #'(case-lambda [x 8][y]))
(syntax-test #'(case-lambda [x][y 9]))
(syntax-test #'(case-lambda [8 8]))
(syntax-test #'(case-lambda [((x)) 8]))
(syntax-test #'(case-lambda [(8) 8]))
(syntax-test #'(case-lambda [(x . 9) 8]))
(syntax-test #'(case-lambda [x . 8]))
(syntax-test #'(case-lambda [(x) . 8]))
(syntax-test #'(case-lambda . [(x) 8]))
(syntax-test #'(case-lambda [(x) 8] . y))
(syntax-test #'(case-lambda [(x) 8] . [y 7]))
(syntax-test #'(case-lambda [(x) 8] [8 7]))
(syntax-test #'(case-lambda [(x) 8] [((y)) 7]))
(syntax-test #'(case-lambda [(x) 8] [(8) 7]))
(syntax-test #'(case-lambda [(x) 8] [(y . 8) 7]))
(syntax-test #'(case-lambda [(x) 8] [y . 7]))
(syntax-test #'(case-lambda [(x) 8] [(y) . 7]))
(syntax-test #'(case-lambda [(x x) 8] [(y) 7]))
(syntax-test #'(case-lambda [(x . x) 8] [(y) 7]))
(syntax-test #'(case-lambda [(y) 7] [(x x) 8]))
(syntax-test #'(case-lambda [(y) 7] [(x . x) 8]))

(test 'yes 'if (if (> 3 2) 'yes 'no))
(test 'no 'if (if (> 2 3) 'yes 'no))
(test '1 'if (if (> 3 2) (- 3 2) (+ 3 2)))
(test-values '(1 2) (lambda () (if (cons 1 2) (values 1 2) 0)))
(test-values '(1 2) (lambda () (if (not (cons 1 2)) 0 (values 1 2))))
(syntax-test #'if)
(syntax-test #'(if))
(syntax-test #'(if . #t))
(syntax-test #'(if #t . 1))
(syntax-test #'(if #t 1 . 2))
(syntax-test #'(if #t))
(syntax-test #'(if #t 1))
(syntax-test #'(if #t 1 2 3))
(syntax-test #'(if #t 1 2 . 3))
(error-test #'(if (values 1 2) 3 4) arity?)

(test (void) 'when (when (> 1 2) 0))
(test (void) 'when (when (> 1 2) (cons 1 2) 0))
(test 0 'when (when (< 1 2) 0))
(test 0 'when (when (< 1 2) (cons 1 2) 0))
(test-values '(0 10) (lambda () (when (< 1 2) (values 0 10))))
(syntax-test #'when)
(syntax-test #'(when))
(syntax-test #'(when . 1))
(syntax-test #'(when 1))
(syntax-test #'(when 1 . 2))
(error-test #'(when (values 1 2) 0) arity?)

(test (void) 'unless (unless (< 1 2) 0))
(test (void) 'unless (unless (< 1 2) (cons 1 2) 0))
(test 0 'unless (unless (> 1 2) 0))
(test 0 'unless (unless (> 1 2) (cons 1 2) 0))
(test-values '(0 10) (lambda () (unless (> 1 2) (values 0 10))))
(syntax-test #'unless)
(syntax-test #'(unless))
(syntax-test #'(unless . 1))
(syntax-test #'(unless 1))
(syntax-test #'(unless 1 . 2))
(error-test #'(unless (values 1 2) 0) arity?)

(define x 2)
(test 3 'define (+ x 1))
(set! x 4)
(test 5 'set! (+ x 1))
(syntax-test #'set!)
(syntax-test #'(set!))
(syntax-test #'(set! x))
(syntax-test #'(set! x 1 2))
(syntax-test #'(set! 1 2))
(syntax-test #'(set! (x) 1))
(syntax-test #'(set! . x))
(syntax-test #'(set! x . 1))
(syntax-test #'(set! x 1 . 2))

(define (set!-not-ever-defined) (set! not-ever-defined (add1 not-ever-defined)))
(err/rt-test (set!-not-ever-defined) exn:fail:contract:variable?)

(set!-values (x) 9)
(test 9 'set!-values x)
(test (void) 'set!-values (set!-values () (values)))
(syntax-test #'set!-values)
(syntax-test #'(set!-values))
(syntax-test #'(set!-values . x))
(syntax-test #'(set!-values x))
(syntax-test #'(set!-values 8))
(syntax-test #'(set!-values (x)))
(syntax-test #'(set!-values (x) . 0))
(syntax-test #'(set!-values x 0))
(syntax-test #'(set!-values (x . y) 0))
(syntax-test #'(set!-values (x . 8) 0))
(syntax-test #'(set!-values (x 8) 0))
(syntax-test #'(set!-values (x) 0 1))
(syntax-test #'(set!-values (x) 0 . 1))
(syntax-test #'(set!-values (x x) 0))
(syntax-test #'(set!-values (x y x) 0))
(syntax-test #'(set!-values (y x x) 0))

(error-test #'(set!-values () 1) arity?)
(error-test #'(set!-values () (values 1 2)) arity?)
(error-test #'(set!-values (x) (values)) arity?)
(error-test #'(set!-values (x) (values 1 2)) arity?)
(error-test #'(set!-values (x y) 1) arity?)
(error-test #'(set!-values (x y) (values 1 2 3)) arity?)

(error-test #'(set! unbound-variable 5) exn:fail:contract:variable?)

(test 'greater 'cond (cond ((> 3 2) 'greater)
			   ((< 3 2) 'less)))
(test 'equal 'cond (cond ((> 3 3) 'greater)
			 ((< 3 3) 'less)
			 (else 'equal)))
(test 2 'cond (cond ((assv 'b '((a 1) (b 2))) => cadr)
		     (else #f)))
(test #f 'cond (cond ((assv 'z '((a 1) (b 2))) => cadr)
		     (else #f)))
(syntax-test #'(cond ((assv 'z '((a 1) (b 2))) => cadr)
		    (else 8)
		    (else #f)))
(test #f 'cond (let ([else #f])
		 (cond ((assv 'z '((a 1) (b 2))) => cadr)
		       (else 8)
		       (#t #f))))
(test 'second 'cond (cond ((< 1 2) (cons 1 2) 'second)))
(test 'second-again 'cond (cond ((> 1 2) 'ok) (else (cons 1 2) 'second-again)))
(test 1 'cond (cond (1)))
(test 1 'cond (cond (#f) (1)))
(test 1 'cond (cond (#f 7) (1)))
(test 2 'cond (cond (#f 7) (1 => add1)))
(test add1 'cond (let ([=> 9]) (cond (#f 7) (1 => add1))))
(non-z '(test 0 'case (case (* 2 3)
		(6 0)
		(else 7))))
(test 'composite 'case (case (* 2 3)
			 ((2 3 5 7) 'prime)
			 ((1 4 6 8 9) 'composite)))
(test 'consonant 'case (case (car '(c d))
			 ((a e i o u) 'vowel)
			 ((w y) 'semivowel)
			 (else 'consonant)))
(test 'second 'case (case 10
		      [(10) (cons 1 2) 'second]
		      [else 5]))
(test 'second-again 'case (case 11
			    [(10) (cons 1 2) 'second]
			    [else (cons 1 2) 'second-again]))
(test-values '(10 9) (lambda ()
		       (cond
			[(positive? 0) 'a]
			[(positive? 10) (values 10 9)]
			[else #f])))
(test-values '(10 9) (lambda ()
		       (case (string->symbol "hello")
			[(bye) 'a]
			[(hello) (values 10 9)]
			[else #f])))
(error-test #'(cond [(values 1 2) 8]) arity?)
(error-test #'(case (values 1 2) [(a) 8]) arity?)

(test #t 'and (and (= 2 2) (> 2 1)))
(test #f 'and (and (= 2 2) (< 2 1)))
(test '(f g) 'and (and 1 2 'c '(f g)))
(test #t 'and (and))
(test-values '(1 12) (lambda () (and (cons 1 2) (values 1 12))))
(test #t 'or (or (= 2 2) (> 2 1)))
(test #t 'or (or (= 2 2) (< 2 1)))
(test #f 'or (or #f #f #f))
(test #f 'or (or))
(test '(b c) 'or (or (memq 'b '(a b c)) (+ 3 0)))
(test-values '(1 12) (lambda () (or (not (cons 1 2)) (values 1 12))))
(syntax-test #'(cond #t))
(syntax-test #'(cond ())  )
(syntax-test #'(cond (1 =>))  )
(syntax-test #'(cond (1 => 3 4))  )
(syntax-test #'(cond . #t))
(syntax-test #'(cond (#t . 1)))
(syntax-test #'(cond (#t 1) #f))
(syntax-test #'(cond (#t 1) . #f))
(error-test #'(cond ((values #t #f) 1)) arity?)
(syntax-test #'case)
(syntax-test #'(case))
(syntax-test #'(case 0 #t))
(syntax-test #'(case . 0))
(syntax-test #'(case 0 . #t))
(syntax-test #'(case 0 (0 #t)))
(syntax-test #'(case 0 ()))
(syntax-test #'(case 0 (0)))
(syntax-test #'(case 0 (0 . 8)))
(syntax-test #'(case 0 ((0 . 1) 8)))
(syntax-test #'(case 0 (0 8) #f))
(syntax-test #'(case 0 (0 8) . #f))
(syntax-test #'(case 0 (else 1) (else 2)))
(syntax-test #'(case 0 ((0) =>)))
(syntax-test #'=>)
(syntax-test #'else)
(syntax-test #'(and . 1))
(syntax-test #'(and 1 . 2))
(syntax-test #'(or . 1))
(syntax-test #'(or 1 . 2))
(error-test #'(and #t (values 1 2) 8) arity?)
(error-test #'(or #f (values 1 2) 8) arity?)

(test 6 'let (let ((x 2) (y 3)) (* x y)))
(test 'second 'let (let ((x 2) (y 3)) (* x y) 'second))
(test 6 'let-values (let-values (((x) 2) ((y) 3)) (* x y)))
(test 6 'let-values (let-values (((x y) (values 2 3))) (* x y)))
(test 35 'let (let ((x 2) (y 3)) (let ((x 7) (z (+ x y))) (* z x))))
(test 35 'let-values (let-values (((x y) (values 2 3))) (let-values (((x) 7) ((z) (+ x y))) (* z x))))
(test 70 'let* (let ((x 2) (y 3)) (let* ((x 7) (z (+ x y))) (* z x))))
(test 70 'let*-values (let ((x 2) (y 3)) (let*-values (((x) 7) ((z) (+ x y))) (* z x))))
(test #t 'letrec (letrec ((-even?
                           (lambda (n) (if (zero? n) #t (-odd? (- n 1)))))
                          (-odd?
                           (lambda (n) (if (zero? n) #f (-even? (- n 1))))))
                   (-even? 88)))
(test #t 'letrec-values (letrec-values (((-even? -odd?)
					 (values
					  (lambda (n) (if (zero? n) #t (-odd? (- n 1))))
					  (lambda (n) (if (zero? n) #f (-even? (- n 1)))))))
				       (-even? 88)))
(define x 34)
(test 5 'let (let ((x 3)) (define x 5) x))
(test 5 'let (let ((x 3)) (define-values (x w) (values 5 8)) x))
(test 34 'let x)
(test 6 'let (let () (define x 6) x))
(test 34 'let x)
(test 7 'let* (let* ((x 3)) (define x 7) x))
(test 34 'let* x)
(test 8 'let* (let* () (define x 8) x))
(test 34 'let* x)
(test 9 'letrec (letrec () (define x 9) x))
(test 34 'letrec x)
(test 10 'letrec (letrec ((x 3)) (define x 10) x))
(test 34 'letrec x)
(teval '(test 5 'letrec (letrec ((x 5)(y x)) y)))
(test 3 'let (let ((y 'apple) (x 3) (z 'banana)) x))
(test 3 'let* (let* ((y 'apple) (x 3) (z 'banana)) x))
(test 3 'letrec (letrec ((y 'apple) (x 3) (z 'banana)) x))
(test 3 'let* (let* ((x 7) (y 'apple) (z (set! x 3))) x))
(test 3 'let* (let* ((x 7) (y 'apple) (z (if (not #f) (set! x 3) #f))) x))
(test 3 'let* (let* ((x 7) (y 'apple) (z (if (not #t) #t (set! x 3)))) x))
(test 3 'let-values (let-values (((y x z) (values 'apple 3 'banana))) x))
(test 3 'let*-values (let*-values (((y x z) (values 'apple 3 'banana))) x))
(test 3 'letrec-values (letrec-values (((y x z) (values 'apple 3 'banana))) x))
(test 3 'let*-values (let*-values (((x y) (values 7 'apple)) ((z) (set! x 3))) x))
(test 3 'let*-values (let*-values (((x y) (values 7 'apple)) ((z) (if (not #f) (set! x 3) #f))) x))
(test 3 'let*-values (let*-values (((x y) (values 7 'apple)) ((z) (if (not #t) #t (set! x 3)))) x))
(test 1 'named-let-scope (let ([f add1]) (let f ([n (f 0)]) n)))

(test-values '(3 4) (lambda () (let ([x 3][y 4]) (values x y))))
(test-values '(3 -4) (lambda () (let loop ([x 3][y -4]) (values x y))))
(test-values '(3 14) (lambda () (let* ([x 3][y 14]) (values x y))))
(test-values '(3 24) (lambda () (letrec ([x 3][y 24]) (values x y))))
(test-values '(3 54) (lambda () (let-values ([(x y) (values 3 54)]) (values x y))))
(test-values '(3 64) (lambda () (let*-values ([(x y) (values 3 64)]) (values x y))))
(test-values '(3 74) (lambda () (letrec-values ([(x y) (values 3 74)]) (values x y))))

(test 'one 'let-values (let-values ([() (values)]) 'one))
(test 'two 'let*-values (let*-values ([() (values)]) 'two))
(test 'three 'letrec-values (letrec-values ([() (values)]) 'three))
(test 'onex 'let-values (let-values ([() (values)][() (values)]) 'onex))
(test 'twox 'let*-values (let*-values ([() (values)][() (values)]) 'twox))
(test 'threex 'letrec-values (letrec-values ([() (values)][() (values)]) 'threex))

(test '(10 11) 'letrec-values (letrec-values ([(names kps)
					       (letrec ([oloop 10])
						 (values oloop (add1 oloop)))])
					     (list names kps)))

(define (error-test-let/no-* expr)
  (syntax-test (datum->syntax #f (cons 'let expr) #f))
  (syntax-test (datum->syntax #f (cons 'let (cons 'name expr)) #f))
  (syntax-test (datum->syntax #f (cons 'letrec expr) #f)))
(define (error-test-let expr)
  (error-test-let/no-* expr)
  (syntax-test (datum->syntax #f (cons 'let* expr) #f)))
(error-test-let #'x)
(error-test-let #'(x))
(error-test-let #'(()))
(error-test-let #'(x ()))
(syntax-test #'(let* x () 1))
(syntax-test #'(letrec x () 1))
(error-test-let #'(x . 1))
(error-test-let #'(() . 1))
(error-test-let #'(((x 1))))
(error-test-let #'(((x 1)) . 1))
(error-test-let #'(((x . 1)) 1))
(error-test-let #'(((1 1)) 1))
(error-test-let #'(((x 1) 1)  1))
(error-test-let #'(((x 1) . 1)  1))
(error-test-let #'(((x 1 1)) 1))
(error-test-let #'(((x 1 1)) 1))
(error-test-let #'(((x 1)) 1 . 2))
(error-test-let/no-* #'(((x 1) (x 2)) 1))
(error-test-let/no-* #'(((x 1) (y 3) (x 2)) 1))
(error-test-let/no-* #'(((y 3) (x 1) (x 2)) 1))
(error-test-let/no-* #'(((x 1) (x 2) (y 3)) 1))
(test 5 'let* (let* ([x 4][x 5]) x))
(error-test-let #'(() (define x 10)))
(error-test-let #'(() (define x 10) (define y 20)))

(define (do-error-test-let-values/no-* expr syntax-test)
  (syntax-test (datum->syntax #f (cons 'let-values expr) #f))
  (syntax-test (datum->syntax #f (cons 'letrec-values expr) #f)))
(define (do-error-test-let-values expr syntax-test)
  (do-error-test-let-values/no-* expr syntax-test)
  (syntax-test (datum->syntax #f (cons 'let*-values expr) #f)))
(define (error-test-let-values/no-* expr)
  (do-error-test-let-values/no-* expr syntax-test))
(define (error-test-let-values expr)
  (do-error-test-let-values expr syntax-test))
(error-test-let-values #'x)
(error-test-let-values #'(x))
(error-test-let-values #'(()))
(error-test-let-values #'(x ()))
(syntax-test #'(let*-values x () 1))
(syntax-test #'(letrec-values x () 1))
(error-test-let-values #'(x . 1))
(error-test-let-values #'(() . 1))
(error-test-let-values #'((((x) 1))))
(error-test-let-values #'((((x) 1)) . 1))
(error-test-let-values #'((((x) . 1)) 1))
(error-test-let-values #'((((1) 1)) 1))
(error-test-let-values #'((((x 1) 1)) 1))
(error-test-let-values #'((((1 x) 1)) 1))
(error-test-let-values #'((((x) 1) . 1)  1))
(error-test-let-values #'((((x) 1 1)) 1))
(error-test-let-values #'((((x . y) 1)) 1))
(error-test-let-values #'((((x . 1) 1)) 1))
(error-test-let-values #'((((x) 1)) 1 . 2))
(error-test-let-values #'((((x x) 1)) 1))
(error-test-let-values #'((((y) 0) ((x x) 1)) 1))
(error-test-let-values #'((((x x) 1) ((y) 0)) 1))
(error-test-let-values/no-* #'((((x) 1) ((x) 2)) 1))
(error-test-let-values/no-* #'((((x) 1) ((y) 3) ((x) 2)) 1))
(error-test-let-values/no-* #'((((y) 3) ((x) 1) ((x) 2)) 1))
(error-test-let-values/no-* #'((((x) 1) ((x) 2) ((y) 3)) 1))
(test 5 'let* (let*-values ([(x) 4][(x) 5]) x))

(do-error-test-let-values #'((((x y) 1)) 1) (lambda (x) (error-test x arity?)))
(do-error-test-let-values #'((((x) (values 1 2))) 1) (lambda (x) (error-test x arity?)))
(do-error-test-let-values #'(((() (values 1))) 1) (lambda (x) (error-test x arity?)))
(do-error-test-let-values #'((((x) (values))) 1) (lambda (x) (error-test x arity?)))

(test 5 'embedded (let () (define y (lambda () x)) (define x 5) (y)))

(let ([wrap (lambda (body)
	      (syntax-test (datum->syntax #f `(let () ,@body) #f))
	      (syntax-test (datum->syntax #f `(let () (begin ,@body)) #f)))])
  (wrap '((define x 7) (define x 8) x))
  (wrap '((define 3 8) x))
  (wrap '((define-values x 8) x)))

(let ([wrap
       (lambda (val body)
	 (teval `(test ,val 'let-begin (let () ,@body)))
	 (teval `(test ,val 'let-begin (let ([xyzw 12]) ,@body)))
	 (teval `(test ,val (lambda () ,@body)))
	 (teval `(test ,val 'parameterize-begin
		       (parameterize () ,@body)))
	 (teval `(test ,val 'parameterize-begin
		       (parameterize ([current-directory (current-directory)])
			 ,@body)))
	 (teval `(test ,val 'with-handlers-begin
		       (with-handlers () ,@body)))
	 (teval `(test ,val 'with-handlers-begin
		       (with-handlers ([void void]) ,@body)))
	 (syntax-test (datum->syntax #f `(when (positive? 1) ,@body) #f))
	 (syntax-test (datum->syntax #f `(unless (positive? -1) ,@body) #f))
	 (syntax-test (datum->syntax #f `(cond [(positive? 1) ,@body][else #f]) #f))
	 (syntax-test (datum->syntax #f `(cond [(positive? -1) 0][else ,@body]) #f))
	 (syntax-test (datum->syntax #f `(case (positive? 1) [(#t) ,@body][else -12]) #f))
	 (syntax-test (datum->syntax #f `(cond [#t ,@body]) #f))
	 (syntax-test (datum->syntax #f `(do ((x 1)) (#t ,@body) ,@body) #f))
	 (syntax-test (datum->syntax #f `(begin0 12 ,@body) #f)))])
  (wrap 5 '((begin (define x 5)) x))
  (wrap 5 '((begin (define x 5) x)))
  (wrap 15 '((begin (define x 5)) (begin (define y (+ x 10)) y)))
  (wrap 13 '((begin) 13))
  (wrap 7 '((begin) (begin) (begin (define x 7) (begin) x)))
  (wrap 7 '((begin (begin (begin (define x 7) (begin) x))))))

(define x 0)
(define (test-begin bg nested-bg)
  (let* ([make-args
	  (lambda (bg b)
	    (if (eq? bg 'begin)
		b
		(let* ([len (length b)]
		       [last (list-ref b (sub1 len))])
		  (cons last
			(let loop ([l b])
			  (if (null? (cdr l))
			      null
			      (cons (car l) (loop (cdr l)))))))))]
	 [test-bg
	  (lambda (v b)
	    (let* ([args (make-args bg b)]
		   [expr (cons bg args)])
	      (printf "~s:~n" expr)
	      (teval `(test ,v (quote ,bg) ,expr))))]
	 [make-bg
	  (lambda (b)
	    (cons nested-bg (make-args nested-bg b)))]
	 [make-test-bg-d
	  (lambda (bg)
	    (lambda (v1 v2 b)
	      (test-bg (if (eq? bg 'begin)
			   v1
			   v2)
		       b)))]
	 [test-bg-d (make-test-bg-d bg)]
	 [test-bg-d2 (make-test-bg-d nested-bg)])
  (teval '(set! x 0))
  (test-bg-d 6 1 '((set! x 5) (+ x 1)))
  (test-bg 5 '(5))
  (test-bg 3 '(2 3))
  (test-bg 3 `(2 (,bg 3)))
  (test-bg 3 `(,(make-bg '(2)) ,(make-bg '(3))))
  (test-bg-d 7 6 '((set! x 6) 'a (+ x 1)))
  (test-bg ''w '((set! x 6) 'a (+ x 1) 'w))
  (test-bg-d 8 7 '('b (set! x 7) (+ x 1)))
  (test-bg-d 9 8 '('b (set! x 8) 'a (+ x 1)))
  (test-bg ''z '('b (set! x 8) 'a (+ x 1) 'z))
  (test-bg-d 7 9 `(,(make-bg '((set! x 6) 'a)) (+ x 1)))
  (test-bg 10 `(,(make-bg '((set! x 60) 'a)) 10))
  (teval '(test 60 'x x))
  (test-bg 10 `(,(make-bg '((set! x 65) 'a)) (add1 20) 10))
  (teval '(test 65 'x x))
  (test-bg ''a `(10 ,(make-bg '((set! x 66) 'a))))
  (teval '(test 66 'x x))
  (test-bg ''a `(10 (add1 32) ,(make-bg '((set! x 67) 'a))))
  (teval '(test 67 'x x))
  (teval '(set! x 6))
  (test-bg-d 8 7 `(,(make-bg '('b (set! x 7) 'a)) (+ x 1)))
  (test-bg-d 9 8 `(,(make-bg '('b (set! x 8))) ,(make-bg '('a (+ x 1)))))
  (test-bg-d2 10 9 `(,(make-bg `(,(make-bg `('b (set! x 9) ,(make-bg '('a (+ x 1)))))))))
  (test-bg ''s `(,(make-bg `(,(make-bg `('b (set! x 9) ,(make-bg '('a (+ x 1) 's))))))))
  (test-bg ''t `(,(make-bg `(,(make-bg `('b (set! x 9) ,(make-bg '('a (+ x 1))))))) 't))
  (teval `(test 5 call-with-values (lambda () ,(make-bg '((values 1 2) (values 1 3 1)))) +))
  (syntax-test (datum->syntax #f `(,bg . 1) #f))
  (syntax-test (datum->syntax #f `(,bg 1 . 2) #f))))

(test-begin 'begin 'begin)
(test-begin 'begin0 'begin)
(test-begin 'begin0 'begin0)
(test-begin 'begin 'begin0)

(syntax-test #'(begin0))
(begin) ; must succeed, but we can't wrap it

(test 4 'implicit-begin (let ([x 4][y 7]) 'y x))
(test 4 'implicit-begin (let ([x 4][y 7]) y x))

(test 5 'implicit-begin (let () (begin) 10 5))

(error-test #'(begin (define foo (let/cc k k)) (foo 10)) exn:application:type?) ; not exn:application:continuation?

(define f-check #t)
(define f (delay (begin (set! f-check #f) 5)))
(test #t (lambda () f-check))
(test 5 force f)
(test #f (lambda () f-check))
(test 5 force f)
(define f-check-2 (delay (values 1 5)))
(test-values '(1 5) (lambda () (force f-check-2)))
(values 1 2)
(test-values '(1 5) (lambda () (force f-check-2)))
(syntax-test #'delay)
(syntax-test #'(delay))
(syntax-test #'(delay . 1))
(syntax-test #'(delay 1 . 2))

(test '(list 3 4) 'quasiquote `(list ,(+ 1 2) 4))
(test '(list a (quote a)) 'quasiquote (let ((name 'a)) `(list ,name ',name)))
(test '(a 3 4 5 6 b) 'quasiquote `(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b))
(test '((foo 7) . cons)
	'quasiquote
	`((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons))))
(test '#(10 5 2 4 3 8) 'quasiquote `#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8))
(test 5 'quasiquote `,(+ 2 3))
(test '(a `(b ,(+ 1 2) ,(foo 4 d) e) f)
      'quasiquote `(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f))
(test '(a `(b ,x ,'y d) e) 'quasiquote
	(let ((name1 'x) (name2 'y)) `(a `(b ,,name1 ,',name2 d) e)))
(test '(list 3 4) 'quasiquote (quasiquote (list (unquote (+ 1 2)) 4)))
(test '`(list ,(+ 1 2) 4) 'quasiquote '(quasiquote (list (unquote (+ 1 2)) 4)))
(test '(()) 'qq `((,@'())))
(define x 5)
(test '(quasiquote (unquote x)) 'qq ``,x)
(test '(quasiquote (unquote 5)) 'qq ``,,x)
(test '(quasiquote (unquote-splicing x)) 'qq ``,@x)
(test '(quasiquote (unquote-splicing 5)) 'qq ``,@,x)
(test '(quasiquote (quasiquote (quasiquote (unquote (unquote (unquote x)))))) 'qq ````,,,x)
(test '(quasiquote (quasiquote (quasiquote (unquote (unquote (unquote 5)))))) 'qq ````,,,,x)

(test '#hash() 'qq `#hash())
(test '#hash(("apple" . 1) ("banana" . 2) ("coconut" . 3))
      'qq
      `#hash(("apple" . 1) ("banana" . 2) ("coconut" . 3)))
(test '#hash(("apple" . 1) ("banana" . 2) ("coconut" . 3))
      'qq
      `#hash(("apple" . ,1) ("banana" . ,(add1 1)) ("coconut" . ,(+ 1 2))))
(test '#hash(("foo" . (1 2 3 4 5)))
      'qq
      `#hash(("foo" . (1 2 ,(+ 1 2) 4 5))))
(test '#hash(("foo" . (1 2 (+ 1 2) 4 5)))
      'qq
      `#hash(("foo" . (1 2 (+ 1 2) 4 5))))
(test '#hash(("foo" . (1 2 3 4 5)))
      'qq
      `#hash(("foo" . (1 2 ,@(list 3 4 5)))))
(test '#hash((,(read) . 1) (,(+ 1 2) . 3))
      'qq
      `#hash((,(read) . 1) (,(+ 1 2) . ,(+ 1 2))))
(test '#hash((,(read) . 2))
      'qq
      `#hash((,(read) . 1) (,(read) . 2)))
(test '#hash(("moo" . 3) ("foo" . (1 2)))
      'qq
      `#hash(("moo" . ,(+ 1 2)) ("foo" . (1 2))))
(test '#hash(("moo" . (+ 1 2)) ("foo" . -1))
      'qq
      `#hash(("moo" . (+ 1 2)) ("foo" . ,(- 1 2))))
(syntax-test #'`#hash(("foo" . ,@(list 1 2 3 4 5))))
(error-test #'(read (open-input-string "`#hash((foo ,@(list 1 2 3 4 5)))")) exn:fail:read?)

(test '(quasiquote (unquote result)) 'qq `(quasiquote ,result))
(test (list 'quasiquote car) 'qq `(,'quasiquote ,car))

(syntax-test #'quasiquote)
(syntax-test #'(quasiquote))
(syntax-test #'(quasiquote . 5))
(syntax-test #'(quasiquote 1 . 2))
(syntax-test #'(quasiquote 1 2))
(syntax-test #'(unquote 7))
(syntax-test #'(unquote-splicing 7))

(syntax-test #'`(1 . ,@5))
(test (cons 1 5) 'qq `(1 ,@5))
(error-test #'`(1 ,@5 2))

(define (qq-test e)
  (syntax-test (datum->syntax #f e #f))
  (syntax-test (datum->syntax #f (list 'quasiquote e) #f))
  (syntax-test (datum->syntax #f (list 'quasiquote e) #f))
  (syntax-test (datum->syntax #f (list 'quasiquote (list 'quasiquote e)) #f))
  (syntax-test (datum->syntax #f (list 'quasiquote (list 'quasiquote (list 'unquote e))) #f))
  (syntax-test (datum->syntax #f (list 'quasiquote (list 'quasiquote (list 'unquote-splicing e))) #f)))
(qq-test #'(unquote))
(qq-test #'(unquote 7 8 9))
(qq-test #'(unquote-splicing))
(qq-test #'(unquote-splicing 7 8 9))

(test '(unquote . 5) 'qq (quasiquote (unquote . 5)))
(test '(unquote 1 . 5) 'qq (quasiquote (unquote 1 . 5)))
(test '(unquote 1 2 . 5) 'qq (quasiquote (unquote 1 2 . 5)))

(test '(unquote 1 2 7 . 5) 'qq (quasiquote (unquote 1 2 ,(+ 3 4) . 5)))
(test '(unquote 1 2 (unquote (+ 3 4)) . 5) 'qq (quasiquote (unquote 1 2 ,',(+ 3 4) . 5)))

(test '(1 2 3 4 . 5) 'qq `(1 ,@'(2 3 4) . 5))

(error-test #'`(10 ,(values 1 2)) arity?)
(error-test #'`(10 ,@(values 1 2)) arity?)

(define add3 (lambda (x) (+ x 3)))
(test 6 'define (add3 3))
(define (add3 x) (+ x 3))
(test 6 'define (add3 3))
(define first car)
(test 1 'define (first '(1 2)))
(syntax-test #'define)
(syntax-test #'(define))
(syntax-test #'(define . x))
(syntax-test #'(define x))
(syntax-test #'(define x . 1))
(syntax-test #'(define 1 2))
(syntax-test #'(define (1) 1))
(syntax-test #'(define (x 1) 1))
(syntax-test #'(define (x a a) 1))
(syntax-test #'(define ((x 1) a) 1))
(syntax-test #'(define ((x b b) a) 1))
(syntax-test #'(define x 1 . 2))
(syntax-test #'(define x 1 2))

(let ()
  (define ((f x) y z) (list x y z))
  (test '(1 2 3) (f 1) 2 3))
(let ()
  (define ((g a) a b) (list a b))
  (test '(2 3) (g 1) 2 3))

(define-values (add3) (lambda (x) (+ x 3)))
(test 6 'define (add3 3))
(define-values (add3 another) (values (lambda (x) (+ x 3)) 9))
(test 6 'define (add3 3))
(test 9 'define another)
(define-values (first second third) (values car cadr caddr))
(test 1 'define (first '(1 2)))
(test 2 'define (second '(1 2)))
(test 3 'define (third '(1 2 3)))
(define-values () (values))
(syntax-test #'define-values)
(syntax-test #'(define-values))
(syntax-test #'(define-values . x))
(syntax-test #'(define-values x))
(syntax-test #'(define-values (x)))
(syntax-test #'(define-values x . 1))
(syntax-test #'(define-values (x) . 1))
(syntax-test #'(define-values 1 2))
(syntax-test #'(define-values (1) 2))
(syntax-test #'(define-values (x 1) 1))
(syntax-test #'(define-values (x . y) 1))
(syntax-test #'(define-values (x) 1 . 2))
(syntax-test #'(define-values (x) 1 2))
(syntax-test #'(define-values (x x) 10))
(syntax-test #'(define-values (x y x) 10))

(syntax-test #'((define x 2) 0 1))
(syntax-test #'(+ (define x 2) 1))
(syntax-test #'(if (define x 2) 0 1))
(syntax-test #'(begin0 (define x 2)))
(syntax-test #'(begin0 (define x 2) 0))
(syntax-test #'(begin0 0 (define x 2)))
(syntax-test #'(begin0 0 (define x 2) (define x 12)))
(syntax-test #'(let () (define x 2)))
(syntax-test #'(letrec () (define x 2)))
(syntax-test #'(lambda () (define x 2)))
(syntax-test #'(lambda () (void (define x 2)) 1))
(syntax-test #'(cond [(< 2 3) (define x 2)] [else 5]))
(syntax-test #'(cond [else (define x 2)]))
(syntax-test #'(cond [else (define x 2) 0]))

;; No good way to test in mzc:
(error-test #'(define x (values)) exn:application:arity?)
(error-test #'(define x (values 1 2)) exn:application:arity?)
(error-test #'(define-values () 3) exn:application:arity?)
(error-test #'(define-values () (values 1 3)) exn:application:arity?)
(error-test #'(define-values (x y) (values)) exn:application:arity?)
(error-test #'(define-values (x y) 3) exn:application:arity?)
(error-test #'(define-values (x y) (values 1 2 3)) exn:application:arity?)

(begin (define ed-t1 1) (define ed-t2 2))
(test 1 'begin-define ed-t1)
(test 2 'begin-define ed-t2)
(begin (begin (begin (begin 10 (define ed-t2.5 2.5) 12))))
(test 2.5 'begin-define ed-t2.5)
(syntax-test #'(if (zero? 0) (define ed-t3 3) (define ed-t3 -3)))
(syntax-test #'(if #t (define ed-t3 3) (define ed-t3 -3)))
(syntax-test #'(if #f (define ed-t3 3) (define ed-t3 -3)))

(test 45 'define
	(let ((x 5))
		(define foo (lambda (y) (bar x y)))
		(define bar (lambda (a b) (+ (* a b) a)))
		(foo (+ x 3))))
(define x 34)
(define (foo) (define x 5) x)
(test 5 foo)
(test 34 'define x)
(define foo (lambda () (define x 5) x))
(test 5 foo)
(test 34 'define x)
(define (foo x) ((lambda () (define x 5) x)) x)
(test 88 foo 88)
(test 4 foo 4)
(test 34 'define x)

(test 5 'define
      (let ()
	(define x 5)
	(define define (lambda (a b) (+ a b)))
	8
	(define x 7)
	x))
(test 8 'define ; used to be 6
      (let ([y 8])
	(define (define z w) 5)
	(define y 6)
	y))

(syntax-test #'(let ()
		(define x 5)))
(syntax-test #'(let ()
		(if #t
		    (define x 5))
		5))

; Can shadow syntax/macros with embedded defines
(test 5 'intdef (let ()
		  (define lambda 5)
		  lambda))
(test 5 'intdef (let ()
		  (define define 5)
		  'ok
		  define))

(syntax-test #'(lambda () (define x 10) (begin)))
(syntax-test #'(lambda () (define x 10) (begin) (begin)))
(syntax-test #'(lambda () (define x 10) (begin) (begin x) (begin)))
(syntax-test #'(lambda () (define-values (x) . 10) x))
(syntax-test #'(lambda () (define-values (x) 10) (begin 1 . 2) x))
(syntax-test #'(lambda () (begin (define-values (x) 10) . 2) x))
(syntax-test #'(lambda () (begin)))
(syntax-test #'(lambda () (define-values . 10) x))
(syntax-test #'(lambda () (define-values x 10) x))
(syntax-test #'(lambda () (define-values (1) 10) x))

(test 87 (lambda () (define x 87) (begin) (begin x)))

(test '#(0 1 2 3 4) 'do (do ((vec (make-vector 5))
			     (i 0 (+ i 1)))
			    ((= i 5) vec)
			  (vector-set! vec i i)))
(test 25 'do (let ((x '(1 3 5 7 9)))
	       (do ((x x (cdr x))
		    (sum 0 (+ sum (car x))))
		   ((null? x) sum))))
(test 1 'let (let foo () 1))
(test '((6 1 3) (-5 -2)) 'let
      (let loop ((numbers '(3 -2 1 6 -5))
		 (nonneg '())
		 (neg '()))
	(cond ((null? numbers) (list nonneg neg))
	      ((negative? (car numbers))
	       (loop (cdr numbers)
		     nonneg
		     (cons (car numbers) neg)))
	      (else
	       (loop (cdr numbers)
		     (cons (car numbers) nonneg)
		     neg)))))
(test 5 'do (do ((x 1)) (#t 5)))
(test-values '(10 5) (lambda () (do ((x 1)) (#t (values 10 5)))))
(syntax-test #'do)
(syntax-test #'(do))
(syntax-test #'(do ()) )
(syntax-test #'(do () ()) )
(syntax-test #'(do (1) (#t 5) 5))
(syntax-test #'(do ((1)) (#t 5) 5))
(syntax-test #'(do ((1 7)) (#t 5) 5))
(syntax-test #'(do ((x . 1)) (#t 5) 5))
(syntax-test #'(do ((x 1) 2) (#t 5) 5))
(syntax-test #'(do ((x 1) . 2) (#t 5) 5))
(syntax-test #'(do ((x 1)) (#t . 5) 5))
(syntax-test #'(do ((x 1)) (#t 5) . 5))

(test 0 'let/cc (let/cc k (k 0) 1))
(test 0 'let/cc (let/cc k 0))
(test 1 'let/cc (let/cc k (cons 1 2) 1))
(test-values '(2 1) (lambda () (let/cc k (values 2 1))))
(test-values '(2 1) (lambda () (let/cc k (k 2 1))))
(syntax-test #'(let/cc))
(syntax-test #'(let/cc . k))
(syntax-test #'(let/cc k))
(syntax-test #'(let/cc k . 1))
(syntax-test #'(let/cc 1 1))

(test 0 'let/ec (let/ec k (k 0) 1))
(test 0 'let/ec (let/ec k 0))
(test 1 'let/ec (let/ec k (cons 1 2) 1))
(test-values '(2 1) (lambda () (let/ec k (values 2 1))))
(test-values '(2 1) (lambda () (let/ec k (k 2 1))))
(syntax-test #'(let/ec))
(syntax-test #'(let/ec . k))
(syntax-test #'(let/ec k))
(syntax-test #'(let/ec k . 1))
(syntax-test #'(let/ec 1 1))

(define x 1)
(define y -1)
(define (get-x) x)

(test 5 'parameterize (parameterize () 5))
(test 6 'parameterize (parameterize ([error-print-width 10]) 6))
(test 7 'parameterize (parameterize ([error-print-width 10]
				     [uncaught-exception-handler void]) 
                        7))
(define oepw (error-print-width))
(error-test #'(parameterize ([error-print-width 777]) (error 'bad)) exn:fail?)
(test oepw 'parameterize (error-print-width))
(error-test #'(parameterize ([error-print-width 777]
                             [current-output-port (current-error-port)])
                (error 'bad)) 
	    exn:fail?)
(error-test #'(parameterize ([error-print-width 'a]) 10))

(define p (make-parameter 1))
(define q (make-parameter 2))
(test '1 'pz-order (parameterize ([p 3][q (p)]) (q)))

(error-test #'(parameterize) syntaxe?)
(error-test #'(parameterize ()) syntaxe?)
(error-test #'(parameterize ((x y))) syntaxe?)
(error-test #'(parameterize ((x y)) . 8) syntaxe?)
(error-test #'(parameterize (x) 8) syntaxe?)
(error-test #'(parameterize (9) 8) syntaxe?)
(error-test #'(parameterize ((x z) . y) 8) syntaxe?)
(error-test #'(parameterize ((x . z)) 8) syntaxe?)
(error-test #'(parameterize ((x . 9)) 8) syntaxe?)
(error-test #'(parameterize ((x . 9)) 8) syntaxe?)

(error-test #'(parameterize ([10 10]) 8))
(error-test #'(parameterize ([(lambda () 10) 10]) 8))
(error-test #'(parameterize ([(lambda (a) 10) 10]) 8))
(error-test #'(parameterize ([(lambda (a b) 10) 10]) 8))

#|
(test #t procedure? (check-parameter-procedure current-directory))
(test #t procedure? (check-parameter-procedure (case-lambda
						[() 0]
						[(x) 0])))
(test 'exn 'not-param (with-handlers ([void (lambda (x) 'exn)])
		    (check-parameter-procedure (lambda () 10))))
(test 'exn 'not-param (with-handlers ([void (lambda (x) 'exn)])
			(check-parameter-procedure (lambda (x) 10))))
(test 'exn 'not-param (with-handlers ([void (lambda (x) 'exn)])
			(check-parameter-procedure (lambda (x y) 10))))
(arity-test check-parameter-procedure 1 1)
|#

(test 1 'time (time 1))
(test -1 'time (time (cons 1 2) -1))
(test-values '(-1 1) (lambda () (time (values -1 1))))
(syntax-test #'time)
(syntax-test #'(time))
(syntax-test #'(time . 1))
(syntax-test #'(time 1 . 2))

; Tests specifically aimed at the compiler
(error-test #'(let ([x (values 1 2)]) x) exn:application:arity?)
; Known primitive
(error-test #'(let ([x (make-pipe)]) x) exn:application:arity?)
; Known local
(error-test #'(let* ([f (lambda () (values 1 2))][x (f)]) x) exn:application:arity?)

; Known local with global in its closure
(test 15 'known (let ([g (lambda ()
			   (letrec ([f (lambda (x)
					 (+ x 5))])
			     (f 10)))])
		  (g)))
; Known local with a set!
(test 16 'known (let ([g (lambda ()
			   (letrec ([f (lambda (x)
					 (let ([y x])
					   (set! x 7)
					   (+ y 5)))])
			     (f 11)))])
		  (g)))
; Known local non-function
(error-test #'(apply (lambda () (let ([f 12]) (f))) null) exn:application:type?)
; Known local with revsed arguments:
(test 10 (letrec ([f (lambda (a b) (if (zero? a) b (f b a)))]) f) 10 0)

(syntax-test #'#%datum)
(syntax-test #'(let ([#%datum 5])
		 1))
(test '(1) '#%datum (#%datum 1))
(test 1 '#%datum (#%datum . 1))
(test 'a '#%datum (#%datum . a))

(syntax-test #'#%app)
(syntax-test #'(#%app . 1))
(syntax-test #'(#%app 2 . 1))
(syntax-test #'(#%app lambda 1))
(syntax-test #'(let ([#%app 5])
		 (+ 1 2)))

(test 3 '#%app (#%app + 1 2))
(syntax-test #'())
(syntax-test #'(#%app))

(syntax-test #'#%top)
(syntax-test #'(#%top 1))
(syntax-test #'(let ([#%top 5])
		 x))
(err/rt-test (#%top . lambda) exn:fail:contract:variable?)
(define x 5)
(test 5 '#%top (#%top . x))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests related to bytecode optimizer.
;; The (if (let ([x M]) (if x x N)) ...)
;;   => (if (if M #t N) ...)
;; converter drops the variable `x', which means
;; that other mappings must adjust

(let ([val 0])
  (let ([g (lambda ()
	     (letrec ([f (lambda (z x)
			   (if (let ([w (even? 81)])
				 (if w
				     w
				     (let ([y x])
				       (set! x 7)
				       (set! val (+ y 5)))))
			       'yes
			       'no))])
	       (f 0 11)))])
    (g))
  (test 16 values val))

(let ([val 0])
  (let ([g (lambda ()
	     (letrec ([f (lambda (z x)
			   (if (let ([w (even? 81)])
				 (if w
				     w
				     (let ([y x])
				       (set! val (+ y 5)))))
			       'yes
			       'no))])
	       (f 0 11)))])
    (g))
  (test 16 values val))

;; Function-inline test where (h (g v 10)) involves two inlines:
(letrec ([f (lambda (x) (h (g v 10)))]
	 [h (lambda (x) (list x x))]
	 [g (lambda (a b) a)]
	 [v (list 'hello)]
	 [w (list 'no!)]) 
  (test '((hello) (hello)) f 10))

;; Inlining introduces a let binding that is immediately dropped:
(test '(1 . 2)
      (let ([x (cons 1 2)]) (let ([f (lambda (x) x)]) (f (lambda (y) x))))
      10)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check #%top-interaction

(module quoting-top-interaction mzscheme
  (provide (all-from-except mzscheme #%top-interaction)
           (rename top-interaction #%top-interaction))
  (define-syntax top-interaction 
    (syntax-rules ()
      [(_ . e) (quote e)])))

(dynamic-require ''quoting-top-interaction #f)
(let ([ns (make-empty-namespace)])
  (namespace-attach-module (current-namespace) ''quoting-top-interaction ns)
  (parameterize ([current-namespace ns])
    (namespace-require ''quoting-top-interaction))
  (test 3 'non-top
        (parameterize ([current-namespace ns])
          (eval '(+ 1 2))))
  (test ''(+ 1 2) 'repl-top
        (let ([s (open-output-bytes)])
          (parameterize ([current-input-port (open-input-string "(+ 1 2)")]
                         [current-namespace ns]
                         [current-output-port s])
            (read-eval-print-loop))
          (let ([p (open-input-bytes (get-output-bytes s))])
            (read p)
            (read p))))
  (let ([tmp-file (make-temporary-file)])
    (let-values ([(base tmp1 mbd?) (split-path tmp-file)])
    (with-output-to-file tmp-file (lambda () (display '(+ 1 2))) #:exists 'truncate/replace)
    (test '(+ 1 2) 'repl-top
          (parameterize ([current-namespace ns])
            (load tmp-file)))
    (with-output-to-file tmp-file (lambda () (display `(module ,tmp1 mzscheme (provide x) (define x 12))))
                         #:exists 'truncate/replace)
    (test 12 'module
          (parameterize ([current-namespace ns])
            (dynamic-require tmp-file 'x)))
    (delete-file tmp-file))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Check that locations for lambda arguments are created
;; one-by-one --- like `let*', and not like `letrec':

(test '((1 10) (x1 10) (x2 z1))
      'lambda-loc
      (let ()
        (define procs null)
        (define again #f)

        (define (f x 
                   [y (let/cc k
                        (unless again
                          (set! again k))
                        (lambda () 'done))]
                   [z 10])
          (set! procs
                (cons (lambda (xv zv)
                        (begin0
                         (list x z)
                         (set! x xv)
                         (set! z zv)))
                      procs))
          (y))

        (f 1)
        (let/cc esc (again esc))

        (list
         ((cadr procs) 'x1 'z1)
         ((car procs) 'x2 'z2)
         ((cadr procs) 'x10 'z10))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require scheme/splicing)

(define abcdefg 10)
(test 12 'splicing-letrec-syntax (splicing-letrec-syntax ([abcdefg (syntax-rules ()
                                                                     [(_) 12])])
                                                         (abcdefg)))
(test 13 'splicing-letrec-syntax (splicing-letrec-syntax ([abcdefg (syntax-rules ()
                                                                     [(_) (abcdefg 10)]
                                                                     [(_ x) (+ 3 x)])])
                                                         (abcdefg)))
(test 13 'splicing-letrec-syntax (let ([abcdefg 9])
                                   (splicing-letrec-syntax ([abcdefg (syntax-rules ()
                                                                       [(_) (abcdefg 10)]
                                                                       [(_ x) (+ 3 x)])])
                                                           (abcdefg))))
(test 12 'splicing-let-syntax (splicing-let-syntax ([abcdefg (syntax-rules ()
                                                               [(_) 12])])
                                                   (abcdefg)))
(test 12 'splicing-let-syntax (let ([abcdefg (lambda () 9)])
                                (splicing-let-syntax ([abcdefg (syntax-rules ()
                                                                 [(_) 12])])
                                                     (abcdefg))))
(test 11 'splicing-let-syntax (let ([abcdefg (lambda (x) x)])
                                (splicing-let-syntax ([abcdefg (syntax-rules ()
                                                                 [(_) (+ 2 (abcdefg 9))]
                                                                 [(_ ?) 77])])
                                                     (abcdefg))))
(define expand-test-use-toplevel? #t)
(splicing-let-syntax ([abcdefg (syntax-rules ()
                                 [(_) 8])])
                     (define hijklmn (abcdefg)))
(define expand-test-use-toplevel? #f)
(test 8 'hijklmn hijklmn)
(test 30 'local-hijklmn (let ()
                          (splicing-let-syntax ([abcdefg (syntax-rules ()
                                                           [(_) 8])])
                                               (define hijklmn (abcdefg)))
                          (define other 22)
                          (+ other hijklmn)))
(test 8 'local-hijklmn (let ()
                         (splicing-let-syntax ([abcdefg (syntax-rules ()
                                                          [(_) 8])])
                                              (begin
                                                (define hijklmn (abcdefg))
                                                hijklmn))))

(test 9 'splicing-letrec-syntax (let ([abcdefg (lambda () 9)])
                                  (splicing-letrec-syntax ([abcdefg (syntax-rules ()
                                                                      [(_) 0])])
                                                          (define x 10))
                                  (abcdefg)))


;; ----------------------------------------

(test 79 'splicing-let (let ()
                         (splicing-let ([x 79])
                           (define (y) x))
                         (y)))
(test 77 'splicing-let (let ()
                         (define q 77)
                         (splicing-let ([q 8]
                                        [x q])
                           (define (z) x))
                         (z)))
(test 81 'splicing-letrec (let ()
                            (define q 77)
                            (splicing-letrec ([q 81]
                                              [x q])
                              (define (z) x))
                            (z)))
(test 82 'splicing-letrec (let ()
                            (define q 77)
                            (splicing-letrec ([x (lambda () (q))]
                                              [q (lambda () 82)])
                              (define (z) x))
                            ((z))))
(test 81 'splicing-letrec (eval
                            '(begin
                               (define q 77)
                               (splicing-letrec ([q 81]
                                                 [x q])
                                                (define (z) x))
                               (z))))
(test 82 'splicing-letrec (eval
                            '(begin
                               (define q 77)
                               (splicing-letrec ([x (lambda () (q))]
                                                 [q (lambda () 82)])
                                                (define (z) x))
                               ((z)))))
(err/rt-test (eval
              '(begin
                 (splicing-letrec ([x q]
                                   [q 81])
                  x)))
             exn:fail:contract:variable?)

(test 82 'splicing-letrec-syntaxes+values
      (let ()
        (define q 77)
        (splicing-letrec-syntaxes+values
           ([(mx) (lambda (stx) (quote-syntax (x)))]
            [(m) (lambda (stx) (quote-syntax (mx)))])
           ([(x) (lambda () (q))]
            [(q) (lambda () 82)])
          (define (a) (m)))
        (a)))

(test 82 'splicing-letrec-syntaxes+values
      (eval
       '(begin
          (define q 77)
          (splicing-letrec-syntaxes+values
              ([(mx) (lambda (stx) (quote-syntax (x)))]
               [(m) (lambda (stx) (quote-syntax (mx)))])
              ([(x) (lambda () (q))]
               [(q) (lambda () 82)])
            (define (a) (m)))
          (a))))

(test 82 'splicing-local
      (let ()
        (define (x) q)
        (define q 77)
        (define-syntax (m stx) (quote-syntax (x)))
        (splicing-local
            [(define-syntax (m stx) (quote-syntax (mx)))
             (define (x) (q))
             (define-syntax (mx stx) (quote-syntax (x)))
             (define (q) 82)]
          (define (a) (m)))
        (a)))

(test 82 'splicing-local
      (eval
       '(begin
          (define (x) q)
          (define q 77)
          (define-syntax (m stx) (quote-syntax (x)))
          (splicing-local
              [(define-syntax (m stx) (quote-syntax (mx)))
               (define (x) (q))
               (define-syntax (mx stx) (quote-syntax (x)))
               (define (q) 82)]
            (define (a) (m)))
          (a))))

;; local names are not visible outside
(test 77 'splicing-local
      (let ()
        (define q 77)
        (define-syntax (m stx) (quote-syntax (x)))
        (splicing-local
            [(define-syntax (m stx) (quote-syntax (q)))
             (define (q) 82)]
          (define (a) (m)))
        (m)))
(test 77 'splicing-local
      (eval
       '(begin
          (define q 77)
          (define-syntax (m stx) (quote-syntax (x)))
          (splicing-local
              [(define-syntax (m stx) (quote-syntax (q)))
               (define (q) 82)]
            (define (a) (m)))
          (m))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(report-errs)
