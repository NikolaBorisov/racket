;;; PARAFFINS -- Compute how many paraffins exist with N carbon atoms.

(define-type Radical (Rec Radical (U 'C 'H 'BCP 'CCP (Vectorof Radical))))

(: gen (Exact-Positive-Integer -> (Vectorof (Listof Radical))))
(define (gen n)
  (let*: ((n/2 : Natural (quotient n 2))
          (radicals : (Vectorof (Listof Radical)) (make-vector (+ n/2 1) '(H))))

         (: rads-of-size (Exact-Positive-Integer -> (Listof Radical)))
         (define (rads-of-size n)
           (let: loop1 : (Listof Radical)
                 ((ps : (Listof (Vectorof Natural))
                      (three-partitions (sub1 n)))
                  (lst : (Listof Radical)
                       '()))
                 (if (null? ps)
                     lst
                     (let* ((p (car ps))
                            (nc1 (vector-ref p 0))
                            (nc2 (vector-ref p 1))
                            (nc3 (vector-ref p 2)))
                       (let: loop2 : (Listof Radical)
                             ((rads1 : (Listof Radical)
                                     (vector-ref radicals nc1))
                              (lst : (Listof Radical)
                                   (loop1 (cdr ps)
                                          lst)))
                             (if (null? rads1)
                                 lst
                                 (let: loop3 : (Listof Radical)
                                       ((rads2 : (Listof Radical)
                                               (if (= nc1 nc2)
                                                   rads1
                                                   (vector-ref radicals nc2)))
                                        (lst : (Listof Radical)
                                             (loop2 (cdr rads1)
                                                    lst)))
                                       (if (null? rads2)
                                           lst
                                           (let: loop4 : (Listof Radical)
                                                 ((rads3 : (Listof Radical)
                                                         (if (= nc2 nc3)
                                                             rads2
                                                             (vector-ref radicals nc3)))
                                                  (lst : (Listof Radical)
                                                       (loop3 (cdr rads2)
                                                              lst)))
                                                 (if (null? rads3)
                                                     lst
                                                     (cons (vector 'C
                                                                   (car rads1)
                                                                   (car rads2)
                                                                   (car rads3))
                                                           (loop4 (cdr rads3)
                                                                  lst))))))))))))

         (: bcp-generator (Natural -> (Listof Radical)))
         (define (bcp-generator j)
           (if (odd? j)
               '()
               (let: loop1 : (Listof Radical)
                     ((rads1 : (Listof Radical)
                             (vector-ref radicals (quotient j 2)))
                      (lst : (Listof Radical)
                           '()))
                     (if (null? rads1)
                         lst
                         (let loop2 ((rads2
                                      rads1)
                                     (lst
                                      (loop1 (cdr rads1)
                                             lst)))
                           (if (null? rads2)
                               lst
                               (cons (vector 'BCP
                                             (car rads1)
                                             (car rads2))
                                     (loop2 (cdr rads2)
                                            lst))))))))

         (: ccp-generator (Exact-Positive-Integer -> (Listof Radical)))
         (define (ccp-generator j)
           (let: loop1 : (Listof Radical)
                 ((ps : (Listof (Vectorof Natural))
                      (four-partitions (sub1 j)))
                  (lst : (Listof Radical)
                       '()))
                 (if (null? ps)
                     lst
                     (let* ((p (car ps))
                            (nc1 (vector-ref p 0))
                            (nc2 (vector-ref p 1))
                            (nc3 (vector-ref p 2))
                            (nc4 (vector-ref p 3)))
                       (let loop2 ((rads1
                                    (vector-ref radicals nc1))
                                   (lst
                                    (loop1 (cdr ps)
                                           lst)))
                         (if (null? rads1)
                             lst
                             (let loop3 ((rads2
                                          (if (= nc1 nc2)
                                              rads1
                                              (vector-ref radicals nc2)))
                                         (lst
                                          (loop2 (cdr rads1)
                                                 lst)))
                               (if (null? rads2)
                                   lst
                                   (let loop4 ((rads3
                                                (if (= nc2 nc3)
                                                    rads2
                                                    (vector-ref radicals nc3)))
                                               (lst
                                                (loop3 (cdr rads2)
                                                       lst)))
                                     (if (null? rads3)
                                         lst
                                         (let loop5 ((rads4
                                                      (if (= nc3 nc4)
                                                          rads3
                                                          (vector-ref radicals nc4)))
                                                     (lst
                                                      (loop4 (cdr rads3)
                                                             lst)))
                                           (if (null? rads4)
                                               lst
                                               (cons (vector 'CCP
                                                             (car rads1)
                                                             (car rads2)
                                                             (car rads3)
                                                             (car rads4))
                                                     (loop5 (cdr rads4)
                                                            lst))))))))))))))

         (let loop ((i 1))
           (if (> i n/2)
               (vector (bcp-generator n)
                       (ccp-generator n))
               (begin
                 (vector-set! radicals i (rads-of-size i))
                 (loop (+ i 1)))))))

(: three-partitions (Natural -> (Listof (Vectorof Natural))))
(define (three-partitions m)
  (let: loop1 : (Listof (Vectorof Natural))
        ((lst : (Listof (Vectorof Natural)) '())
         (nc1 : Integer (quotient m 3)))
        (if (< nc1 0)
            lst
            (let loop2 ((lst lst)
                        (nc2 (quotient (- m nc1) 2)))
              (if (< nc2 nc1)
                  (loop1 lst
                         (sub1 nc1))
                  (loop2 (cons (vector (abs nc1)
                                       (abs nc2)
                                       (abs (- m (+ nc1 nc2)))) ; abs is to appease the typechecker
                               lst)
                         (sub1 nc2)))))))

(: four-partitions (Natural -> (Listof (Vectorof Natural))))
(define (four-partitions m)
  (let: loop1 : (Listof (Vectorof Natural))
        ((lst : (Listof (Vectorof Natural)) '())
         (nc1 : Integer (quotient m 4)))
        (if (< nc1 0)
            lst
            (let loop2 ((lst lst)
                        (nc2 (quotient (- m nc1) 3)))
              (if (< nc2 nc1)
                  (loop1 lst
                         (sub1 nc1))
                  (let ((start (max nc2 (- (quotient (+ m 1) 2) (+ nc1 nc2)))))
                    (let loop3 ((lst lst)
                                (nc3 (quotient (- m (+ nc1 nc2)) 2)))
                      (if (< nc3 start)
                          (loop2 lst (sub1 nc2))
                          (loop3 (cons (vector (abs nc1)
                                               (abs nc2)
                                               (abs nc3)
                                               (abs (- m (+ nc1 (+ nc2 nc3))))) ; abs is to appease the typechecker
                                       lst)
                                 (sub1 nc3))))))))))

(: nb (Exact-Positive-Integer -> Natural))
(define (nb n)
  (let ((x (gen n)))
    (+ (length (vector-ref x 0))
       (length (vector-ref x 1)))))

(let ((input (with-input-from-file "input.txt" read)))
  (time
   (let: loop : Natural
         ((n : Natural 4000) (v : Natural 0))
         (if (zero? n)
             v
             (loop (sub1 n) (nb (if input 17 1)))))))

