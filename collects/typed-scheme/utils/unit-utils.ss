#lang scheme/base

(require scheme/unit
         mzlib/private/unit-utils
         
         (for-syntax
          scheme/pretty
          mzlib/private/unit-compiletime
          scheme/base
          (only-in srfi/1/list s:member delete-duplicates)
          scheme/unit-exptime
          scheme/match))

(provide cnt)

(define-signature-form (cnt stx)
  (syntax-case stx ()
    [(_ nm cnt)
     (list #'nm)
     #;(list #'[contracted (nm cnt)])]))

(define-for-syntax (unprocess-tagged-id ti)
    (if (car ti)
        #`(tag #,(car ti) #,(cdr ti))
        (cdr ti)))

(define-syntax (define-values/link-units/infer stx)
  ;; construct the runtime code
  ;; takes 3 lists of identifiers and a syntax object for location info
  (define (mk imports exports units stx)
    (quasisyntax/loc stx
        (begin (define-compound-unit/infer new-unit@ 
                 (import #,@imports)
                 (export #,@exports)
                 (link #,@units))
               (define-values/invoke-unit/infer new-unit@))))
  
  ;; produce the imports not satisfied by the exports, and all the exports
  ;; exports should not have duplicates
  (define (imps/exps-from-unit u)      
    (let* ([ui (lookup-def-unit u)]
           [unprocess (let ([i (make-syntax-delta-introducer #'u (unit-info-orig-binder ui))])
                        (lambda (p)
                          (unprocess-tagged-id (cons (car p) (i (cdr p))))))]
           [isigs (map unprocess (unit-info-import-sig-ids ui))]
           [esigs (map unprocess (unit-info-export-sig-ids ui))])
      (values isigs esigs)))
  
  (define (drop-from-other-list exp-tagged imp-tagged imp-sources)
    (let loop ([ts imp-tagged] [ss imp-sources])
      (cond
        [(null? ts) null]
        [(for/or ([tinfo2 exp-tagged])
                 (and (eq? (car (car ts)) (car tinfo2))
                      (siginfo-subtype (cdr tinfo2) (cdr (car ts)))))
         (loop (cdr ts) (cdr ss))]
        [else (cons (car ss) (loop (cdr ts) (cdr ss)))])))
  
  (define (drop-duplicates tagged-siginfos sources)
    (let loop ([ts tagged-siginfos] [ss sources] [res-t null] [res-s null])
      (cond
        [(null? ts) (values res-t res-s)]
        [(for/or ([tinfo2 (cdr ts)])
                 (and (eq? (car (car ts)) (car tinfo2))
                      (siginfo-subtype (cdr tinfo2) (cdr (car ts)))))
         (loop (cdr ts) (cdr ss) res-t res-s)]
        [else (loop (cdr ts) (cdr ss) (cons (car ts) res-t) (cons (car ss) res-s))])))
  
  (define (imps/exps-from-units units)
    (define-values (isigs esigs)
      (for/fold 
       ([imps null] [exps null])
       ([u units])
       (let-values ([(i e) (imps/exps-from-unit u)])
         (values (append i imps) (append e exps)))))
    (define-values (isig tagged-import-sigs import-tagged-infos 
                         import-tagged-sigids import-sigs)
      (process-unit-import (datum->syntax #f isigs)))
    
    (define-values (esig tagged-export-sigs export-tagged-infos 
                         export-tagged-sigids export-sigs)
      (process-unit-export (datum->syntax #f esigs)))
    (check-duplicate-subs export-tagged-infos esig)
    (let-values ([(itagged isources) (drop-duplicates import-tagged-infos isig)])
      (values (drop-from-other-list export-tagged-infos itagged isources) esig)))
  
  (syntax-case stx (import export link)
    ;; here the exports are specified - they ought to be a subset of the allowable exports
    [(_ (export . sigs) (link . units))
     (let*-values ([(units) (syntax->list #'units)]
                   [(imps exps) (imps/exps-from-units units)])
       (mk imps (syntax->list #'sigs) units stx))]
    ;; here we just export everything that's available
    [(_ (link . units))
     (andmap identifier? (syntax->list #'units))
     (let*-values ([(units) (syntax->list #'units)]
                   [(imps exps) (imps/exps-from-units units)])
       (mk imps exps units stx))]))

(provide link)

;; Tests

(define-signature x^ (x))
(define-signature y^ (y))
(define-signature z^ (z))

(define-unit y@
  (import z^)
  (export y^)
  (define y (* 2 z)))

(define-unit x@
  (import y^)
  (export x^)
  (define (x) (+ y 1)))

(define z 45)

(define-values/link-units/infer (export x^) (link x@ y@))


(define y 1)

(define-unit xx@ (import y^) (export))
(define-unit zz@ (import y^) (export))
(define-values/link-units/infer (link xx@ zz@))


(define-unit a@ (import) (export y^) (define y values))
(define-unit b@ (import) (export y^) (define y values))
;(define-values/link-units/infer (link a@ b@))



