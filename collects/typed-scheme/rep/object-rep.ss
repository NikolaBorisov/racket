#lang scheme/base

(require scheme/match scheme/contract "rep-utils.ss" "free-variance.ss" "filter-rep.ss")

(dpe CarPE () [#:fold-rhs #:base])
(dpe CdrPE () [#:fold-rhs #:base])
(dpe SyntaxPE () [#:fold-rhs #:base])
(dpe StructPE ([t Type?] [idx natural-number/c])     
    [#:frees (free-vars* t) (free-idxs* t)]
    [#:fold-rhs (*StructPE (type-rec-id t) idx)])

(do Empty () [#:fold-rhs #:base])

(do Path ([p (listof PathElem?)] [v identifier?])
  [#:intern (list p (hash-id v))]
  [#:frees (combine-frees (map free-vars* p)) (combine-frees (map free-idxs* p))]
  [#:fold-rhs (*Path (map pathelem-rec-id p) v)])

;; represents no info about the filters of this expression
;; should only be used for parsing type annotations and expected types
(do NoObject () [#:fold-rhs #:base])

(dlo LEmpty () [#:fold-rhs #:base])

(dlo LPath ([p (listof PathElem?)] [idx index/c])
  [#:frees (combine-frees (map free-vars* p)) (combine-frees (map free-idxs* p))]
  [#:fold-rhs (*LPath (map pathelem-rec-id p) idx)])