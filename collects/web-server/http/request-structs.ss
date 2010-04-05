#lang scheme/base
(require scheme/contract
         scheme/serialize
         scheme/match
         scheme/promise
         net/url
         web-server/private/util)

(define-serializable-struct header (field value))
(define (headers-assq* f hs)
  (match hs
    [(list)
     #f]
    [(list-rest (and h (struct header (af aw))) hs)
     (if (bytes-ci=? af f)
         h
         (headers-assq* f hs))]))
(define (headers-assq f hs)
  (match hs
    [(list)
     #f]
    [(list-rest (and h (struct header (af av))) hs)
     (if (bytes=? af f)
         h
         (headers-assq f hs))]))       
(provide/contract
 [headers-assq (bytes? (listof header?) . -> . (or/c false/c header?))]
 [headers-assq* (bytes? (listof header?) . -> . (or/c false/c header?))]
 [struct header ([field bytes?]
                 [value bytes?])])

(define-serializable-struct binding (id))
(define-serializable-struct (binding:form binding) (value))
(define-serializable-struct (binding:file binding) (filename headers content))
(define (bindings-assq ti bs)
  (match bs
    [(list)
     #f]
    [(list-rest (and b (struct binding (i))) bs)
     (if (equal? ti i)
         b
         (bindings-assq ti bs))]))
(provide/contract
 [bindings-assq (bytes? (listof binding?) . -> . (or/c false/c binding?))]
 [struct binding ([id bytes?])]
 [struct (binding:form binding) ([id bytes?]
                                 [value bytes?])]
 [struct (binding:file binding) ([id bytes?]
                                 [filename bytes?]
                                 [headers (listof header?)]
                                 [content bytes?])])

(define-serializable-struct request (method uri headers/raw bindings/raw-promise post-data/raw host-ip host-port client-ip))
(define (request-bindings/raw r)
  (force (request-bindings/raw-promise r)))

(provide/contract
 [request-bindings/raw (request? . -> . (listof binding?))]
 [struct request ([method bytes?]
                  [uri url?] 
                  [headers/raw (listof header?)]
                  [bindings/raw-promise (promise/c (listof binding?))]
                  [post-data/raw (or/c false/c bytes?)]
                  [host-ip string?] [host-port number?]
                  [client-ip string?])])