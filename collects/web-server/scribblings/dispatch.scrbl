#lang scribble/doc
@(require "web-server.rkt"
          racket/sandbox)
@(require (for-label web-server/servlet
                     web-server/dispatchers/dispatch
                     web-server/servlet-env
                     web-server/dispatch/extend
                     racket/match
                     racket/list
                     net/url
                     xml))

@(define dispatch-eval
   (let ([the-eval (make-base-eval)])
     (the-eval '(require web-server/http
                         net/url
                         racket/list 
                         racket/promise
                         web-server/dispatch
                         web-server/dispatch/extend))
     the-eval))

@title[#:tag "dispatch"]{URL-Based Dispatch}

@defmodule[web-server/dispatch]

The library allows the creation of two-way mappings between permanent URLs and request-handling procedures.

@margin-note{This library was inspired by the @racketmodname[(planet untyped/dispatch)] package.}

@section{Using @racketmodname[web-server/dispatch]}

Suppose you are writing a blog application and want pretty URLs for different views of the site.
You would define some URL dispatching rules as follows:

@interaction[#:eval dispatch-eval
 (define-values (blog-dispatch blog-url)
   (dispatch-rules
    [("") list-posts]
    [("posts" (string-arg)) review-post]
    [("archive" (integer-arg) (integer-arg)) review-archive]
    [else list-posts]))
]

And define your request handlers as follows:
@interaction[#:eval dispatch-eval
(define (list-posts req) `(list-posts))
(define (review-post req p) `(review-post ,p))
(define (review-archive req y m) `(review-archive ,y ,m))
]

Now when a request is sent to your application, it will be directed to the appropriate handler:
@interaction[#:eval dispatch-eval
(define (url->request u)
  (make-request #"GET" (string->url u) empty
                (delay empty) #f "1.2.3.4" 80 "4.3.2.1"))
(blog-dispatch 
 (url->request "http://www.chrlsnchrg.com"))
(blog-dispatch 
 (url->request "http://www.chrlsnchrg.com/"))
(blog-dispatch
 (url->request 
  "http://www.chrlsnchrg.com/posts/Extracurricular-Activity"))
(blog-dispatch
 (url->request "http://www.chrlsnchrg.com/archive/1984/10"))
(blog-dispatch
 (url->request "http://www.chrlsnchrg.com/contact"))
]       

You can also generate these pretty URLs from procedure calls:
@interaction[#:eval dispatch-eval
(blog-url list-posts)
(blog-url review-post "Another-Saturday-Night")
(blog-url review-archive 1984 11)
]

After mastering the world of blogging software, you decide to put the ubiquitous Add-Two-Numbers.com out of business with Sum.com:
@interaction[#:eval dispatch-eval
 (define-values (sum-dispatch sum-url)
   (dispatch-rules
    [((integer-arg) ...) sum]
    [else (lambda (req) (sum req empty))]))
 
 (define (sum req is)
   (apply + is))
 
 (sum-dispatch (url->request "http://www.sum.com/"))
 (sum-dispatch (url->request "http://www.sum.com/2"))
 (sum-dispatch (url->request "http://www.sum.com/2/3/4"))
 (sum-dispatch (url->request "http://www.sum.com/5/10/15/20"))
 
 (sum-url sum empty)
 (sum-url sum (list 1))
 (sum-url sum (list 2 3 5 7))
]

@section{API Reference}

@defform*[#:literals (else)
         [(dispatch-rules
           [dispatch-pattern dispatch-fun]
           ...
           [else else-fun])
          (dispatch-rules
           [dispatch-pattern dispatch-fun]
           ...)]
         #:contracts
         ([else-fun (request? . -> . response/c)]
          [dispatch-fun (request? any/c ... . -> . response/c)])]{
 Returns two values: the first is a dispatching function with the contract @racket[(request? . -> . response/c)]
 that calls the appropriate @racket[dispatch-fun] based on the first @racket[dispatch-pattern] that matches the
 request's URL; the second is a URL-generating function with the contract @racket[(procedure? any/c ... . -> . string?)]
 that generates a URL using @racket[dispatch-pattern] for the @racket[dispatch-fun] given as its first argument.
 
 If @racket[else-fun] is left out, one is provided that calls @racket[(next-dispatcher)] to signal to the Web Server that this
 dispatcher does not apply.
}
 
@racketgrammar[dispatch-pattern
               ()
               (string . dispatch-pattern)
               (bidi-match-expander ... . dispatch-pattern)
               (bidi-match-expander . dispatch-pattern)]

@defform*[#:literals (else)
         [(dispatch-rules+applies
           [dispatch-pattern dispatch-fun]
           ...
           [else else-fun])
          (dispatch-rules+applies
           [dispatch-pattern dispatch-fun]
           ...)]
         #:contracts
         ([else-fun (request? . -> . response/c)]
          [dispatch-fun (request? any/c ... . -> . response/c)])]{
 Like @racket[dispatch-rules], except returns a third value with the contract @racket[(request? . -> . boolean?)] that returns
      @racket[#t] if the dispatching rules apply to the request and @racket[#f] otherwise.
      }

@defform*[#:literals (else)
         [(dispatch-case
           [dispatch-pattern dispatch-fun]
           ...
           [else else-fun])
          (dispatch-case
           [dispatch-pattern dispatch-fun]
           ...)]
         #:contracts
         ([else-fun (request? . -> . response/c)]
          [dispatch-fun (request? any/c ... . -> . response/c)])]{
 Returns a dispatching function as described by @racket[dispatch-rules].
}

@defform[#:literals (else)
         (dispatch-url
          [dispatch-pattern dispatch-fun]
          ...)
         #:contracts
         ([dispatch-fun (request? any/c ... . -> . response/c)])]{
 Returns a URL-generating function as described by @racket[dispatch-rules].
}

@defproc[(serve/dispatch [dispatch (request? . -> . response/c)])
         void]{
 Calls @racket[serve/servlet] with appropriate arguments so that every request is handled by @racket[dispatch].
}
              
@section{Built-in URL patterns}

@racketmodname[web-server/dispatch] builds in a few useful URL component patterns.

@defform[(number-arg)]{
 A @tech{bi-directional match expander} that parses a @racket[number?] from the URL and generates a URL with a number's encoding as a string.
}

@defform[(integer-arg)]{
 A @tech{bi-directional match expander} that parses a @racket[integer?] from the URL and generates a URL with a integer's encoding as a string.
}

@defform[(real-arg)]{
 A @tech{bi-directional match expander} that parses a @racket[real?] from the URL and generates a URL with a real's encoding as a string.
}

@defform[(string-arg)]{
 A @tech{bi-directional match expander} that parses a @racket[string?] from the URL and generates a URL containing the string.
}

@defform[(symbol-arg)]{
 A @tech{bi-directional match expander} that parses a @racket[symbol?] from the URL and generates a URL with a symbol's encoding as a string.
}

@section{Extending @racketmodname[web-server/dispatch]}

@defmodule[web-server/dispatch/extend]

You can create new URL component patterns by defining @tech{bi-directional match expanders}.

@defform[(define-bidi-match-expander id in-xform out-xform)]{
 Binds @racket[id] to a @deftech{bi-directional match expander}
 where @racket[in-xform] is a match expander (defined by @racket[define-match-expander]) that is used when parsing URLs
 and @racket[out-xform] is one used when generating URLs.
 
 Both @racket[in-xform] and @racket[out-xform] should use the syntax @racket[(_xform arg ... _id)] where the @racket[arg]s are 
 specific to @racket[id] and compatible with both @racket[in-xform] and @racket[out-xform]. @racket[_id] will typically be provided
 automatically by @racket[dispatch-rules].
}

@defidform[bidi-match-going-in?]{
 A @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{syntax parameter} used by @tech{bi-directional match expanders} to determine if a URL is being parsed or generated.
}

When defining new patterns, you may find it useful to use these helper functions:

@defform[(define-coercion-match-expander id test? coerce)]{
 Binds @racket[id] to a match expander that expands @racket[(id _x)] to
 @racket[(? test? (app coerce _x))] (i.e., uses @racket[test?] to determine if the pattern matches and @racket[coerce] to transform the binding.)
}
 
@defproc[(make-coerce-safe? [coerce (any/c . -> . any/c)])
         (any/c . -> . boolean?)]{
 Returns a function that returns @racket[#t] if @racket[coerce] would not throw an exception or return @racket[#f] on its input.
         
 @examples[#:eval dispatch-eval
  (define string->number? (make-coerce-safe? string->number))
  (string->number? "1")
  (string->number? "1.2")
  (string->number? "+inf.0")
  (string->number? "one")
 ]
}
