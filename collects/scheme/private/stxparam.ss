
(module stxparam '#%kernel
  (#%require "more-scheme.ss"
             "letstx-scheme.ss"
             "define.ss"
             (for-syntax '#%kernel 
                         "../stxparam-exptime.ss"
                         "stx.ss" "stxcase-scheme.ss" 
                         "small-scheme.ss" 
                         "stxloc.ss" "stxparamkey.ss"))

  (#%provide (for-syntax do-syntax-parameterize))

  (define-for-syntax (do-syntax-parameterize stx let-syntaxes-id)
    (syntax-case stx ()
      [(_ ([id val] ...) body0 body ...)
       (let ([ids (syntax->list #'(id ...))])
	 (with-syntax ([(gen-id ...)
			(map (lambda (id)
			       (unless (identifier? id)
				 (raise-syntax-error
				  #f
				  "not an identifier"
				  stx
				  id))
			       (let* ([rt (syntax-local-value id (lambda () #f))]
				      [sp (if (set!-transformer? rt)
					      (set!-transformer-procedure rt)
					      rt)])
				 (unless (syntax-parameter? sp)
				   (raise-syntax-error
				    #f
				    "not bound as a syntax parameter"
				    stx
				    id))
				 (syntax-local-get-shadower 
				  (syntax-local-introduce (syntax-parameter-target sp)))))
			     ids)])
	   (let ([dup (check-duplicate-identifier ids)])
	     (when dup
	       (raise-syntax-error
		#f
		"duplicate binding"
		stx
		dup)))
           (with-syntax ([let-syntaxes let-syntaxes-id])
             (syntax/loc stx
               (let-syntaxes ([(gen-id) (convert-renamer val)] ...)
                 body0 body ...)))))])))