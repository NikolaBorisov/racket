
;; Command-line parsing is in its own module because it has to be used
;;  both in setup.ss (pre-zo, pre-cm) and setup-go.ss (use zos and cm).
;; This means that command lines will be parsed twice.

#lang scheme/base

(require scheme/cmdline
         rico/command-name)

(provide parse-cmdline)

;; The result of parse-cmdline is three lists:
;;  - An assoc list mapping flag symbols to booleans
;;     (nearly all symbols correspond to parameter names
;;      in setup-go.ss)
;;  - A list of specific collections
;;  - A list of archives

(define (parse-cmdline argv)

  (define x-specific-planet-packages '())
  (define x-flags null)
  (define (add-flags l)
    (set! x-flags (append (reverse l) x-flags)))

  (define-values (short-name long-name)
    (let ([p (find-system-path 'run-file)])
      (let-values ([(base name dir?) (split-path p)])
        (cond
         [(current-command-name)
          (values (format "~a ~a" name (current-command-name))
                  (program+command-name))]
         ;; Hack for bootstrapping, if the program name is "rico",
         ;; then claim to be the "setup" command:
         [(equal? (path->string name) "rico")
          (values (format "~a setup" name)
                  (format "~a setup" p))]
         [else
          (values (path->string name) p)]))))

  (define-values (x-specific-collections x-archives)
    (command-line
     #:program long-name
     #:argv argv
     #:once-each
     [("-c" "--clean") "Delete existing compiled files; implies -nxi"
      (add-flags '((clean #t)
                   (make-zo #f)
                   (call-install #f)
                   (make-launchers #f)
                   (make-info-domain #f)
                   (make-docs #f)))]
     [("-n" "--no-zo") "Do not produce .zo files"
      (add-flags '((make-zo #f)))]
     [("-x" "--no-launcher") "Do not produce launcher programs"
      (add-flags '((make-launchers #f)))]
     [("-i" "--no-install") "Do not call collection-specific pre-installers"
      (add-flags '((call-install #f)))]
     [("-I" "--no-post-install") "Do not call collection-specific post-installers"
      (add-flags '((call-post-install #f)))]
     [("-d" "--no-info-domain") "Do not produce info-domain caches"
      (add-flags '((make-info-domain #f)))]
     [("-D" "--no-docs") "Do not produce documentation"
      (add-flags '((make-docs #f)))]
     [("-U" "--no-user") "Do not setup user-specific collections (implies --no-planet)"
      (add-flags '((make-user #f) (make-planet #f)))]
     [("--no-planet") "Do not setup PLaneT packages"
      (add-flags '((make-planet #f)))]
     [("--avoid-main") "Do not make main-installation files"
      (add-flags '((avoid-main-installation #t)))]
     [("-v" "--verbose") "See names of compiled files and info printfs"
      (add-flags '((verbose #t)))]
     [("-m" "--make-verbose") "See make and compiler usual messages"
      (add-flags '((make-verbose #t)))]
     [("-r" "--compile-verbose") "See make and compiler verbose messages"
      (add-flags '((make-verbose #t)
                   (compiler-verbose #t)))]
     [("--trust-zos") "Trust existing .zos (use only with prepackaged .zos)"
      (add-flags '((trust-existing-zos #t)))]
     [("-p" "--pause") "Pause at the end if there are any errors"
      (add-flags '((pause-on-errors #t)))]
     [("--force") "Treat version mismatches for archives as mere warnings"
      (add-flags '((force-unpacks #t)))]
     [("-a" "--all-users") "Install archives to main (not user-specific) installation"
      (add-flags '((all-users #t)))]
     [("--mode") mode "Select a compilation mode"
      (add-flags `((compile-mode ,mode)))]
     [("--doc-pdf") dir "Write doc PDF to <dir>"
      (add-flags `((doc-pdf-dest ,dir)))]
     [("-l") =>
      (lambda (flag . collections)
        (map (lambda (v)
               ;; A normal-form collection path matches a symbolic module path;
               ;; this is a bit of a hack, but it's not entirely a coincidence:
               (unless (module-path? (string->symbol v))
                 (error (format "bad collection path~a: ~a"
                                (cond [(regexp-match? #rx"/$" v)
                                       " (trailing slash not allowed)"]
                                      [(regexp-match? #rx"\\\\" v)
                                       " (backslash not allowed)"]
                                      [else ""])
                                v)))
               (list v))
             collections))
      '("Setup specific <collection>s only" "collection")]
     #:multi
     [("-P") owner package-name maj min
      "Setup specified PLaneT packages only"
      (set! x-specific-planet-packages (cons (list owner package-name maj min)
                                             x-specific-planet-packages))]
     #:handlers
     (lambda (collections . archives)
       (values (if (null? collections) null (car collections))
               archives))
     '("archive")
     (lambda (s)
       (display s)
       (printf "If no <archive> or -l <collection> is specified, ~a\n"
               "all collections are setup")
       (exit 0))))

    (values short-name x-flags x-specific-collections x-specific-planet-packages x-archives))