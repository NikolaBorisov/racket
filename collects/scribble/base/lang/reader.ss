#lang s-exp syntax/module-reader

scribble/base/lang

#:read        scribble:read-inside
#:read-syntax scribble:read-syntax-inside
#:whole-body-readers? #t
#:wrapper1 (lambda (t) (list* 'doc 'values '() (t)))
#:info
(lambda (key defval default)
  (case key
    [(color-lexer)
     (dynamic-require 'syntax-color/scribble-lexer 'scribble-inside-lexer)]
    [(drscheme:toolbar-buttons)
     (dynamic-require 'scribble/tools/drscheme-buttons 'drscheme-buttons)]
    [else (default key defval)]))

(require (prefix-in scribble: "../../reader.ss"))