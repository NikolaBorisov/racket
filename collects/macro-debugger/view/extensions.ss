#lang scheme/base
(require scheme/class
         (rename-in unstable/class-iop
                    [send/i send:]
                    [send*/i send*:]
                    [init-field/i init-field:])
         scheme/unit
         scheme/list
         scheme/match
         scheme/gui
         framework/framework
         syntax/boundmap
         "interfaces.ss"
         "prefs.ss"
         "warning.ss"
         "hiding-panel.ss"
         (prefix-in s: "../syntax-browser/widget.ss")
         (prefix-in s: "../syntax-browser/keymap.ss")
         (prefix-in s: "../syntax-browser/interfaces.ss")
         "../model/deriv.ss"
         "../model/deriv-util.ss"
         "../model/trace.ss"
         "../model/steps.ss"
         "cursor.ss"
         unstable/gui/notify)
(provide stepper-keymap%
         stepper-syntax-widget%)

;; Extensions

(define stepper-keymap%
  (class s:syntax-keymap%
    (init-field: (macro-stepper widget<%>))
    (inherit-field config
                   controller)
    (inherit add-function
             call-function)

    (define show-macro #f)
    (define hide-macro #f)

    (super-new)

    (define/public (get-hiding-panel)
      (send: macro-stepper widget<%> get-macro-hiding-prefs))

    (add-function "hiding:show-macro"
                  (lambda (i e)
                    (send*: (get-hiding-panel) hiding-prefs<%>
                      (add-show-identifier)
                      (refresh))))

    (add-function "hiding:hide-macro"
                  (lambda (i e)
                    (send*: (get-hiding-panel) hiding-prefs<%>
                      (add-hide-identifier)
                      (refresh))))

    ;; Menu

    (define/override (add-context-menu-items menu)
      (super add-context-menu-items menu)
      (new separator-menu-item% (parent menu))
      (new menu-item% (label "Show selected identifier") (parent menu)
           (demand-callback
            (lambda (i)
              (send i enable (identifier? (send controller get-selected-syntax)))))
           (callback
            (lambda (i e)
              (call-function "hiding:show-macro" i e))))
      (new menu-item% (label "Hide selected identifier") (parent menu)
           (demand-callback
            (lambda (i)
              (send i enable (identifier? (send controller get-selected-syntax)))))
           (callback
            (lambda (i e) (call-function "hiding:hide-macro" i e)))))))


(define stepper-syntax-widget%
  (class s:widget%
    (init-field: (macro-stepper widget<%>))
    (inherit get-text)
    (inherit-field controller)

    (define/override (setup-keymap)
      (new stepper-keymap%
           (editor (get-text))
           (config (send: macro-stepper widget<%> get-config))
           (controller controller)
           (macro-stepper macro-stepper)))

    (define/override (show-props show?)
      (super show-props show?)
      (send: macro-stepper widget<%> update/preserve-view))

    (super-new
     (config (send: macro-stepper widget<%> get-config)))))
