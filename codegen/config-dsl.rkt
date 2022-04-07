#lang racket/base

(require (for-syntax racket/base syntax/parse)
         (rename-in scribble/text [output scrbl-output])

         racket/function
         racket/file

         "logger.rkt"
         "config.rkt"
         "objects.rkt")

(provide (except-out (all-from-out racket/base) #%module-begin)
         (all-from-out "config.rkt" "objects.rkt" "logger.rkt")
         (rename-out [config-modbeg #%module-begin])
         current-inputs
         output
         template
         exclude
         include
         all
         name-matches
         parsed)

(define-syntax (config-modbeg stx)
  (syntax-parse stx
    [(_ body:expr ...)
     (syntax/loc stx
       (#%module-begin
        (provide codegen-config)
        (define codegen-config
          (make-config
           (lambda (i)
             (parameterize ([current-inputs i])
               body ...
               (void)))))))]))

(define current-inputs (make-parameter #f))

(define (output #:to gen-path #:from outputs)
  (log-codegen-info "Outputting to ~a" gen-path)
  (define full-path (path->complete-path gen-path (inputs-gen-path (current-inputs))))
  (make-parent-directory* full-path)
  (call-with-output-file
    #:exists 'truncate/replace
    full-path
    (lambda (port)
      (scrbl-output outputs port))))

(define (template conf-path proc)
  (log-codegen-info "Applying template ~a (~a)" conf-path proc)
  (define mod-path (path->complete-path conf-path (inputs-conf-parent (current-inputs))))
  (dynamic-require mod-path proc))

(define (exclude #:from inputs . filters)
  (remove (apply disjoin filters) inputs))

(define (include #:from inputs . filters)
  (filter (apply disjoin filters) inputs))

(define (all . _args) all)

(define (name-matches . pats)
  (lambda (obj)
    (for/or ([re (in-list pats)])
      (regexp-match-exact? re (api-object-name obj)))))

(define (parsed sym)
  (hash-ref (inputs-parsed (current-inputs)) sym))
