#lang racket/base

(require (for-syntax racket/base syntax/parse)
         (rename-in scribble/text [output scrbl-output])

         racket/function
         racket/file
         racket/system
         racket/list

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
         parsed
         apply-patch
         complete-gen-path
         complete-config-path)

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

(define (complete-gen-path gen-path)
  (path->complete-path gen-path (inputs-gen-path (current-inputs))))

(define (complete-config-path conf-path)
  (path->complete-path conf-path (inputs-conf-parent (current-inputs))))

(define (output #:to gen-path #:from outputs)
  (log-codegen-info "Outputting to ~a" gen-path)
  (define full-path (complete-gen-path gen-path))
  (make-parent-directory* full-path)
  (call-with-output-file
    #:exists 'truncate/replace
    full-path
    (lambda (port)
      (scrbl-output outputs port))))

(define (template conf-path proc)
  (log-codegen-info "Applying template ~a (~a)" conf-path proc)
  (dynamic-require (complete-config-path conf-path) proc))

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

(define (apply-patch
         #:from patch-path
         #:to [out-path '()]
         #:in [in-dir "."]
         . options)
  (log-codegen-info "Applying patch ~a to ~a" patch-path out-path)
  (define code
    (parameterize ([current-directory (complete-gen-path in-dir)])
      (apply
       system*/exit-code
       (or (find-executable-path "patch")
           (raise-user-error "patch is not installed"))
       (append
        options
        (list
         "-i"
         (path->string (complete-config-path patch-path)))
        (map (compose1 path->string complete-gen-path)
             (flatten out-path))))))
  (unless (zero? code)
    (raise
     (exn:fail 
      (format "failed with exit code: ~a" code)
      (current-continuation-marks)))))
