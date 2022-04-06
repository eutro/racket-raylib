#lang racket/base

(require racket/contract
         racket/format)

(provide read-config
         config-codegen
         (struct-out config)
         (struct-out inputs))

(define-struct/contract inputs
  ([gen-path path?]
   [conf-parent path?]
   [parsed hash?])
  #:transparent)

(define-struct/contract config
  ([outputs (-> inputs? void?)]))

(define (read-config path)
  (define config (dynamic-require path 'codegen-config))
  (unless (config? config)
    (raise-user-error (~a "config at " path " did not provide a 'codegen-config value")))
  config)

(define (config-codegen config gen-path conf-parent parsed)
  ((config-outputs config)
   (make-inputs gen-path conf-parent parsed)))

(module reader syntax/module-reader
  raylib/codegen/config-dsl)
