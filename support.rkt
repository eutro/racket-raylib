#lang racket/base

(require ffi/unsafe
         ffi/unsafe/alloc
         racket/runtime-path
         racket/match
         raylib/generated/version
         (for-syntax racket/base
                     syntax/parse))

(provide _pointer-to
         ptr-box
         define-ptr
         borrow
         attach-cleanup
         call-with-cleanup
         let*-with-cleanup
         (protect-out raylib-ffi-lib))

(define-runtime-path lib-path '(lib "raylib/lib"))

(define raylib-ffi-lib
  (or
   (let ([supplied (getenv "RACKET_RAYLIB_PATH")])
     (and supplied (ffi-lib supplied)))
   (let ([RAYLIB_VERSION (number->string RAYLIB_VERSION)])
     (define ((non-bundled name))
       (ffi-lib
        name
        (list RAYLIB_VERSION #f)
        #:fail
        (lambda ()
          (raise
           (exn:fail
            (string-append
             "could not load Raylib;\n"
             "  your operating system may not have prebuilt binaries published;\n"
             "  or they are incompatible with your system;"
             "  consider installing Raylib yourself, and/or setting RACKET_RAYLIB_PATH\n")
            (current-continuation-marks))))))
     (match (list (system-type 'os)
                  (system-type 'arch)
                  (system-type 'word))
       [(list 'unix 'x86_64 _)
        (ffi-lib (build-path lib-path "linux_amd64" "libraylib.so")
                 #:fail (non-bundled "libraylib"))]
       [(list 'macosx _ _)
        (ffi-lib (build-path lib-path "macos" "libraylib.dylib")
                 #:fail (non-bundled "libraylib"))]
       [(list 'windows _ 32)
        (ffi-lib (build-path lib-path "win32_msvc16" "raylib.dll")
                 #:fail (non-bundled "raylib"))]
       [(list 'windows _ 64)
        (ffi-lib (build-path lib-path "win64_msvc16" "raylib.dll")
                 #:fail (non-bundled "raylib"))]
       [_ ((non-bundled))]))))

(define-syntax-rule (_pointer-to _type)
  _pointer)

(define (ptr-box type value)
  (define ptr (malloc type))
  (ptr-set! ptr type value)
  ptr)

(begin-for-syntax
  (struct ptr-var [type ptr]
    #:property prop:set!-transformer
    (lambda (var stx)
      (with-syntax ([type (ptr-var-type var)]
                    [ptr (ptr-var-ptr var)])
        (syntax-parse stx
          [(set! _ value)
           (syntax/loc stx
             (ptr-set! ptr type value))]
          [(this . tail)
           (syntax/loc stx
             ((#%expression . this) . tail))]
          [_
           (syntax/loc stx
             (ptr-ref ptr type))])))))

(define-syntax (define-ptr stx)
  (syntax-parse stx
    [(_ id:id type:expr value:expr)
     (syntax/loc stx
       (begin
         (define ty type)
         (define ptr (malloc ty))
         (ptr-set! ptr ty value)
         (define-syntax id (ptr-var #'ty #'ptr))))]))

(define-syntax (borrow stx)
  (define-syntax-class ptr-var-expr
    #:description "a pointer variable"
    #:attributes (borrow)
    (pattern
     var:id
     #:do [(define binding (syntax-local-value this-syntax (lambda () #f)))]
     #:fail-unless (ptr-var? binding) "binding was not defined as a pointer variable"
     #:attr borrow (ptr-var-ptr binding)))
  (syntax-parse stx
    [(_ var:ptr-var-expr)
     (syntax/loc stx
       var.borrow)]))

(define (attach-cleanup cleanup value)
  (((allocator cleanup) values) value))

(define (call-with-cleanup cleanup value proc)
  (call-with-continuation-barrier
   (lambda ()
     (dynamic-wind
       void
       (lambda () (proc value))
       (lambda () (cleanup value))))))

(define-syntax (let*-with-cleanup stx)
  (syntax-parse stx
    [(_ ([name:id cleanup:expr value:expr] bindings ...) body ...)
     (syntax/loc stx
       (call-with-cleanup
        cleanup
        value
        (lambda (name)
          (let*-with-cleanup
           (bindings ...)
           body ...))))]
    [(_ () body ...)
     (syntax/loc stx
       (let ()
         body ...))]))

(module+ test
  ;; just instantiate them as a test
  (for ([mod (in-list '(raylib/2d/unsafe
                        raylib/generated/unsafe))])
    (dynamic-require mod #f))

  (let*-with-cleanup
   ([_x displayln 4]
    [_x displayln 3]
    [_y displayln 2]
    [_z displayln 1])
   (displayln "And for my next magic trick, I will print a series of values!")
   #t))
