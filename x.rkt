#!/usr/bin/env racket
#lang racket/base

(require (for-syntax racket/base)

         racket/runtime-path
         racket/file
         racket/system

         net/git-checkout
         net/url

         raylib/codegen/codegen)

(define-runtime-path configs-path '(lib "raylib/configs"))
(define-runtime-path parser-path '(lib "raylib/raylib-parser"))
(define-runtime-path git-path '(lib "raylib/raylib-git"))
(define-runtime-path lib-path '(lib "raylib/lib"))
(define-runtime-path root-path ".")

(define fetch-libs (make-parameter #f))
(define checkout-raylib (make-parameter #f))
(define parse-api (make-parameter #f))
(define generate-code (make-parameter #f))
(define delete-raylib (make-parameter #f))

(define (assert-success code)
  (unless (= 0 code)
    (exit code)))

(define (main ref)
  (when (fetch-libs)
    (displayln "--- Fetching Raylib libraries"))

  (when (checkout-raylib)
    (displayln "--- Checking out Raylib")
    (delete-directory/files git-path #:must-exist? #f)
    (git-checkout
     "github.com" "raysan5/raylib"
     #:ref ref
     #:dest-dir git-path
     #:transport 'https))

  (when (parse-api)
    (define git-parser-path (build-path git-path "parser"))

    (delete-directory/files git-parser-path #:must-exist? #f)
    (copy-directory/files parser-path git-parser-path)

    (define make
      (or (find-executable-path "make")
          (raise-user-error "make is not installed")))

    (displayln "--- Parsing Raylib Header")
    (parameterize ([current-directory git-parser-path])
      (assert-success (system*/exit-code make))))

  (raylib-raw-root (url->string (path->url git-path)))

  (when (generate-code)
    (displayln "--- Generating code")
    (for ([flavour (in-list '("2d" "generated"))])
      (displayln (format "---- Generating ~a bindings" flavour))
      (define config (build-path configs-path (format "~a.rkt" flavour)))
      (define target (build-path root-path (format "~a" flavour)))
      (do-codegen
       #:config-source config
       #:clear? #t
       target)))

  (displayln "--- Cleaning up")
  (when (delete-raylib)
    (delete-directory/files git-path #:must-exist? #f))

  (displayln "--- Done"))

(module+ main
  (require racket/cmdline)
  (command-line
   #:once-any
   [("--all")
    "enable all options"
    (for ([opt (in-list (list checkout-raylib parse-api generate-code delete-raylib))])
      (opt #t))]
   #:once-any
   [("--libs") "fetch Raylib binaries from GitHub releases" (fetch-libs #t)]
   [("--no-libs") "don't fetch Raylib binaries from GitHub releases" (fetch-libs #f)]
   #:once-any
   [("--checkout") "checkout Raylib from git" (checkout-raylib #t)]
   [("--no-checkout") "don't checkout Raylib from git" (checkout-raylib #f)]
   #:once-any
   [("--parse") "parse Raylib API headers" (parse-api #t)]
   [("--no-parse") "don't parse Raylib API headers" (parse-api #f)]
   #:once-any
   [("--generate") "generate Racket bindings" (generate-code #t)]
   [("--no-generate") "don't generate Racket bindings" (generate-code #f)]
   #:once-any
   [("--clear-repo") "delete the Raylib git repository when finished" (delete-raylib #t)]
   [("--no-clear-repo") "don't delete the Raylib git repository when finished" (delete-raylib #f)]
   #:args (raylib-ref)
   (main raylib-ref)))
