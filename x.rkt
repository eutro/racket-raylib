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
(define-runtime-path root-path ".")

(define (main ref)
  (displayln "--- Checking out Raylib")
  (delete-directory/files git-path #:must-exist? #f)
  (git-checkout
   "github.com" "raysan5/raylib"
   #:ref ref
   #:dest-dir git-path
   #:transport 'https)
  (define git-parser-path (build-path git-path "parser"))

  (delete-directory/files git-parser-path #:must-exist? #f)
  (copy-directory/files parser-path git-parser-path)

  (define make
    (or (find-executable-path "make")
        (raise-user-error "make is not installed")))

  (define (assert-success code)
    (unless (= 0 code)
      (exit code)))

  (displayln "--- Parsing Raylib Header")
  (parameterize ([current-directory git-parser-path])
    (assert-success (system*/exit-code make)))

  (raylib-raw-root (url->string (path->url git-path)))

  (displayln "--- Generating code")
  (for ([flavour (in-list '("2d" "generated"))])
    (displayln (format "---- Generating ~a bindings" flavour))
    (define config (build-path configs-path (format "~a.rkt" flavour)))
    (define target (build-path root-path (format "~a" flavour)))
    (do-codegen
     #:config-source config
     #:clear? #t
     target))

  (displayln "--- Cleaning up")
  (delete-directory/files git-path #:must-exist? #f))

(module+ main
  (require racket/cmdline)
  (command-line
   #:args (raylib-ref)
   (main raylib-ref)))
