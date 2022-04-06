#!/usr/bin/env racket
#lang racket/base

(require (for-syntax racket/base)
         racket/runtime-path

         raylib/codegen/codegen)

(define-runtime-path configs-path '(lib "raylib/configs"))
(define-runtime-path root-path ".")

(for ([flavour (in-list '("2d" "generated"))])
  (define config (build-path configs-path (format "~a.rkt" flavour)))
  (define target (build-path root-path (format "~a" flavour)))
  (do-codegen
   #:config-source config
   #:clear? #t
   target))
