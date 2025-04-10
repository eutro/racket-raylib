#!/usr/bin/env racket
#lang racket/base

(require (for-syntax racket/base)

         racket/runtime-path
         racket/file
         racket/system
         racket/port
         racket/string
         racket/match

         net/git-checkout
         net/url

         file/untar
         file/unzip
         file/gunzip

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

(define raylib-platforms
  '(("linux_amd64"     tar "libraylib.so.~a.0" "libraylib.so")
    ("macos"           tar "libraylib.~a.0.dylib" "libraylib.dylib")
    ;("win32_mingw-w64" zip "raylib.dll" "raylib.dll")
    ("win32_msvc16"    zip "raylib.dll" "raylib.dll")
    ("win64_mingw-w64" zip "raylib.dll" "raylib.dll")
    ("win64_msvc16"    zip "raylib.dll" "raylib.dll")))

(define (assert-success code)
  (unless (= 0 code)
    (exit code)))

(define untgz ;; file/untgz doesn't provide #:handle-entry
  (make-keyword-procedure
   (lambda (kws kwargs port)
     (define-values (in out) (make-pipe 4096))
     (define t
       (thread
        (lambda ()
          (dynamic-wind
            void
            (lambda () (gunzip-through-ports port out))
            (lambda () (close-output-port out))))))
     (keyword-apply untar kws kwargs in null)
     (copy-port in (open-output-nowhere))
     (thread-wait t))))

(define (extract-from-archive
         type port
         target-file
         out-file)
  (define success? #f)
  (define files null)
  (define (save port)
    (make-parent-directory* out-file)
    (call-with-output-file* out-file
      (lambda (out) (copy-port port out)))
    (set! success? #t))
  (case type
    [(tar)
     (define (tar-filter path dest type size link-target modified perms)
       (set! files (cons path files))
       (and (eq? type 'file)
            (string-suffix? (path->string path) target-file)))
     (define (handle-entry kind path content size attribs)
       (save (make-limited-input-port content size #f))
       null)
     (untgz port #:filter tar-filter #:handle-entry handle-entry)]
    [(zip)
     (define (entry-reader name dir? inflated)
       (set! files (cons name files))
       (when (string-suffix? (bytes->string/utf-8 name) target-file)
         (save inflated)))
     (unzip port entry-reader)])
  (values success? files))

(define (main ref)
  (when (fetch-libs)
    (displayln "--- Fetching Raylib libraries")
    (delete-directory/files lib-path #:must-exist? #f)
    (define download-fmt-tar
      (format "https://github.com/raysan5/raylib/releases/download/~a/raylib-~a_~~a.tar.gz"
              ref ref))
    (define download-fmt-zip
      (format "https://github.com/raysan5/raylib/releases/download/~a/raylib-~a_~~a.zip"
              ref ref))
    (for ([platform (in-list raylib-platforms)])
      (match-define (list platform-name archive-format target-file-fmt out-file-name) platform)
      (define archive-url
        (format
         (if (eq? 'tar archive-format)
             download-fmt-tar
             download-fmt-zip)
         platform-name))
      (define target-file
        (if (string-contains? target-file-fmt "~")
            (format target-file-fmt ref)
            target-file-fmt))
      (define out-file (build-path lib-path platform-name out-file-name))
      (printf "fetching ~a for ~a\n" target-file platform-name)
      (define port (get-pure-port (string->url archive-url) #:redirections 10))
      (dynamic-wind
        void
        (lambda ()
          (define-values (success? files)
            (extract-from-archive
             archive-format
             port
             target-file
             out-file))
          (unless success?
            (printf "could not find ~a in archive at ~a\n"
                    target-file
                    archive-url)
            (displayln "files in archive:")
            (for ([archive-file (in-list files)])
              (displayln archive-file))
            (raise-user-error "extraction failed")))
        (lambda ()
          (close-input-port port)))))

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
    (for ([opt
           (in-list (list fetch-libs
                          checkout-raylib
                          parse-api
                          generate-code
                          delete-raylib))])
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
