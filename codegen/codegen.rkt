#!/usr/bin/env racket
#lang racket/base

(require json
         xml
         xml/path
         net/url

         syntax/datum

         racket/file
         racket/port
         racket/match
         racket/function
         racket/contract/base
         racket/contract/region

         scribble/text

         "logger.rkt"
         "util.rkt"
         "config.rkt"
         "objects.rkt")

(provide raylib-raw-root
         do-codegen)

(define/contract raylib-raw-root
  (parameter/c string?)
  (make-parameter "https://raw.githubusercontent.com/raysan5/raylib/master"))

(define/contract (do-codegen
                  #:config-source [config-source "codegen-conf.rkt"]
                  #:clear? [clear #f]
                  generated-path)
  (->* (path-string?)
       (#:config-source path-string?
        #:clear? boolean?)
       void?)
  (define gen-path (path->complete-path generated-path))
  (define conf-path (path->complete-path config-source gen-path))
  (define kill-logger (make-semaphore))
  (define logger-thread
    (thread
     (let ()
       (define receiver (make-log-receiver codegen-logger 'debug))
       (define (do-logging msg)
         (printf "[~a] ~a\n"
                 (vector-ref msg 0)
                 (vector-ref msg 1)))
       (define (loop)
         (define msg (sync receiver kill-logger))
         (unless (semaphore? msg) (do-logging msg) (loop)))
       (define (finish-logging)
         (define msg (sync/timeout 0 receiver))
         (when msg (do-logging msg) (finish-logging)))
       (thunk
        (parameterize ([current-output-port (current-error-port)])
          (loop)
          (finish-logging))))))
  (dynamic-wind
    void
    (lambda ()
      (when clear
        (log-codegen-info "Deleting ~a" gen-path)
        (delete-directory/files gen-path #:must-exist? #f))
      (generate-bindings gen-path conf-path))
    (lambda ()
      (semaphore-post kill-logger)
      (thread-wait logger-thread))))

(define/contract (generate-bindings gen-path conf-path)
  (-> path? path? void?)

  (log-codegen-info "Retrieving config from ~a" conf-path)
  (define config
    (with-handlers ([exn:fail?
                     (lambda (e)
                       (log-codegen-fatal "error retrieving config, see error")
                       (raise e))])
      (read-config conf-path)))
  (define conf-parent (simplify-path (build-path conf-path 'up)))

  (log-codegen-info "Fetching API")
  (define api-json
    ((compose1 read-json open-input-string fetch-api-url)
     "parser/output/raylib_api.json"))
  (define api-xexpr
    ((compose1 xml->xexpr document-element read-xml open-input-string fetch-api-url)
     "parser/output/raylib_api.xml"))
  (define api-header (fetch-api-url "src/raylib.h"))

  (log-codegen-info "Parsing structs")
  (define structs-parsed (map parse-struct (hash-ref api-json 'structs)))
  (define typedefs-parsed (parse-typedefs api-header))
  (define function-typedefs-parsed (parse-function-typedefs api-header))

  (log-codegen-info "Parsing enums")
  (define enums-parsed (map parse-enum (hash-ref api-json 'enums)))

  (log-codegen-info "Parsing functions")
  (define functions-parsed
    (map parse-function (filter pair? (se-path*/list '(Functions) api-xexpr))))

  (log-codegen-info "Parsing constants")
  (define constants-parsed (map parse-constant (hash-ref api-json 'defines)))

  (log-codegen-info "Generating code")
  (config-codegen
   config
   gen-path
   conf-parent
   (hash
    'structs structs-parsed
    'typedefs typedefs-parsed
    'function-typedefs function-typedefs-parsed
    'enums enums-parsed
    'functions functions-parsed
    'constants constants-parsed))

  (void))

(define (fetch-api-url path)
  (define res-url (format "~a/~a" (raylib-raw-root) path))
  (log-codegen-debug "Fetching ~s" res-url)
  (port->string (get-pure-port (string->url res-url))))

(define (parse-function function-xexpr)
  (datum-case
   (filter pair? function-xexpr) (Param desc name paramCount retType type)
   [(((desc fdesc) (name fname) (paramCount _) (retType return-type))
     (Param ((desc _) (name pname) (type ptype))) ...)
    (let ()
      (define params (datum ((pname . ptype) ...)))
      (define-values (params* varargs)
        (if (and (pair? params) (equal? '("args" . "...") (last params)))
            (values (drop-right params 1) #t)
            (values params #f)))
      (make-api-function
       (datum fname)
       (datum fdesc)
       (datum return-type)
       params*
       varargs))]))

(define (parse-typedefs header)
  (define typedef-lines
    (regexp-match* #px"\ntypedef ([^ ]+) ([^ ]+);" header
                   #:match-select values))
  (for/list ([typedef typedef-lines])
    (match-define (list _ aliased-type alias-name) typedef)
    (make-api-typedef alias-name #f aliased-type)))

(define (parse-function-typedefs header)
  (define typedef-lines
    (filter-map (curry regexp-match #px"typedef (.+)\\(\\*([^\\)]+)\\)\\((.+)\\);")
                (string-split header "\n")))
  (for/list ([typedef typedef-lines])
    (match-define (list _ ret-type name params) typedef)
    (define param-list (string-split params ", "))
    (define (split-param param)
      (match-define (list _ type name)
        (regexp-match #px"^(.+? \\*?)(\\w+)$" param))
      (cons name (string-trim type)))
    (make-api-callback-typedef
     name
     #f
     (string-trim ret-type)
     (map split-param param-list))))

(define (parse-struct struct-json)
  (define (parse-struct-field field-json)
    (make-api-field
     (string-split (hash-ref field-json 'name) ", ")
     (nonempty-or-false (hash-ref field-json 'description))
     (hash-ref field-json 'type)))
  (make-api-struct
   (hash-ref struct-json 'name)
   (nonempty-or-false (hash-ref struct-json 'description))
   (map parse-struct-field (hash-ref struct-json 'fields))))

(define (parse-enum enum-json)
  (define (parse-enum-value value-json)
    (make-api-enum-value
     (hash-ref value-json 'name)
     (nonempty-or-false (hash-ref value-json 'description))
     (hash-ref value-json 'value)))
  (make-api-enum
   (hash-ref enum-json 'name)
   (nonempty-or-false (hash-ref enum-json 'description))
   (map parse-enum-value (hash-ref enum-json 'values))))

(define (parse-constant constant-json)
  (make-api-constant
   (hash-ref constant-json 'name)
   (nonempty-or-false (hash-ref constant-json 'description))
   (hash-ref constant-json 'type)
   (hash-ref constant-json 'value)))

(module+ main
  (require racket/cmdline)

  (define (main)
    (define config-source* "codegen-conf.rkt")
    (define clear #f)
    (command-line
     #:once-any
     [("--url")
      url
      "Set the base request URL (default: https://raw.githubusercontent.com/raysan5/raylib/master)"
      (raylib-raw-root url)]
     #:once-each
     [("--config")
      config-source
      "Set the config file"
      (set! config-source* (path->complete-path config-source))]
     [("--clear")
      "Clear the target directory before generating new files"
      (set! clear #t)]
     #:args (generated-path)
     (do-codegen
      #:config-source config-source*
      #:clear? clear
      generated-path)))

  (main))
