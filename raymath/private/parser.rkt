#lang racket/base

(require (for-syntax racket/base
                     racket/port
                     racket/string
                     racket/list
                     racket/format
                     syntax/parse 
                     syntax/location
                     "../../configs/templates/common.rkt"))

(provide parse-from-file)

(begin-for-syntax
  (define (parse-params params-str)
    (if (string=? params-str "void")
        null
        (for/list ([param (in-list (string-split params-str ", "))])
          (define split (regexp-match #px"^(.+(?: \\**))(\\w+)$" param))
          (define-values (name type)
            (parse-type (caddr split)
                        (string-trim (cadr split))))
          (list (with-input-from-string name read)
                ':
                (with-input-from-string type read)))))

  (define ((parse-from-port stx f) port)
    (define (line-type line)
      (cond
        [(string-prefix? line "//-") #\S]
        [(string-prefix? line "//") #\C]
        [(string-prefix? line "RMAPI ") #\F]
        [(string=? line "") #\space]
        [(string-prefix? line "#") #f]
        [else (error 'line-type "Bad line: ~s" line)]))

    (port-count-lines! port)
    (define-values (lines-list types-list posns-list)
      (for/lists [lines types srclocs]
                 ([(lineno colno pos) (in-producer (λ () (port-next-location port)))]
                  [line (in-lines port)]
                  #:do [(define type (line-type line))]
                  #:when type)
        (values line type (srcloc (object-name port) lineno colno pos #f))))

    (define lines-vec (list->vector lines-list))
    (define posns-vec (list->vector posns-list))
    (define types-str (list->string types-list))
    (define funcs (regexp-match-positions* #rx"C*F" types-str))

    (define func-stxs
      (for/list ([func-pos (in-list funcs)])
        (define proto-pos (sub1 (cdr func-pos)))
        (define comment
          (apply
           ~a
           #:separator " "
           (for/list ([i (in-range (car func-pos) proto-pos)])
             (cadr (regexp-match #px"^//\\s*(.+)$" (vector-ref lines-vec i))))))
        (define proto-line (vector-ref lines-vec proto-pos))
        (define matched
          (regexp-match
           #px"^RMAPI (\\w+) (\\w+)\\((.+)\\)$"
           proto-line))
        (unless matched
          (writeln proto-line))
        (define-values (return-ty0 name0 params-str) (apply values (cdr matched)))
        (define-values (name return-ty) (parse-type name0 return-ty0))
        (define params (parse-params params-str))
        (datum->syntax
         stx
         (list comment
               (with-input-from-string name read)
               '->
               (with-input-from-string return-ty read)
               params)
         (vector-ref posns-vec proto-pos))))

    (quasisyntax/loc stx
      (#,f #,@func-stxs))))

(define-syntax (parse-from-file stx)
  (syntax-parse stx
    [(_ f path:str)
     (call-with-input-file*
       (build-path
        (syntax-source-directory stx)
        (syntax-e #'path))
       (parse-from-port stx #'f))]))
