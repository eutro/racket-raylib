#lang racket/base

(require racket/string
         racket/contract

         "objects.rkt")

(provide (all-defined-out))

(define-struct/contract config
  ([predicate (-> string? (-> api-object? boolean?))]))

(define/contract current-config
  (parameter/c config?)
  (make-parameter (config (lambda (_t) (lambda (_x) #t)))))

(define (include-object? thing)
  (define check? ((config-predicate (current-config)) thing))
  (lambda (obj) (check? obj)))

(define (parse-config config-json)
  (define (make-string-filter ls ift iff)
    (cond
      [(null? ls) (lambda (_x) iff)]
      [else
       (define pattern
         (pregexp
          (cond
            [(list? ls)
             (string-join
              (for/list ([pat (in-list ls)])
                (format "(~a)" pat))
              "|")]
            [(string? ls) ls]
            [else (raise-user-error "filter value must be a string or list of strings")])))
       (lambda (x) (if (regexp-match pattern x) ift iff))]))

  (define (parse-filter filter-json)
    (define thing (hash-ref filter-json 'for #f))
    (define includes (hash-ref filter-json 'include #f))
    (define excludes (hash-ref filter-json 'exclude #f))
    (when (and includes excludes)
      (raise-user-error "filter contains both \"include\" and \"exclude\" values"))
    (define obj-filter
      (cond
        [includes (compose1 (make-string-filter includes #t #f) api-object-name)]
        [excludes (compose1 (make-string-filter excludes #f #t) api-object-name)]
        [else (raise-user-error "filter contains neither include nor exclude value")]))
    (define thing-filter
      (or (and thing (make-string-filter thing obj-filter #f))
          (lambda (_x) obj-filter)))
    thing-filter)

  (define (compose-filters filters)
    (lambda (thing)
      (or (ormap (lambda (f) (f thing)) filters)
          (raise-user-error (format "no filters matching ~s" thing)))))

  (define filters (hash-ref config-json 'filters #f))
  (define filter?
    (if filters
        (compose-filters (map parse-filter filters))
        (lambda (_t) (lambda (_x) #t))))
  (config filter?))
