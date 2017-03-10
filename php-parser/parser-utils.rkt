#lang racket

(require (for-syntax racket/syntax)
         racket/serialize
         (only-in parser-tools/lex position-offset))

(provide
 (struct-out Position)
 read-until
 read-until-eof
 tokenize-string
 sub-ast? get-sub-ast
 ast-struct
 position-start-offset
 position-end-offset
 prop:sub-ast
 extra-ast?
 get-extra-ast
 set-extra-ast!
 (struct-out Extra))

(struct Position (start end))

(define (position-start-offset pos)
  (position-offset (Position-start pos)))

(define (position-end-offset pos)
  (position-offset (Position-end pos)))

(define-values (prop:sub-ast sub-ast? sub-ast-ref)
  (make-struct-type-property 'sub-ast))

(define-values (prop:extra-ast extra-ast? extra-ast-ref)
  (make-struct-type-property 'extra-ast))

(serializable-struct Extra (parent type))

;; do somestuff like (filter (lambda (x) (symbol? (syntax->datum x))) (syntax->list #'(args ...)))
(define-syntax (ast-struct stx)
  (syntax-case stx ()
    [(_ name (args ...) props ...)
     #`(serializable-struct name #,(append (syntax->list #'(args ...)) (list #'(extra-ast #:mutable #:auto))) props ...
               #:property prop:extra-ast
               #,(with-syntax ([get (format-id stx "~a-extra-ast" #'name)]
                               [set (format-id stx "set-~a-extra-ast!" #'name)])
                   #'(cons
                      (lambda (x) (get x))
                      (lambda (x v) (set x v))))
               #:property prop:sub-ast
               (lambda (x)
                 (list
                  #,@(for/list ([a (syntax->list #'(args ...))])
                       (with-syntax ([f (if (symbol? (syntax->datum a)) (format-id stx "~a-~a" #'name a) #f)])
                         #`(if f (f x) '()))))))]
    [(_ name super (args ...) props ...)
     #`(serializable-struct name super #,(append (syntax->list #'(args ...)) (list #'(extra-ast #:mutable #:auto))) props ...
               #:property prop:extra-ast
               #,(with-syntax ([get (format-id stx "~a-extra-ast" #'name)]
                               [set (format-id stx "set-~a-extra-ast!" #'name)])
                   #'(cons
                      (lambda (x) (get x))
                      (lambda (x v) (set x v))))
               #:property prop:sub-ast
               (lambda (x)
                 (list
                  #,@(for/list ([a (syntax->list #'(args ...))])
                       (with-syntax ([f (if (symbol? (syntax->datum a)) (format-id stx "~a-~a" #'name a) #f)])
                         #`(if f (f x) '()))))))]))

(define (get-sub-ast st)
  ((sub-ast-ref st) st))

(define (get-extra-ast st)
  ((car (extra-ast-ref st)) st))

(define (set-extra-ast! st v)
  ((cdr (extra-ast-ref st)) st v))

(define (read-until-eof input-port size)
  (define (loop data size)
    (if (<= size 0)
        data
        (let ([c (read-char input-port)])
          (if (eof-object? c)
              data
              (loop (string-append data (string c))
                    (- size 1))))))
  (loop "" size))

(define (read-until until input-port)
  (define (loop data block)
    (if (equal? until block)
        data
        (let ([read/c (read-char input-port)])
          (if (eof-object? read/c)
              data
              (loop (string-append data (string read/c))
                    (string-append
                     (substring block 1 (string-length block))
                     (string read/c)))))))
  (define block (read-until-eof input-port (string-length until)))
  (loop block block))


(define (tokenize-string quote-char input-port)
  (define (recur data)
    (define read/c (read-char input-port))
    (cond
     [(eof-object? read/c) data]
     [(equal? #\\ read/c) (recur (string-append data (string read/c)
                                           (string (read-char input-port))))]
     [(equal? quote-char read/c) (string-append data (string read/c))]
     [else (recur (string-append data (string read/c)))]))
  (recur ""))
