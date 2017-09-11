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
 position-start-offset
 position-end-offset
 prop:sub-ast)

(struct Position (start end))

(define (position-start-offset pos)
  (position-offset (Position-start pos)))

(define (position-end-offset pos)
  (position-offset (Position-end pos)))

(define-values (prop:sub-ast sub-ast? sub-ast-ref)
  (make-struct-type-property 'sub-ast))

(define (get-sub-ast st)
  ((sub-ast-ref st) st))

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
