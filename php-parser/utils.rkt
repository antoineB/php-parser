#lang racket

(require "parser.rkt"
         (only-in "parser-utils.rkt" Position-start Position-end))

(provide extract-use-stmt)

(define (hash-mutable->hash-immutable h)
  (make-immutable-hash (hash->list h)))

;; Convert a list of UseStmt to 3 hash table (for classes, functions, consts) of
;; alias -> complete namespace.
(define (extract-use-stmt uses)
  (define functions (make-hash))
  (define classes (make-hash))
  (define consts (make-hash))

  (define (group-use-dcl->list-usedcl use)
    (define grp-name (GroupUseDeclaration-name use))
    (for/list ([use (GroupUseDeclaration-uses use)])
      (define use-namespace (UseDeclaration-name use))
      (UseDeclaration
       (Position-start use)
       (Position-end use)
       (NamespaceName
        (Position-start use-namespace)
        (Position-end use-namespace)
        (NamespaceName-global grp-name)
        (append
         (NamespaceName-name grp-name)
         (NamespaceName-name use-namespace)))
       (UseDeclaration-alias use)
       (UseDeclaration-type use))))

  (for* ([use uses]
         [sub (UseStmt-uses use)]
         [lst
          (cond [(UseDeclaration? sub)
                 (list sub)]
                [(GroupUseDeclaration? sub)
                 (group-use-dcl->list-usedcl sub)])])
    (match lst
      [(UseDeclaration _ _ name alias type)
       (define coerce-alias (or alias (last (NamespaceName-name name))))
       (case (UseStmt-type use)
         [(function) (hash-set! functions coerce-alias name)]
         [(const) (hash-set! consts coerce-alias name)]
         [(class #f)
          (case type
            [(function) (hash-set! functions coerce-alias name)]
            [(const) (hash-set! consts coerce-alias name)]
            [(class #f) (hash-set! classes coerce-alias name)])])]))

  (values
   (hash-mutable->hash-immutable classes)
   (hash-mutable->hash-immutable functions)
   (hash-mutable->hash-immutable consts)))
