#lang racket

;; Parse a plain php file.

(require  "parser-utils.rkt"
          parser-tools/yacc
          parser-tools/lex
          (prefix-in : parser-tools/lex-sre))

(define (force-list sexpr)
  (if (list? sexpr)
      sexpr
      (list sexpr)))

(module+ test
  (require rackunit))

(provide php-parse
         php-lexe
         useless-token?
         php-lexer
         php-parser
         php-expr-parser
         (struct-out Variable)
         (struct-out AddrVariable)
         (struct-out ArrayAccess)
         (struct-out BraceAccess)
         (struct-out BraceVariable)
         (struct-out IndirectionVariable)
         (struct-out ObjectChain)
         (struct-out Array)
         (struct-out LiteralArray)
         (struct-out InstanceOfExpr)
         (struct-out TestExpr)
         (struct-out PrintExpr)
         (struct-out AtExpr)
         (struct-out UnsetCast)
         (struct-out Binary)
         (struct-out Infix)
         (struct-out Postfix)
         (struct-out Unary)
         (struct-out Cast)
         (struct-out Assign)
         (struct-out Yield)
         (struct-out Exit)
         (struct-out Clone)
         (struct-out FunctionCallParameter)
         (struct-out FunctionCall)
         (struct-out ObjectAccess)
         (struct-out BraceNaming)
         (struct-out ClassName)
         (struct-out NewExpr)
         (struct-out ExprStmt)
         (struct-out BlockStmt)
         (struct-out EmptyStmt)
         (struct-out LabelStmt)
         (struct-out IfStmt)
         (struct-out WhileStmt)
         (struct-out DoWhileStmt)
         (struct-out BreakStmt)
         (struct-out ContinueStmt)
         (struct-out TryStmt)
         (struct-out FinallyStmt)
         (struct-out CatchStmt)
         (struct-out ThrowStmt)
         (struct-out ReturnStmt)
         (struct-out EchoStmt)
         (struct-out UnsetStmt)
         (struct-out NamespaceStmt)
         (struct-out NamespaceName)
         (struct-out UseDeclaration)
         (struct-out UseStmt)
         (struct-out FunctionDcl)
         (struct-out LambdaDcl)
         (struct-out MethodDcl)
         (struct-out ParameterDcl)
         (struct-out ClassDcl)
         (struct-out InterfaceDcl)
         (struct-out ConstClassDcl)
         (struct-out GotoStmt)
         (struct-out PropertyDcl)
         (struct-out ConstDcl)
         (struct-out DeclareStmt)
         (struct-out GlobalStmt)
         (struct-out StaticStmt)
         (struct-out IssetExpr)
         (struct-out IncludeExpr)
         (struct-out IncludeOnceExpr)
         (struct-out EmptyExpr)
         (struct-out EvalExpr)
         (struct-out RequireExpr)
         (struct-out RequireOnceExpr)
         (struct-out BackQuoteExpr)
         (struct-out ForLoop)
         (struct-out Switch)
         (struct-out ForEachLoop)
         (struct-out ListPattern)
         (struct-out TraitAlias)
         (struct-out TraitStmt)
         (struct-out ChainBrace)
         (struct-out TraitPrecedence)
         (struct-out ChainCall)
         (struct-out ChainArray)
         (struct-out ConstDcls)
         (struct-out ConstClassDcls))


(define-lex-abbrevs
 (lower-letter (:/ "a" "z"))
 (upper-letter (:/ #\A #\Z))
 (blanks (:or #\space #\tab "\r"))
 (newline #\newline)
 (digit (:/ "0" "9"))
 (symbol-regex (:: (:or lower-letter upper-letter #\_)
                   (:* (:or lower-letter upper-letter #\_ digit)))))

(define-empty-tokens op-tokens
  (EOF OPAREN CPAREN OBRACE CBRACE OBRAKET CBRAKET INCLUDE INCLUDE_ONCE EVAL
REQUIRE REQUIRE_ONCE LOGICAL_OR LOGICAL_XOR LOGICAL_AND PRINT YIELD PLUS_EQUAL
MINUS_EQUAL MULT_EQUAL DIV_EQUAL CONCAT_EQUAL MOD_EQUAL AND_EQUAL OR_EQUAL EXPO_EQUAL
XOR_EQUAL SL_EQUAL SR_EQUAL BOOLEAN_OR BOOLEAN_AND IS_EQUAL IS_NOT_EQUAL
IS_IDENTICAL IS_NOT_IDENTICAL IS_SMALLER_OR_EQUAL IS_GREATER_OR_EQUAL SL SR
INSTANCEOF INC DEC INT_CAST DOUBLE_CAST STRING_CAST ARRAY_CAST OBJECT_CAST
BOOL_CAST UNSET_CAST BIN_CAST NEW CLONE EXIT IF ELSEIF ELSE ENDIF ECHO DO WHILE
ENDWHILE FOR ENDFOR FOREACH ENDFOREACH DECLARE ENDDECLARE AS SWITCH ENDSWITCH
CASE DEFAULT BREAK CONTINUE GOTO FUNCTION CONST RETURN TRY CATCH FINALLY THROW
USE INSTEADOF GLOBAL STATIC ABSTRACT FINAL PRIVATE PROTECTED PUBLIC VAR UNSET
ISSET EMPTY CLASS TRAIT INTERFACE EXTENDS IMPLEMENTS OBJECT_OPERATOR
DOUBLE_ARROW LIST ARRAY CALLABLE CLASS_C TRAIT_C METHOD_C FUNC_C LINE FILE
NAMESPACE NS_C DIR OPEN_TAG DOLLAR_OPEN_CURLY_BRACES DOLLAR OPEN_TAG_WITH_ECHO
CLOSE_TAG CURLY_OPEN PAAMAYIM_NEKUDOTAYIM NS_SEPARATOR
SEMICOLON COLON NEG BAR HAT AMPERSTAND COMMA DOT PLUS MINUS DIV MULT MOD TILD AT EXPO
QUESTION ASSIGN SMALLER GREATER LOW_PRIORITY_RULE HALT_COMPILER
BOOL_TRUE BOOL_FALSE
ELLIPSIS))

(define-tokens value-tokens
  (HEREDOC VARIABLE IDENT INTEGER FLOAT QUOTE_STRING D_QUOTE_STRING BACKQUOTE_STRING DOCUMENTATION LINE_COMMENT COMMENT BLANKS NEWLINES))


;; Regex that match the the begining of class|function|property|method. This is
;; used to check during the lexing if a comment can be used to document.
(define doc-comment-regex
  #rx"^(\n|\t| |\r)*(public|protected|private|static|abstract|final|var|function|class|interface|const)")

(define (usefull-doc-comment? input-port)
  (regexp-match-peek doc-comment-regex
                     input-port))

(define get-heredoc-token
  (lexer
   [(:: #\' (:+ (:or lower-letter upper-letter #\_ #\-)) #\' #\newline)
    (begin0
      (string-append
       lexeme
       (read-until
        (substring lexeme 1 (- (string-length lexeme) 2))
        input-port))
      (let-values ([(line col offset) (port-next-location input-port)])
        (set! end-pos (make-position offset line col))))]
   [(:: (:+ (:or lower-letter upper-letter #\_ #\-)) #\newline)
    (begin0
      (string-append
       lexeme
       (read-until
        (substring lexeme 0 (- (string-length lexeme) 1))
        input-port))
      (let-values ([(line col offset) (port-next-location input-port)])
        (set! end-pos (make-position offset line col))))]))

(define-syntax (read-token-until stx)
  (syntax-case stx ()
      [(_ token-name until)
       #'(let ([desc (read-until until input-port)])
           (let-values ([(line col offset) (port-next-location input-port)])
             (set! end-pos (make-position offset line col)))
           (token-name (string-append lexeme desc)))]))

(define-lex-trans (ignore-case stx)
  (syntax-case stx ()
    [(_ re)
     (let ([str (syntax-e #'re)])
       (when (not (string? str))
         (raise-syntax-error #f
                             "all arguments must be strings"
                             stx))
       #`(concatenation #,@(map (lambda (c) #`(union #,(char-upcase c) #,(char-downcase c))) (string->list str))))]))

(define php-lexer-with-keywords
  (lexer-src-pos
   [(:+ newline) (token-NEWLINES lexeme)]
   [(:+ blanks) (token-BLANKS lexeme)]

   [(:or "//" "#") (read-token-until token-LINE_COMMENT "\n")]
   ["/*" (read-token-until token-COMMENT "*/")]
   ["/**" (let* ([comment (read-until "*/" input-port)]
                 [data (string-append lexeme comment)])
            (let-values ([(line col offset) (port-next-location input-port)])
              (set! end-pos (make-position offset line col)))
             (if (not (usefull-doc-comment? input-port))
                (token-COMMENT data)
                (token-DOCUMENTATION data)))]

   ["<<<" (let ([str (get-heredoc-token input-port)])
            (token-HEREDOC (string-append "<<<" str)))]

   ["..." 'ELLIPSIS]
   [(ignore-case "new") 'NEW]
   [(ignore-case "clone") 'CLONE]
   [(ignore-case "exit") 'EXIT]
   [(ignore-case "if") 'IF]
   [(ignore-case "elseif") 'ELSEIF]
   [(ignore-case "else") 'ELSE]
   [(ignore-case "endif") 'ENDIF]
   [(ignore-case "echo") 'ECHO]
   [(ignore-case "do") 'DO]
   [(ignore-case "while") 'WHILE]
   [(ignore-case "endwhile") 'ENDWHILE]
   [(ignore-case "for") 'FOR]
   [(ignore-case "endfor") 'ENDFOR]
   [(ignore-case "foreach") 'FOREACH]
   [(ignore-case "endforeach") 'ENDFOREACH]
   [(ignore-case "declare") 'DECLARE]
   [(ignore-case "enddeclare") 'ENDDECLARE]
   [(ignore-case "as") 'AS]
   [(ignore-case "switch") 'SWITCH]
   [(ignore-case "endswitch") 'ENDSWITCH]
   [(ignore-case "case") 'CASE]
   [(ignore-case "default") 'DEFAULT]
   [(ignore-case "break") 'BREAK]
   [(ignore-case "continue") 'CONTINUE]
   [(ignore-case "goto") 'GOTO]
   [(ignore-case "function") 'FUNCTION]
   [(ignore-case "const") 'CONST]
   [(ignore-case "return") 'RETURN]
   [(ignore-case "try") 'TRY]
   [(ignore-case "catch") 'CATCH]
   [(ignore-case "finally") 'FINALLY]
   [(ignore-case "throw") 'THROW]
   [(ignore-case "use") 'USE]
   [(ignore-case "insteadof") 'INSTEADOF]
   [(ignore-case "global") 'GLOBAL]
   [(ignore-case "static") 'STATIC]
   [(ignore-case "abstract") 'ABSTRACT]
   [(ignore-case "final") 'FINAL]
   [(ignore-case "private") 'PRIVATE]
   [(ignore-case "protected") 'PROTECTED]
   [(ignore-case "public") 'PUBLIC]
   [(ignore-case "var") 'VAR]
   [(ignore-case "unset") 'UNSET]
   [(ignore-case "isset") 'ISSET]
   [(ignore-case "empty") 'EMPTY]
   [(ignore-case "class") 'CLASS]
   [(ignore-case "trait") 'TRAIT]
   [(ignore-case "interface") 'INTERFACE]
   [(ignore-case "extends") 'EXTENDS]
   [(ignore-case "include") 'INCLUDE]
   [(ignore-case "include_once") 'INCLUDE_ONCE]
   [(ignore-case "eval") 'EVAL]
   [(ignore-case "require") 'REQUIRE]
   [(ignore-case "require_once") 'REQUIRE_ONCE]
   [(ignore-case "or") 'LOGICAL_OR]
   [(ignore-case "xor") 'LOGICAL_XOR]
   [(ignore-case "and") 'LOGICAL_AND]
   [(ignore-case "print") 'PRINT]
   [(ignore-case "yield") 'YIELD]
   [(ignore-case "instanceof") 'INSTANCEOF]
   [(ignore-case "implements") 'IMPLEMENTS]
   [(ignore-case "list") 'LIST]
   [(ignore-case "array") 'ARRAY]
   [(ignore-case "callable") 'CALLABLE]
   [(ignore-case "namespace") 'NAMESPACE]
   [(ignore-case "true") 'BOOL_TRUE]
   [(ignore-case "false") 'BOOL_FALSE]

   ["+=" 'PLUS_EQUAL]
   ["-=" 'MINUS_EQUAL]
   ["**=" 'EXPO_EQUAL]
   ["*=" 'MULT_EQUAL]
   ["/=" 'DIV_EQUAL]
   [".=" 'CONCAT_EQUAL]
   ["%=" 'MOD_EQUAL]
   ["&=" 'AND_EQUAL]
   ["|=" 'OR_EQUAL]
   ["^=" 'XOR_EQUAL]
   ["<<=" 'SL_EQUAL]
   [">>=" 'SR_EQUAL]
   ["||" 'BOOLEAN_OR]
   ["&&" 'BOOLEAN_AND]
   ["==" 'IS_EQUAL]
   ["!=" 'IS_NOT_EQUAL]
   ["===" 'IS_IDENTICAL]
   ["!==" 'IS_NOT_IDENTICAL]
   ["<=" 'IS_SMALLER_OR_EQUAL]
   [">=" 'IS_GREATER_OR_EQUAL]
   ["<<" 'SL]
   [">>" 'SR]
   ["++" 'INC]
   ["--" 'DEC]
   ["(int)" 'INT_CAST]
   ["(Integer)" 'INT_CAST]
   ["(integer)" 'INT_CAST]
   ["(double)" 'DOUBLE_CAST]
   ["(Double)" 'DOUBLE_CAST]
   ["(Float)" 'DOUBLE_CAST]
   ["(float)" 'DOUBLE_CAST]
   ["(real)" 'DOUBLE_CAST]
   ["(Real)" 'DOUBLE_CAST]
   ["(string)" 'STRING_CAST]
   ["(String)" 'STRING_CAST]
   ["(array)" 'ARRAY_CAST]
   ["(object)" 'OBJECT_CAST]
   ["(bool)" 'BOOL_CAST]
   ["(Boolean)" 'BOOL_CAST]
   ["(boolean)" 'BOOL_CAST]
   ["(binary)" 'BIN_CAST]
   ["(Binary)" 'BIN_CAST]
   ["(unset)" 'UNSET_CAST]
   ["__NAMESPACE__" 'NS_C]
   ["->" (begin
           (lexer-no-keyword #t)
           'OBJECT_OPERATOR)]
   ["=>" 'DOUBLE_ARROW]
   ["__CLASS__" 'CLASS_C]
   ["__TRAIT__" 'TRAIT_C]
   ["__METHOD__" 'METHOD_C]
   ["__FUNCTION__" 'FUNC_C]
   ["__LINE__" 'LINE]
   ["__FILE__" 'FILE]
   ["__DIR__" 'DIR]
   ["__halt_compiler" 'HALT_COMPILER]

   [(:: "<?php" (:or #\space #\tab #\newline)) 'OPEN_TAG]
   ["<?=" 'OPEN_TAG_WITH_ECHO]
   ["?>" 'CLOSE_TAG]
;;   ["${" 'DOLLAR_OPEN_CURLY_BRACES]
;;   ["{$" 'CURLY_OPEN]
   ["::" 'PAAMAYIM_NEKUDOTAYIM]
   ["\\" 'NS_SEPARATOR]

   [#\' (let ([data (tokenize-string #\' input-port)])
          (token-QUOTE_STRING (string-append "'" data)))]
   [#\" (let ([data (tokenize-string #\" input-port)])
          (token-QUOTE_STRING (string-append "\"" data)))]

   [#\` (let ([data (tokenize-string #\` input-port)])
          (token-BACKQUOTE_STRING (string-append "`" data)))]

   [(:+ digit) (token-INTEGER (string->number lexeme))]
   [(:: (:+ digit) (:or #\e #\E) (:? (:or #\+ #\-)) (:+ digit))
    (token-INTEGER lexeme)]
   [(:: "0x" (:+ (:or digit #\a #\b #\c #\d #\e #\f #\A #\B #\C #\D #\E #\F)))
    (token-INTEGER lexeme)]

   [(:: (:* digit) #\. (:+ digit))
    (token-FLOAT (string->number lexeme))]
   [(:: (:+ digit) #\. (:* digit))
    (token-FLOAT (string->number lexeme))]
   [(:: (:* digit) #\. (:+ digit) (:or #\e #\E) (:? (:or #\+ #\-)) (:+ digit))
    (token-FLOAT (string->number lexeme))]
   [(:: (:+ digit) #\. (:* digit) (:or #\e #\E) (:? (:or #\+ #\-)) (:+ digit))
    (token-FLOAT (string->number lexeme))]

   [(:: "$" symbol-regex) (token-VARIABLE lexeme)]
   [symbol-regex (token-IDENT lexeme)]

   [#\$ 'DOLLAR]
   [#\+ 'PLUS]
   [#\; 'SEMICOLON]
   [#\! 'NEG]
   [#\| 'BAR]
   [#\^ 'HAT]
   [#\& 'AMPERSTAND]
   [#\: 'COLON]
   [#\, 'COMMA]
   [#\. 'DOT]
   [#\- 'MINUS]
   [#\/ 'DIV]
   ["**" 'EXPO]
   [#\* 'MULT]
   [#\% 'MOD]
   [#\~ 'TILD]
   [#\< 'SMALLER]
   [#\> 'GREATER]
   [#\? 'QUESTION]
   [#\@ 'AT]
   [#\{ 'OBRACE]
   [#\} 'CBRACE]
   [#\[ 'OBRAKET]
   [#\] 'CBRAKET]
   [#\( 'OPAREN]
   [#\) 'CPAREN]
   [#\= 'ASSIGN]

   [(eof) 'EOF]
   ))


;; This lexer forbid the use of php keywords, this is needed to permit the lexe
;; of something like "->while" as "->" and "while" where "while" is not lexed as
;; php keywords "while".
(define php-lexer-without-keywords
  (lexer-src-pos
   [(:+ newline) (token-NEWLINES lexeme)]
   [(:+ blanks) (token-BLANKS lexeme)]

   [(:or "//" "#") (read-token-until token-LINE_COMMENT "\n")]
   ["/*" (read-token-until token-COMMENT "*/")]
   ["/**" (let* ([comment (read-until "*/" input-port)]
                 [data (string-append lexeme comment)])
            (let-values ([(line col offset) (port-next-location input-port)])
              (set! end-pos (make-position offset line col)))
             (if (not (usefull-doc-comment? input-port))
                (token-COMMENT data)
                (token-DOCUMENTATION data)))]
   ["<<<" (let ([str (get-heredoc-token input-port)])
            (token-HEREDOC (string-append "<<<" str)))]

   ["+=" 'PLUS_EQUAL]
   ["-=" 'MINUS_EQUAL]
   ["**=" 'EXPO_EQUAL]
   ["*=" 'MULT_EQUAL]
   ["/=" 'DIV_EQUAL]
   [".=" 'CONCAT_EQUAL]
   ["%=" 'MOD_EQUAL]
   ["&=" 'AND_EQUAL]
   ["|=" 'OR_EQUAL]
   ["^=" 'XOR_EQUAL]
   ["<<=" 'SL_EQUAL]
   [">>=" 'SR_EQUAL]
   ["||" 'BOOLEAN_OR]
   ["&&" 'BOOLEAN_AND]
   ["==" 'IS_EQUAL]
   ["!=" 'IS_NOT_EQUAL]
   ["===" 'IS_IDENTICAL]
   ["!==" 'IS_NOT_IDENTICAL]
   ["<=" 'IS_SMALLER_OR_EQUAL]
   [">=" 'IS_GREATER_OR_EQUAL]
   ["<<" 'SL]
   [">>" 'SR]
   ["++" 'INC]
   ["--" 'DEC]
   ["(int)" 'INT_CAST]
   ["(Integer)" 'INT_CAST]
   ["(integer)" 'INT_CAST]
   ["(double)" 'DOUBLE_CAST]
   ["(Double)" 'DOUBLE_CAST]
   ["(Float)" 'DOUBLE_CAST]
   ["(float)" 'DOUBLE_CAST]
   ["(string)" 'STRING_CAST]
   ["(String)" 'STRING_CAST]
   ["(array)" 'ARRAY_CAST]
   ["(object)" 'OBJECT_CAST]
   ["(bool)" 'BOOL_CAST]
   ["(Boolean)" 'BOOL_CAST]
   ["(boolean)" 'BOOL_CAST]
   ["(binary)" 'BIN_CAST]
   ["(Binary)" 'BIN_CAST]
   ["(unset)" 'UNSET_CAST]
   ["__NAMESPACE__" 'NS_C]
   ["->" (begin
           (lexer-no-keyword #t)
           'OBJECT_OPERATOR)]
   ["=>" 'DOUBLE_ARROW]
;;   ["__CLASS__" 'CLASS_C] some people use $this->__CLASS__
   ["__TRAIT__" 'TRAIT_C]
   ["__METHOD__" 'METHOD_C]
   ["__FUNCTION__" 'FUNC_C]
   ["__LINE__" 'LINE]
   ["__FILE__" 'FILE]
   ["__DIR__" 'DIR]
   ["__halt_compiler" 'HALT_COMPILER]

   [(:: "<?php" (:or #\space #\tab #\newline)) 'OPEN_TAG]
   ["<?=" 'OPEN_TAG_WITH_ECHO]
   ["?>" 'CLOSE_TAG]
;;   ["${" 'DOLLAR_OPEN_CURLY_BRACES]
;;   ["{$" 'CURLY_OPEN]
   ["::" 'PAAMAYIM_NEKUDOTAYIM]
   ["\\" 'NS_SEPARATOR]

   [#\' (let ([data (tokenize-string #\' input-port)])
          (token-QUOTE_STRING (string-append "'" data)))]
   [#\" (let ([data (tokenize-string #\" input-port)])
          (token-QUOTE_STRING (string-append "\"" data)))]

   [#\` (let ([data (tokenize-string #\` input-port)])
          (token-BACKQUOTE_STRING (string-append "`" data)))]

   [(:+ digit)
    (token-INTEGER (string->number lexeme))]
   [(:: (:+ digit) (:or #\e #\E) (:? (:or #\+ #\-)) (:+ digit))
    (token-INTEGER lexeme)]
   [(:: "0x" (:+ (:or digit #\a #\b #\c #\d #\e #\f #\A #\B #\C #\D #\E #\F)))
    (token-INTEGER lexeme)]

   [(:: (:* digit) #\. (:+ digit))
    (token-FLOAT (string->number lexeme))]
   [(:: (:+ digit) #\. (:* digit))
    (token-FLOAT (string->number lexeme))]
   [(:: (:* digit) #\. (:+ digit) (:or #\e #\E) (:? (:or #\+ #\-)) (:+ digit))
    (token-FLOAT (string->number lexeme))]
   [(:: (:+ digit) #\. (:* digit) (:or #\e #\E) (:? (:or #\+ #\-)) (:+ digit))
    (token-FLOAT (string->number lexeme))]

   [(:: "$" symbol-regex) (token-VARIABLE lexeme)]
   [symbol-regex (token-IDENT lexeme)]

   [#\$ 'DOLLAR]
   [#\+ 'PLUS]
   [#\; 'SEMICOLON]
   [#\! 'NEG]
   [#\| 'BAR]
   [#\^ 'HAT]
   [#\& 'AMPERSTAND]
   [#\: 'COLON]
   [#\, 'COMMA]
   [#\. 'DOT]
   [#\- 'MINUS]
   [#\/ 'DIV]
   ["**" 'EXPO]
   [#\* 'MULT]
   [#\% 'MOD]
   [#\~ 'TILD]
   [#\< 'SMALLER]
   [#\> 'GREATER]
   [#\? 'QUESTION]
   [#\@ 'AT]
   [#\{ 'OBRACE]
   [#\} 'CBRACE]
   [#\[ 'OBRAKET]
   [#\] 'CBRAKET]
   [#\( 'OPAREN]
   [#\) 'CPAREN]
   [#\= 'ASSIGN]

   [(eof) 'EOF]
   ))

(define lexer-no-keyword (make-parameter #f))

(define (php-lexer input-port)
  (if (lexer-no-keyword)
      (begin
        (lexer-no-keyword #f)
        (php-lexer-without-keywords input-port))
      (php-lexer-with-keywords input-port)))

;; It didn't probably parse all php constructs.
(define-values (php-parser php-expr-parser)
  (apply
   values
   (parser

    (src-pos)
    (start program expr)
    (end EOF)

    (suppress)

    (tokens value-tokens op-tokens)
    (error (lambda x (print x) (print "Sorry the parser error message provide no clues.")))

    (precs
     (nonassoc LOW_PRIORITY_RULE)
     (left INCLUDE INCLUDE_ONCE EVAL REQUIRE REQUIRE_ONCE)
     (left COMMA)
     (left LOGICAL_OR)
     (left LOGICAL_XOR)
     (left LOGICAL_AND)
     (right PRINT)

     (left ASSIGN PLUS_EQUAL MINUS_EQUAL MULT_EQUAL DIV_EQUAL
           CONCAT_EQUAL MOD_EQUAL AND_EQUAL OR_EQUAL XOR_EQUAL SL_EQUAL SR_EQUAL
           EXPO_EQUAL)

     (left QUESTION COLON)
     (left BOOLEAN_OR)
     (left BOOLEAN_AND)
     (left BAR)
     (left HAT)
     (left AMPERSTAND)

     (nonassoc IS_EQUAL IS_NOT_EQUAL IS_IDENTICAL IS_NOT_IDENTICAL)
     (nonassoc SMALLER IS_SMALLER_OR_EQUAL GREATER IS_GREATER_OR_EQUAL)

     (left SL SR)
     (left PLUS MINUS DOT)
     (left MULT DIV MOD)
     (right NEG)
     (nonassoc INSTANCEOF)
     (right TILD INC DEC INT_CAST DOUBLE_CAST STRING_CAST ARRAY_CAST
            OBJECT_CAST BOOL_CAST UNSET_CAST BIN_CAST)
     (right AT)
     (right EXPO)
     (right OBRAKET)
     (nonassoc NEW CLONE)
     (left ELSEIF)
     (left ELSE)
     (left ENDIF)
     (nonassoc YIELD))

    (grammar

     (binary_bit_expr
      [(expr AMPERSTAND expr) (Binary $1-start-pos $3-end-pos 'AMPERSTAND $1 $3)]
      [(expr BAR expr) (Binary $1-start-pos $3-end-pos 'BAR $1 $3)]
      [(expr HAT expr) (Binary $1-start-pos $3-end-pos 'HAT $1 $3)]
      [(expr SL expr) (Binary $1-start-pos $3-end-pos 'SL $1 $3)]
      [(expr SR expr) (Binary $1-start-pos $3-end-pos 'SR $1 $3)])

     (binary_logic_expr
      [(expr BOOLEAN_OR expr) (Binary $1-start-pos $3-end-pos 'BOOLEAN_OR $1 $3)]
      [(expr BOOLEAN_AND expr) (Binary $1-start-pos $3-end-pos 'BOOLEAN_AND $1 $3)]
      [(expr LOGICAL_OR expr) (Binary $1-start-pos $3-end-pos 'LOGICAL_OR $1 $3)]
      [(expr LOGICAL_AND expr) (Binary $1-start-pos $3-end-pos 'LOGICAL_AND $1 $3)]
      [(expr LOGICAL_XOR expr) (Binary $1-start-pos $3-end-pos 'LOGICAL_XOR $1 $3)]
      [(expr IS_IDENTICAL expr) (Binary $1-start-pos $3-end-pos 'IS_IDENTICAL $1 $3)]
      [(expr IS_NOT_IDENTICAL expr) (Binary $1-start-pos $3-end-pos 'IS_NOT_IDENTICAL $1 $3)]
      [(expr IS_EQUAL    expr) (Binary $1-start-pos $3-end-pos 'IS_EQUAL $1 $3)]
      [(expr IS_NOT_EQUAL expr) (Binary $1-start-pos $3-end-pos 'IS_NOT_EQUAL $1 $3)]
      [(expr SMALLER        expr) (Binary $1-start-pos $3-end-pos 'SMALLER $1 $3)]
      [(expr IS_SMALLER_OR_EQUAL expr) (Binary $1-start-pos $3-end-pos 'IS_SMALLER_OR_EQUAL $1 $3)]
      [(expr GREATER expr) (Binary $1-start-pos $3-end-pos 'GREATER $1 $3)]
      [(expr IS_GREATER_OR_EQUAL expr) (Binary $1-start-pos $3-end-pos 'IS_GREATER_OR_EQUAL $1 $3)])

     (binary_op_expr
      [(expr PLUS expr) (prec INC) (Binary $1-start-pos $3-end-pos 'PLUS $1 $3)]
      [(expr MINUS expr) (prec DEC) (Binary $1-start-pos $3-end-pos 'MINUS $1 $3)]
      [(expr MULT expr) (Binary $1-start-pos $3-end-pos 'MULT $1 $3)]
      [(expr EXPO expr) (Binary $1-start-pos $3-end-pos 'EXPO $1 $3)]
      [(expr DIV expr) (Binary $1-start-pos $3-end-pos 'DIV $1 $3)]
      [(expr MOD expr) (Binary $1-start-pos $3-end-pos 'MOD $1 $3)]
      [(expr DOT expr) (Binary $1-start-pos $3-end-pos 'DOT $1 $3)])


     (unary_expr
      [(PLUS expr)  (Unary $1-start-pos $2-end-pos 'PLUS $2)]
      [(MINUS expr) (Unary $1-start-pos $2-end-pos 'MINUS $2)]
      [(NEG expr)   (Unary $1-start-pos $2-end-pos 'NEG $2)]
      [(TILD expr)  (Unary $1-start-pos $2-end-pos 'TILD $2)])

     (infix_expr
      [(INC variable) (Infix $1-start-pos $2-end-pos 'INC $2)]
      [(DEC variable) (Infix $1-start-pos $2-end-pos 'DEC $2)])

     (postfix_expr
      [(variable INC) (Postfix $1-start-pos $2-end-pos 'INC $1)]
      [(variable DEC) (Postfix $1-start-pos $2-end-pos 'DEC $1)])

     (assign_expr
      [(variable ASSIGN expr)
       (Assign $1-start-pos $3-end-pos 'ASSIGN $1 $3)]
      [(variable ASSIGN AMPERSTAND variable)
       (Assign $1-start-pos $4-end-pos 'ASSIGN $1
               (AddrVariable $3-start-pos $4-end-pos $4))]
      [(variable ASSIGN AMPERSTAND NEW class_name_reference ctor_arguments)
       (Assign $1-start-pos $6-end-pos 'ASSIGN $1
               (AddrVariable $3-start-pos $6-end-pos
                             (NewExpr $4-start-pos $6-end-pos $5 $6)))]
      [(variable PLUS_EQUAL   expr) (Assign $1-start-pos $3-end-pos 'PLUS_EQUAL $1 $3)]
      [(variable MINUS_EQUAL  expr) (Assign $1-start-pos $3-end-pos 'MINUS_EQUAL $1 $3)]
      [(variable MULT_EQUAL   expr) (Assign $1-start-pos $3-end-pos 'MULT_EQUAL $1 $3)]
      [(variable EXPO_EQUAL   expr) (Assign $1-start-pos $3-end-pos 'EXPO_EQUAL $1 $3)]
      [(variable DIV_EQUAL    expr) (Assign $1-start-pos $3-end-pos 'DIV_EQUAL $1 $3)]
      [(variable MOD_EQUAL    expr) (Assign $1-start-pos $3-end-pos 'MOD_EQUAL $1 $3)]
      [(variable AND_EQUAL    expr) (Assign $1-start-pos $3-end-pos 'AND_EQUAL $1 $3)]
      [(variable OR_EQUAL     expr) (Assign $1-start-pos $3-end-pos 'OR_EQUAL $1 $3)]
      [(variable XOR_EQUAL    expr) (Assign $1-start-pos $3-end-pos 'XOR_EQUAL $1 $3)]
      [(variable SL_EQUAL     expr) (Assign $1-start-pos $3-end-pos 'SL_EQUAL $1 $3)]
      [(variable SR_EQUAL     expr) (Assign $1-start-pos $3-end-pos 'SR_EQUAL $1 $3)]
      [(variable CONCAT_EQUAL expr) (Assign $1-start-pos $3-end-pos 'CONCAT_EQUAL $1 $3)])


     (cast_expr
      [(BOOL_CAST   expr) (Cast $1-start-pos $2-end-pos 'BOOL_CAST $2)]
      [(INT_CAST           expr) (Cast $1-start-pos $2-end-pos 'INT_CAST $2)]
      [(DOUBLE_CAST expr) (Cast $1-start-pos $2-end-pos 'DOUBLE_CAST $2)]
      [(STRING_CAST expr) (Cast $1-start-pos $2-end-pos 'STRING_CAST $2)]
      [(ARRAY_CAST  expr) (Cast $1-start-pos $2-end-pos 'ARRAY_CAST $2)]
      [(BIN_CAST  expr)   (Cast $1-start-pos $2-end-pos 'BIN_CAST $2)]
      [(OBJECT_CAST expr) (Cast $1-start-pos $2-end-pos 'OBJECT_CAST $2)])

     (variable
      [(base_variable_with_function_calls OBJECT_OPERATOR object_property
                                          method_or_not variable_properties)
       (ObjectChain $1-start-pos $5-end-pos
                    (append (list $1) (force-list $3) (force-list $4) $5))]
      [(base_variable_with_function_calls) $1])

     (parenthesis_expr
      [(OPAREN r_variable CPAREN) $2]
      [(OPAREN expr_without_new CPAREN) $2]
      [(OPAREN yield_expr CPAREN) $2])

     (expr_without_new
      [(parenthesis_expr) $1]
      [(scalar) $1]
      [(LIST OPAREN assignment_list CPAREN ASSIGN expr)
       (Assign $1-start-pos $6-end-pos 'ASSIGN
               (ListPattern $1-start-pos $4-end-pos $3) $6)]
      [(combined_scalar_offset) $1]
      [(combined_scalar) $1]
      [(OPAREN new_expr CPAREN instance_call)
       (if (empty? $4)
           $2
           (ObjectChain $1-start-pos $4-end-pos (cons $2 $4)))]
      [(CLONE expr) (Clone $1-start-pos $2-end-pos $2)]
      [(expr INSTANCEOF class_name_reference)
       (InstanceOfExpr $1-start-pos $3-end-pos $1 $3)]
      [(expr QUESTION expr COLON expr) (TestExpr $1-start-pos $5-end-pos $1 $3 $5)]
      [(expr QUESTION COLON expr) (TestExpr $1-start-pos $4-end-pos $1 null $4)]
      [(UNSET_CAST  expr) (UnsetCast $1-start-pos $2-end-pos $2)]
      [(EXIT exit_expr) (Exit $1-start-pos $2-end-pos $2)]
      [(AT expr) (AtExpr $1-start-pos $2-end-pos $2)]
      [(PRINT expr) (PrintExpr $1-start-pos $2-end-pos $2)]
      [(BACKQUOTE_STRING) (BackQuoteExpr $1-start-pos $1-end-pos $1)]
      [(lambda_expr) $1]
      [(YIELD) 'YIELD]
      [(yield_expr) $1]
      [(internal_functions_in_yacc) $1]
      [(STATIC lambda_expr)
       (LambdaDcl $1-start-pos $2-end-pos
                  (LambdaDcl-documentation $2)
                  true
                  (LambdaDcl-args $2)
                  (LambdaDcl-lexical $2)
                  (LambdaDcl-body $2)
                  (LambdaDcl-reference $2))]
      [(assign_expr) $1]
      [(binary_bit_expr) $1]
      [(binary_logic_expr) $1]
      [(binary_op_expr) $1]
      [(unary_expr) $1]
      [(infix_expr) $1]
      [(postfix_expr) $1]
      [(cast_expr) $1])

     (expr_without_variable
      [(new_expr) $1]
      [(expr_without_new) $1])

     (chaining_method_or_property
      [(chaining_method_or_property variable_property) (append $1 $2)]
      [(variable_property) $1])

     (chaining_dereference
      [(chaining_dereference OBRAKET dim_offset CBRAKET)
       (append $1 (list (ChainArray $2-start-pos $4-end-pos $3)))]
      [(OBRAKET dim_offset CBRAKET)
       (list (ChainArray $1-start-pos $3-end-pos $2))])

     (chaining_instance_call
      [(chaining_dereference chaining_method_or_property) (append $1 $2)]
      [(chaining_dereference) $1]
      [(chaining_method_or_property) $1])

     (instance_call
      [() '()]
      [(chaining_instance_call) $1])

     (lambda_expr
      [(FUNCTION OPAREN parameter_list CPAREN lexical_vars OBRACE inner_statement_list
                 CBRACE)
       (LambdaDcl $1-start-pos $8-end-pos null false $3 $5 $7 false)]
      [(FUNCTION AMPERSTAND OPAREN parameter_list CPAREN lexical_vars OBRACE
                 inner_statement_list CBRACE)
       (LambdaDcl $1-start-pos $9-end-pos null false $4 $6 $8 true)]
      [(DOCUMENTATION FUNCTION OPAREN parameter_list CPAREN lexical_vars OBRACE
                      inner_statement_list CBRACE)
       (LambdaDcl $1-start-pos $9-end-pos $1 false $4 $6 $8 false)]
      [(DOCUMENTATION FUNCTION AMPERSTAND OPAREN parameter_list
                      CPAREN lexical_vars OBRACE inner_statement_list CBRACE)
       (LambdaDcl $1-start-pos $10-end-pos $1 false $5 $7 $9 true)])


     (internal_functions_in_yacc
      [(ISSET OPAREN isset_variables CPAREN) (IssetExpr $1-start-pos $4-end-pos $3)]
      [(EMPTY OPAREN variable CPAREN) (EmptyExpr $1-start-pos $4-end-pos $3)]
      [(EMPTY OPAREN expr_without_variable CPAREN)
       (EmptyExpr $1-start-pos $4-end-pos $3)]
      [(INCLUDE expr) (IncludeExpr $1-start-pos $2-end-pos $2)]
      [(INCLUDE_ONCE expr) (IncludeOnceExpr $1-start-pos $2-end-pos $2)]
      [(INCLUDE OPAREN expr CPAREN) (IncludeExpr $1-start-pos $4-end-pos $3)]
      [(INCLUDE_ONCE OPAREN expr CPAREN) (IncludeOnceExpr $1-start-pos $4-end-pos $3)]
      [(EVAL OPAREN expr CPAREN) (EvalExpr $1-start-pos $4-end-pos $3)]
      [(REQUIRE expr) (RequireExpr $1-start-pos $2-end-pos $2)]
      [(REQUIRE_ONCE expr) (RequireOnceExpr $1-start-pos $2-end-pos $2)]
      [(REQUIRE OPAREN expr CPAREN) (RequireExpr $1-start-pos $4-end-pos $3)]
      [(REQUIRE_ONCE OPAREN expr CPAREN) (RequireOnceExpr $1-start-pos $4-end-pos $3)])

     (isset_variables
      [(isset_variable) (list $1)]
      [(isset_variables COMMA isset_variable) (append $1 (list $3))])

     (isset_variable
      [(variable) $1]
      [(expr_without_variable) $1])

     (new_expr
      [(NEW class_name_reference ctor_arguments) (NewExpr $1-start-pos $3-end-pos $2 $3)])

     (ctor_arguments
      [() '()]
      [(function_call_parameter_list) $1])


     (class_name_reference
      [(class_name) $1]
      [(dynamic_class_name_reference) $1])


     (dynamic_class_name_reference
      [(base_variable OBJECT_OPERATOR object_property
                      dynamic_class_name_variable_properties)
       (ObjectChain $1-start-pos $4-end-pos (append (list $1) $3 $4))]
      [(base_variable) (ObjectChain $1-start-pos $1-end-pos (list $1))])

     (dynamic_class_name_variable_properties
      [(dynamic_class_name_variable_properties dynamic_class_name_variable_property)
       (append $1 (list $2))]
      [() '()])

     (dynamic_class_name_variable_property
      [(OBJECT_OPERATOR object_property) $2])


     (lexical_vars
      [() '()]
      [(USE OPAREN lexical_var_list CPAREN) $3])

     (lexical_var_list
      [(lexical_var_list COMMA VARIABLE)
       (append $1 (list (Variable $3-start-pos $3-end-pos $3)))]
      [(lexical_var_list COMMA AMPERSTAND VARIABLE)
       (append $1 (list (AddrVariable $3-start-pos $4-end-pos
                                      (Variable $4-start-pos $4-end-pos $4))))]
      [(VARIABLE) (list (Variable $1-start-pos $1-end-pos $1))]
      [(AMPERSTAND VARIABLE)
       (list (AddrVariable $1-start-pos
                           $2-end-pos
                           (Variable $2-start-pos $2-end-pos $2)))])


     (exit_expr
      [() null]
      [(OPAREN CPAREN) null]
      [(parenthesis_expr) $1])

     (combined_scalar_offset
      [(combined_scalar OBRAKET dim_offset CBRAKET)
       (ArrayAccess $1-start-pos $4-end-pos $1 $3)]
      [(combined_scalar_offset OBRAKET dim_offset CBRAKET)
       (ArrayAccess $1-start-pos $4-end-pos $1 $3)]
      ;; | T_CONSTANT_ENCAPSED_STRING OBRAKET dim_offset CBRAKET
      ;; use normal string instead
      [(D_QUOTE_STRING OBRAKET dim_offset CBRAKET)
       (ArrayAccess $1-start-pos $4-end-pos $1 $3)]
      [(QUOTE_STRING OBRAKET dim_offset CBRAKET)
       (ArrayAccess $1-start-pos $4-end-pos $1 $3)])

     (combined_scalar
      [(ARRAY OPAREN array_pair_list CPAREN) (Array $1-start-pos $4-end-pos $3)]
      [(OBRAKET array_pair_list CBRAKET) (LiteralArray $1-start-pos $3-end-pos $2)])


     (assignment_list
      [(assignment_list COMMA assignment_list_element) (append $1 (list $3))]
      [(assignment_list_element) (list $1)])

     (assignment_list_element
      [(variable) $1]
      [(LIST OPAREN assignment_list CPAREN) $3]
      [() null])


     (scalar
      ;;T_STRING_VARNAME used for $ { abc }
      [(class_name_scalar) $1]
      [(class_constant) $1]
      [(fully_qualified_class_name) $1]
      [(common_scalar) $1]
      ;;'"' encaps_list '"' we don't treat encapsed variable in php
      ;;string, adding a D_QUOTE_STRING will produce reduce/reduce
      ;;conflicts

      ;;T_START_HEREDOC encaps_list T_END_HEREDOC
      [(HEREDOC) 'HEREDOC]
      [(CLASS_C) 'CLASS_C])


     (class_name_scalar
      [(class_name PAAMAYIM_NEKUDOTAYIM CLASS) (ClassName $1-start-pos $3-end-pos $1 'CLASS)])

     (class_constant
      [(class_name PAAMAYIM_NEKUDOTAYIM IDENT) (ClassName $1-start-pos $3-end-pos $1 $3)]
      [(variable_class_name PAAMAYIM_NEKUDOTAYIM IDENT) (ClassName $1-start-pos $3-end-pos $1 $3)])

     (variable_class_name
      [(reference_variable) $1])

     (r_variable
      [(variable) $1])

     (expr
      [(r_variable) $1]
      [(expr_without_variable) $1])

     (method_or_not
      [(method) (list (ChainCall $1-start-pos $1-end-pos $1))]
      [(array_method_dereference) $1]
      [() null])

     (array_method_dereference
      [(array_method_dereference OBRAKET dim_offset CBRAKET)
       (append $1 (list (ChainArray $2-start-pos $4-end-pos $3)))]
      [(method OBRAKET dim_offset CBRAKET)
       (list (ChainCall $1-start-pos $1-end-pos $1) (ChainArray $2-start-pos $4-end-pos $3))])


     (method
      [(function_call_parameter_list) $1])

     (variable_properties
      [(variable_properties variable_property) (append $1 $2)]
      [() '()])

     (variable_property
      [(OBJECT_OPERATOR object_property method_or_not)
       (cond [(null? $3) (force-list $2)]
             [(FunctionCallParameter? $3) (append (force-list $2) (list $3))]
             [else (append (force-list $2) $3)])])


     (object_property
      [(object_dim_list) $1]
      [(variable_without_objects) $1])


     (object_dim_list
      [(object_dim_list OBRAKET dim_offset CBRAKET)
       (append $1 (list (ChainArray $2-start-pos $4-end-pos $3)))]
      [(object_dim_list OBRACE expr CBRACE)
       (append $1 (list (ChainBrace $2-start-pos $4-end-pos  $3)))]
      [(variable_name) (list (ObjectAccess $1-start-pos $1-end-pos $1))])

     (variable_name
      [(IDENT) $1]
      [(OBRACE expr CBRACE) (BraceNaming $1-start-pos $3-end-pos $2)])


     (variable_without_objects
      [(reference_variable) $1]
      [(indirection reference_variable) (IndirectionVariable $1-start-pos $2-end-pos $2 $1)])


     (indirection [(DOLLAR) 1]
                  [(indirection DOLLAR) (+ 1 $1)])

     (base_variable_with_function_calls
      [(base_variable) $1]
      [(array_function_dereference) $1]
      [(function_call) $1])

     (array_function_dereference
      [(array_function_dereference OBRAKET dim_offset CBRAKET)
       (append $1 (list $3))]
      [(function_call OBRAKET dim_offset CBRAKET) (list $1 $3)])

     (function_call
      [(fully_qualified_class_name function_call_parameter_list)
       (FunctionCall $1-start-pos $2-end-pos $1 $2)]
      [(class_name PAAMAYIM_NEKUDOTAYIM variable_name function_call_parameter_list)
       (FunctionCall $1-start-pos $4-end-pos (ClassName $1-start-pos $3-end-pos $1 $3) $4)]
      [(class_name PAAMAYIM_NEKUDOTAYIM variable_without_objects
                   function_call_parameter_list)
       (FunctionCall $1-start-pos $4-end-pos (ClassName $1-start-pos $3-end-pos $1 $3) $4)]
      [(variable_class_name PAAMAYIM_NEKUDOTAYIM variable_name
                            function_call_parameter_list)
       (FunctionCall $1-start-pos $4-end-pos (ClassName $1-start-pos $3-end-pos $1 $3) $4)]
      [(variable_class_name PAAMAYIM_NEKUDOTAYIM variable_without_objects
                            function_call_parameter_list)
       (FunctionCall $1-start-pos $4-end-pos (ClassName $1-start-pos $3-end-pos $1 $3) $4)]
      [(variable_without_objects function_call_parameter_list)
       (FunctionCall $1-start-pos $2-end-pos $1 $2)]
      )

     (function_call_parameter_list
      [(OPAREN CPAREN) '()]
      [(OPAREN non_empty_function_call_parameter_list CPAREN) $2])

     (w_variable
      [(variable) $1])

     (function_call_parameter
      [(expr_without_variable) (FunctionCallParameter $1-start-pos $1-end-pos $1 #f)]
      [(variable) (FunctionCallParameter $1-start-pos $1-end-pos $1 #f)]
      [(AMPERSTAND w_variable)
       (FunctionCallParameter $1-start-pos $2-end-pos (AddrVariable $1-start-pos $2-end-pos $2) #f)])

     (non_empty_function_call_parameter_list
      [(function_call_parameter) (list $1)]
      [(non_empty_function_call_parameter_list COMMA function_call_parameter)
       (append $1 (list $3))]
      [(non_empty_function_call_parameter_list COMMA ELLIPSIS function_call_parameter)
       (append $1
               (list (FunctionCallParameter (Position-start $4)
                                            (Position-end $4)
                                            (FunctionCallParameter-expr $4)
                                            #t)))])

     (base_variable
      [(reference_variable) $1]
      [(indirection reference_variable) (IndirectionVariable $1-start-pos $2-end-pos $2 $1)]
      [(static_member) $1])

     (static_member
      [(class_name PAAMAYIM_NEKUDOTAYIM variable_without_objects) (ClassName $1-start-pos $3-end-pos $1 $3)]
      [(variable_class_name PAAMAYIM_NEKUDOTAYIM variable_without_objects)
       (ClassName $1-start-pos $3-end-pos $1 $3)])

     (dim_offset
      [() null]
      [(expr) $1])

     (reference_variable
      [(reference_variable OBRAKET dim_offset CBRAKET)
       (ArrayAccess $1-start-pos $4-end-pos $1 $3)]
      [(reference_variable OBRACE expr CBRACE)
       (BraceAccess $1-start-pos $4-end-pos $1 $3)]
      [(compound_variable) $1])

     (compound_variable
      [(VARIABLE) (Variable $1-start-pos $1-end-pos $1)]
      [(DOLLAR OBRACE expr CBRACE) (BraceVariable $1-start-pos $4-end-pos $3)])

     (yield_expr
      [(YIELD expr) (Yield $1-start-pos $2-end-pos $2 null)]
      [(YIELD expr DOUBLE_ARROW expr) (Yield $1-start-pos $4-end-pos  $2 $4)])


     (program
      [(OPEN_TAG top_statement_list CLOSE_TAG) $2]
      [(OPEN_TAG top_statement_list) $2]
      [() '()])

     (top_statement_list
      [() '()]
      [(top_statement_list top_statement) (append $1 (list $2))])

     (top_statement
      [(statement) $1]
      [(function_declaration_statement) $1]
      [(class_declaration_statement) $1]
      [(HALT_COMPILER OPAREN CPAREN SEMICOLON) 'HALT_COMPILER]
      [(NAMESPACE namespace_name SEMICOLON)
       (NamespaceStmt $1-start-pos $3-end-pos $2 null)]
      [(NAMESPACE namespace_name OBRACE top_statement_list CBRACE)
       (NamespaceStmt $1-start-pos $4-end-pos $2 $4)]
      [(NAMESPACE OBRACE top_statement_list CBRACE)
       (NamespaceStmt $1-start-pos $4-end-pos null $3)]
      [(USE use_declarations SEMICOLON) (UseStmt $1-start-pos $3-end-pos $2 'class)]
      [(USE FUNCTION use_declarations SEMICOLON) (UseStmt $1-start-pos $4-end-pos $3 'function)]
      [(USE CONST use_declarations SEMICOLON) (UseStmt $1-start-pos $4-end-pos $3 'const)]
      [(constant_declaration SEMICOLON) $1])

     (function_declaration_statement
      [(unticked_function_declaration_statement) $1])

     (unticked_function_declaration_statement
      [(FUNCTION AMPERSTAND IDENT OPAREN parameter_list CPAREN
                 OBRACE inner_statement_list CBRACE)
       (FunctionDcl $1-start-pos $9-end-pos null $3 $5 $8 true)]
      [(FUNCTION IDENT OPAREN parameter_list CPAREN
                 OBRACE inner_statement_list CBRACE)
       (FunctionDcl $1-start-pos $8-end-pos null $2 $4 $7 false)]
      [(DOCUMENTATION FUNCTION AMPERSTAND IDENT OPAREN parameter_list CPAREN
                      OBRACE inner_statement_list CBRACE)
       (FunctionDcl $1-start-pos $10-end-pos $1 $4 $6 $9 true)]
      [(DOCUMENTATION FUNCTION IDENT OPAREN parameter_list CPAREN
                      OBRACE inner_statement_list CBRACE)
       (FunctionDcl $1-start-pos $9-end-pos $1 $3 $5 $8 false)])

     (parameter_list
      [(non_empty_parameter_list) $1]
      [() '()])

     (is_variadic
      [(ELLIPSIS) #t]
      [() #f])

     (is_reference
      [(AMPERSTAND) #t]
      [() #f])

     (parameter_dcl
      [(optional_class_type is_reference is_variadic VARIABLE)
       (ParameterDcl $1-start-pos $2-end-pos $1 $4 $2 $3 null)]
      [(optional_class_type is_reference is_variadic VARIABLE ASSIGN static_scalar)
       (ParameterDcl $1-start-pos $5-end-pos $1 $4 $2 $3 $6)])

     (non_empty_parameter_list
      [(parameter_dcl) (list $1)]
      [(non_empty_parameter_list COMMA parameter_dcl)
       (append $1 (list $3))])

     (optional_class_type
      [() #f]
      [(ARRAY) 'ARRAY]
      [(CALLABLE) 'CALLABLE]
      [(fully_qualified_class_name) $1])

     (constant_declaration
      [(constant_declaration COMMA IDENT ASSIGN expr)
       (ConstDcls $1-start-pos $5-end-pos
                  (append $1 (list (ConstDcl $3-start-pos $5-end-pos $3 $5))))]
      [(empty_documentation CONST IDENT ASSIGN static_scalar)
       (ConstDcls $1-start-pos $5-end-pos
                  (list (ConstDcl $1-start-pos $5-end-pos $1 $3 $5)))])

     (use_declarations
      [(use_declarations COMMA use_declaration) (append $1 (list $3))]
      [(use_declaration) (list $1)])


     (use_declaration
      [(namespace_name) (UseDeclaration $1-start-pos $1-end-pos
                                        (NamespaceName $1-start-pos $1-end-pos #f $1)
                                        null)]
      [(namespace_name AS IDENT)
       (UseDeclaration $1-start-pos $3-end-pos
                       (NamespaceName $1-start-pos $1-end-pos #f $1)
                       $3)]
      [(NS_SEPARATOR namespace_name)
       (UseDeclaration $1-start-pos $2-end-pos
                       (NamespaceName $1-start-pos $2-end-pos #t $2)
                       null)]
      [(NS_SEPARATOR namespace_name AS IDENT)
       (UseDeclaration $1-start-pos $3-end-pos
                       (NamespaceName $1-start-pos $2-end-pos #t $2)
                       $4)])


     (class_declaration_statement
      [(unticked_class_declaration_statement) $1])

     (empty_documentation
      [() null]
      [(DOCUMENTATION) $1])

     (unticked_class_declaration_statement
      [(empty_documentation class_entry_type IDENT extends_from implements_list OBRACE
                            class_statement_list CBRACE)
       (ClassDcl $2-start-pos $8-end-pos $1 $2 $3 $4 $5 $7)]
      [(empty_documentation INTERFACE IDENT interface_extends_list OBRACE
                            class_statement_list CBRACE)
       (InterfaceDcl $1-start-pos $7-end-pos $1 $3 $4 $6)])

     (class_entry_type
      [(CLASS) '()]
      [(ABSTRACT CLASS) '(ABSTRACT)]
      [(TRAIT) '(TRAIT)]
      [(FINAL CLASS) '(FINAL)])

     (extends_from
      [() null]
      [(EXTENDS fully_qualified_class_name) $2])

     (interface_extends_list
      [() '()]
      [(EXTENDS interface_list) $2])

     (implements_list
      [() '()]
      [(IMPLEMENTS interface_list) $2])

     (interface_list
      [(fully_qualified_class_name) (list $1)]
      [(interface_list COMMA fully_qualified_class_name) (append $1 (list $3))])

     (class_statement_list
      [(class_statement_list class_statement) (append $1 (list $2))]
      [() '()])

     (class_statement
      [(empty_documentation variable_modifiers class_variable_declaration SEMICOLON)
       (PropertyDcl $2-start-pos $3-end-pos $1 $2 $3)]
      [(class_constant_declaration SEMICOLON) $1]
      [(trait_use_statement) $1]
      [(empty_documentation method_modifiers FUNCTION is_reference IDENT OPAREN
                            parameter_list CPAREN method_body)
       (MethodDcl $2-start-pos $8-end-pos $1 $2 $5 $7 $9 $4)])

     (trait_use_statement
      [(USE trait_list trait_adaptations)
       (TraitStmt $1-start-pos $3-end-pos $2 $3)])

     (trait_list
      [(fully_qualified_class_name) (list $1)]
      [(trait_list COMMA fully_qualified_class_name)
       (append $1 (list $3))])

     (trait_adaptations
      [(SEMICOLON) '()]
      [(OBRACE trait_adaptation_list CBRACE) $2])

     (trait_adaptation_list
      [() '()]
      [(non_empty_trait_adaptation_list) $1])

     (non_empty_trait_adaptation_list
      [(trait_adaptation_statement) (list $1)]
      [(non_empty_trait_adaptation_list trait_adaptation_statement)
       (append $1 (list $2))])

     (trait_adaptation_statement
      [(trait_precedence SEMICOLON) $1]
      [(trait_alias SEMICOLON) $1])

     (trait_precedence
      [(trait_method_reference_fully_qualified INSTEADOF trait_reference_list)
       (TraitPrecedence $1-start-pos $3-end-pos $1 $3)])

     (trait_reference_list
      [(fully_qualified_class_name) (list $1)]
      [(trait_reference_list COMMA fully_qualified_class_name)
       (append $1 (list $3))])

     (trait_method_reference
      [(IDENT) $1]
      [(trait_method_reference_fully_qualified) $1])

     (trait_method_reference_fully_qualified
      [(fully_qualified_class_name PAAMAYIM_NEKUDOTAYIM IDENT) (ClassName $1-start-pos $3-end-pos $1 $3)])

     (trait_alias
      [(trait_method_reference AS trait_modifiers IDENT)
       (TraitAlias $1-start-pos $4-end-pos $1 (cons $3 $4))]
      [(trait_method_reference AS member_modifier)
       (TraitAlias $1-start-pos $3-end-pos $1 $3)])

     (trait_modifiers
      [() null]
      [(member_modifier) $1])

     (method_body
      [(SEMICOLON) '()]
      [(OBRACE inner_statement_list CBRACE) $2])

     (class_variable_declaration
      [(class_variable_declaration COMMA VARIABLE)
       (append $1 (list $3))]
      [(class_variable_declaration COMMA VARIABLE ASSIGN static_scalar)
       (append $1 (list (cons $3 $5)))]
      [(VARIABLE) (list $1)]
      [(VARIABLE ASSIGN static_scalar) (list (cons $1 $3))])


     (variable_modifiers
      [(non_empty_member_modifiers) $1]
      [(VAR) (list 'VAR)])

     (method_modifiers
      [() '()]
      [(non_empty_member_modifiers) $1])

     (non_empty_member_modifiers
      [(member_modifier) (list $1)]
      [(non_empty_member_modifiers member_modifier) (append $1 (list $2))])

     (member_modifier
      [(PUBLIC) 'PUBLIC]
      [(PROTECTED) 'PROTECTED]
      [(PRIVATE) 'PRIVATE]
      [(STATIC) 'STATIC]
      [(ABSTRACT) 'ABSTRACT]
      [(FINAL) 'FINAL])

     (class_constant_declaration
      [(class_constant_declaration COMMA IDENT ASSIGN expr)
       (ConstClassDcls $1-start-pos $5-end-pos
                       (append $1 (list (ConstClassDcl $1-start-pos $5-end-pos $3 $5))))]
      [(empty_documentation CONST IDENT ASSIGN static_scalar)
       (ConstClassDcls $1-start-pos $5-end-pos
                       (list (ConstClassDcl $1-start-pos $5-end-pos $1 $3 $5)))])


     (inner_statement
      [(statement) $1]
      [(function_declaration_statement) $1]
      [(class_declaration_statement) $1]
      [(HALT_COMPILER OPAREN CPAREN SEMICOLON) 'HALT_COMPILER]
      )

     (inner_statement_list
      [(inner_statement_list inner_statement) (append $1 (list $2))]
      [() '()])

     (statement
      [(unticked_statement) $1]
      [(IDENT COLON) (LabelStmt $1-start-pos $2-end-pos $1)])

     (else_single
      [(ELSE statement) $2]
      [() (prec LOW_PRIORITY_RULE) '()])

     (elseif_list
      [() '()]
      [(elseif_list  ELSEIF OPAREN expr CPAREN statement)
       (append $1 (list (cons $4 $6)))])

     (unticked_statement
      [(expr SEMICOLON) (ExprStmt $1-start-pos $2-end-pos $1)]
      [(SEMICOLON) (EmptyStmt $1-start-pos $1-end-pos)]
      [(OBRACE inner_statement_list CBRACE) (BlockStmt $1-start-pos $3-end-pos $2)]
      [(IF OPAREN expr CPAREN statement elseif_list else_single)
       (IfStmt $1-start-pos $7-end-pos $3 $5 $6 $7)]
      ;; T_IF OPAREN expr CPAREN COLON inner_statement_list new_elseif_list new_else_single T_ENDIF SEMICOLON
      [(WHILE OPAREN expr  CPAREN statement) (WhileStmt $1-start-pos $5-end-pos $3 $5)]
      [(DO statement WHILE OPAREN expr CPAREN SEMICOLON) (DoWhileStmt $1-start-pos $7-end-pos $5 $2)]
      [(FOR OPAREN for_expr SEMICOLON  for_expr SEMICOLON for_expr CPAREN for_statement)
       (ForLoop $1-start-pos $9-end-pos $3 $5 $7 $9)]
      [(SWITCH OPAREN expr CPAREN	switch_case_list) (Switch $1-start-pos $5-end-pos $3 $5)]
      [(FOREACH OPAREN variable AS foreach_variable foreach_optional_arg CPAREN foreach_statement)
       (ForEachLoop $1-start-pos $8-end-pos $3 $5 $6 $8)]
      [(FOREACH OPAREN expr_without_variable AS variable foreach_optional_arg CPAREN foreach_statement)
       (ForEachLoop $1-start-pos $8-end-pos $3 $5 $6 $8)]
      [(BREAK SEMICOLON) (BreakStmt $1-start-pos $2-end-pos null)]
      [(BREAK expr SEMICOLON) (BreakStmt $1-start-pos $3-end-pos $2)]
      [(CONTINUE SEMICOLON) (ContinueStmt $1-start-pos $2-end-pos null)]
      [(CONTINUE expr SEMICOLON) (ContinueStmt $1-start-pos $3-end-pos $2)]
      [(RETURN SEMICOLON) (ReturnStmt $1-start-pos $2-end-pos null)]
      [(RETURN expr_without_variable SEMICOLON) (ReturnStmt $1-start-pos $3-end-pos $2)]
      [(RETURN variable SEMICOLON) (ReturnStmt $1-start-pos $3-end-pos $2)]
      [(TRY OBRACE inner_statement_list CBRACE catch_statement finally_statement)
       (TryStmt $1-start-pos $6-end-pos $3 $5 $6)]

      [(THROW expr SEMICOLON) (ThrowStmt $1-start-pos $3-end-pos $2)]
      [(ECHO echo_expr_list SEMICOLON) (EchoStmt $1-start-pos $3-end-pos $2)]
      ;; T_INLINE_HTML
      [(yield_expr SEMICOLON) (ExprStmt $1-start-pos $2-end-pos $1)]
      [(GLOBAL global_var_list SEMICOLON) (GlobalStmt $1-start-pos $3-end-pos $2)]
      [(STATIC static_var_list SEMICOLON) (StaticStmt $1-start-pos $3-end-pos $2)]
      [(UNSET OPAREN unset_variables CPAREN SEMICOLON) (UnsetStmt $1-start-pos $5-end-pos $3)]
      [(GOTO IDENT SEMICOLON) (GotoStmt $1-start-pos $3-end-pos $2)]
      [(DECLARE OPAREN declare_list CPAREN declare_statement)
       (DeclareStmt $1-start-pos $5-end-pos $3 $5)]
      )

     (declare_statement
      [(statement) $1]
      [(COLON inner_statement_list ENDDECLARE SEMICOLON) $2])

     (declare_list
      [(IDENT ASSIGN static_scalar) (list (cons $1 $3))]
      [(declare_list COMMA IDENT ASSIGN static_scalar)
       (append $1 (list (cons $3 $5)))])

     (global_var_list
      [(global_var_list COMMA global_var) (append $1 (list $3))]
      [(global_var) (list $1)])

     (global_var
      [(VARIABLE) (Variable $1-start-pos $1-end-pos $1)]
      ;; [(DOLLAR r_variable)
      [(DOLLAR OBRACE expr CBRACE) (BraceVariable $1-start-pos $4-end-pos $3)])

     (static_var_list
      [(static_var_list COMMA VARIABLE)
       (append $1 (list (Variable $3-start-pos $3-end-pos $3)))]
      [(static_var_list COMMA VARIABLE ASSIGN static_scalar)
       (append $1 (list (cons (Variable $3-start-pos $3-end-pos $3) $5)))]
      [(VARIABLE) (list (Variable $1-start-pos $1-end-pos $1))]
      [(VARIABLE ASSIGN static_scalar)
       (list (cons (Variable $1-start-pos $1-end-pos $1) $3))])


     (catch_statement
      [() '()]
      [(CATCH OPAREN fully_qualified_class_name VARIABLE CPAREN
              OBRACE inner_statement_list CBRACE additional_catches)
       (list* (CatchStmt $1-start-pos $9-end-pos $3 $4 $7) $9)])

     (finally_statement
      [() null]
      [(FINALLY OBRACE inner_statement_list CBRACE)
       (FinallyStmt $1-start-pos $4-end-pos $3)])


     (additional_catches
      [(non_empty_additional_catches) $1]
      [() '()])

     (non_empty_additional_catches
      [(additional_catch) (list $1)]
      [(non_empty_additional_catches additional_catch)
       (append $1 (list $2))])

     (additional_catch
      [(CATCH OPAREN fully_qualified_class_name VARIABLE CPAREN
              OBRACE inner_statement_list CBRACE)
       (CatchStmt $1-start-pos $8-end-pos $3 $4 $7)])

     (foreach_optional_arg
      [() null]
      [(DOUBLE_ARROW foreach_variable) $2])

     (foreach_variable
      [(variable) $1]
      [(AMPERSTAND variable) (AddrVariable $1-start-pos $2-end-pos $2)]
      [(LIST OPAREN assignment_list CPAREN) (ListPattern $1-start-pos $4-end-pos $3)])

     (foreach_statement
      [(statement) $1]
      [(COLON inner_statement_list ENDFOREACH SEMICOLON) $2])

     (switch_case_list
      [(OBRACE case_list CBRACE) $2]
      [(OBRACE SEMICOLON case_list CBRACE) $3]
      [(COLON case_list ENDSWITCH SEMICOLON) $2]
      [(COLON SEMICOLON case_list ENDSWITCH SEMICOLON) $3])

     (case_list
      [() '()]
      [(case_list CASE expr case_separator inner_statement_list)
       (append $1 (list (list $3 $5)))]
      [(case_list DEFAULT case_separator  inner_statement_list)
       (append $1 (list (list 'DEFAULT $4)))])

     (case_separator
      [(COLON) null]
      [(SEMICOLON) null])

     (for_expr
      [() null]
      [(non_empty_for_expr) $1])

     (non_empty_for_expr
      [(non_empty_for_expr COMMA  expr) (append $1 (list $3))]
      [(expr) (list $1)])

     (for_statement
      [(statement) $1]
      [(COLON inner_statement_list ENDFOR SEMICOLON) $2])

     (unset_variables
      [(unset_variable) (list $1)]
      [(unset_variables COMMA unset_variable) (append $1 (list $3))])

     (unset_variable
      [(variable) $1])

     (echo_expr_list
      [(echo_expr_list COMMA expr) (append $1 (list $3))]
      [(expr) (list $1)])


     (static_class_name_scalar
      [(class_name PAAMAYIM_NEKUDOTAYIM CLASS) (ClassName $1-start-pos $3-end-pos $1 'CLASS)])


     (static_scalar
      [(common_scalar) $1]
      [(static_class_name_scalar) $1]
      [(fully_qualified_class_name) $1]
      [(PLUS static_scalar) (Infix $1-start-pos $2-end-pos 'PLUS $2)]
      [(MINUS static_scalar) (Infix $1-start-pos $2-end-pos 'MINUS $2)]
      [(ARRAY OPAREN static_array_pair_list CPAREN) (Array $1-start-pos $4-end-pos $3)]
      [(OBRAKET static_array_pair_list CBRAKET) (LiteralArray $1-start-pos $3-end-pos $2)]
      [(static_class_constant) $1]
      [(CLASS_C) 'CLASS_C])

     (static_array_pair_list
      [() null]
      [(non_empty_static_array_pair_list possible_comma) $1])

     (possible_comma
      [() null]
      [(COMMA) null])


     (non_empty_static_array_pair_list
      [(non_empty_static_array_pair_list COMMA static_scalar DOUBLE_ARROW static_scalar)
       (append $1 (list (cons $3 $5)))]
      [(non_empty_static_array_pair_list COMMA static_scalar) (append $1 (list $3))]
      [(static_scalar DOUBLE_ARROW static_scalar) (list (cons $1 $3))]
      [(static_scalar) (list $1)])

     (array_pair_list
      [() '()]
      [(non_empty_array_pair_list possible_comma) $1])

     (non_empty_array_pair_list
      [(non_empty_array_pair_list COMMA expr DOUBLE_ARROW expr)
       (append $1 (list (cons $3 $5)))]
      [(non_empty_array_pair_list COMMA expr)
       (append $1 (list $3))]
      [(expr DOUBLE_ARROW expr) (list (cons $1 $3))]
      [(expr) (list $1)]
      [(non_empty_array_pair_list COMMA expr DOUBLE_ARROW AMPERSTAND w_variable)
       (append $1 (list (cons $3 (AddrVariable $5-start-pos $6-end-pos $6))))]
      [(non_empty_array_pair_list COMMA AMPERSTAND w_variable)
       (append $1 (list (AddrVariable $3-start-pos $4-end-pos $4)))]
      [(expr DOUBLE_ARROW AMPERSTAND w_variable)
       (list (cons $1 (AddrVariable $3-start-pos $4-end-pos $4)))]
      [(AMPERSTAND w_variable)
       (list (AddrVariable $1-start-pos $2-end-pos $2))])


     (common_scalar
      [(BOOL_TRUE) 'BOOL_TRUE]
      [(BOOL_FALSE) 'BOOL_FALSE]
      [(INTEGER) $1]
      [(FLOAT) $1]
      ;; | T_CONSTANT_ENCAPSED_STRING
      [(D_QUOTE_STRING) $1] ;; use this instead of constant encapsed string
      [(QUOTE_STRING) $1]
      [(LINE) 'LINE]
      [(FILE) 'FILE]
      [(DIR) 'DIR]
      [(TRAIT_C) 'TRAIT]
      [(METHOD_C) 'METHOD_C]
      [(FUNC_C) 'FUNC_C]
      [(NS_C) 'NS_C]
      ;; | T_START_HEREDOC T_ENCAPSED_AND_WHITESPACE T_END_HEREDOC
      ;; | T_START_HEREDOC T_END_HEREDOC


      ;;[(HEREDOC) 'HEREDOC] -- not used beacause cause reduce/reduce
      ;;conflicts with scalar rule
      )


     (class_name
      [(STATIC) 'STATIC]
      [(fully_qualified_class_name) $1])


     (static_class_constant
      [(class_name PAAMAYIM_NEKUDOTAYIM IDENT) (ClassName $1-start-pos $3-end-pos $1 $3)])


     (fully_qualified_class_name
      [(namespace_name) (NamespaceName $1-start-pos $1-end-pos #f $1)]
      [(NAMESPACE NS_SEPARATOR namespace_name)
       (NamespaceName $1-start-pos $3-end-pos #t (cons 'NAMESPACE $3))]
      [(NS_SEPARATOR namespace_name)
       (NamespaceName $1-start-pos $2-end-pos #t $2)])


     (namespace_name [(IDENT) (list $1)]
                     [(namespace_name NS_SEPARATOR IDENT)
                      (append $1 (list $3))])))))


(ast-struct NamespaceName Position (global name)
            #:transparent)

(ast-struct ClassName Position (class property) #:transparent)
(ast-struct FunctionCallParameter Position (expr unpack) #:transparent)

;; ---- VARIABLES ----
(ast-struct AddrVariable Position (expr) #:transparent)
(ast-struct ArrayAccess Position (expr inside-expr) #:transparent)
(ast-struct BraceAccess Position (expr inside-expr) #:transparent)
(ast-struct ObjectAccess Position (expr) #:transparent)
(ast-struct BraceNaming Position (expr) #:transparent)
(ast-struct ChainBrace Position (expr) #:transparent)
(ast-struct ChainArray Position (expr) #:transparent)
(ast-struct BraceVariable Position (expr) #:transparent)
(ast-struct Variable Position (name) #:transparent)
(ast-struct IndirectionVariable Position (variable levels) #:transparent)
(ast-struct ObjectChain Position (list) #:transparent)


;; ---- ARRAY LITERAL ----
(ast-struct Array Position (exprs) #:transparent)
(ast-struct LiteralArray Position (exprs) #:transparent)

;; ---- EXPRS ----
(ast-struct BackQuoteExpr Position (value) #:transparent)
(ast-struct Binary Position (op left right) #:transparent)
(ast-struct Infix Position (op expr) #:transparent)
(ast-struct Postfix Position(op expr) #:transparent)
(ast-struct Unary Position (op expr) #:transparent)
(ast-struct Cast Position (to expr) #:transparent)
(ast-struct Assign Position (op left right) #:transparent)
(ast-struct Yield Position (expr alias) #:transparent)
(ast-struct Exit Position (expr) #:transparent)
(ast-struct Clone Position (expr) #:transparent)
(ast-struct NewExpr Position (class parameters) #:transparent)
(ast-struct InstanceOfExpr Position (left right) #:transparent)
(ast-struct TestExpr Position (test then else) #:transparent)
(ast-struct PrintExpr Position (expr) #:transparent)
(ast-struct AtExpr Position (expr) #:transparent)
(ast-struct UnsetCast Position (expr) #:transparent)
(ast-struct FunctionCall Position (expr args) #:transparent)

;; ---- DEDICATED EXPRS ----
(ast-struct UseDeclaration Position (name alias) #:transparent)
(struct ParameterDcl Position (type name reference variadic default)
        #:transparent
        #:property prop:sub-ast
        (lambda (x)
          (list
           (ParameterDcl-type x)
           (ParameterDcl-name x)
           (ParameterDcl-reference x)
           (ParameterDcl-variadic x)
           (ParameterDcl-default x))))

(ast-struct DeclareStmt Position (list stmt) #:transparent)
(ast-struct ListPattern Position (pattern) #:transparent)
(ast-struct TraitAlias Position (from to) #:transparent)
(ast-struct TraitPrecedence Position (from to) #:transparent)

;; ---- STATEMENTS ----
(ast-struct ForLoop Position (init test step statement) #:transparent)
(ast-struct Switch Position (test cases) #:transparent)
(ast-struct ForEachLoop Position (variable loop-variable option stmt) #:transparent)
(ast-struct UseStmt Position (uses type) #:transparent)
(ast-struct GlobalStmt Position (list) #:transparent)
(ast-struct StaticStmt Position (list) #:transparent)
(ast-struct IfStmt Position (test then elseifs else) #:transparent)
(ast-struct WhileStmt Position (test stmt) #:transparent)
(ast-struct DoWhileStmt Position (test stmt) #:transparent)
(ast-struct BlockStmt Position (stmts) #:transparent)
(ast-struct ExprStmt Position (expr) #:transparent)
(ast-struct EmptyStmt Position () #:transparent)
(ast-struct LabelStmt Position (label) #:transparent)
(ast-struct TraitStmt Position (list adapter) #:transparent)
(ast-struct TryStmt Position (stmts catchs finally) #:transparent)
(ast-struct FinallyStmt Position (stmts) #:transparent)
(ast-struct CatchStmt Position (exception variable statement) #:transparent)
(ast-struct ThrowStmt Position (expr) #:transparent)
(ast-struct BreakStmt Position (label) #:transparent)
(ast-struct ContinueStmt Position (label) #:transparent)
(ast-struct ReturnStmt Position (expr) #:transparent)
(ast-struct EchoStmt Position (exprs) #:transparent)
(ast-struct UnsetStmt Position (variables) #:transparent)
(ast-struct GotoStmt Position (label) #:transparent)
(ast-struct EmptyExpr Position (expr) #:transparent)
(ast-struct EvalExpr Position (expr) #:transparent)

(struct FunctionDcl Position
        (documentation name args body reference)
        #:transparent
        #:property prop:sub-ast
        (lambda (x)
          (list
           (FunctionDcl-name x)
           (FunctionDcl-args x)
           (FunctionDcl-body x)
           (FunctionDcl-reference x))))

(struct LambdaDcl Position
        (documentation static args lexical body reference)
        #:property prop:sub-ast
        (lambda (x)
          (list
           (LambdaDcl-static x)
           (LambdaDcl-args x)
           (LambdaDcl-lexical x)
           (LambdaDcl-body x)
           (LambdaDcl-reference x)))
        #:transparent)

(struct ConstClassDcl Position (documentation name value)
        #:transparent
        #:property prop:sub-ast
        (lambda (x)
          (list (ConstClassDcl-name x) (ConstClassDcl-value x))))

(ast-struct RequireExpr Position (expr) #:transparent)
(ast-struct RequireOnceExpr Position (expr) #:transparent)
(ast-struct IssetExpr Position (expr) #:transparent)
(ast-struct IncludeExpr Position (expr) #:transparent)
(ast-struct IncludeOnceExpr Position (expr) #:transparent)

(ast-struct ChainCall Position (args) #:transparent)

(ast-struct NamespaceStmt Position (name body) #:transparent)

(struct MethodDcl Position
        (documentation modifiers name args body reference)
        #:transparent
        #:property prop:sub-ast
        (lambda (x)
          (list
           (MethodDcl-modifiers x)
           (MethodDcl-name x)
           (MethodDcl-args x)
           (MethodDcl-body x)
           (MethodDcl-reference x))))

(struct ClassDcl Position
        (documentation modifiers name extend implements body)
        #:transparent
        #:property prop:sub-ast
        (lambda (x)
          (list
           (ClassDcl-modifiers x)
           (ClassDcl-name x)
           (ClassDcl-extend x)
           (ClassDcl-implements x)
           (ClassDcl-body x))))

(struct InterfaceDcl Position
        (documentation name extends body)
        #:transparent
        #:property prop:sub-ast
        (lambda (x)
          (list
           (InterfaceDcl-name x)
           (InterfaceDcl-extends x)
           (InterfaceDcl-body x))))

(struct PropertyDcl Position
        (documentation modifiers variables)
        #:transparent
        #:property prop:sub-ast
        (lambda (x)
          (list (PropertyDcl-modifiers x) (PropertyDcl-variables x))))

(struct ConstDcl Position
        (documentation name value)
        #:transparent
        #:property prop:sub-ast
        (lambda (x)
          (list (ConstDcl-name x) (ConstDcl-value x))))

(struct ConstDcls Position
        (list)
        #:transparent
        #:property prop:sub-ast
        (lambda (x)
          (ConstDcls-list x)))

(struct ConstClassDcls Position
        (list)
        #:transparent
        #:property prop:sub-ast
        (lambda (x)
          (ConstClassDcls-list x)))

(define (useless-token? token)
  (define name (token-name (position-token-token token)))
  (or (equal? 'LINE_COMMENT name)
      (equal? 'BLANKS name)
      (equal? 'NEWLINES name)
      (equal? 'COMMENT name)))

 (define (php-lexe input-port)
   (when (not (port-counts-lines? input-port))
    (port-count-lines! input-port))
   (reverse
    (let loop ([tokens '()])
      (let ([tok (php-lexer input-port)])
        (if (equal? 'EOF (token-name (position-token-token tok)))
            (cons tok tokens)
            (loop (cons tok tokens)))))))

(define (php-parse input-port)
  (when (not (port-counts-lines? input-port))
    (port-count-lines! input-port))
  (define (token-loop)
    (let* ([tok (php-lexer input-port)]
           [name (token-name (position-token-token tok))])
      (if (or (equal? 'LINE_COMMENT name)
              (equal? 'BLANKS name)
              (equal? 'NEWLINES name)
              (equal? 'COMMENT name))
          (token-loop)
          tok)))
  (php-parser token-loop))


;; TODO - treat those rules

;; scalar
;;  T_STRING_VARNAME

;; unticked_statement
;; global_var

(module+ test
  (define parse-string (compose php-parse open-input-string))
  (define parse-expr (compose ExprStmt-expr
                              first
                              parse-string
                              (curry string-append "<?php ")))

  (let ([ast (parse-string "<?php $this->elephant[32](1,2,3)->jack;")])
    (check-pred ExprStmt? (first ast))
    (check-pred ObjectChain? (ExprStmt-expr (first ast)))
    (define obj-lst (ObjectChain-list (ExprStmt-expr (first ast))))
    (check-pred Variable? (first obj-lst))
    (check-pred ObjectAccess? (second obj-lst))
    (check-pred ChainArray? (third obj-lst))
    (check-pred ChainCall? (fourth obj-lst))
    (check-pred ObjectAccess? (last obj-lst)))

  (let ([ast (parse-string "<?php (new House(1,2,3))[32]->green;")])
    (check-pred ExprStmt? (first ast))
    (check-pred ObjectChain? (ExprStmt-expr (first ast)))
    (let ([obj-lst (ObjectChain-list (ExprStmt-expr (first ast)))])
      (check-pred NewExpr? (first obj-lst))
      (check-pred ChainArray? (second obj-lst))
      (check-pred ObjectAccess? (third obj-lst))))

  (let ([ast (parse-string "<?php (new House(1,2,3))->green;")])
    (check-pred ExprStmt? (first ast))
    (check-pred ObjectChain? (ExprStmt-expr (first ast)))
    (let ([obj-lst (ObjectChain-list (ExprStmt-expr (first ast)))])
      (check-pred NewExpr? (first obj-lst))
      (check-pred ObjectAccess? (second obj-lst))))

  (let ([ast (first (parse-string "<?php function maFun($a, Abc $b, array $c = array()) { return 12; }"))])
    (check-pred FunctionDcl? ast)
    (match-let ([(list a b c) (FunctionDcl-args ast)])
      (check-equal? (ParameterDcl-name a) "$a")
      (check-equal? (NamespaceName-name (ParameterDcl-type b)) '("Abc"))
      (check-pred Array? (ParameterDcl-default c))
      (check-equal? (Array-exprs (ParameterDcl-default c)) empty)))

  (let ([ast (parse-expr "new $this->house;")])
    (check-pred NewExpr? ast)
    (check-pred ObjectChain? (NewExpr-class ast))
    (check-pred ObjectAccess? (second (ObjectChain-list (NewExpr-class ast)))))
  (let ([ast (parse-expr "(my_fun('arg1', 'arg2', 'arg3'));")])
    (check-pred FunctionCall? ast))

  (let ([ast (parse-expr "someFun($a, ...$b);")])
    (check-true (FunctionCallParameter-unpack (second (FunctionCall-args ast))))
    (check-false (FunctionCallParameter-unpack (first (FunctionCall-args ast)))))

  (let ([ast (parse-expr "1**2;")])
    (check-equal? (Binary-op ast) 'EXPO))

  (let ([ast (parse-expr "$a instanceOf \\Exception;")])
    (check-pred InstanceOfExpr? ast)))
