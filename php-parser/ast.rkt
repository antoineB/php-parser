#lang racket

(require (only-in "parser-utils.rkt"
                  ast-struct
                  struct:Position
                  Position
                  prop:sub-ast))

(provide
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
(struct ParameterDcl Position (type name reference default)
        #:transparent
        #:property prop:sub-ast
        (lambda (x)
          (list
           (ParameterDcl-type x)
           (ParameterDcl-name x)
           (ParameterDcl-reference x)
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
