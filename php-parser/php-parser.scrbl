#lang scribble/manual

@require[php-parser scribble/eval (for-label php-parser parser-tools/lex)]

@(define the-eval (make-base-eval))
@(the-eval '(require php-parser))

@title{Parsing php}

@defmodule[php-parser]
@author[@author+email["Antoine Brand" "antoine.brand@sfr.fr"]]

@section{Main functions}
@defproc[(php-parse [input-port input-port?]) (listof sub-ast?)]{
  Parse the input port and return a list of Ast elements.
  }
	 
@;;@racket[(php-parse (open-input-string "<?php $this->method($arg);"))]

@;;@(the-eval '(format "~a" (php-parse (open-input-string "<?php $this->method($arg);"))))

@defproc[(php-lexe [input-port input-port?]) (listof token?)]{
  Tokenize the input port and return a list of token.
  }
          
@;;useless-token?

@defproc[(php-lexer [input-port input-port?]) token?]{
  Extract the next token from the input port.
  }

@defproc[(php-parser [token-fn (-> token?)]) (listof sub-ast?)]{
  Parse from a function that generate at each call a token.
  }

@defproc[(php-expr-parser [token-fn (-> token?)]) (listof sub-ast?)]{
  Same as @racket[php-parser] expect it only parse php expression.
  }


@section{AST elements}

@defstruct[Position ([start position?] [end position?])]{
  Super class of all the AST structs.
  }


@itemlist[
  @item{Variable}
  @item{AddrVariable}
  @item{ArrayAccess}
  @item{BraceAccess}
  @item{BraceVariable}
  @item{IndirectionVariable}
  @item{ObjectChain}
  @item{Array}
  @item{LiteralArray}
  @item{InstanceOfExpr}
  @item{TestExpr}
  @item{PrintExpr}
  @item{AtExpr}
  @item{UnsetCast}
  @item{Binary}
  @item{Infix}
  @item{Postfix}
  @item{Unary}
  @item{Cast}
  @item{Assign}
  @item{Yield}
  @item{Exit}
  @item{Clone}
  @item{FunctionCallParameter}
  @item{FunctionCall}
  @item{ObjectAccess}
  @item{BraceNaming}
  @item{ClassName}
  @item{NewExpr}
  @item{ExprStmt}
  @item{BlockStmt}
  @item{EmptyStmt}
  @item{LabelStmt}
  @item{IfStmt}
  @item{WhileStmt}
  @item{DoWhileStmt}
  @item{BreakStmt}
  @item{ContinueStmt}
  @item{TryStmt}
  @item{FinallyStmt}
  @item{CatchStmt}
  @item{ThrowStmt}
  @item{ReturnStmt}
  @item{EchoStmt}
  @item{UnsetStmt}
  @item{NamespaceStmt}
  @item{NamespaceName}
  @item{UseDeclaration}
  @item{UseStmt}
  @item{FunctionDcl}
  @item{LambdaDcl}
  @item{MethodDcl}
  @item{ParameterDcl}
  @item{ClassDcl}
  @item{InterfaceDcl}
  @item{ConstClassDcl}
  @item{GotoStmt}
  @item{PropertyDcl}
  @item{ConstDcl}
  @item{DeclareStmt}
  @item{GlobalStmt}
  @item{StaticStmt}
  @item{IssetExpr}
  @item{IncludeExpr}
  @item{IncludeOnceExpr}
  @item{EmptyExpr}
  @item{EvalExpr}
  @item{RequireExpr}
  @item{RequireOnceExpr}
  @item{BackQuoteExpr}
  @item{ForLoop}
  @item{Switch}
  @item{ForEachLoop}
  @item{ListPattern}
  @item{TraitAlias}
  @item{TraitStmt}
  @item{ChainBrace}
  @item{TraitPrecedence}
  @item{ChainCall}
  @item{ChainArray}
  @item{ConstDcls}
  @item{ConstClassDcls}]


@;;Position
@;;Position-start
@;;Position-end
@;;Position?
@;;sub-ast?
@;;get-sub-ast
@;;ast-struct
@;;position-start-offset
@;;position-end-offset
@;;prop:sub-ast
@;;extra-ast?
@;;get-extra-ast
@;;set-extra-ast!
@;;Extra?
@;;Extra
@;;Extra-parent
@;;Extra-type