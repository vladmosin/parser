module Parser where

import Tokenizer
import Prelude hiding (lookup)

data AST = ASum Operator AST AST
     | AProd Operator AST AST
     | APow AST AST
     | AAssign String AST
     | ANum Int
     | AIdent String
     | AUnary AST

parse :: String -> Maybe AST
parse input =
  let ts = tokenize input in
  case ts of
  [TEof] -> Nothing
  _ -> let (tree, ts') = expression ts in
     if ts' == [TEof]
     then Just tree
     else error ("Parsing error on: " ++ show ts')

expression :: [Token] -> (AST, [Token])
expression ts =
  let (termNode, ts') = term ts in
  case lookup ts' of
  TOp op | op == Minus || op == Plus ->
    (parseminus termNode op (accept ts'))
  TAssign ->
    case termNode of
    AIdent v -> let (exprNode, ts'') = expression $ accept ts' in
                    (AAssign v exprNode, ts'')
    _ -> error "Syntax error: assignment is only possible to identifiers"
  _ -> (termNode, ts')

parseminus :: AST -> Operator -> [Token] -> (AST, [Token])
parseminus tree op ts = 
  let (termNode, ts') = term ts in
  case lookup ts' of
  TOp op' | op' == Minus || op' == Plus ->
    (parseminus (ASum op tree termNode) op' (accept ts')) 
  _ -> (ASum op tree termNode, ts')

term :: [Token] -> (AST, [Token])
term ts =
  case lookup ts of
  TOp op | op == Minus -> 
    let (termNode, ts') = term $ accept ts in
    (AUnary termNode, ts')
  _ ->
    let (degrNode, ts') = degr ts in
    case lookup ts' of
    TOp op | op == Mult || op == Div ->
      (parsemul degrNode op (accept ts'))
    _ -> (degrNode, ts')

parsemul :: AST -> Operator -> [Token] -> (AST, [Token])
parsemul tree op ts = 
  let (factNode, ts') = degr ts in
  case lookup ts' of
  TOp op' | op' == Mult || op' == Div ->
    (parsemul (AProd op tree factNode) op' (accept ts')) 
  _ -> (AProd op tree factNode, ts')

degr :: [Token] -> (AST, [Token])
degr ts = 
  let (factNode, ts') = factor ts in
  case lookup ts' of
  TOp op | op == Pow ->
    let (degrNode, ts'') = degr $ accept ts' in
    (APow factNode degrNode, ts'')
  _ -> (factNode, ts')
    

factor :: [Token] -> (AST, [Token])
factor ts =
  case lookup ts of
  TLParen ->
    let (exprNode, ts') = expression (accept ts) in
    case lookup ts' of
    TRParen -> (exprNode, accept ts')
    _ -> error "Syntax error: mismatched parentheses"
  TIdent v -> (AIdent v, accept ts)
  TNumber d -> (ANum d, accept ts)
  _ -> error "Syntax error: factor can only be a digit, an identifier or a parenthesised expression"

lookup :: [Token] -> Token
lookup = head

accept :: [Token] -> [Token]
accept = tail

instance Show AST where
  show tree = "\n" ++ show' 0 tree
    where
      show' n t =
        (if n > 0 then \s -> concat (replicate (n - 1) "| ") ++ "|_" ++ s else id)
        (case t of
                  ASum  op l r -> showOp op : "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                  AProd op l r -> showOp op : "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                  AAssign  v e -> v ++ " =\n" ++ show' (ident n) e
                  ANum   i     -> show i
                  AUnary e     -> showOp Minus : "\n" ++ show' n e
                  APow l r     -> showOp Pow : "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                  AIdent i     -> show i)
      ident = (+1)
      showOp Plus  = '+'
      showOp Minus = '-'
      showOp Mult  = '*'
      showOp Div   = '/'
      showOp Pow   = '^'