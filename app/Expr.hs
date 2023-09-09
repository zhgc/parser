module Expr where

import Parser

data Expr = Con Int | Bin Op Expr Expr
data Op   = Plus | Minus | Mul | Div

expr :: Parser Expr
expr = tokenP (term >>= rest)

term :: Parser Expr
term = tokenP (constant <|> paren expr)

rest :: Expr -> Parser Expr
rest e1 = do {p<-op;e2<-term;rest (Bin p e1 e2)} <|> return e1

constant :: Parser Expr
constant  = Con <$> natP

binary :: Parser Expr
binary = Bin <$> op <*> expr <*> expr

op :: Parser Op
op = (Plus <$ symbolP "+") <|> (Minus <$ symbolP "-")

paren :: Parser a -> Parser a
paren p =(\_ x _ -> x) <$> symbolP "(" <*> p <*> symbolP ")"
