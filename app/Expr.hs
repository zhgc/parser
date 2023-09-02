module Expr where

import Parser

data Expr = Con Int | Bin Op Expr Expr
data Op   = Plus | Minus

expr :: Parser Expr
expr = tokenP (constant <|> paren binary)

constant :: Parser Expr
constant  = Con <$> natP

binary :: Parser Expr
binary = Bin <$> op <*> expr <*> expr

op :: Parser Op
op = (Plus <$ symbolP "+") <|> (Minus <$ symbolP "-")

paren :: Parser a -> Parser a
paren p =(\_ x _ -> x) <$> symbolP "(" <*> p <*> symbolP ")"

