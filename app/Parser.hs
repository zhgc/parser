{-#LANGUAGE ApplicativeDo #-}
module Parser where
import Data.Char (isLower, isSpace)
import GHC.Unicode (isDigit)

newtype Parser a = Parser (String -> [(a,String)])

-- Functor和Applicative原本是不用定义的，haskell在更新了Monad继承自Applivative，所以也要定义这两个。
-- 我的教材有一点老了。

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = Parser (\s -> [(f x,y)|(x,y) <- apply p s]) 

instance Applicative Parser where
    pure :: a -> Parser a
    pure x = Parser (\s -> [(x,s)])
-- 仔细一想，这个<*>已经完成的很好了，是否实际上用applicative就可以完成单子语法分析器的功能？
-- 另，我听说有一个叫ApplicativeDo的语言扩展，可以了解一下。
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (<*>) p q = Parser (\s -> [(f x,ys)
                              |(f,xs) <- apply p s,
                               (x,ys) <- apply q xs]) 

instance Monad Parser where
    return :: a -> Parser a
    return = pure

    -- 将分析器 p::Parser a 绑定到函数 q::a -> Parser b 然后返回一个Parser b 
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= q  = Parser (\s -> [(y,s'')
                             |(x,s')  <- apply p s,
                              (y,s'') <- apply (q x) s'])

-- 别名
getcP = getc
satP  = sat 
guardP = guard
failP = fail'
lowerP = lower
manyP  = many
optionalP = optional'
naturalP = natural
noneP = none
lowersP = lowers
-- tokenP = token
intsP = ints


-- 将分析器应用。
apply :: Parser a -> String -> [(a,String)]
apply (Parser p) s = p s

-- 这个操作将apply分析的第一个结果返回 也就是对 [(a,String)]做fst . head获得其中的a
parse :: Parser a -> String -> a 
parse p = fst . head . apply p

-- 一个最基础的字符分析器
-- 当获得空串时，返回空串
getc :: Parser Char 
getc = Parser f
    where f []     = []
          f (c:cs) = [(c,cs)]

-- sat :: (Char -> Bool) -> Parser Char
-- sat p = do {
--     -- 根据bind的定义，如果c获取失败(s = [])，
--     -- 以及根据list Monad的定义 ， 那么
--     -- sat p :: Parser Char = Parser (\s -> [])
--     -- 等于失败
--     c<-getc;
--     -- 判断c是否符合条件
--     if p c then return c
--     else fail'
-- }

-- 用更简洁的方式定义sat 
sat :: (Char -> Bool) -> Parser Char
sat p = do {c<-getc;guard (p c);return c}
-- sa p = parse $ (\c -> guardP (p c) >> return c) <$> getcP
-- sa :: (Char -> Bool) -> Parser Char
--     where c <- getcP
-- app 
-- satP p = case maybeP of 
--     Just c -> pure c
--     Nothing -> failP
--     where maybeP = (\c -> if p c then Just c else Nothing) <$> getc


-- 仔细来看看sat的定义
-- do natation实际上是>>的连锁应用，而>>实际上是>>=\_的语法糖
-- 故而，getc失败就等同于fail
-- 注意，fail >> p 等于 fail故而guard (p c) 等于fail时，分析器sat p为fail
-- 证明：
{- 
fail >> p
=>{拆开语法糖}
fail >>= \_ -> p
=>{使\_ -> p 等于 q}
fail >>= q
=>{根据>>=定义}
Parser (\s -> [(y,s'')
              |(x,s')  <- apply fail s,
               (y,s'') <- apply (q x) s'])
=>{显然，这是不可能的，apply fail s返回空序列，故而上式也为空}
Parser (\s -> [])
-}

-- 或者可以说，fail >>= q 等于 fail
-- 经过分析，return () >> return c 等于return c或者说，只有fail会中断这个过程。其他的都是忽略前文，执行后面的。

guard :: Bool -> Parser ()
guard True  = return ()
guard False = fail'

-- 失败
fail' :: Parser a
fail' = Parser (\_ -> [])

-- 定义一个字符的分析器
char :: Char -> Parser ()
char x = do {_ <- sat (==x);return ()}

-- 这样写疑似没有问题，我继续探索一下
charP :: Char -> Parser ()
charP x = pure () <$> satP (==x)

-- 定义字符串的分析器
string :: [Char] -> Parser ()
string [] = return ()
string (x:xs) = do {char x;string xs;return ()}

-- 这样写疑似也没有问题
stringP :: [Char] -> Parser ()
stringP [] = pure ()
stringP (x:xs) = pure <$> charP x <*> stringP xs

lower :: Parser Char
lower = sat isLower

digit :: Parser Int
digit = do {d <- sat isDigit;return (cvt d)}
    where cvt d = fromEnum d - fromEnum '0'
-- isDigit接受一个字符，判断这个字符是不是数字，是数字返回True

digitP :: Parser Int
digitP = cvt <$> satP isDigit
    where cvt d = fromEnum d - fromEnum '0'

-- 如果p失败，就返回q
(<|>) :: Parser a -> Parser a -> Parser a
(<|>) p q = Parser f 
    where f s = let ps = apply p s 
                in if null ps then apply q s else ps

-- lowers :: Parser String
-- lowers = do {c <- lower;cs <- lowers;return (c:cs)} <|> return ""
-- 这个分析器看上去有点像string，但并不一样
-- string 是获得一个串，然后分析输入这个分析器则是获得输入，当输入不是小写字母时停止
{-
lowers = lower >>= \c -> >>= \_ -> lowers >>= \cs -> >>= \_ -> return (c:cs)
=> {先把第一个绑定左边的设为p ，右边的设为q}
p >>= q
=> {根据绑定的定义}
Parser (\s -> [(y,s'')
              |(x,s')  <- apply p s,
               (y,s'') <- apply (q x) s'])
=> {将p回代}
Parser (\s -> [(y,s'')
              |(x,s')  <- apply lower s,
               (y,s'') <- apply (q x) s'])
=> {如果成功，消耗一个字符，然后继续，如果失败那么就是fail' 等于Parser (\s -> [])}
=> {将q回代，x带入c抵消}
Parser (\s -> [(y,s'')
              |(x,s')  <- apply lower s,
               (y,s'') <- apply (>>= \_ -> lowers >>= \cs -> >>= \_ -> return (x:cs)) s']

=> {绑定操作缺少一个参数，apply会将s'传入，而s'会被_忽略}
(y,s'') 来自 losers >>= \cs -> >>= \_ -> return (x:cs)
=> {根据绑定定义}
Parser (\s -> [(yy,s'')
              |(xx,s')  <- apply lowers s
               (yy,s'') <- apply (>>= \_ -> return (x:xx)) s'])
=> {apply (>>= \_ -> return (x:xx)) s' 根据绑定定义}
(yy,s'') => return (x:xx) :: Parser String
=>{使x:xx等于xs}
Parser (\s -> [(xs,s)]) 注意这里的s已经被消耗过了。
-}

-- many :: Parser a -> Parser [a]
-- many p = do {x<-p;xs<-many p;return (x:xs)} <|> none

-- 根据some和optional'重新定义many。
-- 一个问题是，很多函数在haskell里已经有了定义，这是为什么？因为教材太老了吗？
many :: Parser a -> Parser [a]
many p = optional' (some p)

optional' :: Parser [a] -> Parser [a]
optional' p = p <|> none

natural :: Parser Int
natural = token nat 

nat :: Parser Int
nat = do {ds <- some digit;return (foldl1 shiftl ds)}
    where shiftl m n = 10*m+n

-- app
natP :: Parser Int
natP = foldl1 shiftl <$> some digitP
    where shiftl m n = 10*m+n

none :: Parser [a]
none = return []

-- 根据many的定义重新定义lowers
lowers :: Parser String
lowers = many lower

space :: Parser ()
space = many (sat isSpace) >> return ()

-- applicative风格
spaceP :: Parser ()
spaceP = pure () <$> manyP (sat isSpace)

symbol :: String -> Parser ()
symbol xs = space >> string xs

-- app
symbolP :: [Char] -> Parser ()
symbolP xs = pure <$> spaceP <*> stringP xs

-- 在使用分析器p之前，先去除空格。
token :: Parser a -> Parser a
token p = space >> p

-- app 
tokenP :: (() -> b) -> Parser b
tokenP p = p <$> spaceP 

-- 至少分析一次
some :: Parser a -> Parser [a]
some p = do {x <- p;xs <- many p;return (x:xs)}

--app
someP :: Parser a -> Parser [a]
someP p = (:) <$> p <*> manyP p 

addition :: Parser Int
addition = do {m <- digit;char '+' ;n <- digit;return (m+n)}

-- app这里尤其搞笑，因为charP也返回了某些东西，所以+的行为要自己定义一下
additionP :: Parser Int
additionP = (\m _ n -> m+n) <$> digitP <*> charP '+' <*> digitP

int :: Parser Int
int = do {space;f <- minus;n <- nat;return (f n)}
    where minus = (char '-' >> return negate) <|> return id 

intP :: Parser Int
intP = (\_ f n ->f n) <$> space <*> minus <*> natP
    where minus = (char '-' >> return negate) <|> return id 

-- 分析[int] 列表
ints :: Parser [Int]
ints = bracket (manywith (symbol ",") int)

bracket :: Parser a -> Parser a
bracket p = do {symbol "[";x <- p;symbol "]";return x}

bracketP :: Parser b -> Parser b
bracketP p = (\_ x _ -> x) <$> symbolP "[" <*> p <*> symbolP "]"

manywith :: Parser b -> Parser a -> Parser [a]
manywith q p = optional' (somewith q p)

manywithP :: Parser a1 -> Parser a -> Parser [a]
manywithP q p = optionalP (somewithP q p)

somewith :: Parser b -> Parser a -> Parser [a]
somewith q p = do {x <- p;xs <- many (q >> p);return (x:xs)}

somewithP :: Parser a1 -> Parser a2 -> Parser [a2]
somewithP q p = (:) <$> p <*> many ((\_ b -> b) <$> q <*> p)