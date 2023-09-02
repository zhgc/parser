{-#LANGUAGE ApplicativeDo #-}
module Parser where
import Data.Char (isLower, isSpace)
import GHC.Unicode (isDigit)
import Control.Monad(void)

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
-- 目前基本改了一下，差不多了。
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

-- 将分析器应用。
apply :: Parser a -> String -> [(a,String)]
apply (Parser p) = p

-- 这个操作将apply分析的第一个结果返回 也就是对 [(a,String)]做fst . head获得其中的a
parse :: Parser a -> String -> a
parse p = fst . head . apply p

-- 一个最基础的字符分析器
-- 当获得空串时，返回空串
getcP :: Parser Char
getcP = Parser f
    where f []     = []
          f (c:cs) = [(c,cs)]

-- 用更简洁的方式定义sat 
satP :: (Char -> Bool) -> Parser Char
satP p = do {c<-getcP;guardP (p c);return c}

-- 或者可以说，fail >>= q 等于 fail
-- 经过分析，return () >> return c 等于return c或者说，只有fail会中断这个过程。其他的都是忽略前文，执行后面的。

guardP :: Bool -> Parser ()
guardP True  = return ()
guardP False = failP

-- 失败
failP :: Parser a
failP = Parser (const [])

-- 定义一个字符的分析器
-- 这样写疑似没有问题，我继续探索一下
charP :: Char -> Parser ()
charP x = void (satP (==x))

-- 定义字符串的分析器
{-
hint：
Why not:
  stringP xs = foldr (\ x -> (<*>) (pure <$> charP x)) (pure ()) xs
-}
stringP :: [Char] -> Parser ()
stringP [] = pure ()
stringP (x:xs) = pure <$> charP x <*> stringP xs

lowerP :: Parser Char
lowerP = satP isLower

digitP :: Parser Int
digitP = cvt <$> satP isDigit
    where cvt d = fromEnum d - fromEnum '0'

-- 如果p失败，就返回q
(<|>) :: Parser a -> Parser a -> Parser a
(<|>) p q = Parser f
    where f s = let ps = apply p s
                in if null ps then apply q s else ps

manyP :: Parser a -> Parser [a]
manyP p = optionalP (someP p)

optionalP :: Parser [a] -> Parser [a]
optionalP p = p <|> noneP

naturalP :: Parser Int
naturalP = tokenP natP

-- app
natP :: Parser Int
natP = foldl1 shiftl <$> someP digitP
    where shiftl m n = 10*m+n

noneP :: Parser [a]
noneP = return []

-- 根据many的定义重新定义lowers
lowersP :: Parser String
lowersP = manyP lowerP

spaceP :: Parser ()
spaceP = void (manyP (satP isSpace))

-- app
symbolP :: [Char] -> Parser ()
symbolP xs = pure <$> spaceP <*> stringP xs

-- 在使用分析器p之前，先去除空格。
-- app 
tokenP :: Parser a -> Parser a
tokenP p = (\_ x -> x) <$> spaceP <*> p

-- 至少分析一次
--app
someP :: Parser a -> Parser [a]
someP p = (:) <$> p <*> manyP p

-- app这里尤其搞笑，因为charP也返回了某些东西，所以+的行为要自己定义一下
additionP :: Parser Int
additionP = (\m _ n -> m+n) <$> digitP <*> charP '+' <*> digitP

intP :: Parser Int
intP = (\_ f n ->f n) <$> spaceP <*> minus <*> natP
    where minus = (charP '-' >> return negate) <|> return id

-- 分析[int] 列表
intsP :: Parser [Int]
intsP = bracketP (manywithP (symbolP ",") intP)

bracketP :: Parser b -> Parser b
bracketP p = (\_ x _ -> x) <$> symbolP "[" <*> p <*> symbolP "]"

manywithP :: Parser a1 -> Parser a -> Parser [a]
manywithP q p = optionalP (somewithP q p)

somewithP :: Parser a1 -> Parser a2 -> Parser [a2]
somewithP q p = (:) <$> p <*> manyP ((\_ b -> b) <$> q <*> p)