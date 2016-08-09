{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-} 
import Text.ParserCombinators.Parsec 
import Control.Monad
import Data.Monoid
import Data.Either
import Data.List
import Data.Char

keys = map fst
escapeCodes = 
  [("gt",">")
  ,("lt","<")
  ,("amp","&")
  ,("quot","\"")
  ,("apos","'")]


data Tag = Tag [(String,String)] String (Maybe [TagContent])
  deriving (Show,Eq)
  
data TagContent = TagString String 
                | TagInner Tag
                | TagCData String
   deriving (Show,Eq)

spaceOut p = between (many space) (many space) p   
attr = do
  attr <- spaceOut $ many1 (satisfy (liftM2 ((not.).(||)) isSpace (`elem` "=<>")))
  string "="
  value <- spaceOut $     between (string "\"") (string "\"") (many1 (noneOf "\""))   
                     <|>  between (string "'") (string "'") (many1 (noneOf "'"))		
  return (attr,value)
 

many1Till p e = do
       notFollowedBy e
       x <- p
       xs <- manyTill p (try e)
       return (x:xs)

many1Till' p e = do
                 xs <- many1Till p (lookAhead e)
                 y <- e
                 return (xs,y)

openTag = do
 string "<"
 skipMany space
 many1Till' anyChar $ do
  attrs <- spaceOut $ try (many attr)
  string ">"
  return attrs

closeTag str = do
 x <- string "</"
 y <- string str
 z <- (string ">")
 return y

sparse p t = parse p "" t
sameConstructor (Left _) (Left _) = True
sameConstructor (Right _) (Right _) = True
sameConstructor _ _ = False

groupEithers :: [Either a b] -> [Either [a] [b]]
groupEithers xs = map (\xs@(x:_) -> either (const (Left (lefts xs))) (const (Right (rights xs))) x)  (groupBy sameConstructor xs)
 
tagWithoutContent = do
  (x,a) <- tagWithoutContent'
  return (Tag a x Nothing)

  
tagWithoutContent' = do 
 string "<"
 skipMany space
 many1Till' anyChar $ do
  attrs <- spaceOut $ try (many (try attr))
  string "/>"
  return attrs


closedTag = do
 (x,a) <- openTag
 y <- manyTill ( try (fragmentParse >>= return . Left) <|> (noneOf "><" >>= return . Right)) (try $ closeTag x)
 let s = map (either (TagInner . head) TagString) (groupEithers y)
 return (Tag a x (Just s))
  
fragmentParse = do
  b <- closedTag  
  return b

fullParser =do 
            x <- try tagWithoutContent <|> fragmentParse 
            eof 
            return x
  
showAttribute (x,y) = x++"="++"\""++y++"\""
showAttributes atts = concat $ intersperse " " $ map showAttribute atts
  
showTag (Tag attr str inner) = case inner of 
   Nothing -> "<"++str++attr''++"/>"
   (Just xs) -> "<"++str++attr''++">"++(concatMap showTagInnards xs)++"</"++str++">"
   where attr'' = if null attr' then attr'
                                else " "++attr'
         attr' = showAttributes attr

showTagInnards innard = case innard of
  (TagString str) -> str
  (TagInner inner) -> showTag inner


escapeCode = do 
             char '&' 
             x <- choice (map string (keys escapeCodes) ) 
             char ';'
             return x

w = parse escapeCode "" "&lt;"


cdata = do
   x <- cdata'
   return (TagCData x)                                                                                                                                                                         
 
cdata' = do
 string "<![CDATA["
 many1Till anyChar (string "]]>")
    
q = parse cdata "" "<![CDATA[ hi  ]]>"



replaceParser parser = (\(Right x) ->x) .  parse (replaceParser' parser) "" 

replaceParser' parser = replaceParser'' parser (anyChar >>=(return . (:[])))

replaceParser'' parser p2 = do 
        	            xs <- many (try parser <|> p2 )
                            return (concat xs)

main = do 
       let str = "<one green=\"blue\">five<two>  <![CDATA[ hello  </two>  ]]>    </two></one>"
       print str
       let rs =  map (parse fullParser "") [str,"<single/>","<nocont></nocont>"]
       mapM_ (either (const (print "fail")) (\x -> mapM_ putStrLn [showTag x,show x] )) rs
