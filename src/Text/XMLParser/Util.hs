module Text.XMLParser.Util (

 getTagOfNameChildren
,getTagsByName
,getNonEmptyTagOfNameChildren
,getNonEmptyTagsByName
,getNonEmptyAttributeByName
) where 

import Text.XMLParser.XMLParser
import Data.Maybe

makeNonEmpty [] = Nothing
makeNonEmpty xs@(_:_) = Just xs

getTagName (Tag _ str _) = Just str
getTagName _ = Nothing

getTagText (TagString a) = Just a
getTagText _ = Nothing

getTagChildText (Tag _ _ (Just xs)) = catMaybes $  map getTagText xs
getTagChildText _ =  []

getTagOfNameChildren str (Tag _ str' (Just xs)) 
  | str == str' = Just xs
  | otherwise = Nothing

getTagsByName str xs = filter (maybe False (==str) . getTagName) xs

getNonEmptyTagOfNameChildren str tag = getTagOfNameChildren str tag >>= makeNonEmpty
getNonEmptyTagsByName name xs = makeNonEmpty $ getTagsByName name xs 
getNonEmptyAttributeByName attr (Tag attrs _ _) = lookup attr attrs 

