module Text.XMLParser.Util (
 getTagOfNameChildren
,getTagsByName
,getNonEmptyTagOfNameChildren
,getNonEmptyTagsByName
) where 

import Text.XMLParser.XMLParser


makeNonEmpty [] = Nothing
makeNonEmpty xs@(_:_) = Just xs

getTagName (Tag _ str _) = str  

getTagOfNameChildren str (Tag _ str (Just xs)) 
  | str == str = xs
  | otherwise = Nothing

getTagsByName str xs = filter ((==str) . getTagName) xs

getNonEmptyTagOfNameChildren str tag = getTagOfNameChildren >>= makeNonEmpty
getNonEmptyTagsByName str xs = getTagsByName >>= makeNonEmpty
