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

getTagOfNameChildren str (Tag _ str' (Just xs)) 
  | str == str' = Just xs
  | otherwise = Nothing

getTagsByName str xs = filter ((==str) . getTagName) xs

getNonEmptyTagOfNameChildren str tag = getTagOfNameChildren str tag >>= makeNonEmpty
getNonEmptyTagsByName name xs = makeNonEmpty $ getTagsByName name xs 
