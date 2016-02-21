module GameUtil where
-- UTIL

stail : List Int -> List Int
stail xs = case List.tail xs of 
  Nothing -> []
  Just tl -> tl
 
shead : List Int -> Int
shead xs = case List.head xs of 
  Nothing -> 0
  Just i -> i

updateone index with list = 
   if (List.isEmpty list) then [] else 
     case (index, (shead list)) of 
       (0, 0) -> (with :: (stail list))
       (i, 0) -> 0 :: (updateone  (i-1) with (stail list))
       (i, sh) -> sh :: (updateone  i with (stail list))
