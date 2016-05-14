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

splitAt : Int -> List Int -> List ( List Int )
splitAt k list =
  if List.isEmpty list then [] else
    [List.take k list] ++ (splitAt k (List.drop k list))

transpose : List (List Int) -> List (List Int)
transpose list =
  case list of
    [l1, l2, l3, l4] -> List.map4 (\a b c d -> [a,b,c,d]) l1 l2 l3 l4
    _ -> []


zeroes : List Int -> List Int
zeroes = List.filter <| (==) 0

nonzeroes : List Int -> List Int
nonzeroes = List.filter <| (/=) 0

shift : List Int -> List Int
shift board = nonzeroes board ++ zeroes board

merge : List Int -> List Int
merge list = case list of
    (x::y::xs) ->
      if (x == y) then [x*2, 0] ++ merge xs
      else x :: merge (y::xs)
    xs -> xs

range : Int -> Int -> List Int
range x y = case x == y of
    True -> [y]
    False -> x :: range (x + 1) y

zip : List a -> List b -> List (a, b)
zip = List.map2 (,)
