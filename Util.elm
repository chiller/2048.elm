module Util exposing (..)
-- UTIL

stail : List Int -> List Int
stail xs = case List.tail xs of
  Nothing -> []
  Just tl -> tl

shead : List Int -> Int
shead xs = case List.head xs of
  Nothing -> 0
  Just i -> i

updateone : Int -> Int -> List Int -> List Int
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

mergeAndScore : List Int -> (List Int, Int)
mergeAndScore list = case list of
    (x::y::xs) ->
      if (x == y) then
         let
          (mergedTail, score) = mergeAndScore xs
         in ([x*2, 0] ++ mergedTail, score + x*2)
      else
        let
          (mergedTail, score) = mergeAndScore (y::xs)
        in (x::mergedTail, score)
    xs -> (xs, 0)

-- Begin weird stuff
type alias M a = (List a, Int)
compose : (List a -> List a) -> (List a -> M a) -> (List a -> M a)
compose a b = \ xs -> let (is, i) = b xs in (a is, i)

flatScore : List (M Int) -> M (List Int)
flatScore xs = let (rows, scores) = List.unzip xs in (rows, List.sum scores)

-- End weird stuff
range : Int -> Int -> List Int
range x y = case x == y of
    True -> [y]
    False -> x :: range (x + 1) y

zip : List a -> List b -> List (a, b)
zip = List.map2 (,)
