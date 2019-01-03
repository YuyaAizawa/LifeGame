module Array2D exposing
  ( Array2D
  , initialize
  , isEmpty
  , get
  , width
  , height
  , set
  , map
  , indexedMap
  , toIndexedList
  )

import Array exposing (Array)

type Array2D a
  = {-
    * width : Int = The width of the 2d array.
    * height : Int = The height of the 2d array.
    * array : Array a = The actual array.
  -}
  Array2D Int Int (Array a)

initialize : Int -> Int -> (Int -> Int -> a) -> Array2D a
initialize w h init =
  let
    contents =
      List.range 0 (h-1)
        |> List.concatMap (\y ->
          List.range 0 (w-1)
            |> List.map (\x ->
              init x y))
        |> Array.fromList
  in
    Array2D w h contents

isEmpty : Array2D a -> Bool
isEmpty (Array2D w h _) =
  w == 0 && h == 0

get : Int -> Int -> Array2D a -> Maybe a
get x y (Array2D w h array) =
  Array.get (toIndex x y w h) array

width : Array2D a -> Int
width (Array2D w _ _) =
  w

height : Array2D a -> Int
height (Array2D _ h _) =
  h

set : Int -> Int -> a -> Array2D a -> Array2D a
set x y a (Array2D w h array) =
  Array2D w h (Array.set (toIndex x y w h) a array)

toIndex : Int -> Int -> Int -> Int -> Int
toIndex x y w h =
      if 0 <= x && x < w && 0 <= y && y < h
      then (y * w + x)
      else -1

map : (a -> b) -> Array2D a -> Array2D b
map mapper (Array2D w h array) =
  Array2D w h (array |> Array.map mapper)

indexedMap : (Int -> Int -> a -> b) -> Array2D a -> Array2D b
indexedMap f (Array2D w h array) =
  let
    newArray =
      array
        |> Array.indexedMap (\i -> \a ->
          f (i |> modBy w) (i // w) a)
  in
    Array2D w h newArray

toIndexedList : Array2D a -> List (Int, Int, a)
toIndexedList (Array2D w _ array) =
  array
    |> Array.toList
    |> List.indexedMap (\i -> \a -> (i |> modBy w, i // w, a))