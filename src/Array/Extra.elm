module Array.Extra exposing (unzip, uncons, map2)

{-| Some helpers to make Array more list-like
-}

import Array.Hamt as Array exposing (Array)


{-| unzip for arrays
-}
unzip : Array ( a, b ) -> ( Array a, Array b )
unzip array =
    Array.foldl (\( x, y ) ( xs, ys ) -> ( Array.push x xs, Array.push y ys )) ( Array.empty, Array.empty ) array
        |> Debug.log "unzip"


{-| A List-like uncons operator. Note that the single element is acutally popped from the back, not the front of the array
-}
uncons : Array a -> Maybe ( a, Array a )
uncons array =
    Maybe.map2 (,) (Array.get (Array.length array - 1) array) (Just <| Array.slice 0 -1 array)
        |> Debug.log "uncons"


{-| Combine two arrays, combining them with the given function.
If one array is longer, the extra elements are dropped.
    map2 (+) [1,2,3] [1,2,3,4] == [2,4,6]
    map2 (,) [1,2,3] ['a','b'] == [ (1,'a'), (2,'b') ]
    pairs : Array a -> Array b -> Array (a,b)
    pairs lefts rights =
        map2 (,) lefts rights
-}
map2 : (a -> b -> result) -> Array a -> Array b -> Array result
map2 f ws =
    apply (Array.map f ws)
        |> Debug.log "map2"


{-| Unsafe version of get, don't use this unless you know what you're doing!
-}
getUnsafe : Int -> Array a -> a
getUnsafe n xs =
    case Array.get n xs of
        Just x ->
            x

        Nothing ->
            Debug.crash ("Index " ++ toString n ++ " of Array with length " ++ toString (Array.length xs) ++ " is not reachable.")


{-| Apply an array of functions to an array of values.
-}
apply : Array (a -> b) -> Array a -> Array b
apply fs xs =
    let
        l =
            min (Array.length fs) (Array.length xs)

        fs_ =
            Array.slice 0 l fs
    in
        Array.indexedMap (\n f -> f (getUnsafe n xs)) fs_
