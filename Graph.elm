module Graph exposing (..)


type Graph a
    = Empty
    | Vertex a
    | Overlay (Graph a) (Graph a)
    | Connect (Graph a) (Graph a)


map : (a -> b) -> Graph a -> Graph b
map f graph =
    case graph of
        Empty ->
            Empty

        Vertex x ->
            Vertex (f x)

        Overlay subgraph1 subgraph2 ->
            Overlay (map f subgraph1) (map f subgraph2)

        Connect subgraph1 subgraph2 ->
            Connect (map f subgraph1) (map f subgraph2)


foldr f default graph =
    case graph of
        Empty ->
            default

        Vertex x ->
            f x default

        Overlay subgraph1 subgraph2 ->
            foldr f (foldr f default subgraph2) subgraph1

        Connect subgraph1 subgraph2 ->
            foldr f (foldr f default subgraph2) subgraph1


foldMap f graph =
    case graph of
        Empty ->
            Empty

        Vertex x ->
            f x

        Overlay subgraph1 subgraph2 ->
            Overlay (foldMap f subgraph1) (foldMap f subgraph2)

        Connect subgraph1 subgraph2 ->
            Connect (foldMap f subgraph1) (foldMap f subgraph2)


andThen : (a -> Graph b) -> Graph a -> Graph b
andThen =
    foldMap


map2 : (a -> b -> c) -> Graph a -> Graph b -> Graph c
map2 f graph1 graph2 =
    graph1 |> andThen (\x -> graph2 |> andThen (\y -> vertex (f x y)))


toList =
    foldr (::) []


vertex =
    Vertex


empty =
    Empty


overlay =
    Overlay


connect =
    Connect


{-| Graph consisting of unconnected vertices
-}
vertices : List a -> Graph a
vertices =
    List.foldr overlay empty << List.map vertex


clique : List a -> Graph a
clique =
    List.foldr connect empty << List.map vertex


fromEdgeList : List ( a, a ) -> Graph a
fromEdgeList =
    let
        edge ( x, y ) =
            connect (vertex x) (vertex y)
    in
        List.foldr overlay empty << List.map edge


box : Graph a -> Graph a -> Graph ( a, a )
box x y =
    let
        xs =
            List.map (\b -> map (\a -> ( a, b )) x) <| toList y

        ys =
            List.map (\a -> map (\b -> ( a, b )) y) <| toList x
    in
        List.foldr overlay empty <| xs ++ ys
