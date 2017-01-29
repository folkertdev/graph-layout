module Graph.Layout exposing (..)

import Graph.Layout.ForceDirected as ForceDirected exposing (Spring, Point, defaultConfig, Config)
import Random.Pcg as Random exposing (Generator)
import Array.Hamt as Array exposing (Array)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Time exposing (Time)


static : { seed : Int, stiffness : Float, repulsion : Float, damping : Float } -> List a -> List ( Int, Int ) -> List (Point a)
static ({ seed, stiffness } as config) labels connections =
    let
        springs =
            connections
                |> List.map (\( p1, p2 ) -> { stiffness = stiffness, point1 = p1, point2 = p2, label = (), length = 1 })

        internalConfig =
            { defaultConfig | repulsion = config.repulsion, damping = config.damping, minEnergyThreshold = 0.01 }
    in
        labels
            |> List.map randomPoint
            |> combine
            |> flip Random.step (Random.initialSeed seed)
            |> Tuple.first
            |> ForceDirected.simulate 0.03 internalConfig springs


update : Config -> Time -> LayoutGraph a b -> LayoutGraph a b
update config delta ( nodes, springs ) =
    ( ForceDirected.tick { defaultConfig | minEnergyThreshold = 0.0001 } (Basics.max 1.0e-6 <| Basics.min 0.03 (delta / 100)) springs nodes
    , springs
    )


type alias LayoutGraph a b =
    ( Array (Point a), List (Spring b) )


init : (LayoutGraph a b -> msg) -> List a -> List ( b, Int, Int ) -> ( LayoutGraph a b, Cmd msg )
init toMsg labels connections =
    let
        springs =
            connections
                |> List.map (\( label, p1, p2 ) -> { stiffness = 400, point1 = p1, point2 = p2, label = label, length = 1 })
    in
        ( ( Array.empty, [] )
        , labels
            |> List.map randomPoint
            |> combine
            |> Random.map Array.fromList
            |> Random.map (\nodes -> ( nodes, springs ))
            |> Random.generate toMsg
        )


{-| get edges as (node, node) pairs. This is useful for getting the start and end point of an edge, for example in the view.
-}
stencil : LayoutGraph a b -> List ( Point a, Point a )
stencil ( points, springs ) =
    case springs of
        [] ->
            []

        { point1, point2 } :: rest ->
            case Maybe.map2 (,) (Array.get point1 points) (Array.get point2 points) of
                Nothing ->
                    stencil ( points, rest )

                Just p ->
                    p :: stencil ( points, rest )


randomVector : Generator Vec2
randomVector =
    Random.map2 vec2 (Random.float -10 10) (Random.float -10 10)


randomPoint : a -> Generator (Point a)
randomPoint label =
    Random.map (\location -> ForceDirected.newPoint location label) randomVector


{-| Traverse for Generator
-}
combine : List (Generator a) -> Generator (List a)
combine generators =
    case generators of
        [] ->
            Random.constant []

        g :: gs ->
            Random.map2 (::) g (combine gs)
