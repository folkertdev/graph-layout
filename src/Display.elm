module Display exposing (..)

import Graph.Layout exposing (..)
import Graph.Layout.ForceDirected exposing (..)
import Html exposing (text)
import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import Random.Pcg as Random exposing (Generator)
import List.Extra as List
import Array.Hamt as Array exposing (Array)
import Math.Vector2 as Vec2 exposing (Vec2, vec2, getX, getY)
import Time exposing (Time)
import AnimationFrame
import Graph exposing (Graph, Edge, Node, Adjacency)
import IntDict
import Dict
import State exposing (state, State)


type alias DrawState =
    ( List Vec2, Vec2 )


type alias Path =
    { begin : Vec2, points : List Vec2, end : Vec2 }


translate : (Vec2 -> Vec2) -> Path -> Path
translate f { begin, points, end } =
    { begin = f begin, points = List.map f points, end = f end }


append : Path -> Path -> Path
append first second =
    { begin = first.begin, points = first.points ++ List.map (Vec2.add first.end) second.points, end = Vec2.add first.end second.end }


concat : Vec2 -> List Path -> Path
concat start paths =
    List.foldr append { begin = start, points = [], end = start } paths


reverse : Path -> Path
reverse { begin, points, end } =
    { begin = end, end = begin, points = List.map Vec2.negate points }



{-
   reverse : DrawState -> DrawState
   reverse ( transforms, end ) =
       let
           _ =
               transforms |> List.map Vec2.negate |> List.map (Vec2.add end) |> Debug.log "transforms"

           sum =
               transforms
                   |> List.map Vec2.negate
                   |> List.foldr Vec2.add (vec2 0 0)
                   |> Debug.log "sum"
       in
           ( List.map (Vec2.negate >> flip Vec2.sub (Vec2.scale 1 sum)) transforms
           , Vec2.add end sum
           )
-}


show : (n -> Svg msg) -> LayoutGraph n s -> Svg msg
show nodeLabel ( points, springs ) =
    let
        factor =
            40

        showNode vec =
            viewNodeHelper
                (\label location ->
                    [ {- Svg.circle
                         [ r "10"
                         , cx <| toString (Vec2.getX location)
                         , cy <| toString (Vec2.getY location)
                         ]
                         []
                      -}
                      Svg.text_
                        [ x <| toString (Vec2.getX location)
                        , y <| toString (Vec2.getY location)
                          --, dx <| toString (factor / 3)
                          --, dy <| toString (factor / 4)
                        , textAnchor "middle"
                        , alignmentBaseline "middle"
                        ]
                        -- [ nodeLabel vec.label
                        [ text (toString vec.mass) ]
                    ]
                )
                { vec | location = Vec2.scale factor vec.location }

        showEdge ( p1, p2 ) =
            arrow 10 (Vec2.scale factor p1.location) (Vec2.scale factor p2.location)
    in
        Svg.svg [ height "1000", width "1000", viewBox "-500 -500 1000 1000" ]
            (List.map (showEdge) (stencil ( points, springs ))
                ++ (Array.toList <| Array.map (showNode) points)
            )


lowLevelView : (Point a -> Svg msg) -> (Spring b -> Point a -> Point a -> Svg msg) -> LayoutGraph a b -> List (Svg msg)
lowLevelView viewNode viewSpring ( nodes, edges ) =
    let
        ns =
            Array.map viewNode nodes |> Array.toList

        connections =
            Graph.Layout.stencil ( nodes, edges )

        edges_ =
            List.map unwrapLayoutEdge edges

        springs =
            List.map2 (\spring ( point1, point2 ) -> viewSpring spring point1 point2) edges_ connections
    in
        ns ++ springs


viewNodeHelper : (a -> Vec2 -> List (Svg msg)) -> (Point a -> Svg msg)
viewNodeHelper viewLabel { location, label } =
    Svg.g []
        (viewLabel label location)


viewEdgeHelper : (a -> Vec2 -> Vec2 -> Svg msg) -> (Spring a -> Vec2 -> Vec2 -> Svg msg)
viewEdgeHelper viewLabel { label } p1 p2 =
    viewLabel label p1 p2


orthogonal : Vec2 -> Vec2
orthogonal =
    Vec2.toRecord >> (\{ x, y } -> { x = -y, y = x }) >> Vec2.fromRecord


triangle width height direction_ =
    let
        direction =
            Vec2.normalize direction_

        top =
            direction |> Vec2.scale height

        left =
            direction |> orthogonal |> Vec2.scale (width / 2)

        right =
            direction |> orthogonal |> Vec2.negate |> Vec2.scale (width / 2)
    in
        [ left, right, top ]


test =
    [ reverse (triangle_ 10 10 (vec2 1 0)) ]
        |> toPath (vec2 0 0)
        |> Debug.log "triangle"


triangle_ : Float -> Float -> Vec2 -> Path
triangle_ width height direction_ =
    let
        direction =
            Vec2.normalize direction_

        top =
            direction |> Vec2.scale height

        left =
            direction |> orthogonal |> Vec2.scale (width / 2)

        right =
            direction |> orthogonal |> Vec2.negate |> Vec2.scale (width / 2)
    in
        { begin = vec2 0 0
        , points = diffs [ left, top, right ]
        , end = top
        }


line : Float -> Vec2 -> Float -> Path
line width direction size =
    let
        to =
            Vec2.scale size direction

        offset =
            direction
                |> orthogonal
                |> Vec2.scale (width / 2)
    in
        { begin = vec2 0 0
        , points =
            diffs
                [ Vec2.sub origin offset
                , Vec2.sub to offset
                , Vec2.add to offset
                , Vec2.add origin offset
                ]
        , end = to
        }


diffs vec =
    zipWithTail (flip Vec2.sub) vec vec


zipWithTail f xxs yys =
    case ( xxs, yys ) of
        ( [], _ ) ->
            []

        ( x :: xs, [] ) ->
            []

        ( x :: xs, y :: ys ) ->
            List.map2 f (x :: xs) ys


origin =
    vec2 0 0


startFrom vec =
    ( [], vec )


toPath : Vec2 -> List Path -> String
toPath start paths =
    let
        m vec =
            "m" ++ vectorToString vec

        l vecs =
            "l" ++ String.join "" (List.map vectorToString vecs)

        vectorToString vec =
            toString (getX vec) ++ "," ++ toString (getY vec) ++ " "

        drawState { begin, points, end } =
            "M" ++ vectorToString begin ++ " " ++ l points ++ " " ++ m end
    in
        concat start paths
            |> drawState


arrow margin from to =
    let
        direction =
            Vec2.sub to from
                |> Vec2.normalize

        size =
            Vec2.sub to from |> Vec2.length

        bodyTo =
            Vec2.sub to from
                |> flip Vec2.sub (Vec2.scale 30 direction)
                |> Vec2.add from

        ( shiftedFrom, shiftedTo ) =
            ( Vec2.add from (Vec2.scale margin direction)
            , Vec2.sub to (Vec2.scale margin direction)
            )

        body =
            Svg.line
                [ x1 <| toString (getX shiftedFrom)
                , y1 <| toString (getY shiftedFrom)
                , x2 <| toString (getX bodyTo)
                , y2 <| toString (getY bodyTo)
                , strokeWidth "5"
                , stroke "black"
                ]
                []

        head =
            let
                height =
                    30

                ps =
                    toPath
                        from
                        --[ reverse (triangle_ 70 height direction)
                        [ line 2 direction (size - 2 * height)
                          -- , triangle_ 70 height direction
                        ]
                        |> Svg.Attributes.d
            in
                Svg.path
                    [ stroke "black"
                      --, strokeWidth "5"
                    , fill "black"
                    , ps
                    ]
                    []
    in
        Svg.g [] [ head ]


labels =
    [ "sumatran pine", "silician fir", "japanese larch", "norway spruce", "giant sequoia", "oak", "almond" ]


(=>) =
    (,)


emptyLabels =
    List.map (\( from, to ) -> ( (), from, to ))


exampleGraph =
    uncurry Graph.fromNodeLabelsAndEdgePairs
        ( List.range 1 8 |> List.map toString
        , [ 1 => 2
          , 1 => 3
          , 1 => 4
          , 2 => 5
          , 2 => 6
          , 3 => 7
          , 4 => 1
          , 7 => 8
          , 5 => 8
          , 5 => 8
          , 6 => 8
          ]
            |> List.map (\( a, b ) -> ( a - 1, b - 1 ))
        )


annotateDegree : Graph n e -> Graph ( n, Int ) e
annotateDegree graph =
    Graph.mapContexts
        (\context ->
            let
                node =
                    context.node
            in
                { context | node = { node | label = ( node.label, IntDict.size context.incoming ) } }
        )
        graph


toLayoutGraph : Graph ( n, Int ) e -> LayoutGraph n e
toLayoutGraph graph =
    let
        insert ({ point1, point2 } as edge) cache =
            Dict.update ( point2, point1 )
                (\value ->
                    Just <|
                        case value of
                            Just (Directed spring) ->
                                Bidirectional spring

                            Just (Bidirectional spring) ->
                                Bidirectional spring

                            Nothing ->
                                Directed edge
                )
                cache

        e { from, to, label } =
            { stiffness = 100, point1 = from, point2 = to, label = label, length = 1 }

        v { label } =
            let
                ( value, incoming ) =
                    label
            in
                { location = vec2 0 0, velocity = vec2 0 0, acceleration = vec2 0 0, mass = toFloat incoming, label = value }
    in
        ( Graph.nodes graph
            |> List.map v
            |> Array.fromList
        , Graph.edges graph
            |> List.foldr (\edge cache -> insert (e edge) cache) Dict.empty
            |> Dict.values
        )


initialize : LayoutGraph n e -> Generator (LayoutGraph n e)
initialize ( nodes, edges ) =
    let
        single : Point n -> Generator (Point n)
        single point =
            Random.map (\location -> { point | location = location }) randomVector

        helper =
            Array.map single >> Array.toList >> Graph.Layout.combine >> Random.map Array.fromList
    in
        Random.map2 (,) (helper nodes) (Random.constant edges)



-- Graph n e -> Graph (n, Vec2) (e, Vec2, Vec2)
-- Graph (n, Vec2) (e, Vec2, Vec2) -> Svg msg


binaryTree =
    [ 0 => 1
    , 0 => 2
    , 1 => 3
    , 1 => 4
    , 2 => 5
    , 2 => 6
    ]


type alias Model =
    LayoutGraph String ()


type Msg
    = SetPoints Model
    | Tick Time


subscriptions ( nodes, _ ) =
    let
        minEnergyThreshold =
            0.01
    in
        if not <| (\ps -> totalEnergy ps /= 0 && totalEnergy ps <= minEnergyThreshold) nodes then
            AnimationFrame.diffs Tick
        else
            Sub.batch []


update msg model =
    case msg of
        SetPoints initialModel ->
            ( initialModel, Cmd.none )

        Tick delta ->
            ( Graph.Layout.update { defaultConfig | repulsion = 800 } delta model
            , Cmd.none
            )


main =
    Html.program
        { init =
            -- uncurry (Graph.Layout.init SetPoints) ( labels, emptyLabels binaryTree )
            -- (List.map toString <| List.range 0 6) (emptyLabels binaryTree)
            ( ( Array.empty, [] )
            , Random.generate SetPoints <| initialize << toLayoutGraph <| annotateDegree exampleGraph
              --  , Cmd.none
            )
        , update = update
        , view = show (\label -> Svg.text label)
        , subscriptions = subscriptions
        }
