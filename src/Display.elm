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
import Svg.Path as Path exposing (subpath, Path, lineByMany, startAt, closed, moveBy, open)


show : (n -> Svg msg) -> LayoutGraph n s -> Svg msg
show nodeLabel ( points_, springs ) =
    let
        factor =
            40

        points =
            Array.map (\point -> { point | location = Vec2.scale factor point.location }) points_
    in
        Svg.svg [ height "1000", width "1000", viewBox "-500 -500 1000 1000" ]
            (lowLevelView basicViewNode basicViewEdge ( points, springs ))


basicViewNode : Point a -> Svg msg
basicViewNode point =
    viewNodeHelper
        (\label location ->
            [ Svg.text_
                [ x <| toString (Vec2.getX location)
                , y <| toString (Vec2.getY location)
                , textAnchor "middle"
                , alignmentBaseline "middle"
                ]
                [ text (toString point.mass) ]
            ]
        )
        point


basicViewEdge : Spring a -> Point b -> Point c -> Svg msg
basicViewEdge spring p1 p2 =
    arrow p1.location p2.location { margin = 10, arrowWidth = 10, arrowHeight = 10, lineWidth = 2 }


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


triangle : Float -> Float -> Vec2 -> List Path.Subpath
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
        diffpath (vec2 0 0) (diffs [ vec2 0 0, left, top, right ]) top


diffpath start vecs end =
    [ subpath (moveBy <| Vec2.toTuple start) closed [ lineByMany (List.map Vec2.toTuple vecs) ]
    , subpath (moveBy <| Vec2.toTuple end) open []
    ]


line width direction size =
    let
        to =
            Vec2.scale size direction

        offset =
            direction
                |> orthogonal
                |> Vec2.scale (width / 2)

        origin =
            vec2 0 0

        points =
            diffs
                [ origin
                , Vec2.sub origin offset
                , Vec2.sub to offset
                , Vec2.add to offset
                , Vec2.add origin offset
                ]
    in
        diffpath origin points to


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


arrow : Vec2 -> Vec2 -> { margin : Float, arrowHeight : Float, arrowWidth : Float, lineWidth : Float } -> Svg msg
arrow from to { margin, arrowHeight, arrowWidth, lineWidth } =
    let
        direction =
            Vec2.direction to from

        ( shiftedFrom, shiftedTo ) =
            ( Vec2.add from (Vec2.scale margin direction)
            , Vec2.sub to (Vec2.scale margin direction)
            )

        size =
            Vec2.sub shiftedTo shiftedFrom
                |> Vec2.length

        instructions =
            [ [ subpath (startAt <| Vec2.toTuple shiftedFrom) open [] ]
            , line lineWidth direction (size - arrowHeight)
            , triangle arrowWidth arrowHeight direction
            ]
                |> List.concat
                |> Path.pathToString
                |> Svg.Attributes.d

        path =
            Svg.path
                [ stroke "black"
                  --, strokeWidth "5"
                , fill "black"
                , instructions
                ]
                []
    in
        Svg.g [] [ path ]


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
