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

        springs =
            List.map2 (\spring ( point1, point2 ) -> viewSpring spring point1 point2) edges connections
    in
        ns ++ springs


viewNodeHelper : (a -> Vec2 -> List (Svg msg)) -> (Point a -> Svg msg)
viewNodeHelper viewLabel { location, label } =
    Svg.g []
        (viewLabel label location)


viewEdgeHelper : (a -> Vec2 -> Vec2 -> Svg msg) -> (Spring a -> Vec2 -> Vec2 -> Svg msg)
viewEdgeHelper viewLabel { label } p1 p2 =
    viewLabel label p1 p2


arrow margin from to =
    let
        direction =
            Vec2.sub to from
                |> Vec2.normalize

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
                flip1 =
                    Vec2.toRecord >> (\{ x, y } -> { x = -y, y = x }) >> Vec2.fromRecord >> Vec2.normalize

                corner1 =
                    Vec2.add bodyTo (Vec2.scale 10 <| flip1 direction)

                corner2 =
                    Vec2.add bodyTo (Vec2.scale -10 <| flip1 direction)
            in
                Svg.polygon
                    [ strokeWidth "5"
                    , fill "black"
                    , points <| String.join " " (List.map (\v -> toString (getX v) ++ "," ++ toString (getY v)) [ corner1, corner2, shiftedTo ])
                    ]
                    []
    in
        Svg.g [] [ body, head ]


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
        e { from, to, label } =
            { stiffness = 100, point1 = from, point2 = to, label = label, length = 1 }

        v { label } =
            let
                ( value, incoming ) =
                    label
            in
                { location = vec2 0 0, velocity = vec2 0 0, acceleration = vec2 0 0, mass = 1 / (toFloat incoming) ^ 1, label = value }
    in
        ( Graph.nodes graph |> List.map v |> Array.fromList, Graph.edges graph |> List.map e )


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
            ( ( Array.empty, [] ), Random.generate SetPoints <| initialize << toLayoutGraph <| annotateDegree exampleGraph )
        , update = update
        , view = show (\label -> Svg.text label)
        , subscriptions = subscriptions
        }
