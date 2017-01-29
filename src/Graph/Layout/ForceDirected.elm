module Graph.Layout.ForceDirected exposing (tick, simulate, defaultConfig, Config, Point, newPoint, Spring, totalEnergy)

{-|


This is an elm port of [springy.js](http://getspringy.com/) - a force-directed graph layout algorithm.

There is actually very little approachable information on how to automatically layout graphs.

# Types
@docs Point, Spring, newPoint

# Simulation
@docs simulate, tick

# Configuration
@docs Config, defaultConfig

# Helpers
@docs totalEnergy
-}

import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Array.Hamt as Array exposing (Array)
import Random.Pcg as Random exposing (Generator)


-- custom Array.Extra using Array.Hamt

import Array.Extra as Array


{-| Run the simulation until the total energy in the system is below a certain threshold
-}
simulate : Float -> Config -> List (Spring s) -> List (Point p) -> List (Point p)
simulate timestep config springs points =
    points
        |> Array.fromList
        |> until (\ps -> totalEnergy ps /= 0 && totalEnergy ps <= config.minEnergyThreshold) (tick config timestep springs)
        |> Array.toList


{-| One tick of the simulation: update all the physics

There are two steps

* Calculate forces
    * **Coulombs law** nodes repel each other
    * **Hookes law** springs contract
    * **Attract to centre** to keep the drawing compact
* Apply forces
    * **updateVelocity** transfers acceleration into increase in speed
    * **updatePosition** transfers velocity into change in position

Because the implementation needs to connect nodes with edges by index (the springs store indices into the nodes sequence),
the internal algorithm works on `Array` - which allows constant time random access.

**Note:** Core's Array
implementation is flawed, so [elm-array-exploration](http://package.elm-lang.org/packages/Skinney/elm-array-exploration/2.0.1/Array-Hamt) is used instead.
If you want to directly use `tick`, you need to supply a `Array.Hamt.Array`.
-}
tick : Config -> Float -> List (Spring s) -> Array (Point p) -> Array (Point p)
tick config timestep springs points =
    coulombsLaw config points
        |> (\points -> List.foldl (flip hookesLaw) points springs)
        |> Array.map
            (attractToCentre config
                >> updateVelocity config timestep
                >> updatePosition timestep
            )


{-| Variables that determine the physical system. See also [defaultConfig](#defaultConfig).
-}
type alias Config =
    { damping : Float, repulsion : Float, minEnergyThreshold : Float, maxSpeed : Float }


{-| Default settings for the simulation

    defaultConfig : Config
    defaultConfig =
        { stiffness = 400                  -- how strong do the springs pull
        , repulsion = 400                  -- how strongly do the nodes repel
        , damping = 0.5                    -- limit speed
        , minEnergyThreshold = 0.01        -- stop the simulation when below this
        , maxSpeed = toFloat Random.maxInt -- bound the speed
        }

-}
defaultConfig : Config
defaultConfig =
    { repulsion = 400
    , damping = 0.5
    , minEnergyThreshold = 0.01
    , maxSpeed = toFloat Random.maxInt
    }


{-| A wrapper around a value to make it a node. The wrapped value is meant
to be converted into a label during rendering of the graph.
-}
type alias Point a =
    { location : Vec2, mass : Float, velocity : Vec2, acceleration : Vec2, label : a }


{-| Default point with zeroed velocity and acceleration and unit mass.
-}
newPoint : Vec2 -> a -> Point a
newPoint location value =
    { location = location, velocity = vec2 0 0, acceleration = vec2 0 0, mass = 1, label = value }


{-| A wrapper around a value to make it an edge. The wrapped value is meant
to be converted into a label during rendering of the graph.
-}
type alias Spring a =
    { stiffness : Float, length : Float, point1 : Int, point2 : Int, label : a }


repel : Config -> Vec2 -> Vec2 -> ( Vec2, Vec2 )
repel config v1 v2 =
    let
        d =
            Vec2.sub v1 v2

        distance =
            Vec2.lengthSquared d + 0.1

        direction =
            Vec2.normalize d
                |> Vec2.scale (config.repulsion / (distance * 0.5))
    in
        ( direction
        , Vec2.negate direction
        )


applyForce : Vec2 -> Point a -> Point a
applyForce force point =
    { point
        | acceleration =
            Vec2.scale (1 / point.mass) force
                |> Vec2.add point.acceleration
    }


distribute : Config -> Vec2 -> Array Vec2 -> ( Vec2, Array Vec2 )
distribute config v vs =
    Array.unzip (Array.map (repel config v) vs)
        |> Tuple.mapFirst (Array.foldr Vec2.add (vec2 0 0))


{-| This is the workhorse, but it is actually quite simple

The idea is to build up a list of lists representing the repelling forces, and
then apply these forces to the nodes (as change in acceleration).

This function fuses those two processes together, which does not
materialize an intermediate data structure and is thus faster
-}
fused : (a -> Array a -> ( a, Array a )) -> (a -> a -> a) -> Array a -> Array a
fused expand append array =
    case Array.uncons array of
        Just ( x, xs ) ->
            case expand x xs of
                ( z, zs ) ->
                    Array.push z (Array.map2 append zs (fused expand append xs))

        Nothing ->
            Array.empty


distributeForce : Config -> Array (Point a) -> Array (Point a)
distributeForce config points =
    Array.map .location points
        |> fused (distribute config) Vec2.add
        |> Array.map2 (flip applyForce) points


{-| coulombs law, or more practically, the repelling of nodes.

Nodes that are very close to each other are undesirable, so we make them repel each other
with some force. This is based on coulombs law, which describes how particles with the same charge
repel each other.
-}
coulombsLaw : Config -> Array (Point a) -> Array (Point a)
coulombsLaw config =
    distributeForce config


{-| Hookes law: Springs pulling nodes together

This function is awkward because functional graphs are: The springs hold two integers, that
are indices into the points array. Updating

-}
hookesLaw : Array (Point p) -> Spring s -> Array (Point p)
hookesLaw points ({ stiffness, length, point1, point2 } as original) =
    case Maybe.map2 (,) (Array.get point1 points) (Array.get point2 points) of
        Nothing ->
            points

        Just ( p1, p2 ) ->
            let
                d =
                    Vec2.sub p2.location p1.location

                displacement =
                    length - Vec2.length d

                direction =
                    Vec2.normalize d
                        |> Vec2.scale (stiffness * displacement * 0.5)
            in
                points
                    |> Array.set point1 (applyForce (Vec2.negate direction) p1)
                    |> Array.set point2 (applyForce direction p2)


attractToCentre : Config -> Point a -> Point a
attractToCentre config point =
    point.location
        |> Vec2.scale (-config.repulsion / 50)
        |> flip applyForce point


{-| Applies the acceleration as an increase in velocity
-}
updateVelocity : Config -> Float -> Point a -> Point a
updateVelocity config timestep point =
    let
        boundSpeed vector =
            if Vec2.length vector > config.maxSpeed then
                vector
                    |> Vec2.normalize
                    |> Vec2.scale config.maxSpeed
            else
                vector
    in
        { point
            | velocity =
                Vec2.scale timestep point.acceleration
                    |> Vec2.add point.velocity
                    |> Vec2.scale config.damping
                    |> boundSpeed
            , acceleration = vec2 0 0
        }


{-| Apply velocity as a change in position
-}
updatePosition : Float -> Point a -> Point a
updatePosition timestep point =
    { point | location = Vec2.add point.location (Vec2.scale timestep point.velocity) }


{-| Calculates the total energy in the system `sum <| 0.5 * mass * Vec2.lengthSquared velocity`.
-}
totalEnergy : Array (Point a) -> Float
totalEnergy points =
    Array.foldr (\{ velocity, mass } accum -> accum + 0.5 * mass * Vec2.lengthSquared velocity) 0 points


until : (a -> Bool) -> (a -> a) -> a -> a
until p f x =
    if p x then
        x
    else
        until p f (f x)
