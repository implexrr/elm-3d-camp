module PhysicsWrapper3D exposing 
    ( PhysicsData
    , renderBodies, getBodyByName
    , usePhysics, withMass, withMassU, autoMass, linkTo, hingeTo, unlink
    , moveBody, startAtPos, rotateBody, moveBodyTo
    , push, applyForces, pushIf
    , updateIf, removeLinks
    , constrainBodies
    , inAtmosphere, inSpace, customDamping
    , showVisualizers
    , positionOnScreen, positionOnScreenUnclamped, adjustedPosition, getMass
    , tracker, bodyTracker, bodyTrackerOldCamera, physicsDebug
    , setTags, addTag, removeTag, hasTag, addFlair, isVisible
    )
import Point3d
-- Most of these imports were taken from "3d-elm-camp/BeeMovement.elm", so there may be a lot of unused things
import Angle exposing (Angle)
import Array exposing (Array)
import Axis3d exposing (Axis3d)
import Block3d exposing (Block3d)
import BoundingBox2d exposing (BoundingBox2d)
import BoundingBox3d exposing (BoundingBox3d)
import Camera3d exposing (Camera3d)
import Color exposing (Color)
import Dict exposing (Dict)
import Direction3d exposing (Direction3d)
import Duration exposing (Duration)
import Density exposing (Density)
import Frame3d
import GraphicSVG exposing (Shape, centered, circle, customFont, filled, group, move, outlined, rectangle, rotate, scale, size, solid, text)
import Mass exposing (Kilograms, Mass)
import Force exposing (Force)
import Illuminance
import Length exposing (Meters, Length)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Point3d.Projection as Projection exposing (depth)
import Quantity exposing (Quantity)
import Rectangle2d
import Round
import Scene3d exposing (Entity)
import Scene3d.Material as Material exposing (Material)
import Scene3d.Mesh as Mesh
import Set exposing (Set)
import SketchPlane3d
import SolidAngle
import Speed
import Sphere3d
import Temperature
import Tuple
import Vector3d exposing (Vector3d)
import Viewpoint3d
import Cone3d
import Arc2d
import Arc3d
import Circle3d
import TriangularMesh
import Cylinder3d
import Triangle3d
import LineSegment3d
import WebGL.Texture
import Parameter1d
import Skybox
import Collision
import Wrapper3D exposing (..)
import Wrapper3DCamera as W3C
import Volume

import Physics.World as World exposing (World)
import Physics.Body as Body exposing (Body)
import Physics.Coordinates exposing (WorldCoordinates, BodyCoordinates)
import Physics.Constraint as Constraint exposing (Constraint)
import Physics.Shape as Shape
import List
import List.Extra as List

-- GENERAL FUNCTIONS

{-| Given a camera and screen dimensions, returns the position of a 3D point on the screen.
This function also clamps the result so that it's always within the screen's bounds -}
positionOnScreen : Camera3d units coordinates -> (Int, Int) -> Point3d units coordinates -> (Float, Float)
positionOnScreen camera (width, height) position =
    let
        screenRect =
            Rectangle2d.withDimensions (Pixels.float (toFloat width), Pixels.float (toFloat height)) Quantity.zero Point2d.origin
        screenPoint =
            position
            |> Projection.toScreenSpace camera screenRect
        ( halfW, halfH ) =
            ( toFloat width / 2, toFloat height / 2 )
        (x, y) =
            screenPoint
            |> Point2d.toTuple Pixels.inPixels
            |> Tuple.mapBoth ( clamp -halfW halfW ) ( clamp -halfH halfH )
    in
        ( x, y )

{-| Given a camera and screen dimensions, returns the position of a 3D point on the screen. -}
positionOnScreenUnclamped : Camera3d units coordinates -> (Int, Int) -> Point3d units coordinates -> (Float, Float)
positionOnScreenUnclamped camera (width, height) position =
    let
        screenRect =
            Rectangle2d.withDimensions (Pixels.float (toFloat width), Pixels.float (toFloat height)) Quantity.zero Point2d.origin
        screenPoint =
            position
            |> Projection.toScreenSpace camera screenRect
        (x, y) =
            screenPoint
            |> Point2d.toTuple Pixels.inPixels
    in
        ( x, y )

{-| Get the position of a body in world coordinates, taking into account any offset from its object -}
adjustedPosition : Body PhysicsData -> Point3d Meters WorldCoordinates
adjustedPosition body =
    let
        data = Body.data body
    in
        Body.originPoint body
        |> Point3d.relativeTo (Body.frame body)
        |> Point3d.translateBy (Vector3d.from Point3d.origin (Point3d.relativeTo Frame3d.atOrigin data.originalPos))
        |> Point3d.placeIn (Body.frame body)

objectOffset : Body PhysicsData -> Vector3d Meters BodyCoordinates
objectOffset body =
    let
        data = Body.data body
    in
        Vector3d.from Point3d.origin (Point3d.relativeTo Frame3d.atOrigin data.originalPos)

{-| Returns whether or not the bounding box of the given object is visible to the given camera. -}
isVisible : Camera3d Meters coordinates -> (Int, Int) -> Object coordinates -> Bool
isVisible camera (width, height) obj =
    let
        centre =
            getCentre obj

        getGroupPoints object =
            case object of
                ObjectGroup attr ->
                    case attr.boundingBox of
                        None ->
                            []
                        Box boundingBox ->
                            ( boundingBox
                                |> Block3d.fromBoundingBox
                                |> Block3d.vertices
                            ) ++ List.concatMap getGroupPoints attr.subObjects

                Object _ ->
                    []

        vertices =
            case obj of
                ObjectGroup _ ->
                    getGroupPoints obj

                Object attr ->
                    case attr.boundingBox of
                        None ->
                            []
                        Box boundingBox ->
                            ( boundingBox
                                |> Block3d.fromBoundingBox
                                |> Block3d.vertices
                            )

        viewWindow =
            BoundingBox2d.withDimensions (Pixels.pixels (toFloat width), Pixels.pixels (toFloat height)) Point2d.origin

        objProjection =
            let
                firstPoint =
                    centre
                        |> positionOnScreenUnclamped camera (width, height)
                        |> ( \ (x, y) -> Point2d.pixels x y )
                points =
                    vertices
                        |> List.map (positionOnScreenUnclamped camera (width, height))
                        |> List.map ( \ (x, y) -> Point2d.pixels x y )
            in
                BoundingBox2d.hull firstPoint points

        distance point = depth camera point
        
        tolerance =
            toFloat (width + height) / 2 * 0.25

        foldOR val bs =
            if val then
                True
            else
                case bs of
                    [] ->
                        val
                    (x :: xs) ->
                        foldOR x xs
    in
        (List.map ( \ p -> distance p |> Quantity.greaterThanOrEqualTo Quantity.zero) (centre :: vertices) |> foldOR False) &&
        not (objProjection |> BoundingBox2d.separatedByAtLeast (Pixels.pixels tolerance) viewWindow)

-- PHYSICS FUNCTIONS

type Hinge
    = Basic
    | Hinge (Axis3d Meters BodyCoordinates) (Axis3d Meters BodyCoordinates)

type alias PhysicsData =
    { object : Object BodyCoordinates
    , name : String
    , tags : Set String
    , mass : Mass
    , linkedBodies : List (String, Hinge)
    , originalPos : Point3d Meters WorldCoordinates
    , constantForces :
        List
        { force : Force
        , direction : Direction3d BodyCoordinates
        , relativePosition : Point3d Meters BodyCoordinates }
    , netForce :
        { force : Force
        , direction : Direction3d BodyCoordinates
        }
    , visualizers : Bool
    , initialized : Bool
    , forces : List
        { force : Force
        , direction : Direction3d BodyCoordinates
        , relativePosition : Point3d Meters BodyCoordinates }
    }

{-| Returns a body with the given name from the given world, if it exists. Returns Nothing if it doesn't -}
getBodyByName : String -> World PhysicsData -> Maybe (Body PhysicsData)
getBodyByName name world =
    world 
        |> World.keepIf ( \ body -> (Body.data body).name == name ) 
        |> World.bodies 
        |> List.head

{-| Extracts all physics bodies from a `World` and converts them to their `Object` representations -}
renderBodies : Bool -> World PhysicsData -> List (Object WorldCoordinates)
renderBodies showDebugs world = 
    List.map 
        (\body ->
            let
                bodyFrame : Frame3d.Frame3d Meters WorldCoordinates { defines : BodyCoordinates }
                bodyFrame = Body.frame body

                obj : Object BodyCoordinates
                obj = data.object

                data = Body.data body

                posTuple = Point3d.toTuple Length.inCentimeters data.originalPos

                positionDebugs =
                    group3D 
                        [ cylinder 0.1 100 |> matte Color.red |> move3D (0, 0, 50) |> rotate3D (degrees 90) 0 0 |> move3D posTuple 
                        , cylinder 0.1 100 |> matte Color.green |> move3D (0, 0, 50) |> rotate3D 0 0 (degrees 90) |> move3D posTuple 
                        , cylinder 0.1 100 |> matte Color.blue |> move3D (0, 0, 50) |> move3D posTuple
                        ]

                velocityDebug =
                    let
                        (xDir, yDir, zDir) =
                            Vector3d.direction (Body.velocity body)
                            |> Maybe.withDefault Direction3d.x
                            |> Direction3d.components

                        azimuth =
                            atan2 yDir xDir

                        elevation =
                            atan2 (sqrt (xDir ^ 2 + yDir ^ 2)) zDir

                        magnitude =
                            Vector3d.length (Body.velocity body)
                            |> Speed.inMetersPerSecond
                    in
                        cylinder 0.5 (magnitude * 50)
                        |> matte Color.orange
                        |> rotate3D elevation azimuth 0
                        |> move3D (Point3d.toTuple Length.inCentimeters (adjustedPosition body))

                allForces = data.forces ++ data.constantForces

                maxForce : Force
                maxForce = Maybe.withDefault Quantity.zero
                    <| Maybe.map (\f -> f.force) <| List.maximumBy (\f -> Force.inNewtons f.force) allForces

                maxLength = Length.centimeters 50

                renderForce : { force : Force, relativePosition : Point3d Meters BodyCoordinates, direction : Direction3d BodyCoordinates } -> Object BodyCoordinates
                renderForce { force, relativePosition, direction } =
                    let
                        length =
                            maxLength
                                |> Quantity.multiplyBy
                                ((Force.inNewtons force) / (Force.inNewtons maxForce))

                        forcePos =
                            relativePosition
                            |> Point3d.translateBy (objectOffset body)

                    in
                        arrowStartingAt forcePos direction { length = length, radius = Length.centimeters 1 }
                            |> matte Color.red

                renderForces : Object BodyCoordinates
                renderForces =
                    group3D <| List.map renderForce allForces

            in
                if showDebugs || data.visualizers then
                    group3D
                        [
                            group3D 
                                [ obj
                                , positionDebugs
                                , renderForces
                                ]
                            |> placeObject bodyFrame
                        ,   velocityDebug
                        ]
                else
                    obj
                    |> placeObject bodyFrame
                )
        ( World.bodies world )

tracker :
    Camera3d Meters WorldCoordinates ->
    { a | width : Quantity Int Pixels,
          height : Quantity Int Pixels } ->
    Point3d Meters WorldCoordinates ->
    Float ->
    GraphicSVG.Color ->
    GraphicSVG.Color ->
    String ->
    Bool ->
    Shape msg
tracker camera { width, height } point trackerSize trackerColour textColour name showDistance =
    let
         relativeSize = trackerSize / 15
         distance = depth camera point
    in
    if distance |> Quantity.greaterThan Quantity.zero then
    group
        [ rectangle relativeSize ( 15 * relativeSize )
          |> filled trackerColour
        , rectangle ( 15 * relativeSize ) relativeSize
          |> filled trackerColour
        , circle ( 5 * relativeSize )
          |> outlined ( solid relativeSize ) trackerColour
        , rectangle relativeSize ( 15 * relativeSize )
          |> filled trackerColour
          |> rotate ( degrees -45 )
          |> move ( 10 * relativeSize, 10 * relativeSize )
        , text (name ++ if showDistance then (" (" ++ String.fromFloat (Length.inKilometers distance) ++ "km)") else "")
          |> customFont "Audiowide"
          |> size ( 12 * relativeSize )
          |> filled trackerColour
          -- |> addOutline ( solid ( 0.25 * relativeSize ) ) black
          |> move ( 15 * relativeSize, 15 * relativeSize )
        ]
    |> move ( point |> positionOnScreen camera (Pixels.toInt width, Pixels.toInt height) )
        else group []

bodyTracker :
    String ->
    { a | world : World PhysicsData
        , cameraModel : W3C.Model WorldCoordinates
        , width : Quantity Int Pixels
        , height : Quantity Int Pixels
    } ->
    Float ->
    GraphicSVG.Color ->
    GraphicSVG.Color ->
    Bool ->
    Shape msg
bodyTracker objToTrack model trackerSize trackerColour textColour showDistance =
    let
        toTrack = List.filter (\body -> (Body.data body).name == objToTrack) (World.bodies model.world)
        trackingName =
                    case List.head toTrack of
                        Just body ->
                            (Body.data body).name
                        Nothing ->
                            "Nothing"
        bodyMaybe = List.head toTrack
    in
        case bodyMaybe of
            Just body ->
                tracker model.cameraModel.camera model (body |> adjustedPosition) trackerSize trackerColour textColour trackingName showDistance
            Nothing ->
                group []

{-| Creates a body tracker from a model that uses the old camera code. Should be considered deprecated. -}
bodyTrackerOldCamera :
    String ->
    { a | world : World PhysicsData
        , camera : Camera3d Meters WorldCoordinates
        , width : Quantity Int Pixels
        , height : Quantity Int Pixels
    } ->
    Float ->
    GraphicSVG.Color ->
    GraphicSVG.Color ->
    Bool ->
    Shape msg
bodyTrackerOldCamera objToTrack model trackerSize trackerColour textColour showDistance =
    let
        toTrack = List.filter (\body -> (Body.data body).name == objToTrack) (World.bodies model.world)
        trackingName =
                    case List.head toTrack of
                        Just body ->
                            (Body.data body).name
                        Nothing ->
                            "Nothing"
        bodyMaybe = List.head toTrack
    in
        case bodyMaybe of
            Just body ->
                tracker model.camera model (body |> adjustedPosition) trackerSize trackerColour textColour trackingName showDistance
            Nothing ->
                group []

physicsDebug :
    String ->
    { a | world : World PhysicsData, width : Quantity Int Pixels, height : Quantity Int Pixels, cameraModel : W3C.Model WorldCoordinates } ->
    Float ->
    GraphicSVG.Color ->
    GraphicSVG.Color ->
    Shape msg
physicsDebug objToTrack model trackerSize trackerColour textColour =
    let
        toTrack = getBodyByName objToTrack model.world
        trackingName =
            case toTrack of
                Just body ->
                    (Body.data body).name
                Nothing ->
                    "Nothing"
        relativeSize = trackerSize / 15
        textLeft = -300 * toFloat (unwrapQ model.width) / 1920
        bodyInfo =
            case toTrack of
                Just body ->
                    let
                        frame = Body.frame body
                        vectorToString vector =
                            "{ x = " ++ Round.round 2 (unwrapQ (Vector3d.xComponent vector)) ++ "m/s, y = " ++ Round.round 2 (unwrapQ (Vector3d.yComponent vector)) ++ "m/s, z = " ++ Round.round 2 (unwrapQ (Vector3d.zComponent vector)) ++ "m/s }"
                        pointToString point =
                            let
                                (x, y, z) = Point3d.toTuple Length.inMeters point
                            in
                                "{ x = " ++ Round.round 2 x ++ "m, y = " ++ Round.round 2 y ++ "m, z = " ++ Round.round 2 z ++ "m }"
                        dirToString dir =
                            "{ x = " ++ Round.round 2 (Direction3d.xComponent dir) ++ ", y = " ++ Round.round 2 (Direction3d.yComponent dir) ++ ", z = " ++ Round.round 2 (Direction3d.zComponent dir) ++ " }"
                    in
                        group
                            [ text ("Name: " ++ trackingName) |> customFont "Audiowide" |> filled textColour |> move (textLeft,50)
                            , text ("Velocity: " ++ vectorToString (Body.velocity <| body)) |> customFont "Audiowide" |> filled textColour |> move (textLeft,35)
                            , text ("Origin Point: " ++ pointToString (Frame3d.originPoint <| frame)) |> customFont "Audiowide" |> filled textColour |> move (textLeft,20)
                            , text ("Rotational velocity: " ++ vectorToString (Body.angularVelocity <| body)) |> customFont "Audiowide" |> filled textColour |> move (textLeft,5)
                            , text ("x direction: " ++ dirToString (Frame3d.xDirection <| frame)) |> customFont "Audiowide" |> filled textColour |> move (textLeft,-10)
                            , text ("y direction: " ++ dirToString (Frame3d.yDirection <| frame)) |> customFont "Audiowide" |> filled textColour |> move (textLeft,-25)
                            , text ("z direction: " ++ dirToString (Frame3d.zDirection <| frame)) |> customFont "Audiowide" |> filled textColour |> move (textLeft,-40)
                            , text (Debug.toString (Body.data body).visualizers) |> customFont "Audiowide" |> filled textColour |> move (textLeft,-55)
                            ] |> scale (2 * (toFloat (unwrapQ model.height) / 970)) |> move (0, -250 * toFloat (unwrapQ model.height) / 970)
                Nothing ->
                    text "Error: Invalid Physics Body" |> centered |> filled textColour
        theTracker = bodyTracker objToTrack model trackerSize trackerColour textColour True
    in
    group
        [ bodyInfo
        , theTracker
        ]
        
{- Functions related to creating and altering physics bodies -}

{-| Creates a list of physics shapes from an object for constructing a compound body. -}
objToShapes : Object BodyCoordinates -> List Shape.Shape
objToShapes object =
    case object of
        ObjectGroup attr ->
            List.concatMap objToShapes attr.subObjects
        Object attr ->
            case (attr.boundingBox, attr.approxShape, attr.customMesh) of
                -- Rings are represented by cylinders
                -- (Composite boxes, RingShape r t, _) ->
                --     [ Shape.cylinder 12
                --         (Cylinder3d.centeredOn
                --             Point3d.origin
                --             Direction3d.z
                --             { radius = Length.centimeters r, length = Length.centimeters t }
                --             |> Cylinder3d.rotateAround Axis3d.y attr.rotation.pitch
                --             |> Cylinder3d.rotateAround Axis3d.x attr.rotation.roll
                --             |> Cylinder3d.rotateAround Axis3d.z attr.rotation.yaw
                --         )
                --     ]
                -- Spheres will be spherical.
                (Box _, Sphere shape, Primitive) ->
                    [ Shape.sphere 
                        shape
                    ]
                -- Cylinders are cylindrical. May behave strangely with certain custom meshes.
                (Box _, Cylinder shape, Primitive) ->
                    [ Shape.cylinder 12
                        shape
                    ]
                (Box _, Cylinder shape, TexturedCylinder _ _) ->
                    [ Shape.cylinder 12
                        shape
                    ]
                -- Custom objects can use their mesh as the shape, though strange behaviour
                -- may occur if it isn't convex and watertight.
                (Box _, _, CustomObject mesh _) ->
                    [ mesh
                        |> TriangularMesh.mapVertices ( \ vertex -> vertex.position )
                        |> Shape.unsafeConvex
                    ]
                -- All other single colliders will be represented as a block.
                (Box _, _, _) ->
                    [ Shape.block (getCollider object) ]
                -- Non-colliding objects will retain their lack of collisions.
                (None, _, _) ->
                    []

{-| Turns an object into a physics body and assigns it the given identifier -}
usePhysics : String -> Object BodyCoordinates -> Body PhysicsData
usePhysics name object =
    let
        bodyData =
            { object = object
            , name = name
            , tags = Set.empty
            , mass = Mass.kilograms 0
            , linkedBodies = []
            , originalPos = getPosition object
            , constantForces = []
            , netForce = { force = Force.newtons 0, direction = Direction3d.x }
            , visualizers = False
            , initialized = False
            , forces = []
            }
    in
        case object of
            ObjectGroup attr ->
                Body.compound
                    (objToShapes object)
                    bodyData
            Object attr ->
                case attr.boundingBox of
                    Box _ ->
                        case (attr.approxShape, attr.customMesh) of
                            -- Spheres will be spherical.
                            (Sphere shape, Primitive) ->
                                Body.sphere
                                    shape
                                    bodyData
                            -- Cylinders are cylindrical. May behave strangely with certain custom meshes.
                            (Cylinder shape, Primitive) ->
                                Body.cylinder
                                    shape
                                    bodyData
                            (Cylinder shape, TexturedCylinder _ _) ->
                                Body.cylinder
                                    shape
                                    bodyData
                            -- Custom objects can use their mesh as the shape, though strange behaviour
                            -- may occur if it isn't convex and watertight.
                            (_, CustomObject mesh _) ->
                                Body.compound
                                    [ mesh
                                        |> TriangularMesh.mapVertices ( \ vertex -> vertex.position )
                                        |> Shape.unsafeConvex
                                    ]
                                    bodyData
                            -- All other single colliders will be represented as a block.
                            (_, _) ->
                                Body.block
                                    (getCollider object)
                                    bodyData
                    None ->
                        Body.particle
                            bodyData

{-| Add an object to the body that does not collide. -}
addFlair : Object BodyCoordinates -> Body PhysicsData -> Body PhysicsData
addFlair obj body =
    let
        data = Body.data body
    in
        body
            |> Body.withData 
                { data 
                | object =
                    group3D
                        [ data.object
                        , obj
                        ]
                }

{-| Replace a body's tags with the given set. -}
setTags : Set String -> Body PhysicsData -> Body PhysicsData
setTags newTags body =
    let
        data = Body.data body
    in
        body |> Body.withData { data | tags = newTags }

{-| Add a tag to a given body. -}
addTag : String -> Body PhysicsData -> Body PhysicsData
addTag tag body =
    let
        data = Body.data body
    in
        body
            |> Body.withData
                { data 
                | tags = data.tags 
                    |> Set.insert tag
                }

{-| Remove a tag from a given body. -}
removeTag : String -> Body PhysicsData -> Body PhysicsData
removeTag tag body =
    let
        data = Body.data body
    in
        body
            |> Body.withData
                { data 
                | tags = data.tags 
                    |> Set.remove tag
                }

-- The following three functions probably shouldn't be used
{-| Wrapper for setting start point of physics bodies -}
startAtPos : (Length, Length, Length) -> Body PhysicsData -> Body PhysicsData
startAtPos (x, y, z) body =
    let
        data = Body.data body
    in
        if not data.initialized then
            body
            |> Body.moveTo (Point3d.xyz x y z)
        else
            body

{-| Wrapper for translating physics bodies -}
moveBody : (Length, Length, Length) -> Body PhysicsData -> Body PhysicsData
moveBody (x, y, z) body =
    let
        data = Body.data body
    in
        body
        |> Body.translateBy (Vector3d.xyz x y z)
        -- |> Body.withData
        --     { data 
        --     | object = 
        --         data.object 
        --         |> move3D (Length.inCentimeters x, Length.inCentimeters y, Length.inCentimeters z)
        --     }

{-| Wrapper for translating physics bodies -}
moveBodyTo : Point3d Meters WorldCoordinates -> Body PhysicsData -> Body PhysicsData
moveBodyTo pt body =
    body
    |> Body.moveTo pt

{-| Wrapper for rotating bodies around the aircraft principal axes.
Order is `pitch`, `yaw`, then `roll`. -}
rotateBody : Angle -> Angle -> Angle -> Body PhysicsData -> Body PhysicsData
rotateBody pitch yaw roll body =
    let
        data = Body.data body
    in
        body
        |> Body.rotateAround Axis3d.y pitch
        |> Body.rotateAround Axis3d.x roll
        |> Body.rotateAround Axis3d.z yaw

{-| Turns a physics body into a dynamic body and assigns it the given mass, in kilograms -}
withMass : Float -> Body PhysicsData -> Body PhysicsData
withMass mass body =
    withMassU (Mass.kilograms mass) body

{-| Turns a physics body into a dynamic body and assigns it the given mass, in kilograms -}
withMassU : Mass -> Body PhysicsData -> Body PhysicsData
withMassU mass body =
    let
        curData = Body.data body
    in
        body
        |> Body.withBehavior (Body.dynamic mass)
        |> Body.withData
            { curData | mass = mass }

{-| Given density and a physics object, calculate mass and turn it into a dynamic body. -}
autoMass : Density -> Body PhysicsData -> Body PhysicsData
autoMass density body =
    let
        mass =
            getVolume (Body.data body).object |> Quantity.at density
    in
        body
        |> withMass (Mass.inKilograms mass)

{-| Given density and a physics object, calculate mass and turn it into a dynamic body. -}
getMass : Density -> Object coordinates -> Mass
getMass density object =
    getVolume object |> Quantity.at density


{-| Applies damping values to a body -}
inAtmosphere : Body PhysicsData -> Body PhysicsData
inAtmosphere body =
    body 
    |> Body.withDamping
        { linear = 0.25
        , angular = 0.1
        }

{-| Removes damping from a body -}
inSpace : Body PhysicsData -> Body PhysicsData
inSpace body =
    body 
    |> Body.withDamping
        { linear = 0
        , angular = 0
        }

{-| Allows you to set custom linear and angular damping values for a body.
The physics library will automatically clamp these between 0 and 1. -}
customDamping : Float -> Float -> Body PhysicsData -> Body PhysicsData
customDamping lin ang body =
    body 
    |> Body.withDamping
        { linear = lin
        , angular = ang
        }

{-| Signals that `from` should be constrained with `linkName` -}
linkTo : String -> Body PhysicsData -> Body PhysicsData
linkTo linkName from =
    let
        fromAttr = Body.data from
    in
    from 
    |> Body.withData
        { fromAttr
        | linkedBodies = (linkName, Basic) :: fromAttr.linkedBodies
        }

{-| Signals that `from` should be constrained with `linkName`,
via a hinge between two axes. -}
hingeTo :
    String
    -> Axis3d Meters BodyCoordinates
    -> Axis3d Meters BodyCoordinates
    -> Body PhysicsData
    -> Body PhysicsData
hingeTo linkName axis1 axis2 from =
    let
        fromAttr = Body.data from
    in
    from 
    |> Body.withData
        { fromAttr
        | linkedBodies = (linkName, Hinge axis1 axis2) :: fromAttr.linkedBodies
        }

{-| Removes the link between `linkName` and `from` that is stored in `from`.
This does *not* remove any links from the named body.
Your world update function should handle the actual removal of constraints. -}
unlink : String -> Body PhysicsData -> Body PhysicsData
unlink linkName from =
    let
        fromAttr = Body.data from
    in
    from 
    |> Body.withData
        { fromAttr
        | linkedBodies = List.filter ( \ (name, _) -> name /= linkName ) fromAttr.linkedBodies
        }

{-| Adds constraints between `b1` and `b2` if they are linked.
Use with `World.constrain`. -}
constrainBodies : Body PhysicsData -> Body PhysicsData -> List Constraint
constrainBodies b1 b2 =
    let
        b1Data = Body.data b1
        b2Data = Body.data b2
        relevantLinks =
            b2Data.linkedBodies
                |> List.filter (\ (name, _) -> name == b1Data.name)
        -- Not checking name validity since we already know that from relevantLinks
        createConstraint linkData =
            case linkData of
                (_, Basic) ->
                    Constraint.lock Frame3d.atOrigin Frame3d.atOrigin
                (_, Hinge a1 a2) ->
                    Constraint.hinge a1 a2
    in
        List.map createConstraint relevantLinks

{- Functions relating to movement and forces -}

{-| Signals that a body should have a particular force applied to it, in some relative direction,
at a point relative to the centre of the body. -}
push : Force -> Direction3d BodyCoordinates -> Point3d Meters BodyCoordinates -> Body PhysicsData -> Body PhysicsData
push force direction location body =
    let
        data = Body.data body
    in
        body 
        |> Body.withData
            { data 
            | constantForces = { force = force, direction = direction, relativePosition = location } :: data.constantForces
            }

{-| Applies a force to a body. Use with `World.update`. -}
applyForces : Body PhysicsData -> Body PhysicsData
applyForces body =
    let
        frame =
            Body.frame body

        data =
            Body.data body

        forces = data.constantForces ++ data.forces

       --applyOneForce : Body PhysicsData -> Body PhysicsData
        applyOneForce forceData b =
            let
                force = forceData.force
                
                bFrame = Body.frame b

                bData =
                    Body.data b

                direction =
                    forceData.direction
                    |> Direction3d.placeIn bFrame

                adjustedOrigin =
                    Point3d.origin
                    |> Point3d.placeIn frame

                pos =
                    forceData.relativePosition
                    |> Point3d.placeIn frame
                    |> Point3d.translateBy (Vector3d.from adjustedOrigin (adjustedPosition body))
            in
                b
                |> Body.applyForce force direction pos
                |> Body.withData { bData | netForce =
                                    { force = force
                                    , direction = forceData.direction
                                    }
                                  }

    in
        List.foldl applyOneForce body forces

{- Pipe bodies into this to enable visualizers -}
showVisualizers : Body PhysicsData -> Body PhysicsData
showVisualizers body =
    let
        data =
            Body.data body

    in
        body |> Body.withData { data | visualizers = True }

{- Update Functions -}

{-| Applies a force to the named body if the given condition is met. Use with `World.update`. -}
pushIf : String -> (Body PhysicsData -> Bool) -> Force -> Direction3d BodyCoordinates -> Point3d Meters BodyCoordinates -> Body PhysicsData -> Body PhysicsData
pushIf bodyName condition force relativeDir relativePos body =
    if condition body && (Body.data body).name == bodyName then
        let
            data =
                Body.data body

            newForce = Vector3d.withLength force relativeDir
        in
            body
            |> Body.withData
                { data
                | netForce = { force = Vector3d.length newForce, direction = Maybe.withDefault Direction3d.x (Vector3d.direction newForce) }
                , forces = { force = force, relativePosition = relativePos, direction = relativeDir } :: data.forces
                }
    else
        body

{-| Applies the given update function to a body if the condition is met. 
Use with `World.update`. -}
updateIf : (Body PhysicsData -> Bool) -> (Body PhysicsData -> Body PhysicsData) -> Body PhysicsData -> Body PhysicsData
updateIf condition updateFunction body =
    if condition body then
        body |> updateFunction
    else
        body

{-| A predicate that returns True if the given body has the given tag. -}
hasTag : String -> Body PhysicsData -> Bool
hasTag tag body =
    let
        data = Body.data body
    in
        Set.member tag data.tags

{-| Removes links both ways between the bodies associated with the given names.
It is recommended to use this with `updateIf` -}
removeLinks : String -> String -> Body PhysicsData -> Body PhysicsData
removeLinks name1 name2 body =
    let
        data = Body.data body
    in
        if data.name == name1 || data.name == name2 then
            body
            |> Body.withData
                { data
                | linkedBodies =
                    List.filter ( \ (name, _) -> name /= name1 && name /= name2 ) data.linkedBodies
                }
        else
            body
