{- 3D Spaceship Slot for MOR. Modified from the 3D physics slot.
 -}

module SpaceshipScene exposing (main)

import Angle exposing (Angle)
import Array exposing (Array)
import Axis3d
import Block3d exposing (Block3d)
import Browser
import Browser.Dom
import Browser.Events
import BoundingBox3d exposing (BoundingBox3d)
import Camera3d exposing (Camera3d)
import Color exposing (Color)
import Density exposing (Density)
import Dict exposing (Dict)
import Direction3d exposing (Direction3d)
import Duration exposing (Duration)
import Frame3d
import Mass
import Force
import Html exposing (Html)
import Html.Attributes as Attributes exposing (style)
import Illuminance
import Json.Decode as Decode exposing (Decoder)
import Length exposing (Length, Meters)
import LuminousFlux exposing (LuminousFlux)
import Pixels exposing (Pixels)
import Point3d exposing (Point3d)
import Point3d.Projection as Projection
import Quantity exposing (Quantity)
import Round
import Scene3d exposing (Entity)
import Scene3d.Light as Light exposing (Chromaticity, Light)
import Scene3d.Material as Material exposing (Material)
import Scene3d.Mesh as Mesh
import SketchPlane3d
import SolidAngle
import SpaceshipPart exposing (Action(..), Key(..), KeyAction(..), Part, Spaceship, addFuelTank, addStabilizer, antaresEngine, balancedEngine, customPart, efficientEngine, heavyEngine, initSpaceshipWorld, merlin, movePart, paintPart, partGroup, rotatePart, saturnV, shuttleEngine, texturePart, vulcainTwo)
import Sphere3d
import Speed
import Task
import Temperature
import Vector3d exposing (Vector3d)
import Viewpoint3d
import Cone3d
import Point2d
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
import Collision exposing (..)
import Wrapper3D exposing (..)
import PhysicsWrapper3D exposing (..)
import Dict exposing (Dict)

import GraphicSVG.Widget as Widget
import GraphicSVG exposing(..)

import GSVGSkybox as GS
import GSVGTexture as GT exposing (svgTexture)
import TextureLoader as TL exposing (getColorTexture, getMetallicityTexture, getRoughnessTexture, loadTexture, TextureType(..))

import Physics.World as World exposing (World)
import Physics.Body as Body exposing (Body)
import Physics.Coordinates exposing (WorldCoordinates, BodyCoordinates)
import Physics.Constraint as Constraint exposing (Constraint)
import Physics.Shape as Shape
import Block3d
import List
import Acceleration exposing (Acceleration)

{-header<b>3D Physics Slot!</b>: Make things in 3D that move! -}


{-editable-}
-- Create your spaceship!
-- SmallModel includes textures and meshes
mySpaceship : SmallModel -> Spaceship
mySpaceship model =
    {
        commandModule = group3D
            [
                cone 400 600 |> matte Color.gray
                        |> move3D (0, 0 , 2000)
            ]
    ,   parts = [                                      -- a list of parts in your spaceship
                    saturnV "thruster1a"
                        |> rotatePart (Angle.degrees 0) (Angle.degrees 0) (Angle.degrees 0)
                        |> movePart (Length.meters -2, Length.meters -2, Length.meters -7.9)
                ,   saturnV "thruster1b"
                        |> rotatePart (Angle.degrees 0) (Angle.degrees 0) (Angle.degrees 0)
                        |> movePart (Length.meters 2, Length.meters -2, Length.meters -7.9)
                ,   saturnV "thruster1c"
                        |> rotatePart (Angle.degrees 0) (Angle.degrees 0) (Angle.degrees 0)
                        |> movePart (Length.meters -2, Length.meters 2, Length.meters -7.9)
                ,   saturnV "thruster1d"
                        |> rotatePart (Angle.degrees 0) (Angle.degrees 0) (Angle.degrees 0)
                        |> movePart (Length.meters 2, Length.meters 2, Length.meters -7.9)
                ,   merlin "thruster2"
                        |> rotatePart (Angle.degrees 90) (Angle.degrees 0) (Angle.degrees 0)
                        |> movePart (Length.meters -6, Quantity.zero, Length.meters 15)
                ,   merlin "thruster3"
                        |> rotatePart (Angle.degrees -90) (Angle.degrees 0) (Angle.degrees 0)
                        |> movePart (Length.meters 6, Quantity.zero, Length.meters 15)
                ,   merlin "thruster4"
                        |> rotatePart (Angle.degrees -90) (Angle.degrees -90) (Angle.degrees 0)
                        |> movePart (Length.meters 0, Length.meters -6, Length.meters 15)
                ,   merlin "thruster5"
                        |> rotatePart (Angle.degrees -90) (Angle.degrees 90) (Angle.degrees 0)
                        |> movePart (Length.meters 0, Length.meters 6, Length.meters 15)
                ,   addFuelTank "fueltank1" (Length.meters 4) (Length.meters 25) 100
                        |> movePart (Length.meters 0, Length.meters 0, Length.meters -5)
                ,   addStabilizer "stabilizer" (Length.meters 4) (Length.meters 0.5)
                        |> movePart (Quantity.zero, Quantity.zero, Length.meters 20)
                ]
    ,   keyActions = \ key keyAction ->
            case (key, keyAction) of
                (Spacebar, Pressed) ->
                    ManyActions
                        [
                            EnableThruster "thruster1a" 100
                        ,   EnableThruster "thruster1b" 100
                        ,   EnableThruster "thruster1c" 100
                        ,   EnableThruster "thruster1d" 100
                        ]
                (Spacebar, Released) ->
                    ManyActions
                        [
                            DisableThruster "thruster1a"
                        ,   DisableThruster "thruster1b"
                        ,   DisableThruster "thruster1c"
                        ,   DisableThruster "thruster1d"
                        ,   EnableStabilizer "stabilizer"
                        ]
                (Key "w", Pressed) -> EnableThruster "thruster4" 100
                (Key "s", Pressed) -> EnableThruster "thruster5" 100
                (Key "w", Released) -> DisableThruster "thruster4"
                (Key "s", Released) -> DisableThruster "thruster5"

                (Key "a", Pressed) -> EnableThruster "thruster2" 100
                (Key "d", Pressed) -> EnableThruster "thruster3" 100
                (Key "a", Released) -> DisableThruster "thruster2"
                (Key "d", Released) -> DisableThruster "thruster3"
                _ -> DoNothing
                 -- a list of keys and actions they do
    }

-- You can change the colour of your UI
colourUI =
    GraphicSVG.hsl (degrees 150) 1 0.5
colourUIText =
    GraphicSVG.white

-- Set this to true if you want the spaceship to fly according to the flight plan below
-- Or false if you want to just fly it yourself
useAutoPilot = False

-- You can use this to program how your spaceship flies!
autoPilotRoutine : Float -> Action
autoPilotRoutine =
    animationPieces
        [ ( 5.0 -- This will fire your forward thrusters for 5 seconds. Try changing it!
          , \ time -> 
              ManyActions
                  [ EnableThruster "thruster1a" 100
                  , EnableThruster "thruster1b" 100
                  , EnableThruster "thruster1c" 100
                  , EnableThruster "thruster1d" 100
                  ]
          )
        , ( 0.1 -- This will turn off those thrusters
          , \ time -> 
              ManyActions
                  [ DisableThruster "thruster1a"
                  , DisableThruster "thruster1b"
                  , DisableThruster "thruster1c"
                  , DisableThruster "thruster1d"
                  ]
          )
        ]
        ( \ _ -> 
              DoNothing
        )

-- Create your own space station!!
-- space station is 350 km above the Earth
spaceStationSize = (Length.inCentimeters <| Length.meters 20)

spaceStation model =
    group3D
        [
            box 20 200 15
            |> matte (Color.rgb255 232 226 209)
            , rectangle3D 100 50
            |> textured (getColorTexture "solarcell" model) (constantTexture 0.5) (constantTexture 0.5)
            |> rotateY3D (degrees 90)
            |> move3D (0,100,0)
            , rectangle3D 100 50
            |> textured (getColorTexture "solarcell" model) (constantTexture 0.5) (constantTexture 0.5)
            |> rotateY3D (degrees 90)
            |> move3D (0,-100,0)
        ]


-- Use "loadTexture [name] [url]" to load in texture images from the Internet!
-- Give each one a unique name.
-- You can list many of them!
myTextures = 
    [ loadTexture "earth" TexColor "https://raw.githubusercontent.com/CSchank/CSchank.github.io/master/img/Earth.png"
    , loadTexture "dec" TexColor "https://sharyuwu.github.io/image/dec.png"
    , loadTexture "decBroken1" TexColor "https://sharyuwu.github.io/image/decBroken1.png"
    ]

-- Usage: `svgTexture "name" "name`, where shape is any 2D shape or group
-- Give each one a unique name.
-- You can list many of them!
svgTextures =
    [ svgTexture "squares" TexColor squares
    , svgTexture "solarcell" TexColor solarCellsvg
    ]

-- SVG textures are 50 by 50
squares =
    group
        [ square 25 |> filled purple |> move (12.5,12.5)
        , square 25 |> filled orange |> move (-12.5,12.5)
        , square 25 |> filled purple |> move (-12.5,-12.5)
        , square 25 |> filled orange |> move (12.5,-12.5)
        ]

-- Put your 2D shapes here, and they will be overlayed on top of the screen!
overlay : Model -> List (Shape Msg)
overlay model =
    [ angleDisplay model
    , spaceshipDebug "spaceship" model 30 colourUI colourUIText
    , spaceGameUI model
    , cameraUI model
    ]

-- Here you can specify what images to use to create the skybox. Just replace "todo" with a link to an image. (Keep the quotes, though!)
skyboxType =
    --Skybox.URLSkybox textureBottom textureTop textureSide1 textureSide2 textureSide3 textureSide4
    -- Some other options (comment in the one above and comment one of these out)
    -- Skybox.GSVGSkybox False skyboxTop skyboxSides skyBoxBottom
    -- Skybox.GSVGSphericalSkybox False skyboxTop
    Skybox.URLSphericalSkybox "https://cschank.github.io/img/milky.jpg" 0

-- this is 50 by 50
skyboxTop : Shape msg
skyboxTop =
    group
        [
            square 50 |> filled lightBlue
        ,   circle 10 |> filled yellow
        ]

-- this is 200 by 50
skyboxSides : Shape msg
skyboxSides =
    group
        [
            rect 200 50 |> filled lightBlue |> move (0,25)
        ,   rect 200 50 |> filled green |> move(0,-25)
        ,   triangle 10 |> filled darkGreen |> rotate (degrees -30) |> move (0,5)
        ,   text "abcdefghijklmnopqrstuvwxyz" |> centered |> size 16 |> filled red
        ]

-- this is 50 by 50
skyBoxBottom : Shape msg
skyBoxBottom =
    group
        [
        ]


textureBottom : String
textureBottom =
    "todo"

textureTop : String
textureTop =
    "todo"

textureSide1 : String
textureSide1 =
    "todo"

textureSide2 : String
textureSide2 =
    "todo"

textureSide3 : String
textureSide3 =
    "todo"

textureSide4 : String
textureSide4 =
    "todo"


{-endeditable-}






{-extra-}
type alias SmallModel =
    { meshStore : MeshStore BodyCoordinates, textureLoader : TL.Model }

-- Time in seconds for fixing the satellite
levelAdjust = 10

internalSpaceStation model =
    (if model.showDetailedSpaceStation then spaceStation model else cube 1 |> matte Color.black)
        |> scaleTo3D spaceStationSize Direction3d.x
        |> rotateY3D (degrees 90)
        |> spaceStationOrbit model

spaceGameUI : Model -> Shape Msg
spaceGameUI model =
    let
        showCaution =
            if earthToShip model |> Quantity.lessThan (Length.kilometers 125) then
                0.6 + 0.4 * sin (5 * model.time - 1.5)
            else 0
        showGameOver =
            model.gameState == OutOfFuel || model.gameState == Crashed
        fixprocess =
            case model.gameState of
                RepairInProgress time ->
                    (model.time - time) / toFloat levelAdjust
                _ -> 0

        spaceshipPosition =
            model.spaceshipState.world
            |> getBodyByName "spaceship"
            |> Maybe.map adjustedPosition
            |> Maybe.withDefault Point3d.origin

        satellitePosition =
            getPosition (satelliteModel model)
        spaceStationPosition =
            getPosition (internalSpaceStation model)

        distanceToSatellite =
            Length.inKilometers <| Vector3d.length <| Vector3d.from spaceshipPosition satellitePosition
        distanceToSpaceStation =
            Length.inKilometers <| Vector3d.length <| Vector3d.from spaceshipPosition spaceStationPosition
        objectiveText =
            case model.gameState of
                RepairNotStarted ->
                    "Get to the satellite and remain nearby for " ++ String.fromFloat levelAdjust ++ " seconds to repair it!"
                RepairInProgress _ ->
                    "Stay near the satellite to repair it!"
                Repaired ->
                    "Return to the space station to complete your mission!"
                Successful t ->
                    "Mission completed at T+"++Round.round 3 t++"s"
                _ ->
                    ""
        crosshairs =
            case model.cameraState of
                -- Show velocity
                Orbiting ->
                    let
                        velocityPos =
                            spaceshipPosition
                            |> Point3d.translateBy
                                ( model.spaceshipState.world
                                    |> getBodyByName "spaceship"
                                    |> Maybe.map Body.velocity
                                    |> Maybe.withDefault Vector3d.zero
                                    |> Vector3d.toRecord Speed.inMetersPerSecond
                                    |> Vector3d.fromMeters
                                )
                        
                        distance = Projection.depth model.camera velocityPos
                    in
                        if distance |> Quantity.greaterThan Quantity.zero then
                            group
                                [ circle 20
                                  |> outlined (solid 0.5) colourUI
                                , circle 2.5
                                  |> filled colourUI
                                , rectangle 20 1
                                  |> filled colourUI
                                  |> move (-20, -0.5)
                                , rectangle 20 1
                                  |> filled colourUI
                                  |> move (20, -0.5)
                                , rectangle 1 20
                                  |> filled colourUI
                                  |> move (-0.5, -20)
                                , rectangle 1 20
                                  |> filled colourUI
                                  |> move (-0.5, 20)
                                , text "VELOCITY"
                                  |> size 16
                                  |> customFont "Audiowide"
                                  |> centered
                                  |> filled colourUIText
                                  |> move (0, -45)
                                ]
                            |> move (positionOnScreen model.camera (unwrapQ model.width, unwrapQ model.height) velocityPos)
                        else
                            group []
                -- Show crosshairs in the middle of the screen (Where they're pointing)
                SpaceshipForwards ->
                    group
                        [ circle 20
                          |> outlined (solid 0.5) colourUI
                        , circle 2.5
                          |> filled colourUI
                        , rectangle 20 1
                          |> filled colourUI
                          |> move (-20, -0.5)
                        , rectangle 20 1
                          |> filled colourUI
                          |> move (20, -0.5)
                        , rectangle 1 20
                          |> filled colourUI
                          |> move (-0.5, -20)
                        , rectangle 1 20
                          |> filled colourUI
                          |> move (-0.5, 20)
                        ]
    in
        group
            [ cautionSign model (earthToShip model) |> makeTransparent showCaution
            --, satelliteSign model (round <| satelliteToShip model) |> makeTransparent (0.6 + 0.4 * sin (5 * model.time - 1.5))
            , case model.gameState of
                RepairInProgress time ->
                        repair model (model.time - time) levelAdjust
                _ -> group []
            , if showGameOver then missionFailSign model |> scale 3 else group []
            , move (-60, 0) <| GraphicSVG.map SpaceshipMsg <| SpaceshipPart.overlay model.width model.height colourUI model.spaceshipState
            , crosshairs
            , text
                ( "Mission Time: T+" ++ Round.round 3 model.gameTime ++ "s" )
              |> centered
              |> customFont "Audiowide"
              |> size 24
              |> filled colourUIText
              |> move ( 0, toFloat (unwrapQ model.height) / 2 - 50 )
            , text objectiveText
              |> centered
              |> customFont "Audiowide"
              |> size 20
              |> filled colourUIText
              |> move ( 0, toFloat (unwrapQ model.height) / 2 - 75 )
            , tracker
                model.camera
                model
                (getPosition <| satelliteModel model)
                20
                colourUI
                colourUI
                ("satellite (" ++ Round.round 3 distanceToSatellite ++ "km)")
                False
            , tracker
                model.camera
                model
                (getPosition <| internalSpaceStation model)
                20
                colourUI
                colourUI
                ("space station (" ++ Round.round 3 distanceToSpaceStation ++ "km)")
                False
            ]

cameraUI model =
    group
        [ group
              [ roundedRect 40 40 10
                |> filled colourUI
              , text "+"
                |> size 16
                |> centered
                |> customFont "Audiowide"
                |> filled colourUIText
                |> move (0,-2.5)
                |> rotate (degrees 180)
              ]
            |> move (toFloat (unwrapQ model.width) / 2 - 125, toFloat (unwrapQ model.height) / 2 - 150)
            |> notifyTap (ChangeZoom <| Length.meters -10)
        , group
              [ roundedRect 40 40 10
                |> filled colourUI
              , text "-"
                |> size 16
                |> centered
                |> customFont "Audiowide"
                |> filled colourUIText
                |> move (0,-2.5)
                |> rotate (degrees 180)
              ]
            |> move (toFloat (unwrapQ model.width) / 2 - 125, toFloat (unwrapQ model.height) / 2 - 200)
            |> notifyTap (ChangeZoom <| Length.meters 10)
        , group
            [ roundedRect 140 60 10
              |> filled colourUI
            , text "Cycle Camera"
              |> size 16
              |> centered
              |> customFont "Audiowide"
              |> filled colourUIText
              |> move (0, -5)
            ]
          |> move (toFloat (unwrapQ model.width) / 2 - 125, toFloat (unwrapQ model.height) / 2 - 260)
          |> notifyTap CycleCameraState
        ]

spaceshipDebug objToTrack model trackerSize trackerColour textColour =
    let
        toTrack = getBodyByName objToTrack model.spaceshipState.world
        trackingName =
            case toTrack of
                Just body ->
                    (Body.data body).name
                Nothing ->
                    "Nothing"
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
        theTracker = bodyTrackerOldCamera objToTrack model trackerSize trackerColour textColour True
    in
    group
        [ bodyInfo
        --, theTracker
        ]

spaceStationOrbit model object = 
  let
    spaceStationtoEarth = (Length.inCentimeters <| Length.kilometers 150)
  in
    object
        |> move3D (0, Length.inCentimeters <| Length.meters 300, 0)
     -- |> move3D (0,(Length.inCentimeters <| Length.kilometers -100),0)
     -- |> move3D (0, 0, earthSize)
     -- |> move3D (0, 0, spaceStationtoEarth)
    --  |> move3D (0, (Length.inCentimeters <| Length.meters ((cos model.time) * 10)), (Length.inCentimeters <| Length.meters (-(sin model.time) * 10)))

-- Earth has a radius of 3,959 miles
-- A distance from earth to satellite is 36,000 km
earthSize = (Length.inCentimeters <| Length.kilometers 100)
satelliteSize = (Length.inCentimeters <| Length.meters 20)

spaceGameAssets : Model -> List (Object WorldCoordinates)
spaceGameAssets model =
        [ earthModel model
          |> move3D (0,Length.inCentimeters <| Length.kilometers 100,0)
        , satelliteModel model
        ]

earthModel model =    
    group3D
        [ sphere earthSize
            |> textured (getColorTexture "earth" model) (constantTexture 0) (constantTexture 0)
            |> rotateZ3D (degrees model.time)
            |> move3D (0, 0, earthSize)
        ]

satelliteModel : Model -> Object WorldCoordinates
satelliteModel model = 
    let
        earthToSatellite = (Length.inCentimeters <| Length.kilometers 100)

        repaired =
            case model.gameState of
                Repaired -> True
                Successful _ -> True
                _ -> False
    in
     group3D [
    (if model.showDetailedSatellite then
     satellite model repaired else cube 1 |> matte Color.black)
            |> rotateY3D (degrees 180)
            |> scaleTo3D satelliteSize Direction3d.y
            |> move3D (0,Length.inCentimeters <| Length.kilometers 150,0)
            |> move3D (0, -earthToSatellite, 0)
            |> move3D (0, -earthToSatellite, earthSize/2)
    ]

-- Satellite Code
satellite model ifBreaken =  
    let
        dec = if ifBreaken then "decBroken1"
             else "dec"
    in group3D
        [ -- Making an ellipsoid requires you to specify the length, width, and height. You also need to pass in the model.
        cylinder 20 15
          |> matte (Color.rgb255 222 218 211)
          |> move3D (0, 0, 7.5)
        ,cylinder 25 20 
          |> matte (Color.rgb255 222 218 211)
          |> move3D (0, 0, 5)
        ,cylinder 25 10
          |> matte (Color.rgb255 222 218 211)
          |> move3D (0, 0, 5)
          |> move3D (0,0,-20)
        ,cylinder 30 25
          |> matte (Color.rgb255 222 218 211)
          |> move3D (0, 0, 12.5)
          |> move3D (0,0,-30)
        ,cylinder 25 10
          |> matte (Color.rgb255 222 218 211)
          |> move3D (0, 0, 5)
          |> move3D (0,0,-40)
        ,cylinder 26 20
          |> matte (Color.rgb255 222 218 211)
          |> move3D (0, 0, 10)
          |> rotateZ3D (degrees 180)
          |> move3D (0,0,-50) 
        ,cylinder 20 20
          |> matte (Color.rgb255 222 218 211)
          |> move3D (0, 0, 10)
          |> move3D (0,0,-70)
        , wing1 model ifBreaken
        , wing2 model ifBreaken
        ,truncatedCone 10 20 15 model.meshStoreWorld
          |> matte (Color.rgb255 222 218 211)
          |> rotateX3D (degrees 180)
          |> move3D (0,0,15)
          |> scale3D 2
        ,link
          |> rotateX3D (degrees 90)
          |> move3D (15,0,-16.5)
          |> scale3D 0.9
        ,link
          |> rotateX3D (degrees 90)
          |> move3D (15,0,16.5)
          |> scale3D -0.9
        , cube 5
          |> matte (Color.rgb255 222 218 211)
          |> move3D (0,0,50)
        , linkDish
        , linkDish
          |> rotateZ3D (degrees 90) 
        ]
wing1 model ifBreaken =  
    if ifBreaken then
        group3D [customPolygon brokenWingPoint
            |> textured (getColorTexture "solarcell" model) (constantTexture 1) (constantTexture 1)
            |> rotateZ3D (degrees 90)
            |> move3D (100,-30,-15)
            |>scale3D 0.95
        ]
    else
        rectangle3D 100 50
            |> textured (getColorTexture "solarcell" model) (constantTexture 0.5) (constantTexture 0.5)
            |> move3D (100,0,-15)
wing2 model ifBreaken =  
    if ifBreaken then
        rectangle3D 100 50
            |> textured (getColorTexture "solarcell" model) (constantTexture 1) (constantTexture 1)
            |> move3D (-100,0,-15)
    else
        rectangle3D 100 50
            |> textured (getColorTexture "solarcell" model) (constantTexture 0.5) (constantTexture 0.5)
            |> move3D (-100,0,-15)
brokenWingPoint = 
   [(2.1512,46.789),(58.442,47.148),(59.159,3.0476),(53.422,-5.557),(50.196,0.8963),(45.893,-5.198),(41.232,-9.859),(34.778,-3.764),(34.420,-10.93),(25.815,-7.708),(20.078,3.4061),(14.341,-13.44),(11.831,-15.59),(6.8123,-10.93),(5.0196,-16.31),(0.7170,-12.01),(2.1512,46.789)]

linkDish = group3D [
        cylinder 0.5 45
          |> matte (Color.rgb255 222 218 211)
          |> move3D (0, 0, 22.5)
          |> rotateY3D (degrees 180)
          |> rotateY3D (degrees -45)
          |> move3D (0,0,50)
        , cylinder 0.5 45
          |> matte (Color.rgb255 222 218 211)
          |> move3D (0, 0, 22.5)
          |> rotateY3D (degrees -180)
          |> rotateY3D (degrees 45)
          |> move3D (0,0,50)
  ]
link = group3D [
        cylinder 0.5 40
          |> matte (Color.rgb255 222 218 211)
          |> move3D (0, 0, 20)
          |> rotateY3D (degrees 90)
        ,cylinder 0.5 50
          |> matte (Color.rgb255 222 218 211)
          |> move3D (0, 0, 25)
          |> rotateY3D (degrees 55)
        ,cylinder 0.5 50
          |> matte (Color.rgb255 222 218 211)
          |> move3D (0, 0, 25)
          |> rotateY3D (degrees 125)
  ]

-- SVG textures for solar cell
rectW = 13
rectH = 18
scW = 3
scH = 3
sgW = 6
sgH = 4

solarCellsvg = group
  [
    rect (rectW*scW*4) (rectH*scH*4)
      |> filled white
    ,solarGroup sgW sgH
      |> move (-(rectW*scW)*sgW/3.2 , -(rectH*scH)*sgH/3)
  ]

solarGroup a b = group
  <| List.map (\x -> solarcell scW scH |> move x |>scale 0.3) (layoutWH a b (rectW*scW*2) (rectH*scH*2))


solarcell a b = group
  <| List.map (\x -> solarelm |> move x) (layoutWH a b (rectW*2) (rectH*2))


solarelm = group [
    rect 10 15
     |> filled (rgb 50 76 168)
    ,rect 10 15
     |> filled (rgb 50 76 168)
     |> move (rectW, 0)
    ,rect 10 15
     |> filled (rgb 50 76 168)
     |> move (0, rectH)
    ,rect 10 15
     |> filled (rgb 50 76 168)
     |> move (rectW, rectH)
   ]
   
layout15 a width = List.map (\x -> width * toFloat(x)) (List.range 0 (a-1))

layoutWH a b width height = List.concat 
                  <| List.map (\x -> List.map (\y -> ( y, height * toFloat (x))) (layout15 a width)
                        ) (List.range 0 (b-1))


repair model process total =
  group [
    rect (10 * process )  30
      |> filled orange
      |> move (-(10 * total)/2+5 + (5 * (process-1)), 0)
    , rect (10 * total ) 30
      |> outlined (solid 1) orange
    , text "REPAIRING"
      |> customFont "Audiowide"
      |> size 11
      |> centered
      |> filled black
      |> move (0, -5)
  ]


-- Caution sign. Show this sign when the rocket is too close to earth
cautionSign model distance =
    let
        earthSizeOffsetKM =
            Length.centimeters earthSize
            |> Length.inKilometers
    in
        group
            [ rect 200 30
              |> filled (rgb 252 198 3)
            , rect 190 20
              |> filled (rgb 255 207 31)
              |> makeTransparent 1
            , triangle 8
              |> filled black
              |> rotate (degrees 90)
              |> move (-85, -1)
            , text "!"
              |> customFont "Audiowide"
              |> size 11
              |> centered
              |> filled yellow
              |> move (-85, -4)
            , text ( "CAUTION " ++ Round.round 3 ((distance |> Length.inKilometers) - earthSizeOffsetKM) ++ " KM" )
              |> customFont "Audiowide"
              |> size 13
              |> centered
              |> filled black
              |> move (10, -5)
            ]

-- Mission fail sign. Show this sign when the rocket bump into earth
-- or satellite

missionFailSign model =
  group [
    rect 100 30
      |> filled white
    , rect 100 30
      |> outlined (solid 3) red
    , text "MISSION FAILED"
      |> customFont "Audiowide"
      |> size 11
      |> centered
      |> filled red
      |> move (0, -5)
  ] |> rotate (degrees 15)

satelliteSign model distance = group
  [
    rect 120 30
      |> filled (rgb 96 65 181)
    , rect 110 20
      |> filled (rgb 173 143 255)
      |> makeTransparent 0.5
    , text ("REPAIR" ++ Round.round 2 distance ++ " m")
      |> customFont "Audiowide"
      |> size 14
      |> centered
      |> filled white
      |> move (-25, -5)
  ]

-- calculate the distance from earth to ship
earthToShip model =
    let
        spaceshipPosition =
            model.spaceshipState.world
            |> getBodyByName "spaceship"
            |> Maybe.map adjustedPosition
            |> Maybe.withDefault Point3d.origin

        earthPosition = getPosition <| (earthModel model |> move3D (0, Length.inCentimeters <| Length.kilometers 100, 0))
        distance =
            earthPosition
            |> Point3d.distanceFrom spaceshipPosition
    in
        distance

-- calculate the distance from satellite to ship
satelliteToShip model =
  let

    spaceshipPosition =
        model.spaceshipState.world
            |> getBodyByName "spaceship"
            |> Maybe.map adjustedPosition
            |> Maybe.withDefault Point3d.origin

    satellitePosition = getPosition <| satelliteModel model
    distanceToSpaceStation = Length.inMeters <| Vector3d.length <| Vector3d.from spaceshipPosition satellitePosition
  in
    distanceToSpaceStation

debugMeshNames model = text (Debug.toString (List.map (\(key,_) -> key) (Dict.toList model.meshStore.generatedMeshes))) |> filled black

type CameraState
    = Orbiting
    | SpaceshipForwards

type alias Model =
    { width : Quantity Int Pixels
    , height : Quantity Int Pixels
    , time : Float
    , gameTime : Float
    , orbiting : Bool
    , azimuth : Angle
    , elevation : Angle
    , cameraPos : Point3d Meters WorldCoordinates
    , camera : Camera3d Meters WorldCoordinates
    , cameraState : CameraState
    , meshStore : MeshStore BodyCoordinates
    , meshStoreWorld : MeshStore WorldCoordinates
    , widget : Widget.Model
    , gSkyboxModel : GS.Model
    , gTextureModel : GT.Model
    , textureLoader : TL.Model
    , world : World PhysicsData
    , state : State
    , gameState : GameState
    , spaceshipState : SpaceshipPart.Model
    , cameraZoom : Length
    , showDetailedSatellite : Bool
    , showDetailedSpaceStation : Bool
    }

type RepairTime
   = Started Float 
   | NotStarted

type State
    = Loaded
    | Loading

type GameState
    = RepairNotStarted
    | RepairInProgress Float {-starting time-}
    | Crashed
    | OutOfFuel
    | Repaired
    | Successful Float

type Msg
    = Resize (Quantity Int Pixels) (Quantity Int Pixels)
    | Tick Duration
    | MouseDown
    | MouseMove (Quantity Float Pixels) (Quantity Float Pixels)
    | MouseUp
    | VisibilityChange Browser.Events.Visibility
    | GenerateMeshes (List (Object WorldCoordinates)) CoordType
    | WidgetMsg Widget.Msg
    | Reset
    | MoveCamera (Direction3d WorldCoordinates) Length
    | SkyboxMsg GS.Msg
    | GSVGTextureMsg GT.Msg
    | TextureLoadMsg TL.Msg
    | AddBodies
    | SpaceshipMsg SpaceshipPart.Msg
    | ChangeZoom Length
    | CycleCameraState

type CoordType
    = WorldCoords
    | BodyCoords

{-| Create both a Light and an Entity (a bright glowing sphere) representing a
particular point light
-}
pointLight :
    { position : Point3d Meters WorldCoordinates
    , chromaticity : Chromaticity
    , intensity : LuminousFlux
    }
    -> ( Light WorldCoordinates Never, Entity WorldCoordinates )
pointLight properties =
    let
        -- Create a sphere to represent a light bulb
        lightsphere =
            Sphere3d.atPoint properties.position (Length.kilometers 5)

        -- Calculate the luminance of the sphere surface by dividing the given
        -- total luminous flux of the light by the surface area of the sphere
        -- and by the solid angle of a hemisphere (assuming that each point on
        -- the surface of the bulb emits light equally in all directions)...I
        -- am not 100% sure this is exactly correct =)
        sphereLuminance =
            properties.intensity
                |> Quantity.per (SolidAngle.spats 0.5)
                |> Quantity.per (Sphere3d.surfaceArea lightsphere)

        -- Create an emissive (glowing) material for the sphere
        sphereMaterial =
            Material.emissive properties.chromaticity sphereLuminance
    in
    ( Light.point Light.neverCastsShadows properties
    , Scene3d.sphere sphereMaterial lightsphere
    )


view : Model -> Html Msg
view model =
    let
        -- The sun
        (sunLighting, sun) =
            pointLight
                {
                    position = Point3d.fromTuple Length.kilometers (0, -200, 0)
                ,   chromaticity = Light.sunlight
                ,   intensity = LuminousFlux.lumens 200000000000000
                }
        softLighting = Light.soft
                { upDirection = Direction3d.positiveZ
                , chromaticity = Light.fluorescent
                , intensityAbove = Illuminance.lux 15
                , intensityBelow = Illuminance.lux 15
                }

        spaceshipFrame = SpaceshipPart.spaceshipFrame model.spaceshipState

        thrustersOn =
            List.map .position <|
                List.filter (\t -> t.thrustPercent > 0) <| Dict.values model.spaceshipState.spaceshipState.thrusterStates

        thrusterLight : Point3d Meters BodyCoordinates -> Light WorldCoordinates Never
        thrusterLight pt =
            let
                globalPt = pt |> Point3d.placeIn spaceshipFrame
            in
                Tuple.first <| pointLight
                    {
                        position = globalPt
                    ,   chromaticity = Light.color Color.orange
                    ,   intensity = LuminousFlux.lumens 1000000
                    }

        lights =
            case thrustersOn of
                [] -> Scene3d.twoLights sunLighting softLighting
                l1::[] -> Scene3d.threeLights sunLighting (thrusterLight l1) softLighting
                l1::l2::[] -> Scene3d.fourLights sunLighting (thrusterLight l1) (thrusterLight l2) softLighting
                l1::l2::l3::[] -> Scene3d.fiveLights sunLighting (thrusterLight l1) (thrusterLight l2) (thrusterLight l3) softLighting
                l1::l2::l3::l4::[] -> Scene3d.sixLights sunLighting (thrusterLight l1) (thrusterLight l2) (thrusterLight l3) (thrusterLight l4) softLighting
                l1::l2::l3::l4::l5::[] -> Scene3d.sevenLights sunLighting (thrusterLight l1) (thrusterLight l2) (thrusterLight l3) (thrusterLight l4) (thrusterLight l5) softLighting
                l1::l2::l3::l4::l5::l6::rest -> Scene3d.eightLights sunLighting (thrusterLight l1) (thrusterLight l2) (thrusterLight l3) (thrusterLight l4) (thrusterLight l5) (thrusterLight l6) softLighting
                --l1::l2::l3::l4::l5::l6::l7::rest -> Scene3d.eightLights sunLighting (thrusterLight l1) (thrusterLight l2) (thrusterLight l3) (thrusterLight l4) (thrusterLight l5) (thrusterLight l6) (thrusterLight l7)

        textures = model.textureLoader.colorTextures

        baseEntities =
            [ sun
            , case skyboxType of
                Skybox.GSVGSkybox _ _ _ _ _ ->
                    Skybox.skybox 
                        [ Dict.get "skyB" textures
                        , Dict.get "skyT" textures
                        , Dict.get "skyS1" textures
                        , Dict.get "skyS2" textures
                        , Dict.get "skyS3" textures
                        , Dict.get "skyS4" textures
                        ]
                        0
                Skybox.URLSkybox _ _ _ _ _ _ _ ->
                    Skybox.skybox 
                        [ Dict.get "skyB" textures
                        , Dict.get "skyT" textures
                        , Dict.get "skyS1" textures
                        , Dict.get "skyS2" textures
                        , Dict.get "skyS3" textures
                        , Dict.get "skyS4" textures
                        ]
                        0
                Skybox.URLSphericalSkybox _ _ ->
                    Skybox.roundSkybox
                        (Dict.get "skyT" textures)
                        (Length.inCentimeters <| Length.kilometers 1000)
                Skybox.GSVGSphericalSkybox _ _ _ ->
                    Skybox.roundSkybox
                        (Dict.get "skyT" textures)
                        5000
                Skybox.BasicSkybox _ ->
                    Scene3d.nothing
            ]

    in
        case model.state of
            Loaded ->
                Html.div []
                    [   case skyboxType of
                            Skybox.GSVGSkybox debug sT sS sB _ ->
                                Html.div [style "position" "absolute", style "left" "0px", style "top" (String.fromInt (unwrapQ model.height) ++ "px")]
                                [
                                    -- Html.h1 [] [Html.text "Skybox Debug"]
                                Html.map SkyboxMsg <| GS.drawSkybox debug model.gSkyboxModel sT sS sB
                                ]
                            Skybox.GSVGSphericalSkybox debug sT _ ->
                                Html.div [style "position" "absolute", style "left" "0px", style "top" (String.fromInt (unwrapQ model.height) ++ "px")]
                                [
                                    -- Html.h1 [] [Html.text "Skybox Debug"]
                                Html.map SkyboxMsg <| GS.drawSkybox debug model.gSkyboxModel sT (group []) (group [])
                                ]
                            _ -> Html.span [] []
                    ,   Scene3d.custom
                            { lights = lights --Scene3d.oneLight softLighting
                            , camera = model.camera
                            , clipDepth = Length.centimeters 10
                            , exposure = Scene3d.exposureValue 6
                            , toneMapping = Scene3d.hableFilmicToneMapping
                            , whiteBalance = Light.fluorescent
                            , antialiasing = Scene3d.multisampling
                            , dimensions = ( model.width, model.height )
                            , background = Scene3d.backgroundColor Color.lightBlue
                            , entities =  -- baseEntities 
                                           baseEntities ++ renderEntities (renderBodies False model.spaceshipState.world)
                                          ++ renderEntities (spaceGameAssets model)
                                          ++ renderEntities [internalSpaceStation model]
                            }
                            |> withOverlay (overlay model) model
                    ]
            Loading ->
                Html.div []
                    [ Html.text "Loading..."
                    ,  Html.map GSVGTextureMsg <| GT.drawTextures False model.gTextureModel
                    ]

angleDisplay : Model -> Shape Msg
angleDisplay model = group
    [
        text ("azimuth: " ++ String.fromInt (round <| unwrapQ model.azimuth * 180 / pi) ++ "º")
                                |> filled black
                                |> move (toFloat (unwrapQ model.width) / 2 - 160, toFloat (unwrapQ model.height) / 2 - 50)
    ,   text ("elevation: " ++ String.fromInt (round <| unwrapQ model.elevation * 180 / pi) ++ "º")
            |> filled black
            |> move (toFloat (unwrapQ model.width) / 2 - 160, toFloat (unwrapQ model.height) / 2 - 60)
    ]

cameraControls : Model -> Shape Msg
cameraControls model =
    group
        [ group 
            [ roundedRect 60 40 10
              |> filled green
            , text "Reset"
              |> size 16
              |> centered
              |> filled black
              |> move (0,-5)
            ]
            |> move (toFloat (unwrapQ model.width) / 2 - 125, toFloat (unwrapQ model.height) / 2 - 90)
            |> notifyTap Reset
        -- Forward
        , group
            [ roundedRect 40 40 10
              |> filled green
            , text "|"
              |> size 16
              |> centered
              |> sansserif
              |> filled black
              |> move (0,-2.5)
              |> rotate (degrees 180)
            , text "v"
              |> size 16
              |> centered
              |> sansserif
              |> filled black
              |> move (0,-7.5)
              |> rotate (degrees 180)
            ]
          |> move (toFloat (unwrapQ model.width) / 2 - 125, toFloat (unwrapQ model.height) / 2 - 150)
          |> notifyTap (MoveCamera Direction3d.x (Length.centimeters 10))
          -- Backward
          , group
            [ roundedRect 40 40 10
              |> filled green
            , text "|"
              |> size 16
              |> centered
              |> sansserif
              |> filled black
              |> move (0,-2.5)
            , text "v"
              |> size 16
              |> centered
              |> sansserif
              |> filled black
              |> move (0,-7.5)
            ]
          |> move (toFloat (unwrapQ model.width) / 2 - 125, toFloat (unwrapQ model.height) / 2 - 200)
          |> notifyTap (MoveCamera Direction3d.x (Length.centimeters -10))
          -- Left
          , group
            [ roundedRect 40 40 10
              |> filled green
            , text "|"
              |> size 16
              |> centered
              |> sansserif
              |> filled black
              |> move (0,-2.5)
              |> rotate (degrees -90)
            , text "v"
              |> size 16
              |> centered
              |> sansserif
              |> filled black
              |> move (0,-7.5)
              |> rotate (degrees -90)
            ]
          |> move (toFloat (unwrapQ model.width) / 2 - 175, toFloat (unwrapQ model.height) / 2 - 200)
          |> notifyTap (MoveCamera Direction3d.y (Length.centimeters 10))
          -- Right
          , group
            [ roundedRect 40 40 10
              |> filled green
            , text "|"
              |> size 16
              |> centered
              |> sansserif
              |> filled black
              |> move (0,-2.5)
              |> rotate (degrees 90)
            , text "v"
              |> size 16
              |> centered
              |> sansserif
              |> filled black
              |> move (0,-7.5)
              |> rotate (degrees 90)
            ]
          |> move (toFloat (unwrapQ model.width) / 2 - 75, toFloat (unwrapQ model.height) / 2 - 200)
          |> notifyTap (MoveCamera Direction3d.y (Length.centimeters -10))
          , group
            [ roundedRect 40 40 10
              |> filled green
            , text "Up"
              |> size 14
              |> centered
              |> sansserif
              |> filled black
              |> move (0,-4)
            ]
          |> move (toFloat (unwrapQ model.width) / 2 - 175, toFloat (unwrapQ model.height) / 2 - 150)
          |> notifyTap (MoveCamera Direction3d.z (Length.centimeters 10))
          , group
            [ roundedRect 40 40 10
              |> filled green
            , text "Down"
              |> size 14
              |> centered
              |> sansserif
              |> filled black
              |> move (0,-4)
            ]
          |> move (toFloat (unwrapQ model.width) / 2 - 75, toFloat (unwrapQ model.height) / 2 - 150)
          |> notifyTap (MoveCamera Direction3d.z (Length.centimeters -10))
        ]
{-endextra-}


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ -- Listen for resize events so we can render full screen
          Browser.Events.onResize
            (\width height ->
                Resize
                    (Pixels.pixels width)
                    (Pixels.pixels height)
            )

        -- Subscribe to animation frames to animate the cubes
        , Browser.Events.onAnimationFrameDelta (Duration.seconds >> Tick)

        -- Listen for visibility change events so we can stop orbiting if the
        -- user switches to a different tab etc.
        , Browser.Events.onVisibilityChange VisibilityChange

        -- Listen for orbit-related mouse events
        , if model.orbiting then
            Sub.batch
                [ Browser.Events.onMouseMove mouseMoveDecoder
                , Browser.Events.onMouseUp (Decode.succeed MouseUp)
                ]

          else
            Browser.Events.onMouseDown (Decode.succeed MouseDown)
        {-, case skyboxType of
            Skybox.GSVGSkybox _ _ _ _ ->
                Sub.map SkyboxMsg (GS.subscriptions model.gSkyboxModel)
            _ -> Sub.none-}
        , Sub.map GSVGTextureMsg (GT.subscriptions model.gTextureModel)
        , Sub.map SpaceshipMsg SpaceshipPart.subscriptions
        ]

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

init : () -> ( Model, Cmd Msg )
init _ =
    let
        (wModel, _) = Widget.init 0 0 "widget"
        (gSkyboxModel, gSCmd) = GS.initialModel

        allSvgTextures = svgTextures ++
            (case skyboxType of
                 Skybox.GSVGSkybox _ top sides bottom _ ->
                     GS.getTexturesToLoad top sides bottom
                 Skybox.GSVGSphericalSkybox _ shape _ ->
                     GS.getTexturesToLoad shape (group []) (group [])
                 _ -> []
            )

        (gTextureModel, gTCmd) = GT.initialModel allSvgTextures
    in
    ( { width = Quantity.zero
      , height = Quantity.zero
      , time = 0
      , gameTime = 0
      , orbiting = False
      , azimuth = Angle.degrees 180
      , elevation = Angle.degrees 30
      , cameraPos = Point3d.centimeters -(earthSize*2) 0 (earthSize*2.5)
      , camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.orbitZ
                        { focalPoint = Point3d.centimeters 0 0 20
                        , azimuth = Angle.degrees 180
                        , elevation = Angle.degrees 30
                        , distance = Length.meters 3
                        }
                , verticalFieldOfView = Angle.degrees 45
                }
      , cameraState = Orbiting
      , meshStore = { generatedMeshes = Dict.empty, generatedShadows = Dict.empty }
      , meshStoreWorld = { generatedMeshes = Dict.empty, generatedShadows = Dict.empty }
      , widget = wModel
      , gSkyboxModel = gSkyboxModel
      , gTextureModel = gTextureModel
      , textureLoader = TL.init
      , state = Loading
      , world = World.empty
      , spaceshipState =
            SpaceshipPart.init
                { generatedMeshes = Dict.empty, generatedShadows = Dict.empty }
                (mySpaceship
                    { textureLoader = TL.init, meshStore = { generatedMeshes = Dict.empty, generatedShadows = Dict.empty } } )
      , cameraZoom = Length.meters 100
      , showDetailedSatellite = False
      , showDetailedSpaceStation = False
      , gameState = RepairNotStarted
      }
    , Cmd.batch
        [ Task.perform
            -- The scene gets resized to match the browser window
            (\{ viewport } ->
                Resize
                    (Pixels.int (round viewport.width))
                    (Pixels.int (round viewport.height))
            )
            Browser.Dom.getViewport
        , Cmd.map TextureLoadMsg <| case skyboxType of
            Skybox.URLSkybox top bottom side1 side2 side3 side4 _ ->
                TL.fetchTextures
                    ( [ loadTexture "skyB" TexColor bottom
                      , loadTexture "skyT" TexColor top
                      , loadTexture "skyS1" TexColor side1
                      , loadTexture "skyS2" TexColor side2
                      , loadTexture "skyS3" TexColor side3
                      , loadTexture "skyS4" TexColor side4
                      ] ++ myTextures
                    )
                    TL.init
            Skybox.URLSphericalSkybox texture _ ->
                TL.fetchTextures
                    ( loadTexture "skyT" TexColor texture :: myTextures )
                    TL.init
            _ -> TL.fetchTextures myTextures TL.init
        , case skyboxType of
            Skybox.GSVGSkybox _ _ _ _ _ -> Cmd.map SkyboxMsg gSCmd
            Skybox.GSVGSphericalSkybox _ _ _ -> Cmd.map SkyboxMsg gSCmd
            _ -> Cmd.none
        , Cmd.map GSVGTextureMsg gTCmd
        ]
    )

update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Resize width height ->
            let
                (wModel, wCmd) = Widget.init (toFloat <| unwrapQ width) (toFloat <| unwrapQ height) "widget"
            in
            ( { model | width = width, height = height, widget = wModel }, Cmd.map WidgetMsg wCmd )

        Tick t ->
            let
                tickRate =
                    Duration.milliseconds 1 |> Quantity.per Duration.second

                updatedTime =
                    Duration.seconds model.time |> Quantity.plus (tickRate |> Quantity.for t)

                timeAsNum = Duration.inSeconds updatedTime

                repairRadius = Length.meters 1200

                shipCrashed =
                    (distanceToSatellite |> Quantity.lessThan (Length.meters 10))
                    || (earthToShip model |> Quantity.lessThan (Length.centimeters earthSize))
                    || (distanceToSpaceStation |> Quantity.lessThan (Length.meters 100))

                newGameState =
                    case model.gameState of
                        RepairNotStarted ->
                            if distanceToSatellite |> Quantity.lessThan repairRadius then
                                RepairInProgress model.time
                            else if shipCrashed then
                                Crashed
                            else
                                model.gameState
                        RepairInProgress time ->
                            if model.time - time > toFloat levelAdjust then
                                Repaired
                            else if distanceToSatellite |> Quantity.greaterThan repairRadius then
                                RepairNotStarted
                            else if shipCrashed then
                                Crashed
                            else model.gameState
                        Repaired ->
                            if distanceToSpaceStation |> Quantity.lessThan (Length.meters 1500) then
                                Successful model.time
                            else if shipCrashed then
                                Crashed
                            else model.gameState
                        _ -> model.gameState

                spaceshipPosition =
                    Maybe.withDefault Point3d.origin <| Maybe.map adjustedPosition
                        <| getBodyByName "spaceship" model.spaceshipState.world

                satellitePosition = getPosition (satelliteModel model)
                spaceStationPosition = getPosition (internalSpaceStation model)

                distanceToSatellite = Vector3d.length <| Vector3d.from spaceshipPosition satellitePosition
                distanceToSpaceStation = Vector3d.length <| Vector3d.from spaceshipPosition spaceStationPosition

                newCamera =
                    case model.cameraState of
                        Orbiting ->
                            Camera3d.perspective
                                { viewpoint =
                                    Viewpoint3d.orbitZ
                                        { focalPoint = spaceshipPosition
                                        , azimuth = model.azimuth
                                        , elevation = model.elevation
                                        , distance = model.cameraZoom
                                        }
                                , verticalFieldOfView = Angle.degrees 45
                                }
                        SpaceshipForwards ->
                            let
                                spaceship =
                                    getBodyByName "spaceship" model.spaceshipState.world

                                frame =
                                    Maybe.withDefault Frame3d.atOrigin (Maybe.map Body.frame spaceship)
                            in
                                Camera3d.perspective
                                    { viewpoint =
                                        Viewpoint3d.lookAt
                                            { focalPoint = spaceshipPosition
                                            , eyePoint =
                                                Point3d.meters 0 0 (-(Length.inMeters <| model.cameraZoom))
                                                |> Point3d.placeIn frame
                                            , upDirection = Frame3d.yDirection frame
                                            }
                                    , verticalFieldOfView = Angle.degrees 45
                                    }

                newGameTime =
                    if not ((case model.gameState of
                                Successful _ -> True
                                _ -> False
                            ) || (model.gameState == Crashed || model.gameState == OutOfFuel)) then
                        timeAsNum
                    else
                        model.gameTime


            in
                ( case model.state of
                    Loaded ->
                        { model 
                        | time = timeAsNum
                        , gameTime = newGameTime
                        , gameState = newGameState
                        , camera = newCamera
                        , showDetailedSatellite = distanceToSatellite |> Quantity.lessThanOrEqualTo (Length.kilometers 10)
                        , showDetailedSpaceStation = distanceToSpaceStation |> Quantity.lessThanOrEqualTo (Length.kilometers 10)
                        }
                    Loading ->
                        model
                , Cmd.batch
                    [ Task.perform 
                        ( \ _ -> GenerateMeshes ( renderBodies False model.spaceshipState.world ) BodyCoords )
                        (Task.succeed True)
                    , Task.perform
                        ( \ _ -> GenerateMeshes ( spaceGameAssets model) WorldCoords )
                        (Task.succeed True)
                    ]
                )
        MouseDown ->
            ( { model | orbiting = True && model.state == Loaded }, Cmd.none )

        MouseUp ->
            ( { model | orbiting = False }, Cmd.none )

        VisibilityChange Browser.Events.Visible ->
            ( model, Cmd.none )

        VisibilityChange Browser.Events.Hidden ->
            ( { model | orbiting = False }, Cmd.none )

        MouseMove dx dy ->
            if model.orbiting && model.cameraState == Orbiting then
                let
                    rotationRate =
                        Angle.degrees 0.5 |> Quantity.per Pixels.pixel

                    newAzimuth =
                        model.azimuth
                            |> Quantity.minus (dx |> Quantity.at rotationRate)

                    newElevation =
                        model.elevation
                            |> Quantity.plus (dy |> Quantity.at rotationRate)
                            |> Quantity.clamp (Angle.degrees -90) (Angle.degrees 90)
                in
                ( { model
                    | orbiting = True
                    , azimuth = newAzimuth
                    , elevation = newElevation
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        GenerateMeshes objectList coordinateSystem ->
            case objectList of
                [] ->
                    (model, Cmd.none)
                (object :: rest) ->
                    case object of
                        ObjectGroup attr ->
                            ( model, Cmd.batch [ Task.perform ( \ _ -> GenerateMeshes ( attr.subObjects ++ rest ) coordinateSystem ) (Task.succeed True) ])
                        Object attr ->
                            case attr.customMesh of
                                -- Skip generating if the object uses a primitive mesh
                                Primitive ->
                                    ( model, Cmd.batch [ Task.perform (\_ -> GenerateMeshes rest coordinateSystem) (Task.succeed True) ])

                                -- Otherwise, check if the mesh already exists, and generate the mesh + shadow and store it if necessary
                                _ ->
                                    case coordinateSystem of
                                        BodyCoords ->
                                            let
                                                meshExists =
                                                    Dict.member (getMeshName attr.customMesh) model.meshStore.generatedMeshes
                                            in
                                                if meshExists then
                                                    -- Don't regenerate if the mesh already exists
                                                    ( model, Cmd.batch [ Task.perform (\_ -> GenerateMeshes rest coordinateSystem) (Task.succeed True) ] )
                                                else
                                                    -- Generate the mesh if it's missing
                                                    let
                                                        generatedMesh =
                                                            case attr.customMesh of
                                                                PolyCone points (xtip, ytip, ztip) ->
                                                                    generatePolyCone points (xtip, ytip, ztip)
                                                                
                                                                PolyCylinder points height ->
                                                                    generatePolyCylinder points height

                                                                Ellipsoid (length, width, height) ->
                                                                    generateEllipsoid length width height

                                                                Ring radius thickness ->
                                                                    generateRing radius thickness

                                                                TexturedCylinder radius height ->
                                                                    generateTexturedCylinder radius height

                                                                TruncatedCone topR botR height ->
                                                                    generateTruncatedCone topR botR height

                                                                CustomObject triMesh doCulling ->
                                                                    generateDynamicMesh 
                                                                        (triMesh
                                                                            |> TriangularMesh.mapVertices
                                                                                ( \ vertex -> { position = vertex.position |> Point3d.placeIn Frame3d.atOrigin, uv = vertex.uv } )
                                                                        )
                                                                        doCulling

                                                                -- Since primitives are handled above, this path should never be reached unless
                                                                -- somebody forgets to update it when new meshes are added
                                                                _ ->
                                                                    generateEllipsoid 0 0 0

                                                        updatedMeshes = Dict.insert generatedMesh.name generatedMesh.mesh model.meshStore.generatedMeshes
                                                        
                                                        updatedShadows = Dict.insert generatedMesh.name generatedMesh.shadow model.meshStore.generatedShadows

                                                        updatedMeshStore = { generatedMeshes = updatedMeshes, generatedShadows = updatedShadows }
                                                    in
                                                        ( { model | meshStore = updatedMeshStore }, Cmd.batch [ Task.perform (\_ -> GenerateMeshes rest coordinateSystem) (Task.succeed True) ] )
                                        WorldCoords ->
                                            let
                                                meshExists =
                                                    Dict.member (getMeshName attr.customMesh) model.meshStoreWorld.generatedMeshes
                                            in
                                                if meshExists then
                                                    -- Don't regenerate if the mesh already exists
                                                    ( model, Cmd.batch [ Task.perform (\_ -> GenerateMeshes rest coordinateSystem) (Task.succeed True) ] )
                                                else
                                                    -- Generate the mesh if it's missing
                                                    let
                                                        generatedMesh =
                                                            case attr.customMesh of
                                                                PolyCone points (xtip, ytip, ztip) ->
                                                                    generatePolyCone points (xtip, ytip, ztip)
                                                                
                                                                PolyCylinder points height ->
                                                                    generatePolyCylinder points height

                                                                Ellipsoid (length, width, height) ->
                                                                    generateEllipsoid length width height

                                                                Ring radius thickness ->
                                                                    generateRing radius thickness

                                                                TexturedCylinder radius height ->
                                                                    generateTexturedCylinder radius height

                                                                TruncatedCone topR botR height ->
                                                                    generateTruncatedCone topR botR height

                                                                CustomObject triMesh doCulling ->
                                                                    generateDynamicMesh triMesh doCulling

                                                                -- Since primitives are handled above, this path should never be reached unless
                                                                -- somebody forgets to update it when new meshes are added
                                                                _ ->
                                                                    generateEllipsoid 0 0 0

                                                        updatedMeshes = Dict.insert generatedMesh.name generatedMesh.mesh model.meshStoreWorld.generatedMeshes
                                                        
                                                        updatedShadows = Dict.insert generatedMesh.name generatedMesh.shadow model.meshStoreWorld.generatedShadows

                                                        updatedMeshStore = { generatedMeshes = updatedMeshes, generatedShadows = updatedShadows }
                                                    in
                                                        ( { model | meshStoreWorld = updatedMeshStore }, Cmd.batch [ Task.perform (\_ -> GenerateMeshes rest coordinateSystem) (Task.succeed True) ] )

        -- This is needed for our widget
        WidgetMsg wMsg ->
            let
                (newWModel, wCmd) = Widget.update wMsg model.widget
            in
            ( { model | widget = newWModel }, Cmd.map WidgetMsg wCmd )

        Reset -> ( { model | azimuth = Angle.degrees 180, elevation = Angle.degrees 30, cameraPos = Point3d.centimeters -(earthSize*2) 0 (earthSize*2.5) } , Cmd.none )

        MoveCamera direction amount ->
            ( { model | cameraPos = model.cameraPos |> Point3d.translateBy (Vector3d.withLength amount direction) } , Cmd.none )

        SkyboxMsg sMsg ->
            let
                (gSkyboxModel, gSCmd) = GS.update sMsg model.gSkyboxModel
            in
                ( { model | gSkyboxModel = gSkyboxModel } , Cmd.map SkyboxMsg gSCmd)

        GSVGTextureMsg tMsg ->
            let
                (gTextureModel, gTCmd) = GT.update tMsg model.gTextureModel
            in
                ( { model | gTextureModel = gTextureModel }
                ,
                    case tMsg of
                        GT.GeneratedPNG (pngName, pngUrl) ->
                            Cmd.batch
                                [
                                    Cmd.map TextureLoadMsg (TL.fetchTexture (loadTexture pngName TexColor pngUrl) model.textureLoader)
                                ,   Cmd.map GSVGTextureMsg gTCmd
                                ]
                        _ -> Cmd.map GSVGTextureMsg gTCmd
                )

        TextureLoadMsg tlMsg ->
            let
                (tlModel, tlCmd) = TL.update tlMsg model.textureLoader
            in
                if TL.allTexturesLoaded tlModel then
                    ( { model | textureLoader = tlModel, state = Loaded
                              , spaceshipState = SpaceshipPart.init
                                     model.meshStore
                                     (mySpaceship
                                         { textureLoader = tlModel, meshStore = model.meshStore } )
                     }
                    , Cmd.batch
                        [ Cmd.map TextureLoadMsg tlCmd
                        , Task.perform
                            ( \ _ -> AddBodies )
                            (Task.succeed True)
                        ]
                    )
                else
                    (   { model 
                        | textureLoader = tlModel
                        }
                    ,   Cmd.map TextureLoadMsg tlCmd
                    )

        AddBodies ->
            case model.state of
                Loaded ->
                    ( model, Cmd.none)
                Loading ->
                    ( model
                    , Cmd.batch
                        [ Task.perform
                            ( \ _ -> AddBodies )
                            (Task.succeed True)
                        ]
                    )
        SpaceshipMsg sMsg ->
            let
                (newSSState, sCmd) = SpaceshipPart.update model.meshStore (autoPilotRoutine model.time, useAutoPilot) sMsg model.spaceshipState
            in
            ( { model |
                spaceshipState = newSSState
              }
            , Cmd.map SpaceshipMsg sCmd
            )

        ChangeZoom length ->
            ( { model |
                cameraZoom = Quantity.clamp Quantity.zero (Length.kilometers 10) (model.cameraZoom |> Quantity.plus length)
              }
            , Cmd.none
            )

        CycleCameraState ->
            ( { model 
              | cameraState =
                    case model.cameraState of
                        -- This needs to be updated if we ever add more camera modes
                        Orbiting ->
                            SpaceshipForwards
                        SpaceshipForwards ->
                            Orbiting
              }
            , Cmd.none
            )

mouseMoveDecoder : Decoder Msg
mouseMoveDecoder =
    Decode.map2 MouseMove
        (Decode.field "movementX" (Decode.map Pixels.pixels Decode.float))
        (Decode.field "movementY" (Decode.map Pixels.pixels Decode.float))



-- Detect if show the causion sign and game over sign
aggregate boxList = 
     case BoundingBox3d.aggregateN boxList of
       Just attri -> attri
       Nothing  -> BoundingBox3d.singleton Point3d.origin
    
findCenter object = 
  case object of
    ObjectGroup attr ->
      case attr.boundingBox of
        Box box -> BoundingBox3d.centerPoint box
        None -> Point3d.origin -- This case should never happend
    Object attr ->
       case attr.boundingBox of
        Box box -> BoundingBox3d.centerPoint box
        None -> Point3d.origin -- This case should never happend
        
{-html
<!DOCTYPE HTML>
<html>
<head>
    <meta charset="UTF-8">
    <title>Main</title>
    <link rel="icon" href="favicon.ico?v=1" />
    <link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Audiowide">
    <script src="app.js"></script>
</head>

<body>
<div id="elm"></div>

<div id="canvas"></div>
<script>
{{ elmjs }}

  var app = Elm.{{ modulename }}.init({
    node: document.getElementById('elm')
  });

function triggerDownload (widgetID, imgURI) {
  var evt = new MouseEvent('click', {
    view: window,
    bubbles: false,
    cancelable: true
  });

  var a = document.createElement('a');
  a.setAttribute('download', widgetID + '.png');
  a.setAttribute('href', imgURI);
  a.setAttribute('target', '_blank');

  a.dispatchEvent(evt);
}

app.ports.createPNG.subscribe(function([widgetID, width, height]) {
  var canvas = document.createElement("CANVAS");
  canvas.width=width;
  canvas.height=height;
  var ctx = canvas.getContext('2d');

  var svg = document.getElementById(widgetID);
  var data = (new XMLSerializer()).serializeToString(svg);
  var DOMURL = window.URL || window.webkitURL || window;

  var img = new Image();
  var svgBlob = new Blob([data], {type: 'image/svg+xml;charset=utf-8'});
  var url = DOMURL.createObjectURL(svgBlob);

  img.onload = function () {
    ctx.drawImage(img, 0, 0);
    DOMURL.revokeObjectURL(url);

    var imgURI = canvas
        .toDataURL('image/png')
        .replace('image/png', 'image/octet-stream');

    //triggerDownload(widgetID,imgURI);
    app.ports.receivePNG.send([widgetID, imgURI]);
  };

  img.src = url;
});

  </script>
</body>
</html>

endhtml-}
