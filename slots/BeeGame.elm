{- New file for working on the Bee Game -}


module BeeGame exposing (main)

-- Most of these imports were taken from "3d-elm-camp/BeeMovement.elm", so there may be a lot of unused things

import Angle exposing (Angle)
import Arc2d
import Arc3d
import Array exposing (Array)
import Axis3d
import Block3d exposing (Block3d)
import BoundingBox3d exposing (BoundingBox3d)
import Browser
import Browser.Dom
import Browser.Events
import Camera3d
import Circle3d
import Collision exposing (noCollide, isColliding, isCollidingByName, getCollisions, nameObject)
import Color exposing (Color)
import Cone3d
import Cylinder3d
import Dict exposing (Dict)
import Direction3d exposing (Direction3d)
import Duration exposing (Duration)
import Frame3d
import GSVGSkybox as GS
import GraphicSVG exposing (..)
import GraphicSVG.Widget as Widget
import Html exposing (Html)
import Html.Attributes as HA exposing (style)
import Illuminance
import Json.Decode as Decode exposing (Decoder)
import Length exposing (Meters)
import LineSegment3d
import LuminousFlux exposing (LuminousFlux)
import Parameter1d
import Pixels exposing (Pixels)
import Point2d
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import Scene3d exposing (Entity)
import Scene3d.Light as Light exposing (Chromaticity, Light)
import Scene3d.Material as Material exposing (Material)
import Scene3d.Mesh as Mesh
import SketchPlane3d
import Skybox
import SolidAngle
import Sphere3d
import Task
import Temperature
import Triangle3d
import TriangularMesh
import Vector3d exposing (Vector3d)
import Viewpoint3d
import WebGL.Texture
import Wrapper3D exposing (..)
import PhysicsWrapper3D

import GSVGTexture as GT exposing (svgTexture)
import TextureLoader as TL exposing (loadTexture, TextureType(..), getColorTexture, getRoughnessTexture, getMetallicityTexture)
import Json.Encode exposing (object)

{-header<b>3D Bee Simulator!</b>: Simulate a bee in 3D!-}
{-editable-}
-- Consider this the equivalent of "myShapes" on the other slots. You start out with a basic shape
myEntities model =
    [ polygon3D 8 10 |> textured (getColorTexture "squares" model) (constantTexture 0) (constantTexture 0) |> rotateY3D (degrees 90) |> move3D (-50,0,50)
    ]

-- Please put all of your flowers here, and NOT in myEntities
myFlowers model =
    [ 
        -- Make some flowers!
    ]

-- move / edit the light
light =
    pointLight
        { position = Point3d.centimeters 0 0 100    -- position of the light
        , chromaticity = Light.sunlight             -- the colour of the light (see https://package.elm-lang.org/packages/ianmackenzie/elm-3d-scene/latest/Scene3d-Light#Chromaticity)
        , intensity = LuminousFlux.lumens 10     -- how intense the light is
        }
showLight = True -- whether to show the light ball or not

-- Set to False to turn off shadows
drawShadows = True

-- Put all of your bee models in here. Make sure that they are all grouped properly.
myBees : List (TextureStore -> MeshStore WorldCoordinates -> Float -> Object WorldCoordinates)
myBees =
    [ defaultBee
    ]

overlay : Model -> List (Shape Msg)
overlay model =
    [ angleDisplay model
    , beePicker model
    , beeUI model
    ]

-- Use "loadTexture [name] [url]" to load in texture images from the Internet!
myTextures =
    [ loadTexture "example" TexColor "Put an image URL here!"
    ]

-- Usage: `svgTexture "name" "name`, where shape is any 2D shape or group
svgTextures =
    [ svgTexture "squares" TexColor squares
    ]

-- SVG textures are 50 by 50
squares =
    group
    [
        square 25 |> filled purple |> move (12.5,12.5)
    ,   square 25 |> filled orange |> move (-12.5,12.5)
    ,   square 25 |> filled purple |> move (-12.5,-12.5)
    ,   square 25 |> filled orange |> move (12.5,-12.5)
    ]

-- Here you can specify whether you want a skybox generated from your own 2D shapes, or one from pictures on the Internet
skyboxType =
    Skybox.URLSkybox textureBottom textureTop textureSide1 textureSide2 textureSide3 textureSide4 4000
    -- Some other options (comment in the one above and comment one of these out)
    -- Skybox.GSVGSkybox False skyboxTop skyboxSides skyBoxBottom 4000
    -- Skybox.GSVGSphericalSkybox False skyboxTop 4000
    -- Skybox.URLSphericalSkybox "https://cschank.github.io/img/milky.jpg" 4000

-- This is 50 by 50
skyboxTop : Shape msg
skyboxTop =
    group
        [ square 50 |> filled lightBlue
        , circle 10 |> filled yellow
        ]

-- This is 200 by 50
skyboxSides : Shape msg
skyboxSides =
    group
        [ rect 200 50 |> filled lightBlue |> move ( 0, 25 )
        , rect 200 50 |> filled green |> move ( 0, -25 )
        , triangle 10 |> filled darkGreen |> rotate (degrees -30) |> move ( 0, 5 )
        , text "abcdefghijklmnopqrstuvwxyz" |> centered |> size 16 |> filled red
        ]

-- This is 50 by 50
skyBoxBottom : Shape msg
skyBoxBottom =
    group
        [ square 50 |> filled green
        ]

-- This colour is used to create the floor. If you want custom colours, use Color.hsl or Color.rgb!
floorColour =
    Color.green

-- Here you can specify what images to use to create the skybox. Just replace "todo" with a link to an image. (Keep the quotes, though!)
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

-- The default bee model that's loaded when something goes wrong.
-- You can use this as a base when making your own bees!
defaultBee textures meshes time =
    let
        useColor name = getColorTexture name { textureLoader = textures }
        useRoughness name = getRoughnessTexture name { textureLoader = textures }
        useMetallicity name = getMetallicityTexture name { textureLoader = textures }

        wing =
            cylinder 15 1 |> matte (Color.rgba 0.9 0.9 0.9 0.5) |> noCollide
    in
    group3D
        [ ellipsoid 20 10 10 meshes
            |> matte Color.yellow
        , wing |> move3D ( 0, 20, 0 ) |> rotateX3D (degrees (-15 * sin (10 * time))) |> move3D ( 0, 0, 8 )
        , wing |> move3D ( 0, -20, 0 ) |> rotateX3D (degrees (15 * sin (10 * time))) |> move3D ( 0, 0, 8 )
        ]
        |> move3D ( 0, 0, 20 )

debugMeshNames model = text (Debug.toString (List.map (\(key,_) -> key) (Dict.toList model.meshStore.generatedMeshes))) |> filled black

type WorldCoordinates
    = WorldCoordinates

type Direction
    = Up
    | Down
    | Left
    | Right
    | Forward
    | Backward
    | RotLeft
    | RotRight
    | None

type alias TextureStore = TL.Model

-- Game Constants

-- How fast the bee moves
speed : Float
speed = 0.8

-- Degrees per tick
rotSpeed : Float
rotSpeed = 2.5

-- Collision detection is turned off for this many seconds after a collision
-- This is done because the bee will almost always still be colliding with something for
-- a few frames after being bounced away.
collisionCooldown : Float
collisionCooldown = 0.25

-- The amount of pollen that is turned into honey on each tick
honeyCreationRate : Float
honeyCreationRate = 0.015

-- Bee can carry this much pollen before having to drop it off
maxPollen : Int
maxPollen = 20

-- Time, in seconds, before the bee can collect pollen again
pollenCooldown : Float
pollenCooldown = 30

type alias Model =
    { width : Quantity Int Pixels
    , height : Quantity Int Pixels
    , time : Float
    , lastCollisionTime : Float
    , orbiting : Bool
    , azimuth : Angle
    , elevation : Angle
    , meshStore : MeshStore WorldCoordinates
    , beePos : Point3d Meters WorldCoordinates
    , camPos : Point3d Meters WorldCoordinates
    , beeRot : Angle
    , velocity : Vector3d Meters WorldCoordinates
    , rotVelocity : Float
    , dirLR : Direction
    , dirFB : Direction
    , dirUD : Direction
    , rotLR : Direction
    , bee : TextureStore -> MeshStore WorldCoordinates -> Float -> Object WorldCoordinates
    , beeModels : Array (TextureStore -> MeshStore WorldCoordinates -> Float -> Object WorldCoordinates)
    , beeIndex : Int
    , beePollen : Int
    , storedPollen : Float
    , storedHoney : Float
    , choosingBee : Bool
    , mapFlowers : List (Object WorldCoordinates)
    , flowerCollisionTimes : Dict String Float
    , widget : Widget.Model
    , gSkyboxModel : GS.Model
    , gTextureModel : GT.Model
    , textureLoader : TL.Model
    }

type Msg
    = Resize (Quantity Int Pixels) (Quantity Int Pixels)
    | Tick Duration
    | MouseDown
    | MouseMove (Quantity Float Pixels) (Quantity Float Pixels)
    | MouseUp
    | KeyDown String
    | KeyUp String
    | VisibilityChange Browser.Events.Visibility
    | GenerateMeshes (List (Object WorldCoordinates))
    | InitializeFlowers
    | WidgetMsg Widget.Msg
    | Reset
    | SkyboxMsg GS.Msg
    | ChangeBee Bool
    | ToggleBeePicker
    | GSVGTextureMsg GT.Msg
    | TextureLoadMsg TL.Msg

{-| Create both a Light and an Entity (a bright glowing sphere) representing a
particular point light
-}
pointLight :
    { position : Point3d Meters WorldCoordinates
    , chromaticity : Chromaticity
    , intensity : LuminousFlux
    }
    -> ( Light WorldCoordinates Bool, Entity WorldCoordinates )
pointLight properties =
    let
        -- Create a sphere to represent a light bulb
        lightsphere =
            Sphere3d.atPoint properties.position (Length.millimeters 100)

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
    ( Light.point (Light.castsShadows drawShadows) properties
    , Scene3d.sphere sphereMaterial lightsphere
    )

view : Model -> Html Msg
view model =
    let
        -- Incandescent light bulb
        ( firstLight, firstLightBall ) =
            light

        -- Rough approximation of sunlight
        sun =
            Light.directional (Light.castsShadows drawShadows)
                { direction = Direction3d.xyZ (Angle.degrees -30) (Angle.degrees -50)
                , chromaticity = Light.sunlight
                , intensity = Illuminance.lux 250
                }

        -- Add some soft lighting to fill in shadowed areas
        softLighting =
            Light.soft
                { upDirection = Direction3d.positiveZ
                , chromaticity = Light.fluorescent
                , intensityAbove = Illuminance.lux 50
                , intensityBelow = Illuminance.lux 5
                }

        -- Create a quad to act as a 'floor'
        plane =
            square3D 8000
                |> matte floorColour

        -- Define camera as usual
        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.orbitZ
                        { focalPoint = model.camPos
                        , azimuth = model.beeRot |> Quantity.plus (Angle.degrees 180)
                        , elevation = model.elevation
                        , distance = Length.meters 3
                        }
                , verticalFieldOfView = Angle.degrees 45
                }

        textures = model.textureLoader.colorTextures

        -- Things that are always shown
        baseEntities =
            let
                rotatedBee =
                    if model.choosingBee then
                        model.bee model.textureLoader model.meshStore model.time

                    else
                        model.bee model.textureLoader model.meshStore model.time
                            |> rotateZ3D (Angle.inRadians model.beeRot)

                beeShape =
                    let
                        position =
                            let
                                coords =
                                    Point3d.toRecord Length.inCentimeters model.beePos
                            in
                            ( coords.x, coords.y, coords.z )
                    in
                        rotatedBee
                        |> move3D position
                        |> move3D ( 0, 0, 5 * sin model.time )

            in
            (if showLight then [firstLightBall] else []) ++
                ( Skybox.skybox
                    [ Dict.get "skyB" textures
                    , Dict.get "skyT" textures
                    , Dict.get "skyS1" textures
                    , Dict.get "skyS2" textures
                    , Dict.get "skyS3" textures
                    , Dict.get "skyS4" textures
                    ]
                    4000
                :: renderEntities [ plane, beeShape ] )

        visibleObjects =
            List.filter
                (PhysicsWrapper3D.isVisible camera (model.width |> Pixels.inPixels, model.height |> Pixels.inPixels))
                (myEntities model ++ myFlowers model)

    in
    Html.div []
        [ case skyboxType of
            Skybox.GSVGSkybox debug sT sS sB _ ->
                Html.div [ style "position" "absolute", style "left" "0px", style "top" (String.fromInt (unwrapQ model.height) ++ "px") ]
                    [ -- Html.h1 [] [Html.text "Skybox Debug"]
                      Html.map SkyboxMsg <| GS.drawSkybox debug model.gSkyboxModel sT sS sB
                    ]

            _ ->
                Html.span [] []
        , Scene3d.custom
            { lights = Scene3d.threeLights firstLight sun softLighting
            , camera = camera
            , clipDepth = Length.centimeters 10
            , exposure = Scene3d.exposureValue 6
            , toneMapping = Scene3d.hableFilmicToneMapping
            , whiteBalance = Light.fluorescent
            , antialiasing = Scene3d.multisampling
            , dimensions = ( model.width, model.height )
            , background = Scene3d.backgroundColor Color.lightBlue
            , entities = baseEntities ++ renderEntities visibleObjects
            }
            |> withOverlay (overlay model) model
        , Html.map GSVGTextureMsg <| GT.drawTextures False model.gTextureModel
        ]

-- Displays information about camera rotation, and allows you to reset it
angleDisplay : Model -> Shape Msg
angleDisplay model =
    group
        [ text ("azimuth: " ++ String.fromInt (round <| unwrapQ model.azimuth * 180 / pi) ++ "??")
            |> filled black
            |> move ( -(toFloat (unwrapQ model.width) / 2) + 95, toFloat (unwrapQ model.height) / 2 - 50 )
        , text ("elevation: " ++ String.fromInt (round <| unwrapQ model.elevation * 180 / pi) ++ "??")
            |> filled black
            |> move ( -(toFloat (unwrapQ model.width) / 2) + 95, toFloat (unwrapQ model.height) / 2 - 60 )
        , group
            [ roundedRect 60 40 10
                |> filled green
            , text "Reset"
                |> size 16
                |> centered
                |> filled black
                |> move ( 0, -5 )
            ]
            |> move ( -(toFloat (unwrapQ model.width) / 2) + 125, toFloat (unwrapQ model.height) / 2 - 90 )
            |> notifyTap Reset
        ]

-- Consists of the pollen and honey meters
beeUI : Model -> Shape Msg
beeUI model = 
    let
        barHeight = toFloat (unwrapQ model.height) * 0.75

        barScale = toFloat model.beePollen / toFloat maxPollen

        pollenMeter = 
            group [ roundedRect 60 barHeight 10
                    |> filled gray
                    |> move ( toFloat (unwrapQ model.width) / 2 - 125, -50 )
                  , roundedRect 60 barHeight 10
                    |> filled yellow
                    |> scaleY barScale
                    |> move ( toFloat (unwrapQ model.width) / 2 - 125, -50 )
                  , text "Pollen Meter"
                    |> sansserif
                    |> size 24
                    |> centered
                    |> filled black
                    |> move ( toFloat (unwrapQ model.width) / 2 - 125, (toFloat (unwrapQ model.height) * 0.75) / 2 - 40 )
                  , text (String.fromInt model.beePollen)
                    |> sansserif
                    |> size 24
                    |> centered
                    |> filled black
                    |> move ( toFloat (unwrapQ model.width) / 2 - 125, -58 )
                  ]

        honeyMeter = 
            group [ text ("Stored Honey: " ++ String.fromInt (round model.storedHoney) ++ " mL")
                    |> sansserif
                    |> size 24
                    |> centered
                    |> filled black
                    |> move ( toFloat (unwrapQ model.width) / 2 - 125, (toFloat (unwrapQ model.height) * 0.75) / 2 + 40 )
                  ]
    in
        group [ pollenMeter
              , honeyMeter
              ]

-- The Bee Picker allows you to swap between different bee models, defined in "myBees"
beePicker : Model -> Shape Msg
beePicker model =
    if model.choosingBee then
        group
            [ group
                [ roundedRect 200 80 10 |> filled (hsl (degrees 200) 1 0.5)
                , text "Done Choosing" |> size 24 |> centered |> filled black |> move ( 0, -8 )
                ]
                |> move ( 0, -(toFloat (unwrapQ model.height) / 2 - 100) )
                |> notifyTap ToggleBeePicker
            , group
                [ roundedRect 200 80 10 |> filled (hsl (degrees 135) 1 0.35)
                , text "Previous Bee" |> size 24 |> centered |> filled black |> move ( 0, -8 )
                ]
                |> move ( -250, -(toFloat (unwrapQ model.height) / 2 - 100) )
                |> notifyTap (ChangeBee False)
            , group
                [ roundedRect 200 80 10 |> filled (hsl (degrees 135) 1 0.35)
                , text "Next Bee" |> size 24 |> centered |> filled black |> move ( 0, -8 )
                ]
                |> move ( 250, -(toFloat (unwrapQ model.height) / 2 - 100) )
                |> notifyTap (ChangeBee True)
            ]

    else
        group
            [ roundedRect 200 80 10 |> filled (hsl (degrees 200) 1 0.5)
            , text "Change Bee Model" |> size 24 |> centered |> filled black |> move ( 0, -8 )
            ]
            |> move ( 0, -(toFloat (unwrapQ model.height) / 2 - 100) )
            |> notifyTap ToggleBeePicker

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

        -- Listen for key presses
        , Browser.Events.onKeyDown keyDecoder
        , Browser.Events.onKeyUp keyUpDecoder

        -- Listen for orbit-related mouse events
        , if model.orbiting then
            Sub.batch
                [ Browser.Events.onMouseMove mouseMoveDecoder
                , Browser.Events.onMouseUp (Decode.succeed MouseUp)
                ]

          else
            Browser.Events.onMouseDown (Decode.succeed MouseDown)
        , case skyboxType of
            Skybox.GSVGSkybox _ _ _ _ _ ->
                Sub.map SkyboxMsg (GS.subscriptions model.gSkyboxModel)

            _ ->
                Sub.none
        , Sub.map GSVGTextureMsg (GT.subscriptions model.gTextureModel)
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
init () =
    let
        ( wModel, _ ) =
            Widget.init 0 0 "widget"

        (gSkyboxModel, gSCmd) = GS.initialModel

        (gTextureModel, gTCmd) = GT.initialModel svgTextures

        -- This is probably a really hacky solution
        dummyModel = 
            { time = 0
            , meshStore = 
                { generatedMeshes = Dict.empty, generatedShadows = Dict.empty }
            , textureLoader = TL.init 
            }

        initialBees =
            List.map
                ( \ bee -> bee TL.init { generatedMeshes = Dict.empty, generatedShadows = Dict.empty } 0 )
                myBees
    in
    ( { width = Quantity.zero
      , height = Quantity.zero
      , time = 0
      , lastCollisionTime = 0
      , orbiting = False
      , azimuth = Angle.degrees 0
      , elevation = Angle.degrees 30
      , meshStore = { generatedMeshes = Dict.empty, generatedShadows = Dict.empty }
      , widget = wModel
      , gSkyboxModel = gSkyboxModel
      , beePos = Point3d.origin
      , camPos = Point3d.origin
      , beeRot = Angle.degrees 0
      , velocity = Vector3d.zero
      , rotVelocity = 0
      , dirFB = None
      , dirLR = None
      , dirUD = None
      , rotLR = None
      , bee = defaultBee
      , beeModels = Array.fromList myBees
      , beeIndex = 0
      , beePollen = 0
      , storedPollen = 0
      , storedHoney = 0
      , choosingBee = True
      , mapFlowers = []
      , flowerCollisionTimes = Dict.empty
      , gTextureModel = gTextureModel
      , textureLoader = TL.init
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
        , Task.perform 
            ( \ _ -> 
                GenerateMeshes 
                    ( myEntities dummyModel ++ initialBees )
            ) 
            (Task.succeed True)
        , Task.perform (\_ -> InitializeFlowers) (Task.succeed True)
        , case skyboxType of
            Skybox.GSVGSkybox _ _ _ _ _ ->
                Cmd.map SkyboxMsg gSCmd

            _ ->
                Cmd.none
        , Cmd.map GSVGTextureMsg gTCmd
        ]
    )

update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Resize width height ->
            let
                ( wModel, wCmd ) =
                    Widget.init (toFloat <| unwrapQ width) (toFloat <| unwrapQ height) "widget"
            in
            ( { model | width = width, height = height, widget = wModel }, Cmd.map WidgetMsg wCmd )

        Tick t ->
            let
                tickRate =
                    Duration.milliseconds 1 |> Quantity.per Duration.second

                updatedTime =
                    Duration.seconds model.time |> Quantity.plus (tickRate |> Quantity.for t)

                timeAsNum =
                    Duration.inSeconds updatedTime

                -- This should only be used for collision detection
                updatedBee =
                    let
                        rotatedBee =
                            if model.choosingBee then
                                model.bee model.textureLoader model.meshStore model.time

                            else
                                model.bee model.textureLoader model.meshStore model.time
                                    |> rotateZ3D (Angle.inRadians model.beeRot)

                        position =
                            let
                                coords =
                                    Point3d.toRecord Length.inCentimeters model.beePos
                            in
                                ( coords.x, coords.y, coords.z )
                    in
                        rotatedBee
                        |> move3D position
                        |> move3D ( 0, 0, 5 * sin model.time )

                collided = isColliding updatedBee (myEntities model)

                collidedWithFlower = isColliding updatedBee model.mapFlowers

                collidedWithBeehive = isCollidingByName updatedBee (myEntities model) "beehive"

                collisionOnCooldown = model.time < model.lastCollisionTime + collisionCooldown

                collidingFlowers = getCollisions updatedBee model.mapFlowers

                -- This updates the last collision time of each flower that was bumped into, leaving the rest of them alone
                updatedFlowerTimes = 
                    if not collidedWithFlower then
                        model.flowerCollisionTimes
                    else
                        Dict.fromList (updatedFlowerTimesHelper collidingFlowers)  

                updatedFlowerTimesHelper flowers = 
                    case flowers of
                        [] -> []
                        flower :: rest ->
                            let
                                dictAsList = Dict.toList model.flowerCollisionTimes
                            in
                                case flower of
                                    Object attr ->
                                        List.map 
                                            ( \ (index, origTime) -> 
                                                if index == attr.name && not (pollenOnCooldown [flower]) then 
                                                    (index, timeAsNum) 
                                                else 
                                                    (index, origTime) 
                                            ) 
                                            dictAsList 
                                        ++ updatedFlowerTimesHelper rest
                                    ObjectGroup attr ->
                                        List.map 
                                            ( \ (index, origTime) -> 
                                                if index == attr.name && not (pollenOnCooldown [flower]) then 
                                                    (index, timeAsNum) 
                                                else 
                                                    (index, origTime) 
                                            ) 
                                            dictAsList 
                                        ++ updatedFlowerTimesHelper rest

                -- For simplicity, if one flower in the group that you collided with is off cooldown, then you get pollen
                pollenOnCooldown flowers = 
                    case flowers of
                        [] -> True
                        flower :: rest ->
                            let
                                flowerName =
                                    case flower of
                                        ObjectGroup attr -> attr.name
                                        Object attr -> attr.name
                                timeFromDict = Maybe.withDefault 0 (Dict.get flowerName model.flowerCollisionTimes)
                            in
                                not (timeFromDict < 1e-6) && (model.time < timeFromDict + pollenCooldown) && pollenOnCooldown rest

                updatedStoredPollen = 
                    if not collisionOnCooldown && collidedWithBeehive && model.storedPollen > honeyCreationRate then
                        model.storedPollen + toFloat model.beePollen - honeyCreationRate
                    else if not collisionOnCooldown && collidedWithBeehive then
                        model.storedPollen + toFloat model.beePollen
                    else if model.storedPollen > honeyCreationRate then
                        model.storedPollen - honeyCreationRate
                    else
                        model.storedPollen

                updatedStoredHoney = 
                    if model.storedPollen > honeyCreationRate then
                        model.storedHoney + honeyCreationRate
                    else
                        model.storedHoney

                updatedBeePollen = 
                    if not collisionOnCooldown && collidedWithBeehive then
                        0
                    else if not (pollenOnCooldown collidingFlowers) && collidedWithFlower && model.beePollen < maxPollen then
                        model.beePollen + 1
                    else
                        model.beePollen

                -- Bee moves by adding different vectors to its velocity depending on which keys are being pressed
                -- This allows for movement in more than 1 direction at a time
                updatedVelocity =
                    -- If the player is choosing bee models, the bee will stop moving
                    if model.choosingBee then
                        model.velocity
                            |> Vector3d.minus (Vector3d.scaleBy 0.1 model.velocity)
                    -- If the bee bumps into an object, it should bounce backwards a little
                    else if not collisionOnCooldown && (collided || collidedWithFlower) then
                        let
                            -- This scaling is done in order to prevent the bee from just being able
                            -- to phase through any objects by moving through them slowly.
                            -- The specific numbers were just obtained through trial and error.
                            bounceFactor =
                                -- This first check should prevent the bee from being launched into orbit in some edge cases
                                if Length.inCentimeters (Vector3d.length model.velocity) < 1e-6 then
                                    0
                                else if Length.inCentimeters (Vector3d.length model.velocity) < 3 then
                                    6 / Length.inCentimeters (Vector3d.length model.velocity)
                                else if Length.inCentimeters (Vector3d.length model.velocity) < 6 then
                                    15 / Length.inCentimeters (Vector3d.length model.velocity)
                                else if Length.inCentimeters (Vector3d.length model.velocity) < 10 then
                                    20 / Length.inCentimeters (Vector3d.length model.velocity)
                                else
                                    2.5
                        in
                            model.velocity
                            |> Vector3d.minus (Vector3d.scaleBy bounceFactor model.velocity)
                    -- Otherwise it works as normal
                    else
                        model.velocity
                            |> Vector3d.plus
                                (if not (model.dirFB == None) then
                                    Vector3d.withLength (Length.centimeters speed) (directionConverter model model.dirFB)
                                else
                                    Vector3d.zero
                                )
                            |> Vector3d.plus
                                (if not (model.dirLR == None) then
                                    Vector3d.withLength (Length.centimeters speed) (directionConverter model model.dirLR)
                                else
                                    Vector3d.zero
                                )
                            |> Vector3d.plus
                                (if not (model.dirUD == None) then
                                    if model.dirUD == Down && hitGround then
                                        Vector3d.zero
                                    else
                                        Vector3d.withLength (Length.centimeters speed) (directionConverter model model.dirUD)
                                else
                                    Vector3d.zero
                                )
                            |> Vector3d.minus (Vector3d.scaleBy 0.1 model.velocity)

                updatedBeePos =
                    if goingDown && hitGround then
                        let
                            x =
                                Vector3d.xComponent model.velocity

                            y =
                                Vector3d.yComponent model.velocity

                            velocityNoZ =
                                Vector3d.xyz x y (Length.centimeters 0)
                        in
                        model.beePos
                            |> Point3d.translateBy velocityNoZ
                    else
                        model.beePos
                            |> Point3d.translateBy model.velocity

                newBeeRot =
                    case model.rotLR of
                        RotLeft -> 
                            model.beeRot |> Quantity.plus (Angle.degrees rotSpeed)

                        RotRight ->
                            model.beeRot |> Quantity.minus (Angle.degrees rotSpeed)

                        _ -> 
                            model.beeRot

                goingDown =
                    not (Quantity.greaterThanOrEqualTo (Length.centimeters 0) (Vector3d.zComponent model.velocity))

                hitGround =
                    not (Quantity.greaterThanOrEqualTo (Length.centimeters 0) (Point3d.zCoordinate model.beePos))

            in
            ( { model
                | time = timeAsNum
                , camPos = updatedBeePos |> Point3d.translateIn Direction3d.z (Length.centimeters 30)
                , velocity = updatedVelocity
                , beePos = updatedBeePos
                , beeRot = newBeeRot
                , azimuth = newBeeRot
                , beePollen = updatedBeePollen
                , storedPollen = updatedStoredPollen
                , storedHoney = updatedStoredHoney
                , lastCollisionTime = 
                    if not collisionOnCooldown && (collided || collidedWithFlower) then
                        model.time
                    else
                        model.lastCollisionTime
                , flowerCollisionTimes = updatedFlowerTimes
              }
            , Cmd.batch
                [ Task.perform 
                    ( \ _ -> GenerateMeshes ( myEntities model ) )
                    (Task.succeed True)
                ]
            )

        MouseDown ->
            ( { model | orbiting = True }, Cmd.none )

        MouseUp ->
            ( { model | orbiting = False }, Cmd.none )

        VisibilityChange Browser.Events.Visible ->
            ( model, Cmd.none )

        VisibilityChange Browser.Events.Hidden ->
            ( { model | orbiting = False }, Cmd.none )

        MouseMove dx dy ->
            if model.orbiting then
                let
                    rotationRate =
                        Angle.degrees 0.5 |> Quantity.per Pixels.pixel

                    newAzimuth =
                        model.beeRot
                            |> Quantity.minus (dx |> Quantity.at rotationRate)

                    newElevation =
                        model.elevation
                            |> Quantity.plus (dy |> Quantity.at rotationRate)
                            |> Quantity.clamp (Angle.degrees -5) (Angle.degrees 85)
                in
                ( { model
                    | orbiting = True
                    , azimuth = newAzimuth
                    , beeRot = newAzimuth
                    , elevation = newElevation
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        KeyDown key ->
            let
                dir =
                    toDirection key
            in
            case dir of
                Left ->
                    ( { model
                        | dirLR = Left
                      }
                    , Cmd.none
                    )

                Right ->
                    ( { model
                        | dirLR = Right
                      }
                    , Cmd.none
                    )

                Forward ->
                    ( { model
                        | dirFB = Forward
                      }
                    , Cmd.none
                    )

                Backward ->
                    ( { model
                        | dirFB = Backward
                      }
                    , Cmd.none
                    )

                Up ->
                    ( { model
                        | dirUD = Up
                      }
                    , Cmd.none
                    )

                Down ->
                    ( { model
                        | dirUD = Down
                      }
                    , Cmd.none
                    )

                RotLeft ->
                    ( { model
                        | rotLR = RotLeft
                      }
                    , Cmd.none
                    )

                RotRight ->
                    ( { model
                        | rotLR = RotRight
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        KeyUp key ->
            let
                dir =
                    toDirection key
            in
            case dir of
                Left ->
                    ( { model | dirLR = None }, Cmd.none )

                Right ->
                    ( { model | dirLR = None }, Cmd.none )

                Forward ->
                    ( { model | dirFB = None }, Cmd.none )

                Backward ->
                    ( { model | dirFB = None }, Cmd.none )

                Up ->
                    ( { model | dirUD = None }, Cmd.none )

                Down ->
                    ( { model | dirUD = None }, Cmd.none )

                RotLeft ->
                    ( { model | rotLR = None }, Cmd.none )

                RotRight ->
                    ( { model | rotLR = None }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GenerateMeshes objectList ->
            case objectList of
                [] ->
                    (model, Cmd.none)
                (object :: rest) ->
                    case object of
                        ObjectGroup attr ->
                            ( model, Cmd.batch [ Task.perform ( \ _ -> GenerateMeshes ( attr.subObjects ++ rest ) ) (Task.succeed True) ])
                        Object attr ->
                            case attr.customMesh of
                                -- Skip generating if the object uses a primitive mesh
                                Primitive ->
                                    ( model, Cmd.batch [ Task.perform (\_ -> GenerateMeshes rest) (Task.succeed True) ])

                                -- Otherwise, check if the mesh already exists, and generate the mesh + shadow and store it if necessary
                                _ ->
                                    let
                                        meshExists =
                                            Dict.member (getMeshName attr.customMesh) model.meshStore.generatedMeshes
                                    in
                                        if meshExists then
                                            -- Don't regenerate if the mesh already exists
                                            ( model, Cmd.batch [ Task.perform (\_ -> GenerateMeshes rest) (Task.succeed True) ] )
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

                                                updatedMeshes = Dict.insert generatedMesh.name generatedMesh.mesh model.meshStore.generatedMeshes
                                                
                                                updatedShadows = Dict.insert generatedMesh.name generatedMesh.shadow model.meshStore.generatedShadows

                                                updatedMeshStore = { generatedMeshes = updatedMeshes, generatedShadows = updatedShadows }
                                            in
                                                ( { model | meshStore = updatedMeshStore }, Cmd.batch [ Task.perform (\_ -> GenerateMeshes rest) (Task.succeed True) ] )

        -- This is needed for our widget
        WidgetMsg wMsg ->
            let
                ( newWModel, wCmd ) =
                    Widget.update wMsg model.widget
            in
            ( { model | widget = newWModel }, Cmd.map WidgetMsg wCmd )

        Reset ->
            ( { model | azimuth = Angle.degrees 0, elevation = Angle.degrees 30 }, Cmd.none )

        SkyboxMsg sMsg ->
            let
                (gSkyboxModel, gSCmd) = GS.update sMsg model.gSkyboxModel
            in
                ( { model | gSkyboxModel = gSkyboxModel } , Cmd.map SkyboxMsg gSCmd)

        ChangeBee increasing ->
            let
                newIndex =
                    if increasing then
                        if model.beeIndex + 1 >= Array.length model.beeModels then
                            0

                        else
                            model.beeIndex + 1

                    else if model.beeIndex - 1 < 0 then
                        Array.length model.beeModels - 1

                    else
                        model.beeIndex - 1

                arrElem =
                    Array.get newIndex model.beeModels

                newBee =
                    case arrElem of
                        Just theBee ->
                            theBee

                        Nothing ->
                            defaultBee

                -- Make sure a default bee is defined somewhere!
            in
            ( { model | beeIndex = newIndex, bee = newBee }, Cmd.none )

        ToggleBeePicker ->
            if model.choosingBee then
                ( { model | choosingBee = False }, Cmd.none )

            else
                ( { model | choosingBee = True }, Cmd.none )

        InitializeFlowers ->
            let
                listFlowers = List.indexedMap nameFlower (myFlowers model)
                nameFlower index obj = 
                    case obj of
                        Object _ ->
                            obj 
                            |> nameObject ("flower" ++ String.fromInt index)
                        ObjectGroup attr ->
                            ObjectGroup
                                { attr 
                                | subObjects = List.map (nameFlower index) attr.subObjects
                                , name = "flower" ++ String.fromInt index 
                                }
                makeCollisionTimes =
                    \ object ->
                        case object of
                            ObjectGroup attr ->
                                (attr.name, 0)
                            Object attr ->
                                (attr.name, 0)

            in
                ( { model 
                  | flowerCollisionTimes = Dict.fromList (List.map makeCollisionTimes listFlowers)
                  , mapFlowers = listFlowers
                  }
                , Cmd.none 
                )

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
            (
                { model | textureLoader = tlModel }
            ,   Cmd.map TextureLoadMsg tlCmd
            )

mouseMoveDecoder : Decoder Msg
mouseMoveDecoder =
    Decode.map2 MouseMove
        (Decode.field "movementX" (Decode.map Pixels.pixels Decode.float))
        (Decode.field "movementY" (Decode.map Pixels.pixels Decode.float))

keyDecoder : Decoder Msg
keyDecoder =
    Decode.map KeyDown (Decode.field "key" Decode.string)

keyUpDecoder : Decoder Msg
keyUpDecoder =
    Decode.map KeyUp (Decode.field "key" Decode.string)

-- Converts a key "code" to a Direction
toDirection : String -> Direction
toDirection string =
    case string of
        "ArrowLeft" ->
            RotLeft

        "a" ->
            RotLeft

        "A" ->
            RotLeft

        "ArrowRight" ->
            RotRight

        "d" ->
            RotRight

        "D" ->
            RotRight

        "ArrowUp" ->
            Forward

        "w" ->
            Forward

        "W" ->
            Forward

        "ArrowDown" ->
            Backward

        "s" ->
            Backward

        "S" ->
            Backward

        "q" ->
            Left

        "Q" ->
            Left

        "e" ->
            Right

        "E" ->
            Right

        " " ->
            Up

        "Shift" ->
            Down

        _ ->
            None

-- Converts a Direction to a Direction3d that is relative to the bee
directionConverter : Model -> Direction -> Direction3d coords
directionConverter model dir =
    let
        forwardDir =
            Direction3d.xyZ model.beeRot (Angle.degrees 0)

        backwardDir =
            Direction3d.xyZ (model.beeRot |> Quantity.plus (Angle.degrees 180)) (Angle.degrees 0)

        rightDir =
            Direction3d.xyZ (model.beeRot |> Quantity.plus (Angle.degrees 270)) (Angle.degrees 0)

        leftDir =
            Direction3d.xyZ (model.beeRot |> Quantity.plus (Angle.degrees 90)) (Angle.degrees 0)

        upDir =
            Direction3d.xyZ model.beeRot (Angle.degrees 90)

        downDir =
            Direction3d.xyZ model.beeRot (Angle.degrees -90)
    in
    case dir of
        Forward ->
            forwardDir

        Backward ->
            backwardDir

        Left ->
            leftDir

        Right ->
            rightDir

        Up ->
            upDir

        Down ->
            downDir

        _ ->
            downDir

{-html
<!DOCTYPE HTML>
<html>
<head>
    <meta charset="UTF-8">
    <title>Main</title>
    <link rel="icon" href="favicon.ico?v=1" />
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
