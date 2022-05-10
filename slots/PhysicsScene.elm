module PhysicsScene exposing (main)

import Acceleration exposing (Acceleration)
import Angle exposing (Angle)
import Arc2d
import Arc3d
import Array exposing (Array)
import Axis3d
import Block3d exposing (Block3d)
import Browser
import Browser.Dom
import Browser.Events
import BoundingBox3d exposing (BoundingBox3d)
import Camera3d exposing (Camera3d)
import Circle3d
import Collision exposing (..)
import Color exposing (Color)
import Cone3d
import Cylinder3d
import Density exposing (Density)
import Dict exposing (Dict)
import Direction3d exposing (Direction3d)
import Duration exposing (Duration)
import Frame3d
import Mass
import Force
import GraphicSVG.Widget as Widget
import GraphicSVG exposing(..)
import GSVGSkybox as GS
import GSVGTexture as GT exposing (svgTexture)
import Html exposing (Html)
import Html.Attributes as HA exposing (style)
import Html.Events.Extra.Wheel as Wheel
import Illuminance
import Json.Decode as Decode exposing (Decoder)
import Length exposing (Length, Meters)
import LineSegment3d
import List
import LuminousFlux exposing (LuminousFlux)
import Parameter1d
import Physics.World as World exposing (World)
import Physics.Body as Body exposing (Body)
import Physics.Coordinates exposing (WorldCoordinates, BodyCoordinates)
import Physics.Constraint as Constraint exposing (Constraint)
import Physics.Shape as Shape
import PhysicsWrapper3D exposing (..)
import Pixels exposing (Pixels)
import Point2d
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import Round
import Scene3d exposing (Entity)
import Scene3d.Light as Light exposing (Chromaticity, Light)
import Scene3d.Material as Material exposing (Material)
import Scene3d.Mesh as Mesh
import SketchPlane3d
import Skybox
import SolidAngle
-- import SpaceshipPart exposing (movePart)
import Sphere3d
import Speed
import Task
import Temperature
import TextureLoader as TL exposing (loadTexture, TextureType(..), getColorTexture, getRoughnessTexture, getMetallicityTexture)
import Triangle3d
import TriangularMesh
import Vector3d exposing (Vector3d)
import Viewpoint3d
import WebGL.Texture
import Wrapper3D exposing (..)
import Wrapper3DCamera as W3C
import Skybox exposing (SkyboxType(..))

{-header<b>3D Physics Slot!</b>: Make things in 3D that physically move! -}

{- 3D Physics Slot for MOR. -}

{-editable-}
-- Put your physics objects here!
myPhysicsObjects model =
    [   -- You can make physics apply to 3D objects! All you need to do is pipe it into "usePhysics", and give it a name.
        box 30 100 30
            |> plastic Color.red 1
            |> move3D (0,0,0)
            |> usePhysics "1x torque"
            |> withMassU (Mass.kilograms 1000)
            |> push
                (Force.newtons 100)
                (Direction3d.xyZ (Angle.degrees 180) (Angle.degrees 0))
                (Point3d.centimeters -15 15 0)
            |> push
                (Force.newtons 100)
                (Direction3d.xyZ (Angle.degrees 0) (Angle.degrees 0))
                (Point3d.centimeters 15 -15 0)
            |> showVisualizers
    ,   box 30 100 30
            |> plastic Color.white 1
            |> move3D (0,-150,0)
            |> usePhysics "2x torque"
            |> withMassU (Mass.kilograms 1000)
            |> push
                (Force.newtons 100)
                (Direction3d.xyZ (Angle.degrees 180) (Angle.degrees 0))
                (Point3d.centimeters -15 30 0)
            |> push
                (Force.newtons 100)
                (Direction3d.xyZ (Angle.degrees 0) (Angle.degrees 0))
                (Point3d.centimeters 15 -30 0)
            |> showVisualizers
    ,   box 30 100 30
            |> plastic Color.blue 1
            |> move3D (0,150,0)
            |> usePhysics "no torque"
            |> withMassU (Mass.kilograms 1000)
            |> push
                (Force.newtons 100)
                (Direction3d.xyZ (Angle.degrees 180) (Angle.degrees 0))
                (Point3d.centimeters -15 0 0)
            |> push
                (Force.newtons 100)
                (Direction3d.xyZ (Angle.degrees 0) (Angle.degrees 0))
                (Point3d.centimeters 15 0 0)
            |> showVisualizers
    ]

-- Put your update functions here! Try using "pushIf" or "updateIf"
myPhysicsUpdates : Model -> List (Body PhysicsData -> Body PhysicsData)
myPhysicsUpdates model =
    [ -- This call to "pushIf" will apply a 100N force downwards to the body called "1x torque",
      -- after 5 seconds have passed in the simulation.
    --   pushIf
    --     "1x torque" -- Name of body
    --     ( \ body -> model.time > 5 ) -- Any function that takes a body and returns a bool
    --     (Force.newtons 100) -- Force to apply
    --     (Direction3d.xyZ (Angle.degrees 0) (Angle.degrees (-90))) -- Relative direction
    --     (Point3d.centimeters 0 0 0) -- Relative location at which the force is applied
    ]

useGravity = False

-- Set this to true show position, velocity, and force visualizers for ALL bodies
showPhysicsVisualizers = True

-- Distance that the camera moves when you click the buttons in the top right corner
cameraMoveDistance = Length.centimeters 25

skyboxSize = Length.meters 20

-- Create a quad to act as a 'floor'
floor =
    square3D (floorSize * 100)
    |> matte floorColour
    |> move3D (0,0,-100)
    |> usePhysics "floor"

-- move / edit the light
light =
    pointLight
        { position = Point3d.centimeters 0 0 100    -- position of the light
        , chromaticity = Light.sunlight             -- the colour of the light (see https://package.elm-lang.org/packages/ianmackenzie/elm-3d-scene/latest/Scene3d-Light#Chromaticity)
        , intensity = LuminousFlux.lumens 10000     -- how intense the light is
        }
showLight = True -- whether to show the light ball or not

-- Use "loadTexture [name] [url]" to load in texture images from the Internet!
-- Give each one a unique name.
-- You can list many of them!
myTextures = 
    [ loadTexture "example" TexColor "Put an image URL here!"
    ]

-- Usage: `svgTexture "name" "name`, where shape is any 2D shape or group
-- Give each one a unique name.
-- You can list many of them!
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

-- Put your 2D shapes here, and they will be overlayed on top of the screen!
overlay : Model -> List (Shape Msg)
overlay model =
    [   angleDisplay model
    ,   cameraControls model
    ,   bodyTracker "no torque" model 30 (hsl (degrees 225) 1 0.65) black False
    ,   bodyTracker "1x torque" model 30 GraphicSVG.red black False
    ,   bodyTracker "2x torque" model 30 GraphicSVG.white black False
    ]

-- This colour is used to create the floor. If you want custom colours, use Color.hsl or Color.rgb!
floorColour = Color.green

-- Floor size, in metres. Set to 0 to disable.
floorSize = 2.5

-- Here you can specify what images to use to create the skybox. Just replace "todo" with a link to an image. (Keep the quotes, though!)
skyboxType =
    Skybox.URLSkybox textureBottom textureTop textureSide1 textureSide2 textureSide3 textureSide4 (skyboxSize |> Length.inCentimeters)
    -- Some other options (comment in the one above and comment one of these out)
    -- Skybox.GSVGSkybox False skyboxTop skyboxSides skyBoxBottom (skyboxSize |> Length.inCentimeters)
    -- Skybox.GSVGSphericalSkybox False skyboxTop (skyboxSize |> Length.inCentimeters)
    -- Skybox.URLSphericalSkybox "https://cschank.github.io/img/milky.jpg" (skyboxSize |> Length.inCentimeters)

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

debugMeshNames model = text (Debug.toString (List.map (\(key,_) -> key) (Dict.toList model.meshStore.generatedMeshes))) |> filled black    

type alias Model =
    { width : Quantity Int Pixels
    , height : Quantity Int Pixels
    , time : Float
    , tlTime : Float
    , meshStore : MeshStore BodyCoordinates
    , widget : Widget.Model
    , gSkyboxModel : GS.Model
    , gTextureModel : GT.Model
    , textureLoader : TL.Model
    , cameraModel : (W3C.Model WorldCoordinates)
    , world : World PhysicsData
    , state : State
    , initialized : Bool
    }

type State
    = Loaded
    | Loading

type Msg
    = Resize (Quantity Int Pixels) (Quantity Int Pixels)
    | Tick Duration
    | GenerateMeshes (List (Object WorldCoordinates))
    | WidgetMsg Widget.Msg
    | SkyboxMsg GS.Msg
    | GSVGTextureMsg GT.Msg
    | TextureLoadMsg TL.Msg
    | CameraMsg (W3C.Msg WorldCoordinates)
    | AddBodies

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
    ( Light.point (Light.castsShadows False) properties
    , Scene3d.sphere sphereMaterial lightsphere
    )

adjustZoom : Wheel.Event -> Msg
adjustZoom wheelEvent =
    let
        zoomSpeed =
            Length.centimeters 15
        deltaZoom =
            if wheelEvent.deltaY > 0 then
                zoomSpeed
            else
                zoomSpeed |> Quantity.negate
    in
        CameraMsg (W3C.AdjustZoom deltaZoom)

view : Model -> Html Msg
view model =
    let
        -- Incandescent light bulb
        ( firstLight, firstLightBall ) =
            light

        -- Rough approximation of sunlight
        thirdLight =
            Light.directional (Light.castsShadows True)
                { direction = Direction3d.xyZ (Angle.degrees -90) (Angle.degrees -45)
                , chromaticity = Light.sunlight
                , intensity = Illuminance.lux 100
                }

        -- Add some soft lighting to fill in shadowed areas
        softLighting =
            Light.soft
                { upDirection = Direction3d.positiveZ
                , chromaticity = Light.fluorescent
                , intensityAbove = Illuminance.lux 30
                , intensityBelow = Illuminance.lux 5
                }

        textures = model.textureLoader.colorTextures

        baseEntities =
            (if showLight then [ firstLightBall ] else []) ++
            [ case skyboxType of
                Skybox.GSVGSkybox _ _ _ _ size ->
                    Skybox.skybox 
                        [ Dict.get "skyB" textures
                        , Dict.get "skyT" textures
                        , Dict.get "skyS1" textures
                        , Dict.get "skyS2" textures
                        , Dict.get "skyS3" textures
                        , Dict.get "skyS4" textures
                        ]
                        size
                Skybox.URLSkybox _ _ _ _ _ _ size ->
                    Skybox.skybox 
                        [ Dict.get "skyB" textures
                        , Dict.get "skyT" textures
                        , Dict.get "skyS1" textures
                        , Dict.get "skyS2" textures
                        , Dict.get "skyS3" textures
                        , Dict.get "skyS4" textures
                        ]
                        size
                Skybox.URLSphericalSkybox _ size ->
                    Skybox.roundSkybox
                        (Dict.get "skyT" textures)
                        size
                Skybox.GSVGSphericalSkybox _ _ size ->
                    Skybox.roundSkybox
                        (Dict.get "skyT" textures)
                        size
                BasicSkybox _ ->
                    Scene3d.nothing
            ]

    in
        case model.state of
            Loaded ->
                Html.div [ Wheel.onWheel adjustZoom ]
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
                            { lights = Scene3d.threeLights firstLight thirdLight softLighting
                            , camera = model.cameraModel.camera
                            , clipDepth = Length.centimeters 10
                            , exposure = Scene3d.exposureValue 6
                            , toneMapping = Scene3d.hableFilmicToneMapping
                            , whiteBalance = Light.fluorescent
                            , antialiasing = Scene3d.multisampling
                            , dimensions = ( model.width, model.height )
                            , background =
                                case skyboxType of
                                    BasicSkybox colour ->
                                        Scene3d.backgroundColor colour
                                    _ ->
                                        Scene3d.backgroundColor Color.lightBlue
                            , entities = baseEntities ++ renderEntities (renderBodies showPhysicsVisualizers model.world)
                            }
                            |> withOverlay (overlay model) model
                    -- ,  Html.map GSVGTextureMsg <| GT.drawTextures False model.gTextureModel
                    ]
            Loading ->
                Html.div []
                    [ Html.text "Loading..."
                    , Html.br [] []
                    , Html.text ("Fetched " ++ String.fromInt model.textureLoader.numberFinished ++ " textures out of the " ++ String.fromInt (Dict.size model.textureLoader.requestedURLs) ++ " that were requested.")
                    , Html.br [] []
                    , Html.text ("Timing out and loading anyways in " ++ Round.round 2 (10 - model.tlTime) ++ " seconds.")
                    , Html.map GSVGTextureMsg <| GT.drawTextures False model.gTextureModel
                    ]

angleDisplay : Model -> Shape Msg
angleDisplay model = group
    [
        text ("azimuth: " ++ String.fromInt (round <| unwrapQ model.cameraModel.azimuth * 180 / pi) ++ "º")
                                |> filled black
                                |> move (toFloat (unwrapQ model.width) / 2 - 160, toFloat (unwrapQ model.height) / 2 - 50)
    ,   text ("elevation: " ++ String.fromInt (round <| unwrapQ model.cameraModel.elevation * 180 / pi) ++ "º")
            |> filled black
            |> move (toFloat (unwrapQ model.width) / 2 - 160, toFloat (unwrapQ model.height) / 2 - 60)
    ]

cameraControls : Model -> Shape Msg
cameraControls model =
  let
    cameraButton angle =
      group
        [ roundedRect 40 40 10
          |> filled green
        , text "|"
          |> size 16
          |> centered
          |> sansserif
          |> filled black
          |> move (0,-2.5)
          |> rotate angle
        , text "v"
          |> size 16
          |> centered
          |> sansserif
          |> filled black
          |> move (0,-7.5)
          |> rotate angle
        ]
    modeToggle =
      group
        [ roundedRect 140 40 10
          |> filled green
        , text "Switch Camera Mode"
          |> size 12
          |> centered
          |> sansserif
          |> filled black
          |> move (0,-4)
        ]
      |> notifyTap
        ( CameraMsg
          <| W3C.SetCameraMode
          <| W3C.alternateCameraMode model.cameraModel.cameraMode
        )
    modeDisplay =
      group
        [ roundedRect 140 40 10
          |> filled green
        , text ("Current: " ++ (W3C.cameraModeToString model.cameraModel.cameraMode))
          |> size 12
          |> centered
          |> sansserif
          |> filled black
          |> move (0,-4)
        ]
    standardControls =
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
          |> notifyTap (CameraMsg W3C.Reset)
        -- Forward
        , cameraButton (degrees 180)
          |> move (toFloat (unwrapQ model.width) / 2 - 125, toFloat (unwrapQ model.height) / 2 - 150)
          |> notifyTap (CameraMsg (W3C.MoveCamera Direction3d.x cameraMoveDistance))
        -- Backward
        , cameraButton (degrees 0)
          |> move (toFloat (unwrapQ model.width) / 2 - 125, toFloat (unwrapQ model.height) / 2 - 200)
          |> notifyTap (CameraMsg (W3C.MoveCamera Direction3d.x (Quantity.negate cameraMoveDistance)))
        -- Left
        , cameraButton (degrees 270)
          |> move (toFloat (unwrapQ model.width) / 2 - 175, toFloat (unwrapQ model.height) / 2 - 200)
          |> notifyTap (CameraMsg (W3C.MoveCamera Direction3d.y cameraMoveDistance))
        -- Right
        , cameraButton (degrees 90)
          |> move (toFloat (unwrapQ model.width) / 2 - 75, toFloat (unwrapQ model.height) / 2 - 200)
          |> notifyTap (CameraMsg (W3C.MoveCamera Direction3d.y (Quantity.negate cameraMoveDistance)))
        -- Up
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
          |> notifyTap (CameraMsg (W3C.MoveCamera Direction3d.z cameraMoveDistance))
        -- Down
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
          |> notifyTap (CameraMsg (W3C.MoveCamera Direction3d.z (Quantity.negate cameraMoveDistance)))
        -- Zoom In
        , group
            [ roundedRect 40 40 10
              |> filled green
            , text "+"
              |> size 28
              |> centered
              |> sansserif
              |> filled black
              |> move (0,-10)
            ]
          |> move (toFloat (unwrapQ model.width) / 2 - 225, toFloat (unwrapQ model.height) / 2 - 150)
          |> notifyTap (CameraMsg (W3C.AdjustZoom (Length.meters (-0.5))))
        -- Zoom Out
        , group
            [ roundedRect 40 40 10
              |> filled green
            , text "-"
              |> size 28
              |> centered
              |> sansserif
              |> filled black
              |> move (0,-8)
            ]
          |> move (toFloat (unwrapQ model.width) / 2 - 225, toFloat (unwrapQ model.height) / 2 - 200)
          |> notifyTap (CameraMsg (W3C.AdjustZoom (Length.meters 0.5)))
        -- Camera Mode
        , modeToggle
          |> move (toFloat (unwrapQ model.width) / 2 - 125, toFloat (unwrapQ model.height) / 2 - 250)
        , modeDisplay
          |> move (toFloat (unwrapQ model.width) / 2 - 125, toFloat (unwrapQ model.height) / 2 - 300)
        ]
  in
    case model.cameraModel.cameraMode of
      W3C.OrbitUnlocked ->
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
            |> notifyTap (CameraMsg W3C.Reset)
          , modeToggle
            |> move (toFloat (unwrapQ model.width) / 2 - 125, toFloat (unwrapQ model.height) / 2 - 150)
          , modeDisplay
            |> move (toFloat (unwrapQ model.width) / 2 - 125, toFloat (unwrapQ model.height) / 2 - 200)
          ]
      _ ->
        standardControls
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

        {-, case skyboxType of
            Skybox.GSVGSkybox _ _ _ _ ->
                Sub.map SkyboxMsg (GS.subscriptions model.gSkyboxModel)
            _ -> Sub.none-}
        , Sub.map GSVGTextureMsg (GT.subscriptions model.gTextureModel)
        , Sub.map CameraMsg (W3C.subscriptions model.cameraModel)
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
      , tlTime = 0
      , meshStore = { generatedMeshes = Dict.empty, generatedShadows = Dict.empty }
      , widget = wModel
      , gSkyboxModel = gSkyboxModel
      , gTextureModel = gTextureModel
      , cameraModel = W3C.basicCamera
      , textureLoader = TL.init
      , state = Loading
      , initialized = False
      , world =
            if useGravity then
                World.empty 
                |> World.withGravity (Acceleration.metersPerSecondSquared 0.0980665) Direction3d.negativeZ
            else
                World.empty
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

                clearForces body =
                    let
                        data = Body.data body
                    in
                        body |> Body.withData
                            { data | forces = [] }

                timeAsNum = Duration.inSeconds updatedTime
                simulatedWorld =
                    World.update applyForces model.world
                    |> World.update clearForces
                    |> World.simulate (tickRate |> Quantity.for t)

                updatedWorld =
                    List.foldl
                        ( \ updateFunc world -> world |> World.update updateFunc )
                        simulatedWorld
                        (myPhysicsUpdates model)
                    |> World.update 
                        ( \ body ->
                            let
                                data = Body.data body
                            in
                                body |> Body.withData { data | initialized = True }
                        )
                    |> World.constrain constrainBodies

                addBodies = model.state == Loaded && not model.initialized

            in
                ( case model.state of
                    Loaded ->
                        { model | time = timeAsNum, world = updatedWorld, initialized = model.initialized || addBodies }
                    Loading ->
                        { model | tlTime = timeAsNum }
                , Cmd.batch
                    [ Task.perform 
                        ( \ _ -> GenerateMeshes ( renderBodies False model.world ) )
                        (Task.succeed True)
                    , if addBodies then
                        Task.perform
                            ( \ _ -> AddBodies )
                            (Task.succeed True)
                        else
                            Cmd.none
                    ]
                )

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
                                                ( { model | meshStore = updatedMeshStore }, Cmd.batch [ Task.perform (\_ -> GenerateMeshes rest) (Task.succeed True) ] )

        -- This is needed for our widget
        WidgetMsg wMsg ->
            let
                (newWModel, wCmd) = Widget.update wMsg model.widget
            in
            ( { model | widget = newWModel }, Cmd.map WidgetMsg wCmd )

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
                if TL.allTexturesLoaded tlModel || model.tlTime > 10 then
                    ( { model | textureLoader = tlModel, state = Loaded }
                    , Cmd.batch
                        [ Cmd.map TextureLoadMsg tlCmd
                        ]
                    )
                else
                    (   { model 
                        | textureLoader = tlModel
                        }
                    ,   Cmd.map TextureLoadMsg tlCmd
                    )

        CameraMsg camMsg ->
            let
                w3cModel = W3C.update camMsg model.cameraModel
            in
                ( { model | cameraModel = w3cModel }
                , Cmd.none
                )

        AddBodies ->
            case model.state of
                Loaded ->
                    ( { model 
                    | world = 
                            List.foldl 
                                ( \ body world -> world |> World.add body ) 
                                model.world (myPhysicsObjects model ++ [ floor ] )
                    }
                    , Cmd.none
                    )
                Loading ->
                    ( model
                    , Task.perform
                        ( \ _ -> AddBodies )
                        (Task.succeed True)
                    )

{-html
<!DOCTYPE HTML>
<html>
<head>
    <meta charset="UTF-8">
    <title>Main</title>
    <link rel="icon" href="favicon.ico?v=1" />
    <!-- This should let us use custom fonts properly -->
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
