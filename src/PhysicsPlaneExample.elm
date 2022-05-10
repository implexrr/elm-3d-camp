module PhysicsPlaneExample exposing (main)

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
import GSVGTexture as GT
import Html exposing (Html)
import Html.Attributes as HA exposing (style)
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
import SpaceshipPart exposing (movePart)
import Sphere3d
import Speed
import Task
import Temperature
import TextureLoader as TL
import Triangle3d
import TriangularMesh
import Vector3d exposing (Vector3d)
import Viewpoint3d
import WebGL.Texture
import Wrapper3D exposing (..)

{-header<b>3D Physics Slot!</b>: Make things in 3D that physically move! -}

{- 3D Physics Slot for MOR. -}

{-editable-}
-- Put your physics objects here!
myPhysicsObjects model =
    let
        useTexture = getTexture model.textureLoader.textures
    in
        [   -- You can make physics apply to 3D objects! All you need to do is pipe it into "usePhysics", and give it a name.
            cylinder 10 40 |> metallic Color.lightGray 0.2 |> rotate3D (degrees 90) 0 0 |> move3D (-20,0,0)
            |> usePhysics "planeBase"
            -- Afterwards, pipe it into "withMass" and give it a mass (in kilograms) to enable movement.
            -- You don't need this if you're just making a static object.
            |> withMass 100
            -- You can pipe the body into "push" to apply a constant force to it at all times
            |> push (Force.newtons 5) (Direction3d.xy (Angle.degrees 90)) (Point3d.centimeters 0 0 0)
            -- "inAtmosphere" simulates air resistance. If you don't want this, you can pipe into "inSpace" instead.
            |> inAtmosphere
        ,   polyCylinder [(25,0), (-25,0), (-25,-25), (25,0)] 1 model.meshStore |> metallic Color.lightGray 0.2 |> move3D (0,-10,-0.5)
            |> usePhysics "leftWing"
            |> withMass 1
            -- Using names, you can link objects to one another and make them move or rotate together
            |> linkTo "planeBase"
            |> inAtmosphere
        ,   polyCylinder (List.reverse [(25,0), (-25,0), (-25,25), (25,0)]) 1 model.meshStore |> metallic Color.lightGray 0.2 |> move3D (0,10,-0.5)
            |> usePhysics "rightWing"
            |> withMass 1
            |> linkTo "planeBase"
            |> inAtmosphere
        ,   cone 8 10 |> metallic Color.darkGray 0.5 |> rotate3D (degrees 90) 0 0 |> move3D (-30,0,0)
            |> usePhysics "thruster"
            |> withMass 25
            |> linkTo "planeBase"
            |> inAtmosphere
            |> showVisualizers
        ]

myPhysicsUpdates : Model -> List (Body PhysicsData -> Body PhysicsData)
myPhysicsUpdates model =
    [ -- This call to "pushIf" will apply a 100N force forwards to the body called "thruster",
      -- if that body's velocity is less than 25cm/s
      pushIf
        "thruster" -- Name of body
        ( \ body -> Vector3d.length (Body.velocity body) |> Quantity.lessThan ( Speed.metersPerSecond 0.25 ) ) -- Any function that takes a body and returns a bool
        (Force.newtons 100) -- Force to apply
        (Direction3d.xyZ (Angle.degrees 0) (Angle.degrees 0)) -- Relative direction
        (Point3d.centimeters -30 0 0) -- Relative location at which the force is applied
    ]

useGravity = False

-- Set this to true show position, velocity, and force visualizers for ALL bodies
showPhysicsVisualizers = False

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
    [ loadTexture "example" "Put an image URL here!"
    ]

-- Usage: `svgTexture "name" "name`, where shape is any 2D shape or group
-- Give each one a unique name.
-- You can list many of them!
svgTextures =
    [ svgTexture "squares" squares
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
    ,   physicsDebug "thruster" model 30 (hsl (degrees 225) 1 0.65)
    ]

-- This colour is used to create the floor. If you want custom colours, use Color.hsl or Color.rgb!
floorColour = Color.green

-- Floor size, in metres. Set to 0 to disable.
floorSize = 2.5

-- Here you can specify what images to use to create the skybox. Just replace "todo" with a link to an image. (Keep the quotes, though!)
skyboxType =
    Skybox.URLSkybox textureBottom textureTop textureSide1 textureSide2 textureSide3 textureSide4
    -- Some other options (comment in the one above and comment one of these out)
    -- Skybox.GSVGSkybox False skyboxTop skyboxSides skyBoxBottom
    -- Skybox.GSVGSphericalSkybox False skyboxTop
    -- Skybox.URLSphericalSkybox "https://cschank.github.io/img/milky.jpg"

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

physicsDebug : String -> Model -> Float -> GraphicSVG.Color -> Shape Msg
physicsDebug objToTrack model trackerSize trackerColour =
    let
        toTrack = List.filter (\body -> (Body.data body).name == objToTrack) (World.bodies model.world)
        trackingName =
            case List.head toTrack of
                Just body ->
                    (Body.data body).name
                Nothing ->
                    "Nothing"
        bodyMaybe = List.head toTrack
        relativeSize = trackerSize / 15
        tracker =
            case bodyMaybe of
                Just body ->
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
                        , text objToTrack
                          |> customFont "Audiowide"
                          |> size ( 12 * relativeSize )
                          |> filled trackerColour
                          -- |> addOutline ( solid ( 0.25 * relativeSize ) ) black
                          |> move ( 15 * relativeSize, 15 * relativeSize )
                        ]
                    |> move ( body |> adjustedPosition |> positionOnScreen model.camera (Pixels.toInt model.width, Pixels.toInt model.height) )
                Nothing ->
                    group []
        textLeft = -300 * toFloat (unwrapQ model.width) / 1920
        bodyInfo =
            case bodyMaybe of
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
                            [ text ("Name: " ++ trackingName) |> customFont "Audiowide" |> filled black |> move (textLeft,50)
                            , text ("Velocity: " ++ vectorToString (Body.velocity <| body)) |> customFont "Audiowide" |> filled black |> move (textLeft,35)
                            , text ("Origin Point: " ++ pointToString (Frame3d.originPoint <| frame)) |> customFont "Audiowide" |> filled black |> move (textLeft,20)
                            , text ("Rotational velocity: " ++ vectorToString (Body.angularVelocity <| body)) |> customFont "Audiowide" |> filled black |> move (textLeft,5)
                            , text ("x direction: " ++ dirToString (Frame3d.xDirection <| frame)) |> customFont "Audiowide" |> filled black |> move (textLeft,-10)
                            , text ("y direction: " ++ dirToString (Frame3d.yDirection <| frame)) |> customFont "Audiowide" |> filled black |> move (textLeft,-25)
                            , text ("z direction: " ++ dirToString (Frame3d.zDirection <| frame)) |> customFont "Audiowide" |> filled black |> move (textLeft,-40)
                            ] |> scale (2 * (toFloat (unwrapQ model.height) / 970)) |> move (0, -250 * toFloat (unwrapQ model.height) / 970)
                Nothing ->
                    text "Error: Invalid Physics Body" |> centered |> filled black
    in
    group
        [ bodyInfo
        , tracker
        ]
    

type alias Model =
    { width : Quantity Int Pixels
    , height : Quantity Int Pixels
    , time : Float
    , tlTime : Float
    , orbiting : Bool
    , azimuth : Angle
    , elevation : Angle
    , cameraPos : Point3d Meters WorldCoordinates
    , camera : Camera3d Meters WorldCoordinates
    , meshStore : MeshStore BodyCoordinates
    , widget : Widget.Model
    , gSkyboxModel : GS.Model
    , gTextureModel : GT.Model
    , textureLoader : TL.Model
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
    | MouseDown
    | MouseMove (Quantity Float Pixels) (Quantity Float Pixels)
    | MouseUp
    | VisibilityChange Browser.Events.Visibility
    | GenerateMeshes (List (Object WorldCoordinates))
    | WidgetMsg Widget.Msg
    | Reset
    | MoveCamera (Direction3d WorldCoordinates) Length
    | SkyboxMsg GS.Msg
    | GSVGTextureMsg GT.Msg
    | TextureLoadMsg TL.Msg
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

        textures = model.textureLoader.textures

        baseEntities =
            (if showLight then [ firstLightBall ] else []) ++
            [ case skyboxType of
                Skybox.GSVGSkybox _ _ _ _ ->
                    Skybox.skybox 
                        [ Dict.get "skyB" textures
                        , Dict.get "skyT" textures
                        , Dict.get "skyS1" textures
                        , Dict.get "skyS2" textures
                        , Dict.get "skyS3" textures
                        , Dict.get "skyS4" textures
                        ]
                        (Length.inCentimeters skyboxSize)
                Skybox.URLSkybox _ _ _ _ _ _ ->
                    Skybox.skybox 
                        [ Dict.get "skyB" textures
                        , Dict.get "skyT" textures
                        , Dict.get "skyS1" textures
                        , Dict.get "skyS2" textures
                        , Dict.get "skyS3" textures
                        , Dict.get "skyS4" textures
                        ]
                        (Length.inCentimeters skyboxSize)
                Skybox.URLSphericalSkybox _ ->
                    Skybox.roundSkybox
                        (Dict.get "skyT" textures)
                        (Length.inCentimeters skyboxSize)
                Skybox.GSVGSphericalSkybox _ _ ->
                    Skybox.roundSkybox
                        (Dict.get "skyT" textures)
                        (Length.inCentimeters skyboxSize)
            ]

    in
        case model.state of
            Loaded ->
                Html.div []
                    [   case skyboxType of
                            Skybox.GSVGSkybox debug sT sS sB ->
                                Html.div [style "position" "absolute", style "left" "0px", style "top" (String.fromInt (unwrapQ model.height) ++ "px")]
                                [
                                    -- Html.h1 [] [Html.text "Skybox Debug"]
                                Html.map SkyboxMsg <| GS.drawSkybox debug model.gSkyboxModel sT sS sB
                                ]
                            Skybox.GSVGSphericalSkybox debug sT ->
                                Html.div [style "position" "absolute", style "left" "0px", style "top" (String.fromInt (unwrapQ model.height) ++ "px")]
                                [
                                    -- Html.h1 [] [Html.text "Skybox Debug"]
                                Html.map SkyboxMsg <| GS.drawSkybox debug model.gSkyboxModel sT (group []) (group [])
                                ]
                            _ -> Html.span [] []
                    ,   Scene3d.custom
                            { lights = Scene3d.threeLights firstLight thirdLight softLighting
                            , camera = model.camera
                            , clipDepth = Length.centimeters 10
                            , exposure = Scene3d.exposureValue 6
                            , toneMapping = Scene3d.hableFilmicToneMapping
                            , whiteBalance = Light.fluorescent
                            , antialiasing = Scene3d.multisampling
                            , dimensions = ( model.width, model.height )
                            , background = Scene3d.backgroundColor Color.lightBlue
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
          |> notifyTap (MoveCamera Direction3d.x cameraMoveDistance)
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
          |> notifyTap (MoveCamera Direction3d.x (Quantity.negate cameraMoveDistance))
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
          |> notifyTap (MoveCamera Direction3d.y cameraMoveDistance)
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
          |> notifyTap (MoveCamera Direction3d.y (Quantity.negate cameraMoveDistance))
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
          |> notifyTap (MoveCamera Direction3d.z cameraMoveDistance)
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
          |> notifyTap (MoveCamera Direction3d.z (Quantity.negate cameraMoveDistance))
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
                 Skybox.GSVGSkybox _ top sides bottom ->
                     GS.getTexturesToLoad top sides bottom
                 Skybox.GSVGSphericalSkybox _ shape ->
                     GS.getTexturesToLoad shape (group []) (group [])
                 _ -> []
            )

        (gTextureModel, gTCmd) = GT.initialModel allSvgTextures
    in
    ( { width = Quantity.zero
      , height = Quantity.zero
      , time = 0
      , tlTime = 0
      , orbiting = False
      , azimuth = Angle.degrees 180
      , elevation = Angle.degrees 30
      , cameraPos = Point3d.centimeters 0 0 20
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
      , meshStore = { generatedMeshes = Dict.empty, generatedShadows = Dict.empty }
      , widget = wModel
      , gSkyboxModel = gSkyboxModel
      , gTextureModel = gTextureModel
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
            Skybox.URLSkybox top bottom side1 side2 side3 side4 ->
                TL.fetchTextures  ([
                                ("skyB", top),
                                ("skyT", bottom),
                                ("skyS1", side1),
                                ("skyS2", side2),
                                ("skyS3", side3),
                                ("skyS4", side4)
                               ] ++ myTextures) TL.init
            Skybox.URLSphericalSkybox texture ->
                TL.fetchTextures
                    ( ("skyT", texture) :: myTextures )
                    TL.init
            _ -> TL.fetchTextures myTextures TL.init
        , case skyboxType of
            Skybox.GSVGSkybox _ _ _ _ -> Cmd.map SkyboxMsg gSCmd
            Skybox.GSVGSphericalSkybox _ _ -> Cmd.map SkyboxMsg gSCmd
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
                simulatedWorld =
                    World.simulate (tickRate |> Quantity.for t) model.world 
                    |> World.update applyForces
                    
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

                updatedCamera =
                    Camera3d.perspective
                        { viewpoint =
                            Viewpoint3d.orbitZ
                                { focalPoint = model.cameraPos
                                , azimuth = model.azimuth
                                , elevation = model.elevation
                                , distance = Length.meters 3
                                }
                        , verticalFieldOfView = Angle.degrees 45
                        }

                addBodies = model.state == Loaded && not model.initialized

            in
                ( case model.state of
                    Loaded ->
                        { model | time = timeAsNum, world = updatedWorld, camera = updatedCamera, initialized = model.initialized || addBodies }
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
        MouseDown ->
            ( { model | orbiting = True && model.state == Loaded }, Cmd.none )

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

        Reset -> ( { model | azimuth = Angle.degrees 180, elevation = Angle.degrees 30, cameraPos = Point3d.centimeters 0 0 20 } , Cmd.none )

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
                        GT.GeneratedPNG pngUrl ->
                            Cmd.batch
                                [
                                    Cmd.map TextureLoadMsg (TL.fetchTexture pngUrl model.textureLoader)
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

mouseMoveDecoder : Decoder Msg
mouseMoveDecoder =
    Decode.map2 MouseMove
        (Decode.field "movementX" (Decode.map Pixels.pixels Decode.float))
        (Decode.field "movementY" (Decode.map Pixels.pixels Decode.float))

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
