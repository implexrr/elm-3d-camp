module BlockBuilder exposing (..)

import Angle exposing (Angle)
import Array exposing (Array)
import Axis3d
import Block3d exposing (Block3d)
import Browser
import Browser.Dom
import Browser.Events
import BoundingBox3d exposing (BoundingBox3d)
import Camera3d
import Color exposing (Color)
import Dict exposing (Dict)
import Direction3d exposing (Direction3d)
import Duration exposing (Duration)
import Frame3d
import Html exposing (Html)
import Html.Attributes as Attributes exposing (style)
import Html.Events.Extra.Wheel as Wheel
import Illuminance
import Json.Decode as Decode exposing (Decoder)
import Length exposing (Length, Meters)
import LuminousFlux exposing (LuminousFlux)
import Pixels exposing (Pixels)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import Scene3d exposing (Entity)
import Scene3d.Light as Light exposing (Chromaticity, Light)
import Scene3d.Material as Material exposing (Material)
import Scene3d.Mesh as Mesh
import SketchPlane3d
import SolidAngle
import Sphere3d
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
import Wrapper3DCamera as W3C
import Dict exposing (Dict)

import GraphicSVG.Widget as Widget
import GraphicSVG exposing (..)
import Html.Attributes as HA

import TextureLoader as TL exposing (loadTexture, TextureType(..), getColorTexture, getRoughnessTexture, getMetallicityTexture)
import PhysicsWrapper3D
import Wrapper3DApp exposing (..)
import BlockBuilderLib exposing (..)

{-editable-}
myBlocks model =
    [ basicBlock 10 10 10
        |> moveBlock (0, 0, 50)
    , basicBlock 25 25 10
        |> moveBlock (0, 0, 40)
    , basicBlock 50 50 20
        |> moveBlock (20, 20, 10)
    , blockGroup
        (List.map (\ x -> basicBlock 10 10 10 |> moveBlock (x * 15, x * 15, 0)) (List.map toFloat (List.range 0 600)))
    ]

-- move / edit the light
lightData =
    { position = Point3d.centimeters 0 0 100    -- position of the light
    , chromaticity = Light.sunlight             -- the colour of the light (see https://package.elm-lang.org/packages/ianmackenzie/elm-3d-scene/latest/Scene3d-Light#Chromaticity)
    , intensity = LuminousFlux.lumens 10000     -- how intense the light is
    , castsShadows = True                       -- whether the light will cast shadows
    , showEntity = True                         -- whether the light ball will be rendered (the light itself shines regardless)
    }

-- Use "loadTexture [name] [type] [url]" to load in texture images from the Internet!
-- Give each one a unique name, and specify its type (TexColor, TexRoughness, or TexMetallicity).
-- You can list many of them!
myTextures = 
    [ loadTexture "example" TexColor "https://lyao6104.github.io/Textures/MSCSpaceTextures/FuelTank1.png"
    , loadTexture "example" TexRoughness "https://lyao6104.github.io/Textures/MSCSpaceTextures/FuelTank1.png"
    ]

{-endeditable-}

{-extra-}
myEntities model =  
    [ square3D 250
        |> matte Color.green
    , customObject (blockGroup (myBlocks model)).mesh True model.meshStore
        -- |> matte Color.red
        |> textured (getColorTexture "example" model) (getRoughnessTexture "example" model) (constantTexture 0.5)
    -- , customObject (TriangularMesh.combine [ TriangularMesh.indexed (Array.fromList [ { position = Point3d.meters -0.05 -0.05 -0.05, uv = ( 0, 1 ) }, { position = Point3d.meters 0.05 -0.05 -0.05, uv = ( 1, 1 ) }, { position = Point3d.meters 0.05 0.05 -0.05, uv = ( 1, 0 ) }, { position = Point3d.meters -0.05 0.05 -0.05, uv = ( 0, 0 ) }, { position = Point3d.meters -0.05 -0.05 0.05, uv = ( 0, 0 ) }, { position = Point3d.meters 0.05 -0.05 0.05, uv = ( 1, 0 ) }, { position = Point3d.meters 0.05 0.05 0.05, uv = ( 1, 1 ) }, { position = Point3d.meters -0.05 0.05 0.05, uv = ( 0, 1 ) } ]) [ ( 0, 2, 1 ), ( 0, 3, 2 ), ( 4, 5, 6 ), ( 4, 6, 7 ), ( 1, 2, 6 ), ( 1, 6, 5 ), ( 0, 7, 3 ), ( 0, 4, 7 ), ( 0, 1, 5 ), ( 0, 5, 4 ), ( 3, 6, 2 ), ( 3, 7, 6 ) ] |> TriangularMesh.mapVertices ( \ v -> { v | position = v.position |> Point3d.translateBy (Vector3d.centimeters 0 0 50 )} ) , TriangularMesh.indexed (Array.fromList [ { position = Point3d.meters -0.125 -0.125 -0.05, uv = ( 0, 1 ) }, { position = Point3d.meters 0.125 -0.125 -0.05, uv = ( 1, 1 ) }, { position = Point3d.meters 0.125 0.125 -0.05, uv = ( 1, 0 ) }, { position = Point3d.meters -0.125 0.125 -0.05, uv = ( 0, 0 ) }, { position = Point3d.meters -0.125 -0.125 0.05, uv = ( 0, 0 ) }, { position = Point3d.meters 0.125 -0.125 0.05, uv = ( 1, 0 ) }, { position = Point3d.meters 0.125 0.125 0.05, uv = ( 1, 1 ) }, { position = Point3d.meters -0.125 0.125 0.05, uv = ( 0, 1 ) } ]) [ ( 0, 2, 1 ), ( 0, 3, 2 ), ( 4, 5, 6 ), ( 4, 6, 7 ), ( 1, 2, 6 ), ( 1, 6, 5 ), ( 0, 7, 3 ), ( 0, 4, 7 ), ( 0, 1, 5 ), ( 0, 5, 4 ), ( 3, 6, 2 ), ( 3, 7, 6 ) ] |> TriangularMesh.mapVertices ( \ v -> { v | position = v.position |> Point3d.translateBy (Vector3d.centimeters 0 0 40 )} ) , TriangularMesh.indexed (Array.fromList [ { position = Point3d.meters -0.25 -0.25 -0.1, uv = ( 0, 1 ) }, { position = Point3d.meters 0.25 -0.25 -0.1, uv = ( 1, 1 ) }, { position = Point3d.meters 0.25 0.25 -0.1, uv = ( 1, 0 ) }, { position = Point3d.meters -0.25 0.25 -0.1, uv = ( 0, 0 ) }, { position = Point3d.meters -0.25 -0.25 0.1, uv = ( 0, 0 ) }, { position = Point3d.meters 0.25 -0.25 0.1, uv = ( 1, 0 ) }, { position = Point3d.meters 0.25 0.25 0.1, uv = ( 1, 1 ) }, { position = Point3d.meters -0.25 0.25 0.1, uv = ( 0, 1 ) } ]) [ ( 0, 2, 1 ), ( 0, 3, 2 ), ( 4, 5, 6 ), ( 4, 6, 7 ), ( 1, 2, 6 ), ( 1, 6, 5 ), ( 0, 7, 3 ), ( 0, 4, 7 ), ( 0, 1, 5 ), ( 0, 5, 4 ), ( 3, 6, 2 ), ( 3, 7, 6 ) ] |> TriangularMesh.mapVertices ( \ v -> { v | position = v.position |> Point3d.translateBy (Vector3d.centimeters 20 20 10 )} ) ])
    --       True model.meshStore
    --       |> matte Color.blue
    ]

-- Put your 2D shapes here, and they will be overlayed on top of the screen!
overlay : Model -> List (Shape Msg)
overlay model =
    let
        screenW = model.width |> Pixels.toInt
        screenH = model.height |> Pixels.toInt
        meshCode =
            group
                [ text "Your Mesh Code:"
                    |> fixedwidth
                    |> bold
                    |> size 16
                    |> filled black
                , html (toFloat screenW) (toFloat screenH / 2)
                    ( Html.pre []
                        [ Html.text (blockGroup (myBlocks model)).codeString
                        ]
                    )
                ]
    in
        [ angleDisplay model
        , cameraControls model
        , meshCode
            |> move (-(toFloat screenW) / 2 + 100, 0)
        ]

-- Here you can specify what images to use to create the skybox.
skyboxType =
    Skybox.BasicSkybox Color.lightBlue

-- You can add your own data to the model, but don't remove anything or else things won't work anymore.
-- If you add your own values to track, make sure you also add an initial value under `init`!
type alias Model =
    { width : Quantity Int Pixels
    , height : Quantity Int Pixels
    , time : Float
    , meshStore : MeshStore WorldCoordinates
    , widget : Widget.Model
    , cameraModel : W3C.Model WorldCoordinates
    , textureLoader : TL.Model
    , zoomSpeed : Length
    }

init : ( Model, Cmd Msg )
init =
    let
        (wModel, _) = Widget.init 0 0 "widget"
    in
    ( { width = Quantity.zero
      , height = Quantity.zero
      , time = 0
      , meshStore = { generatedMeshes = Dict.empty, generatedShadows = Dict.empty }
      , widget = wModel
      , cameraModel = W3C.basicCamera
      , textureLoader = TL.init
      , zoomSpeed = Length.centimeters 15
      }
    , Cmd.batch
        [ Cmd.map TextureLoadMsg <| case skyboxType of
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
        ]
    )

-- Do not remove any of these message types.
-- If you add your own, remember to also handle them in the `update` function!
type Msg
    = Tick Duration
    | WidgetMsg Widget.Msg
    | TextureLoadMsg TL.Msg
    | CameraMsg (W3C.Msg WorldCoordinates)

update message model =
    case message of
        -- This gets called around 60 times per second
        Tick t ->
           let
                tickRate =
                    Duration.milliseconds 1 |> Quantity.per Duration.second

                updatedTime =
                    Duration.seconds model.time |> Quantity.plus (tickRate |> Quantity.for t)

                timeAsNum = Duration.inSeconds updatedTime

            in
                ( { model | time = timeAsNum }
                , Cmd.none
                )

        -- This is needed for our overlay
        WidgetMsg wMsg ->
            let
                (newWModel, wCmd) = Widget.update wMsg model.widget
            in
            ( { model | widget = newWModel }, Cmd.map WidgetMsg wCmd )

        TextureLoadMsg tlMsg ->
            let
                (tlModel, tlCmd) = TL.update tlMsg model.textureLoader
            in
            (
                { model | textureLoader = tlModel }
            ,   Cmd.map TextureLoadMsg tlCmd
            )

        CameraMsg camMsg ->
            let
                w3cModel = W3C.update camMsg model.cameraModel
            in
                ( { model | cameraModel = w3cModel }
                , Cmd.none
                )

debugMeshNames model = text (Debug.toString (List.map (\(key,_) -> key) (Dict.toList model.meshStore.generatedMeshes))) |> filled black

-- Distance that the camera moves when you click the buttons in the top right corner
cameraMoveDistance = Length.centimeters 25

type WorldCoordinates
    = WorldCoordinates

main =
    graphicScene
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , title = "3D Slot"
        }

view model =
    world
        myEntities
        overlay
        lightData
        skyboxType
        (Html.div [] [])
        Tick

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


subscriptions model =
    Sub.batch
        [ Sub.map CameraMsg (W3C.subscriptions model.cameraModel)
        , Browser.Events.onAnimationFrameDelta (Duration.seconds >> Tick)
        ]

