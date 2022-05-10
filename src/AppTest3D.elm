module AppTest3D exposing (..)

import Wrapper3D exposing (..)
import Wrapper3DApp exposing (..)
import Direction3d
import Quantity
import Length
import Wrapper3DCamera
import Dict
import Point3d
import LuminousFlux
import Scene3d.Light as Light
import GraphicSVG exposing (..)
import Pixels
import Angle
import Color
import Duration exposing (Duration)
import TextureLoader exposing (getColorTexture, getMetallicityTexture, getRoughnessTexture, loadTexture, TextureType(..))
import Skybox

type WorldCoordinates =
    WorldCoordinates

type Msg
    = CameraMsg (Wrapper3DCamera.Msg WorldCoordinates)
    | TextureLoadMsg (TextureLoader.Msg)
    | Tick Duration

myObjects model =
    [ square3D 500
        |> textured (getColorTexture "sand" model) (constantTexture 1) (constantTexture 0)
        |> rotateZ3D (degrees (model.time * 10))
    , cube 25
        |> matte Color.red
        |> move3D (25, 25, 25)
    ]

myOverlay model =
    [ text ("azimuth: " ++ String.fromInt (round <| Angle.inRadians model.cameraModel.azimuth * 180 / pi) ++ "º")
                                |> filled black
                                |> move (toFloat (Pixels.inPixels model.width) / 2 - 160, toFloat (Pixels.inPixels model.height) / 2 - 50)
    , text ("elevation: " ++ String.fromInt (round <| Angle.inRadians model.cameraModel.elevation * 180 / pi) ++ "º")
            |> filled black
            |> move (toFloat (Pixels.inPixels model.width) / 2 - 160, toFloat (Pixels.inPixels model.height) / 2 - 60)
    , cameraControls model
    , text (String.fromFloat model.time)
        |> centered
        |> size 12
        |> filled black
    ]

myTextures =
  [ loadTexture "sand" TexColor "https://lyao6104.github.io/Textures/Materials/SandDark1.png"
  ]

cameraMoveDistance = Length.centimeters 25

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
          <| Wrapper3DCamera.SetCameraMode
          <| Wrapper3DCamera.alternateCameraMode model.cameraModel.cameraMode
        )
    modeDisplay =
      group
        [ roundedRect 140 40 10
          |> filled green
        , text ("Current: " ++ (Wrapper3DCamera.cameraModeToString model.cameraModel.cameraMode))
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
          |> notifyTap (CameraMsg Wrapper3DCamera.Reset)
        -- Forward
        , cameraButton (degrees 180)
          |> move (toFloat (unwrapQ model.width) / 2 - 125, toFloat (unwrapQ model.height) / 2 - 150)
          |> notifyTap (CameraMsg (Wrapper3DCamera.MoveCamera Direction3d.x cameraMoveDistance))
        -- Backward
        , cameraButton (degrees 0)
          |> move (toFloat (unwrapQ model.width) / 2 - 125, toFloat (unwrapQ model.height) / 2 - 200)
          |> notifyTap (CameraMsg (Wrapper3DCamera.MoveCamera Direction3d.x (Quantity.negate cameraMoveDistance)))
        -- Left
        , cameraButton (degrees 270)
          |> move (toFloat (unwrapQ model.width) / 2 - 175, toFloat (unwrapQ model.height) / 2 - 200)
          |> notifyTap (CameraMsg (Wrapper3DCamera.MoveCamera Direction3d.y cameraMoveDistance))
        -- Right
        , cameraButton (degrees 90)
          |> move (toFloat (unwrapQ model.width) / 2 - 75, toFloat (unwrapQ model.height) / 2 - 200)
          |> notifyTap (CameraMsg (Wrapper3DCamera.MoveCamera Direction3d.y (Quantity.negate cameraMoveDistance)))
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
          |> notifyTap (CameraMsg (Wrapper3DCamera.MoveCamera Direction3d.z cameraMoveDistance))
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
          |> notifyTap (CameraMsg (Wrapper3DCamera.MoveCamera Direction3d.z (Quantity.negate cameraMoveDistance)))
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
          |> notifyTap (CameraMsg (Wrapper3DCamera.AdjustZoom (Length.meters (-0.5))))
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
          |> notifyTap (CameraMsg (Wrapper3DCamera.AdjustZoom (Length.meters 0.5)))
        -- Camera Mode
        , modeToggle
          |> move (toFloat (unwrapQ model.width) / 2 - 125, toFloat (unwrapQ model.height) / 2 - 250)
        , modeDisplay
          |> move (toFloat (unwrapQ model.width) / 2 - 125, toFloat (unwrapQ model.height) / 2 - 300)
        ]
  in
    case model.cameraModel.cameraMode of
      Wrapper3DCamera.OrbitUnlocked ->
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
            |> notifyTap (CameraMsg Wrapper3DCamera.Reset)
          , modeToggle
            |> move (toFloat (unwrapQ model.width) / 2 - 125, toFloat (unwrapQ model.height) / 2 - 150)
          , modeDisplay
            |> move (toFloat (unwrapQ model.width) / 2 - 125, toFloat (unwrapQ model.height) / 2 - 200)
          ]
      _ ->
        standardControls

myPointLight =
    { position = Point3d.meters 0 0 1
    , chromaticity = Light.sunlight
    , intensity = LuminousFlux.lumens 10000
    , castsShadows = True
    , showEntity = True
    }

init =
    ( { time = 0
      , width = Quantity.zero
      , height = Quantity.zero
      , zoomSpeed = Length.centimeters 15
      , cameraModel = Wrapper3DCamera.basicCamera
      , meshStore = { generatedMeshes = Dict.empty, generatedShadows = Dict.empty }
      , textureLoader = TextureLoader.init
      }
    , Cmd.batch
        [ Cmd.map TextureLoadMsg (TextureLoader.fetchTextures myTextures TextureLoader.init)
        ]
    )

subscriptions model =
    Sub.none

update msg model =
    case msg of
        CameraMsg camMsg ->
            ( { model | cameraModel = Wrapper3DCamera.update camMsg model.cameraModel }, Cmd.none )
        TextureLoadMsg tlMsg ->
            let
                (tlModel, tlCmd) = TextureLoader.update tlMsg model.textureLoader
            in
              ( { model | textureLoader = tlModel }
              , Cmd.map TextureLoadMsg tlCmd
              )
        Tick t ->
            let
                tickRate =
                    Duration.milliseconds 1 |> Quantity.per Duration.second

                updatedTime =
                    Duration.seconds model.time |> Quantity.plus (tickRate |> Quantity.for t)

                timeAsNum = Duration.inSeconds updatedTime

            in
                ( { model | time = timeAsNum }, Cmd.none )

main =
    graphicScene
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , title = "3D Slot"
        }

view =
    world myObjects myOverlay myPointLight (Skybox.BasicSkybox Color.black) Tick