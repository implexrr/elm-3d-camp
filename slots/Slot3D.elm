{- 3D Slot for MOR. Modified from "3d-elm-camp/WorkshopTemplateBlank.elm".
 -}

module Slot3D exposing (main)

-- Most of these imports were taken from "3d-elm-camp/BeeMovement.elm", so there may be a lot of unused things
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

import GSVGSkybox as GS
import GSVGTexture as GT exposing (svgTexture)
import TextureLoader as TL exposing (loadTexture, TextureType(..), getColorTexture, getRoughnessTexture, getMetallicityTexture)
import PhysicsWrapper3D
import Wrapper3DApp exposing (..)

{-header<b>3D Slot!</b>: Make things in 3D! -}


{-editable-}

-- Consider this the equivalent of "myShapes" on the other slots. You start out with some basic shapes.
myEntities model =  
    [ square3D 250 -- This is the floor.
        |> matte Color.green
    -- Making an ellipsoid requires you to specify the length, width, and height. You also need to pass in the model.
    , ellipsoid 25 10 15 model.meshStore
        |> matte Color.purple
        |> move3D (50, 50, 50)
    , polygon3D 8 10 |> textured (getColorTexture "squares" model) (constantTexture 0) (constantTexture 0) |> rotateY3D (degrees 90) |> move3D (-50,0,50)
    , sphere 10 |> textured (getColorTexture "squares" model) (constantTexture 0) (constantTexture 0) |> move3D (-25,50,50)
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
    [ loadTexture "example" TexColor "Put an image URL here!"
    ]

-- Usage: `svgTexture "name" "type" shape`, where shape is any 2D shape or group
-- Give each one a unique name, and specify its type (TexColor, TexRoughness, or TexMetallicity).
-- You can list many of them!
svgTextures =
    [ svgTexture "squares" TexColor squares
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
    , cameraControls model
    , circle 10 |> filled red |> move (100, 0)
    ]

-- Here you can specify what images to use to create the skybox.
skyboxType =
    Skybox.URLSkybox
        "Skybox Top URL"
        "Skybox Bottom URL"
        "Skybox Side 1 URL"
        "Skybox Side 2 URL"
        "Skybox Side 3 URL"
        "Skybox Side 4 URL"
        1000
    -- Some other options (comment in the one above and comment one of these out)
    -- Skybox.GSVGSkybox False skyboxTop skyboxSides skyBoxBottom 500
    -- Skybox.GSVGSphericalSkybox False skyboxTop 500
    -- Skybox.URLSphericalSkybox "https://cschank.github.io/img/milky.jpg" 1000

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

-- You can add your own data to the model, but don't remove anything or else things won't work anymore.
-- If you add your own values to track, make sure you also add an initial value under `init`!
type alias Model =
    { width : Quantity Int Pixels
    , height : Quantity Int Pixels
    , time : Float
    , meshStore : MeshStore WorldCoordinates
    , widget : Widget.Model
    , gSkyboxModel : GS.Model
    , gTextureModel : GT.Model
    , cameraModel : W3C.Model WorldCoordinates
    , textureLoader : TL.Model
    , zoomSpeed : Length
    }

init : ( Model, Cmd Msg )
init =
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
      , meshStore = { generatedMeshes = Dict.empty, generatedShadows = Dict.empty }
      , widget = wModel
      , gSkyboxModel = gSkyboxModel
      , gTextureModel = gTextureModel
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
        , case skyboxType of
            Skybox.GSVGSkybox _ _ _ _ _ -> Cmd.map SkyboxMsg gSCmd
            Skybox.GSVGSphericalSkybox _ _ _ -> Cmd.map SkyboxMsg gSCmd
            _ -> Cmd.none
        , Cmd.map GSVGTextureMsg gTCmd
        ]
    )

-- Do not remove any of these message types.
-- If you add your own, remember to also handle them in the `update` function!
type Msg
    = Tick Duration
    | WidgetMsg Widget.Msg
    | SkyboxMsg GS.Msg
    | GSVGTextureMsg GT.Msg
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
                , case tMsg of
                    GT.GeneratedPNG (name, imgURL) ->
                        let
                            nameOnly =
                                case List.head (List.reverse (String.indices "Tex" name)) of
                                    Nothing ->
                                        name
                                    Just i ->
                                        name |> String.left i
                            texType =
                                case TL.readTextureType (String.dropLeft (String.length nameOnly) name) of
                                    Nothing ->
                                        TexColor
                                    Just tex ->
                                        tex
                        in
                            Cmd.batch
                                [ Cmd.map TextureLoadMsg
                                    ( TL.fetchTexture
                                        ( loadTexture
                                            nameOnly
                                            texType
                                            imgURL
                                        )
                                        model.textureLoader
                                    )
                                , Cmd.map GSVGTextureMsg gTCmd
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

        CameraMsg camMsg ->
            let
                w3cModel = W3C.update camMsg model.cameraModel
            in
                ( { model | cameraModel = w3cModel }
                , Cmd.none
                )

{-endeditable-}

{-extra-}

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
        (Html.map GSVGTextureMsg <| GT.drawTextures False model.gTextureModel)
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
        [ Sub.map GSVGTextureMsg (GT.subscriptions model.gTextureModel)
        , Sub.map CameraMsg (W3C.subscriptions model.cameraModel)
        , Browser.Events.onAnimationFrameDelta (Duration.seconds >> Tick)
        ]

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
