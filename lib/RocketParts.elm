{- 3D Slot for MOR. Modified from "3d-elm-camp/WorkshopTemplateBlank.elm".
 -}

module RocketParts exposing (..)

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
import Illuminance
import Json.Decode as Decode exposing (Decoder)
import Length exposing (Meters)
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
import Collision
import Wrapper3D exposing (..)
import Dict exposing (Dict)

import GraphicSVG.Widget as Widget
import GraphicSVG exposing(..)
import Html.Attributes as HA

import GSVGSkybox as GS
import GSVGTexture as GT exposing (svgTexture)
import TextureLoader as TL exposing (loadTexture, TextureType(..))

{-header<b>3D Slot!</b>: Make things in 3D! -}


{-editable-}

-- Consider this the equivalent of "myShapes" on the other slots. You start out with a basic shape
myEntities model =  
    [ --booster model 10 100
    -- specified 
    -- fuelTank(the width of the shuttle)
    -- height (the long of the shuttle)
    -- amount of thrusters
    -- how many raw of thrusters
    -- how many thrusters in a raw
    shuttle model 20 80 20 4 2 2
        |> rotateX3D (degrees 90)
        |> rotateY3D (degrees 90)
        |> move3D (0,0,40)
    ]

shuttle model fuelTank height power amount raw rawAmount = group3D [
    cylinder fuelTank height
       |> matte (Color.rgb255 222 218 211)
     , cone fuelTank 25
       |> matte (Color.rgb255 222 218 211)
       |> move3D (0,0,height)
     , rectangle3D height ((fuelTank*2)+5)
       |> matte (Color.rgb255 222 218 211)
       |> rotateY3D (degrees 90)
       |> move3D (0,0,height/2)
     , customPolygon polyPoints
       |> matte (Color.rgb255 222 218 211)
       |> rotateY3D (degrees 90)
       |> rotateX3D (degrees 90)
       |> move3D (0,-fuelTank/2,10)
       |> scale3D 3
    , customPolygon polyPoints
       |> matte (Color.rgb255 222 218 211)
       |> rotateY3D (degrees 90)
       |> rotateX3D (degrees 180)
       |> move3D (0,-fuelTank/2,-10)
       |> scale3D -3
     , truncatedCone fuelTank (fuelTank+20) 20 model.meshStore
       |> matte (Color.rgb255 222 218 211)
       |> move3D (0,0,-20)
    -- specify 
    -- the size of fuelTank
    -- the height  of thruste, total amount of thruste
    -- how many row of thruster, how many thruster in a row
    , thruster model (fuelTank) power amount raw rawAmount 
  ]

polyPoints = 
  [(-10,-10),(10,10),(10,-10),(-10,-10)]

-- The maxminam thruster is 6
thruster : Model -> Float -> Float -> Int -> Int -> Int -> Object WorldCoordinates
thruster model width height amount raw amountInRaw = 
  let
   amountF = toFloat amount
  in
    group3D 
      <| List.map (\x -> 
         truncatedCone (width/amountF) ((width/amountF)+5) 20 model.meshStore
            |> matte (Color.rgb255 222 218 211)
            |> move3D x) 
        (thrusterPost amount ((width/amountF)+5) raw amountInRaw (-height-10))

thrusterPost : Int -> Float -> Int -> Int -> Float -> List (Float, Float, Float)    
thrusterPost totalAmount width raw amountInRaw height =
  let
    drawAmount = (\x -> 
      let 
        remainder = (totalAmount - (amountInRaw * x))
      in
        if (remainder//amountInRaw >= 1) then
          amountInRaw
        else
          remainder
      )
  in
  List.concat <|
    List.map 
      (\x -> 
         thrusterPostInRaw width (drawAmount x) height x (amountInRaw-(drawAmount x)) (raw-1) (amountInRaw-1)
      ) (List.range 0 (raw-1))

thrusterPostInRaw width amount height rawNow adjectY adjectXRaw adjectYRaw =
  List.map (\x -> (-width*2*(toFloat rawNow) + width * (toFloat adjectXRaw)
                  ,(width*2*(toFloat x)) + width*(toFloat adjectY) - (width * (toFloat adjectYRaw))
                  ,height)) 
   (List.range 0 (amount-1))

{-- 
booster model redius tail = group3D
   [ cylinder redius tail
       |> matte (Color.rgb255 222 218 211)
     , cone redius 30
       |> matte (Color.rgb255 222 218 211)
       |> move3D (0,0,tail)
     , engine model redius 200 -- this is kind of wrong now, since the length changes
   ]
--}

engine : { a | meshStore : MeshStore coordinates } -> Float -> Float -> Material.Textured coordinates -> Object coordinates
engine model radius length material = truncatedCone (radius / 2) radius length model.meshStore material


-- move / edit the light
light =
    pointLight
        { position = Point3d.centimeters 0 0 100    -- position of the light
        , chromaticity = Light.sunlight             -- the colour of the light (see https://package.elm-lang.org/packages/ianmackenzie/elm-3d-scene/latest/Scene3d-Light#Chromaticity)
        , intensity = LuminousFlux.lumens 10000     -- how intense the light is
        }
showLight = False -- whether to show the light ball or not

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
    [
        angleDisplay model
    ]

-- This colour is used to create the floor. If you want custom colours, use Color.hsl or Color.rgb!
floorColour = Color.green

-- Floor size, in metres. Set to 0 to disable.
floorSize = 2.5

-- Here you can specify what images to use to create the skybox. Just replace "todo" with a link to an image. (Keep the quotes, though!)
skyboxType =
    Skybox.URLSkybox textureBottom textureTop textureSide1 textureSide2 textureSide3 textureSide4 1000
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

type WorldCoordinates
    = WorldCoordinates

type alias Model =
    { width : Quantity Int Pixels
    , height : Quantity Int Pixels
    , time : Float
    , orbiting : Bool
    , azimuth : Angle
    , elevation : Angle
    , meshStore : MeshStore WorldCoordinates
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
    | VisibilityChange Browser.Events.Visibility
    | GenerateMeshes (List (Object WorldCoordinates))
    | WidgetMsg Widget.Msg
    | Reset
    | SkyboxMsg GS.Msg
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
    ( Light.point (Light.castsShadows True) properties
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

        -- Create a quad to act as a 'floor'
        plane =
            square3D (floorSize * 100)
            |> matte floorColour

        -- Define camera as usual
        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.orbitZ
                        { focalPoint = Point3d.centimeters 0 0 20
                        , azimuth = model.azimuth
                        , elevation = model.elevation
                        , distance = Length.meters 3
                        }
                , verticalFieldOfView = Angle.degrees 45
                }

        textures = model.textureLoader.colorTextures

        baseEntities =
            (if showLight then [ firstLightBall ] else []) ++
            ( case skyboxType of
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
                Skybox.BasicSkybox _ ->
                    Scene3d.nothing
            ) :: renderEntities [ ]

    in
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
                    { lights = Scene3d.threeLights firstLight thirdLight softLighting
                    , camera = camera
                    , clipDepth = Length.centimeters 10
                    , exposure = Scene3d.exposureValue 6
                    , toneMapping = Scene3d.hableFilmicToneMapping
                    , whiteBalance = Light.fluorescent
                    , antialiasing = Scene3d.multisampling
                    , dimensions = ( model.width, model.height )
                    , background = Scene3d.backgroundColor Color.lightBlue
                    , entities = baseEntities ++ renderEntities (myEntities model)
                    }
                    |> withOverlay (overlay model) model
            ,  Html.map GSVGTextureMsg <| GT.drawTextures False model.gTextureModel
            ]

angleDisplay : Model -> Shape Msg
angleDisplay model = group
    [
        text ("azimuth: " ++ String.fromInt (round <| unwrapQ model.azimuth * 180 / pi) ++ "??")
                                |> filled black
                                |> move (toFloat (unwrapQ model.width) / 2 - 160, toFloat (unwrapQ model.height) / 2 - 50)
    ,   text ("elevation: " ++ String.fromInt (round <| unwrapQ model.elevation * 180 / pi) ++ "??")
            |> filled black
            |> move (toFloat (unwrapQ model.width) / 2 - 160, toFloat (unwrapQ model.height) / 2 - 60)
    ,   group [
                    roundedRect 60 40 10
                            |> filled green
                ,   text "Reset"
                        |> size 16
                        |> centered
                        |> filled black
                        |> move (0,-5)
                ]
                |> move (toFloat (unwrapQ model.width) / 2 - 125, toFloat (unwrapQ model.height) / 2 - 90)
                |> notifyTap Reset
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
      , orbiting = False
      , azimuth = Angle.degrees 0
      , elevation = Angle.degrees 30
      , meshStore = { generatedMeshes = Dict.empty, generatedShadows = Dict.empty }
      , widget = wModel
      , gSkyboxModel = gSkyboxModel
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

            in
                ( { model | time = timeAsNum }
                , Cmd.batch
                    []
                    --[ Task.perform
                    --    ( \ _ -> GenerateMeshes ( myEntities model ) )
                    --    (Task.succeed True)
                    --]
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

        Reset -> ( { model | azimuth = Angle.degrees 0, elevation = Angle.degrees 30 } , Cmd.none )

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
            (
                { model | textureLoader = tlModel }
            ,   Cmd.map TextureLoadMsg tlCmd
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
