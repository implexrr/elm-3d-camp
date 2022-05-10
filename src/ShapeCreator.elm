{- Testing file for the 3D slot
 -}

module ShapeCreator exposing (main)

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
import SolidAngle
import Sphere3d
import Task
import Vector3d exposing (Vector3d)
import Viewpoint3d
import WebGL.Texture
import Skybox
import Wrapper3D exposing (..)
import Dict exposing (Dict)
import LineSegment3d
import Round

import GraphicSVG.Widget as Widget
import GraphicSVG exposing(..)
import Html.Attributes as HA exposing (style)

import GSVGSkybox as GS
import GSVGTexture as GT exposing (svgTexture)
import TextureLoader as TL exposing (loadTexture, TextureType(..), getColorTexture, getRoughnessTexture, getMetallicityTexture)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import List
import ShapeCreateAssets exposing (..)
import String exposing (..)
-- import Airplane exposing (airplane)

-- import Main exposing (Model)


{-header<b>3D Slot!</b>: Make things in 3D! -}


{-editable-}

-- Consider this the equivalent of "myShapes" on the other slots. You start out with a basic shape
myEntities model =
    [
        if (List.member model.shape [Cube, Boxx, Cone, Cylinder]) then (shapeFun model) else (shapeFunTextured model)-- the selection boxes
    ]

    -- This group is just used for debugging the meshes
    -- , Scene3d.group (List.map (Scene3d.mesh (Material.metal {baseColor = Color.red, roughness = 0.3 })) (Dict.values model.generatedMeshes))
    --   |> move (25,25,50)



-- Put any custom meshes you need generated in here. Make sure the values are identical.


polygonDict = Dict.fromList [
    (0 , [(-10,-10), (10,-10), (10,10),(-10,-10)])
    , (1 , [(0.5944,35.071),(-8.123,10.897),(-35.86,10.105),(-14.46,-6.142),(-22.39,-33.08),(1.3869,-16.44),(22.390,-31.90),(16.049,-7.727),(36.656,10.105),(10.105,10.105),(0.5944,35.071)])
    , (2, [(-2.575,-42.60),(2.5758,-42.99),(2.5758,-24.37),(23.578,-26.35),(21.597,-19.21),(42.600,-1.783),(37.448,1.3869),(41.015,15.653),(28.334,14.068),(25.560,19.616),(13.275,8.9164),(16.445,32.297),(8.5201,29.126),(0.1981,44.185),(-8.916,30.315),(-16.04,32.693),(-12.48,8.5201),(-24.37,18.823),(-27.54,14.464),(-41.41,15.653),(-37.44,1.7832),(-42.99,-1.386),(-21.20,-19.21),(-22.78,-25.95),(-1.783,-24.37),(-2.575,-42.60)])
  ]

adjustWidth model = relativeP 1260 model.width
adjustHeight model = relativeP 600 model.height

windowHight model = Basics.toFloat (unwrapQ model.height)
windowWidth model = Basics.toFloat (unwrapQ model.width)

-- Texture information

myTextures = 
    [ loadTexture "example" TexColor "https://sharyuwu.github.io/image/texture1.png"
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

overlay : Model -> List (Shape (Msg Model))
overlay model =
    [
    angleDisplay model
      |> move (-40, 0)
      |> scale (1 * (ratioHelper model))
    , shapeCreator model 
    --, navigationButtons model |> scale 0.9
    ]

polygonList model = 
    let
      mainRectWidth = 330
      mainRectHeight = 66
    in
    group [rect mainRectWidth mainRectHeight
          |> filled (rgba 255 255 255 0.5)
          |> addOutline (solid 1) lightGrey
        , rect 90 22
          |> filled white |> addOutline (solid 1) lightGrey
          |> move ( -100 , mainRectHeight / 2)
        , text "Polygon" |> serif |> italic
          |> size 17
          |> filled titleColour
          |> move ( -130 , mainRectHeight / 2.3)
         ,"polyPoints = " |> copiable model
           |> scale 1.5
           |> move ( -mainRectWidth/2 + 10 , 0)
         , listCode (getPolygonDic model) |> copiable model
           |> scale 1
           |> move ( -mainRectWidth/2 + 10 , -mainRectHeight/3.5)
         ,roundedRect 140  22  5
          |> filled (rgba 255 137 5 (0.6 + 0.4 * sin (5 * model.time)))
          |> addOutline (solid 1) lightGrey
          |> move ( 20 , mainRectHeight / 2)
         , text "Next Polygon" |> fixedwidth
          |> size 15 |> filled black
          |> move ( -33, mainRectHeight / 2.3)
          |> notifyTap NextPolygon
        ] 

ratioHelper model = 
  if (adjustHeight model) < (adjustWidth model) then
    (adjustHeight model)
  else
    (adjustWidth model)

shapeCreator model = 
  let
    ifShowSVG  =
      case model.textureStyle of
         Url -> 0
         Svg -> 1
  in
  group [
        --shape creator
        stencils model
          |> scale 0.65
          |> move ( -205, 90)
        , stamps model
          |> scale 0.8
          |> move ( 0, 95)
        , colours model
          |> scale 0.71
          |> move ( 240, -5)
        , transforms model
          |> scale 0.7
          |> move ( -240, 5)
        , tweaks model
          |> scale 0.7
          |> move ( -250, -85)
        , yourCode model
          |> scale 0.8
          |> move ( 0, -80)
        , polygonList model
          |> scale 0.5
          |> move ( -100, -130)
        , yourTexture model
          |> scale 0.65
          |> move ( 80, -130)
        , showSVGTexture
          |> scale 0.60
          |> move ( 150, -83)
          |> makeTransparent ifShowSVG
        ] |> scale (2 * (ratioHelper model))

showSVGTexture =
  let
      mainRectWidth = 55
      mainRectHeight = 65
  in
    group
            [
              rect mainRectWidth mainRectHeight
                |> filled (rgba 255 255 255 0.5)
                |> addOutline (solid 1) lightGrey
                |> move (0, 5)
            , rect 45 9 |> filled white |> addOutline (solid 1) lightGrey
                |> move (0 , mainRectHeight/2)
              , squares
                |> clip (square 50 |> ghost)
                |> addOutline (solid 1) black
              ,text "SVG Texture" |> serif |> italic |> centered |> size 8 |> filled titleColour |> move (0,30)
            ]

yourTexture model =
  let
    mainRectWidth = 250
    mainRectHeight = 50

    loadtextureString =
      case model.textureStyle of
         Url ->
           "loadTexture \"example\" TexColor \"https://sharyuwu.github.io/image/texture1.png\""
         Svg ->
           "svgTexture \"squares\" TexColor squares"
    titleString =
      case model.textureStyle of
         Url ->
           "URl Texture"
         Svg ->
           "Svg Texture"
  in
  group [
    rect mainRectWidth mainRectHeight
          |> filled (rgba 255 255 255 0.5)
          |> addOutline (solid 1) lightGrey
        , rect 80 15 |> filled white |> addOutline (solid 1) lightGrey
          |> move (-60 , mainRectHeight/2)
        , text (titleString) |> serif |> italic
          |> size 11 
          |> filled titleColour
          |> move (-85 , mainRectHeight/2.4)
        , loadtextureString |> copiable model
          |> move (-120, 0)
        ,roundedRect 70  15  5
          |> filled (rgba 255 137 5 (0.6 + 0.4 * sin (5 * model.time)))
          |> addOutline (solid 1) lightGrey
          |> move ( 20 , mainRectHeight / 2)
         , text "Change Mode" |> fixedwidth
          |> size 10 |> filled black
          |> move ( -10, mainRectHeight / 2.3)
          |> notifyTap TextureMode
  ]

navigationButtons model = group [
            button1 model
            , button2 model
          ]

button1 model = group [ circle 40
                |>filled (rgba 255 255 255 0.5)
                |> addOutline (solid 5) (rgba 255 137 5 0.8)
                ,
                polygon [(0,40),(0,0),(40,0)]
                |> filled (rgba 255 137 5 0.8)
            ]

button2 model = group [
                circle 40
                |> filled (rgba 255 255 255 0.5)
                |> addOutline (solid 5) (rgba 197 125 149 0.8)
                ,

                text "R"
                |> centered
                |> fixedwidth
                |> size (relativeP 10 model.height)
                |> bold
                |> filled (rgba 197 125 149 0.8)

            ] 

-- This colour is used to create the floor. If you want custom colours, use Color.hsl or Color.rgb!
floorColour = Color.green

-- Here you can specify what images to use to create the skybox. Just replace "todo" with a link to an image. (Keep the quotes, though!)
skyboxType = Skybox.GSVGSkybox False skyboxTop skyboxSides skyBoxBottom 1000 -- Skybox.URLSkybox textureBottom textureTop textureSide1 textureSide2 textureSide3 textureSide4

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
        [ square 50 |> filled green
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
    --shapecreator
    , notify : Notifications
    , shape : Stencil
    , draw : Draw
    , style : LineStyle
    , lineWidth : Float
    , l : Float
    , w : Float
    , h : Float
    , sides : Int
    , xAngle : Float
    , yAngle : Float
    , zAngle : Float
    , mouth : Float
    , txt : String
    , clr : Colour
    , red : Int
    , green : Int
    , blue : Int
    , hasMove : Bool
    , hasRotateX : Bool
    , hasRotateY : Bool
    , hasRotateZ : Bool
    , hasScale : Bool
    , scl : Float
    , sclx : Float
    , scly : Float
    , x : Float
    , y : Float
    , z : Float
    , r : Float
    , roughness : Float
    , thickness : Float
    , currentButton : ButtonDir
    , buttonDownTime : Float
    , nextPolygon : Int
    , textureStyle : TextureStyle
    }

type Msg m
    = Resize (Quantity Int Pixels) (Quantity Int Pixels)
    | Tick Duration
    | MouseDown
    | MouseMove (Quantity Float Pixels) (Quantity Float Pixels)
    | MouseUp
    | VisibilityChange Browser.Events.Visibility
    | GenerateMeshes (List (Object WorldCoordinates))
    -- | GenerateShadows String (List (Mesh.Shadow WorldCoordinates))
    | WidgetMsg Widget.Msg
    | Reset
    | SkyboxMsg GS.Msg
    | GSVGTextureMsg GT.Msg
    | TextureLoadMsg TL.Msg
    --shape creator stuff
    | Sten Stencil
    | Draw Draw
    | LStyle
    | CycleText
    | SetColour Colour
    | Toggle Transforms
    | TransM (Model -> Model)
    | Notif Notifications
    | ButtonDown ButtonDir
    | NextPolygon
    | TextureMode

type ButtonDir
    = RedUp
    | RedDown
    | BlueUp
    | BlueDown
    | GreenUp
    | GreenDown
    | None


-- the type of stencil selected, these correspond to functions exported by GraphicSVG


type Stencil
    = Cube
    | Square3D
    | Rectangle3D
    | Boxx
    | Sphere
    | Cone
    | Cylinder
    | Ring
    | PolyCone
    | PolyCylinder
    | Ellipsoid
    | Polygon3D
    | CustomPolygon
    | TexturedCylinder
    | TruncatedCone



-- type of drawing


type Draw
    = Metal
    | Plastic
    | Matte
    | Texture

type TextureStyle
    = Url
    | Svg

type Colour
    = Black
    | Blue
    | Brown
    | Charcoal
    | DarkBlue
    | DarkBrown
    | DarkCharcoal
    | DarkGray
    | DarkGreen
    | DarkGrey
    | DarkOrange
    | DarkPurple
    | DarkRed
    | DarkYellow
    | Gray
    | Green
    | Grey
    | LightBlue
    | LightBrown
    | LightCharcoal
    | LightGray
    | LightGreen
    | LightGrey
    | LightOrange
    | LightPurple
    | LightRed
    | LightYellow
    | Orange
    | Purple
    | Red
    | White
    | Yellow
    | RGB


type Transforms
    = Move
    | RotateX
    | RotateY
    | RotateZ
    | Scale


type Notifications
    = NotifyTap
    | NotifyTapAt
    | NotifyEnter
    | NotifyEnterAt
    | NotifyLeave
    | NotifyLeaveAt
    | NotifyMouseMoveAt
    | NotifyMouseDown
    | NotifyMouseDownAt
    | NotifyMouseUp
    | NotifyMouseUpAt
    | NotifyTouchStart
    | NotifyTouchStartAt
    | NotifyTouchEnd
    | NotifyTouchEndAt
    | NotifyTouchMoveAt


type LineStyle
    = Solid
    | Dotted
    | Dashed
    | Longdash
    | Dotdash



-- update helper


cycleTxt s =
    case s of
        "Hello" ->
            "Bonjour"

        "Bonjour" ->
            "Namaste"

        "Namaste" ->
            "Gutten Tag"

        "Gutten Tag" ->
            "Jó napot"

        "Jó napot" ->
            "Dobro utro"

        "Dobro utro" ->
            "Sat Shri Akaal"

        -- "Sat Shri Akaal" ->
        _ ->
            "Hello"




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

view : Model -> Html (Msg Model)
view model =
    let
        -- Incandescent light bulb
        ( firstLight, firstLightBall ) =
            pointLight
                { position = Point3d.centimeters 0 0 100
                , chromaticity = Light.sunlight
                , intensity = LuminousFlux.lumens 10000
                }


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
                , verticalFieldOfView = Angle.degrees 30
                }


        textures = model.textureLoader.colorTextures

        baseEntities =
            [ firstLightBall ] ++
            ( case skyboxType of
                Skybox.GSVGSkybox _ _ _ _ _ ->
                    Skybox.skybox 
                        [ Dict.get "skyB" textures
                        , Dict.get "skyT" textures
                        , Dict.get "skyS1" textures
                        , Dict.get "skyS2" textures
                        , Dict.get "skyS3" textures
                        , Dict.get "skyS4" textures
                        ]
                        1000
                Skybox.URLSkybox _ _ _ _ _ _ _ ->
                    Skybox.skybox 
                        [ Dict.get "skyB" textures
                        , Dict.get "skyT" textures
                        , Dict.get "skyS1" textures
                        , Dict.get "skyS2" textures
                        , Dict.get "skyS3" textures
                        , Dict.get "skyS4" textures
                        ]
                        1000
                Skybox.URLSphericalSkybox _ _ ->
                    Skybox.roundSkybox
                        (Dict.get "skyT" textures)
                        500
                Skybox.GSVGSphericalSkybox _ _ _ ->
                    Skybox.roundSkybox
                        (Dict.get "skyT" textures)
                        500
                _ ->
                  Skybox.roundSkybox
                        (Dict.get "skyT" textures)
                        500
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
                    , entities = baseEntities ++ debugs ++ renderEntities (myEntities model)
                    }
                    |> withOverlay (overlay model) model
            ,  Html.map GSVGTextureMsg <| GT.drawTextures False model.gTextureModel
            ] 

angleDisplay : Model -> Shape (Msg m)
angleDisplay model = group
    [
        text ("azimuth: " ++ String.fromInt (round <| unwrapQ model.azimuth * 180 / pi) ++ "º")
                                |> size 12
                                |> filled black
                                |> move (230, 250)
    ,   text ("elevation: " ++ String.fromInt (round <| unwrapQ model.elevation * 180 / pi) ++ "º")
            |> size 12
            |> filled black
            |> move (230, 240)
    ,   group [
                    roundedRect 60 40 10
                            |> filled green
                ,   text "Reset"
                        |> size 16
                        |> centered
                        |> filled black
                        |> move (0, 0)
                ]
                |> move (260, 214)
                |> notifyTap Reset
    ]
{-endextra-}


subscriptions : Model -> Sub (Msg m)
subscriptions model =
    Sub.batch
        [ -- Listen for resize events so we can render full screen
          Browser.Events.onResize
            (\widthh heightt ->
                Resize
                    (Pixels.pixels widthh)
                    (Pixels.pixels heightt)
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
        , case skyboxType of
            Skybox.GSVGSkybox _ _ _ _ _ ->
                Sub.map SkyboxMsg (GS.subscriptions model.gSkyboxModel)
            _ -> Sub.none
        ]

main : Program () Model (Msg Model)
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

init : () -> ( Model, Cmd (Msg m) )
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
      --shape creator
        , notify = NotifyTap
        , shape = Cube
        , draw = Matte
        , style = Solid
        , lineWidth = 1
        , l = 20
        , w = 10
        , h = 15
        , sides = 5
        , xAngle = 30
        , yAngle = 30
        , zAngle = 30
        , mouth = 0.75
        , txt = "Hello"
        , clr = RGB
        , red = 255
        , green = 0
        , blue = 0
        , hasMove = False
        , hasRotateX = False
        , hasRotateY = False
        , hasRotateZ = False
        , hasScale = False
        , scl = 2
        , sclx = 2
        , scly = 2
        , x = 0
        , y = 0
        , z = 0
        , r = 10
        , thickness = 3
        , currentButton = None
        , buttonDownTime = 0
        , roughness = 0.5
        , nextPolygon = 0
        , textureStyle = Url
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
           {--Skybox.URLSkybox top bottom side1 side2 side3 side4 _ ->
                TL.fetchTextures  ([
                                ("skyB", top),
                                ("skyT", bottom),
                                ("skyS1", side1),
                                ("skyS2", side2),
                                ("skyS3", side3),
                                ("skyS4", side4)
                               ] ++ myTextures) TL.init
            --}
            Skybox.URLSphericalSkybox texture _ ->
                TL.fetchTextures
                    ( loadTexture "skyT" TexColor texture :: myTextures )
                    TL.init
            _ -> TL.fetchTextures myTextures TL.init
            {--Skybox.URLSphericalSkybox texture _ ->
                TL.fetchTextures
                    ( ("skyT", texture) :: myTextures )
                    TL.init
            _ -> TL.fetchTextures myTextures TL.init
            --}
        , case skyboxType of
            Skybox.GSVGSkybox _ _ _ _ _ -> Cmd.map SkyboxMsg gSCmd
            Skybox.GSVGSphericalSkybox _ _ _ -> Cmd.map SkyboxMsg gSCmd
            _ -> Cmd.none
        , Cmd.map GSVGTextureMsg gTCmd
        ]
    )

update : Msg m -> Model -> ( Model, Cmd (Msg m) )
update message model =
    case message of
        Resize width height ->
            let
                (wModel, wCmd) = Widget.init (Basics.toFloat <| unwrapQ width ) (Basics.toFloat <| unwrapQ height) "widget"
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
                                                        Wrapper3D.PolyCone points (xtip, ytip, ztip) ->
                                                            generatePolyCone points (xtip, ytip, ztip)
                                                        
                                                        Wrapper3D.PolyCylinder points height ->
                                                            generatePolyCylinder points height

                                                        Wrapper3D.Ellipsoid (length, width, height) ->
                                                            generateEllipsoid length width height

                                                        Wrapper3D.Ring radius thickness ->
                                                            generateRing radius thickness

                                                        Wrapper3D.TexturedCylinder radius height ->
                                                            generateTexturedCylinder radius height

                                                        Wrapper3D.TruncatedCone topR botR height ->
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

        -- GenerateShadows name meshes ->
        --     case meshes of
        --         [] ->
        --             (model, Cmd.none)
        --         (mesh :: rest) ->
        --             let
        --                 updatedDict = Dict.insert name mesh model.generatedShadows
        --             in
        --                 ( { model | generatedShadows = updatedDict }, Cmd.batch [ Task.perform (\_ -> GenerateShadows name rest) (Task.succeed True) ])

        -- This is needed for our widget
        WidgetMsg wMsg ->
            let
                (newWModel, wCmd) = Widget.update wMsg model.widget
            in
            ( { model | widget = newWModel }, Cmd.map WidgetMsg wCmd )

        Reset -> ( { model | azimuth = Angle.degrees 0, elevation = Angle.degrees 30
                            , shape = Cube
                            , draw = Matte
                            , l = 20
                            , w = 10
                            , h = 15
                            , sides = 5
                            , xAngle = 30
                            , yAngle = 30
                            , zAngle = 30
                            , clr = RGB
                            , red = 255
                            , green = 0
                            , blue = 0
                            , hasMove = False
                            , hasRotateX = False
                            , hasRotateY = False
                            , hasRotateZ = False
                            , hasScale = False
                            , scl = 2
                            , x = 0
                            , y = 0
                            , z = 0
                            , r = 10
                            , thickness = 3
                            , currentButton = None
                            , buttonDownTime = 0
                            , roughness = 0
                             } , Cmd.none )

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
{--        GSVGTextureMsg tMsg ->
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
                ) --}

        TextureLoadMsg tlMsg ->
            let
                (tlModel, tlCmd) = TL.update tlMsg model.textureLoader
            in
            (
                { model | textureLoader = tlModel }
            ,   Cmd.map TextureLoadMsg tlCmd
            )

        --shape creator
        ButtonDown dir ->
            ( { model | currentButton = dir, clr = RGB }, Cmd.none)

        Sten stencil ->
            ({ model | shape = stencil }, Cmd.none)

        Draw draw ->
            ({ model | draw = draw }, Cmd.none)

        LStyle ->
            ({ model
                | style =
                    case model.style of
                        Solid ->
                            Dotted

                        Dotted ->
                            Dashed

                        Dashed ->
                            Longdash

                        Longdash ->
                            Dotdash

                        Dotdash ->
                            Solid
            }, Cmd.none)

        CycleText ->
            ({ model | txt = cycleTxt model.txt }, Cmd.none)

        Toggle Move ->
            ({ model | hasMove = not model.hasMove }, Cmd.none)

        Toggle RotateX ->
            ({ model | hasRotateX = not model.hasRotateX }, Cmd.none)

        Toggle RotateY ->
             ({ model | hasRotateY = not model.hasRotateY }, Cmd.none)

        Toggle RotateZ ->
             ({ model | hasRotateZ = not model.hasRotateZ }, Cmd.none)

        Toggle Scale ->
            ({ model | hasScale = not model.hasScale }, Cmd.none)

        TransM t ->
           (t model, Cmd.none)
          -- (model, Cmd.none)

        SetColour clr ->
            ({ model | clr = clr }, Cmd.none)

        -- ran out of room for notifications, but left them here for a possible future improvement
        Notif notif ->
            ({ model | notify = notif }, Cmd.none)

        NextPolygon ->
            if ((Dict.get (model.nextPolygon + 1) polygonDict) == Nothing) then
              ({ model | nextPolygon = 0 } , Cmd.none)
            else
              ({ model | nextPolygon = model.nextPolygon + 1 } , Cmd.none)
        TextureMode ->
            case model.textureStyle of
              Url ->
               ({ model | textureStyle = Svg } , Cmd.none)
              Svg ->
               ({ model | textureStyle = Url } , Cmd.none)

mouseMoveDecoder : Decoder (Msg m)
mouseMoveDecoder =
    Decode.map2 MouseMove
        (Decode.field "movementX" (Decode.map Pixels.pixels Decode.float))
        (Decode.field "movementY" (Decode.map Pixels.pixels Decode.float))


debugs : List (Entity WorldCoordinates)
debugs =
    [ Scene3d.lineSegment (Material.color Color.darkRed)
        (LineSegment3d.along Axis3d.x (Length.centimeters 0) (Length.centimeters 5000))
    , Scene3d.lineSegment (Material.color Color.darkGreen)
        (LineSegment3d.along Axis3d.y (Length.centimeters 0) (Length.centimeters 5000))
    , Scene3d.lineSegment (Material.color Color.darkBlue)
        (LineSegment3d.along Axis3d.z (Length.centimeters 0) (Length.centimeters 5000))
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



--graphics svg

-- this case catches every other string and turns it into Hello
-- since there are an infinite number of Strings, we need a catch-all case
-- main view components


stencils model =
  let
    mainRectWidth = 293
    mainRectHeight = 168
  in
    group
        [ rect mainRectWidth mainRectHeight
          |> filled (rgba 255 255 255 0.5)
          |> addOutline (solid 1) lightGrey
        , rect 80 12 |> filled white |> addOutline (solid 1) lightGrey
          |> move (-88 , mainRectHeight/2)
        , text "1. Pick a Mold!" |> serif |> italic
          |> size 10 
          |> filled titleColour
          |> move (-120 , mainRectHeight/2.1)
        , group <|
            List.map2
                (\ss y ->
                    stencilString model ss
                        |> text
                        |> fixedwidth
                        |> size 10 
                        |> filled black
                        |> notifyTap (Sten ss)
                        |> move ( -130, 68 )
                        |> time1 model ss 280 10
                        |> move ( 0, y )
                )
                [ Cube, Square3D, Rectangle3D, Boxx, Sphere, Cone, Cylinder, Ring, PolyCone
                , PolyCylinder, Ellipsoid, Polygon3D, CustomPolygon, TexturedCylinder, TruncatedCone ]
                (List.map (\x -> -10.5 * Basics.toFloat x) (List.range 0 20))
                
        ]


stamps model =
  let
    mainRectWidth = 150
    mainRectHeight = 50
  in
    group
        [ rect mainRectWidth mainRectHeight |> filled (rgba 255 255 255 0.5) |> addOutline (solid 1) lightGrey
        , rect 100 12 |> filled white
            |> addOutline (solid 1) lightGrey
            |> move (0 , mainRectHeight/2)
        , text "2. Pick a Material!" |> serif |> italic |> size 10 |> filled titleColour
            |> move (-40 , mainRectHeight/2.3)
        , group <|
            List.map2
                (\ss y ->
                    stampString model ss
                        |> text
                        |> fixedwidth
                        |> size 10
                        |> filled black
                        |> notifyTap (Draw ss)
                        |> move ( -60, 10)
                        |> time2 model ss 140 10
                        |> move ( 0, y )
                )
                [ Metal, Plastic, Matte, Texture ]
                (List.map (\x -> -10.5 * Basics.toFloat x) (List.range 0 20))
        ]


colours model =
  let
    mainRectWidth = 150
    mainRectHeight = 364
  in
    group
        [ rect mainRectWidth mainRectHeight |> filled (rgba 255 255 255 0.5) |> addOutline (solid 1) lightGrey
        , rect 100 12 |> filled white |> addOutline (solid 1) lightGrey
         |> move ( 0, mainRectHeight/2 )
        , text "3. Pick a Colour!" |> serif |> italic |> size 10 |> filled titleColour
         |> move ( -38, mainRectHeight/2.05 )
        , group <|
            List.map2
                (\ss y ->
                    clrString model ss
                        |> text
                        |> fixedwidth
                        |> size 10
                        |> filled black
                        |> notifyTap (SetColour ss)
                        |> move ( -65, 165 )
                        |> time3 model ss 140 10
                        |> move ( 0, y )
                )
                [ Black
                , White
                , Blue
                , DarkBlue
                , LightBlue
                , Brown
                , DarkBrown
                , LightBrown
                , Charcoal
                , DarkCharcoal
                , LightCharcoal
                , Gray
                , DarkGray
                , LightGray
                , Green
                , DarkGreen
                , LightGreen
                , Orange
                , DarkOrange
                , LightOrange
                , Purple
                , DarkPurple
                , LightPurple
                , Yellow
                , DarkYellow
                , LightYellow
                , Red
                , DarkRed
                , LightRed
                ]
                (List.map (\x -> -10.2 * Basics.toFloat x) (List.range 0 40))
        , rect 140 10
            |> filled (rgba 1 1 1 0)
            |> notifyTap (SetColour RGB)
            |> time3 model RGB 140 10
            |> move (0 , -10)
        , group
            [ triangle 8
                |> filled (rgb 255 10 10)
                |> rotate (degrees -30)
                |> move ( -95, -10 )
                |> notifyTap
                    (TransM
                        (\m ->
                            { m
                                | red =
                                    if m.red < 254 then
                                        m.red + 1

                                    else
                                        255
                            }
                        )
                    )
                |> notifyMouseDown (ButtonDown RedUp)
                |> notifyMouseUp (ButtonDown None)
            , triangle 8
                |> filled (rgb 180 140 140)
                |> rotate (degrees 30)
                |> move ( -84, -9 )
                |> notifyTap
                    (TransM
                        (\m ->
                            { m
                                | red =
                                    if m.red > 1 then
                                        m.red - 1

                                    else
                                        0
                            }
                        )
                    )
                |> notifyMouseDown (ButtonDown RedDown)
                |> notifyMouseUp (ButtonDown None)
            , triangle 8
                |> filled (rgb 10 255 10)
                |> rotate (degrees -30)
                |> move ( -65, -10 )
                |> notifyTap
                    (TransM
                        (\m ->
                            { m
                                | green =
                                    if m.green < 254 then
                                        m.green + 1

                                    else
                                        255
                            }
                        )
                    )
                |> notifyMouseDown (ButtonDown GreenUp)
                |> notifyMouseUp (ButtonDown None)
            , triangle 8
                |> filled (rgb 140 180 140)
                |> rotate (degrees 30)
                |> move ( -54, -9 )
                |> notifyTap
                    (TransM
                        (\m ->
                            { m
                                | green =
                                    if m.green > 1 then
                                        m.green - 1

                                    else
                                        0
                            }
                        )
                    )
                |> notifyMouseDown (ButtonDown GreenDown)
                |> notifyMouseUp (ButtonDown None)
            , triangle 8
                |> filled (rgb 10 10 255)
                |> rotate (degrees -30)
                |> move ( -34.4, -9 )
                |> notifyTap
                    (TransM
                        (\m ->
                            { m
                                | blue =
                                    if m.blue < 254 then
                                        m.blue + 1

                                    else
                                        255
                            }
                        )
                    )
                |> notifyMouseDown (ButtonDown BlueUp)
                |> notifyMouseUp (ButtonDown None)
            , triangle 8
                |> filled (rgb 140 140 180)
                |> rotate (degrees 30)
                |> move ( -24, -9 )
                |> notifyTap
                    (TransM
                        (\m ->
                            { m
                                | blue =
                                    if m.blue > 1 then
                                        m.blue - 1

                                    else
                                        0
                            }
                        )
                    )
                |> notifyMouseDown (ButtonDown BlueDown)
                |> notifyMouseUp (ButtonDown None)
            , "lighter" |> code  model |> move ( -95, -25 )
            , rect 32 10
                |> filled blank
                |> move ( -75, -27 )
                |> notifyTap
                    (TransM
                        (\m ->
                            { m
                                | red = clamp 0 255 (m.red + 5)
                                , blue = clamp 0 255 (m.blue + 5)
                                , green = clamp 0 255 (m.green + 5)
                            }
                        )
                    )
            , "darker" |> code model |> move ( -38, -25 )
            , rect 32 10
                |> filled blank
                |> move ( -30, -27 )
                |> notifyTap
                    (TransM
                        (\m ->
                            { m
                                | red = clamp 0 255 (m.red - 5)
                                , blue = clamp 0 255 (m.blue - 5)
                                , green = clamp 0 255 (m.green - 5)
                            }
                        )
                    )
            , "colourful" |> code model|> move ( -100, -40 )
            , rect 40 10
                |> filled blank
                |> move ( -80, -47 )
                |> notifyTap
                    (TransM
                        (\m ->
                            case getBrightest model.red model.green model.blue of
                                All ->
                                    m

                                BrRed ->
                                    { m
                                        | red = clamp 0 255 (m.red + colourAmount * 2)
                                        , blue = clamp 0 255 (m.blue - colourAmount)
                                        , green = clamp 0 255 (m.green - colourAmount)
                                    }

                                BrGreen ->
                                    { m
                                        | red = clamp 0 255 (m.red - colourAmount)
                                        , blue = clamp 0 255 (m.blue - colourAmount)
                                        , green = clamp 0 255 (m.green + colourAmount * 2)
                                    }

                                BrBlue ->
                                    { m
                                        | red = clamp 0 255 (m.red - colourAmount)
                                        , blue = clamp 0 255 (m.blue + colourAmount * 2)
                                        , green = clamp 0 255 (m.green - colourAmount)
                                    }

                                RedAndGreen ->
                                    { m
                                        | red = clamp 0 255 (m.red + colourAmount)
                                        , blue = clamp 0 255 (m.blue - colourAmount * 2)
                                        , green = clamp 0 255 (m.green + colourAmount)
                                    }

                                RedAndBlue ->
                                    { m
                                        | red = clamp 0 255 (m.red + colourAmount)
                                        , blue = clamp 0 255 (m.blue + colourAmount)
                                        , green = clamp 0 255 (m.green - colourAmount * 2)
                                    }

                                BlueAndGreen ->
                                    { m
                                        | red = clamp 0 255 (m.red - colourAmount * 2)
                                        , blue = clamp 0 255 (m.blue + colourAmount)
                                        , green = clamp 0 255 (m.green + colourAmount)
                                    }
                        )
                    )
            , "colourless" |> code model |> move ( -44, -40 )
            , rect 44 10
                |> filled blank
                |> move ( -23, -47 )
                |> notifyTap
                    (TransM
                        (\m ->
                            case getBrightest model.red model.green model.blue of
                                All ->
                                    m

                                BrRed ->
                                    { m
                                        | red = clamp 0 255 (m.red - colourAmount * 2)
                                        , blue = clamp 0 255 (m.blue + colourAmount)
                                        , green = clamp 0 255 (m.green + colourAmount)
                                    }

                                BrGreen ->
                                    { m
                                        | red = clamp 0 255 (m.red + colourAmount)
                                        , blue = clamp 0 255 (m.blue + colourAmount)
                                        , green = clamp 0 255 (m.green - colourAmount * 2)
                                    }

                                BrBlue ->
                                    { m
                                        | red = clamp 0 255 (m.red + colourAmount)
                                        , blue = clamp 0 255 (m.blue - colourAmount * 2)
                                        , green = clamp 0 255 (m.green + colourAmount)
                                    }

                                RedAndGreen ->
                                    { m
                                        | red = clamp 0 255 (m.red - colourAmount)
                                        , blue = clamp 0 255 (m.blue + colourAmount * 2)
                                        , green = clamp 0 255 (m.green - colourAmount)
                                    }

                                RedAndBlue ->
                                    { m
                                        | red = clamp 0 255 (m.red - colourAmount)
                                        , blue = clamp 0 255 (m.blue - colourAmount)
                                        , green = clamp 0 255 (m.green + colourAmount * 2)
                                    }

                                BlueAndGreen ->
                                    { m
                                        | red = clamp 0 255 (m.red + colourAmount * 2)
                                        , blue = clamp 0 255 (m.blue - colourAmount)
                                        , green = clamp 0 255 (m.green - colourAmount)
                                    }
                        )
                    )
            ]
            |> move ( 50, -130 )
        ]


transforms model =
  let
    mainRectWidth = 175
    mainRectHeight = 70.5
  in
    group
        [ rect mainRectWidth mainRectHeight |> filled (rgba 255 255 255 0.5) |> addOutline (solid 1) lightGrey
        , rect 100 12 |> filled white |> addOutline (solid 1) lightGrey
          |> move ( -25, mainRectHeight/2 )
        , text "4. Apply Transforms!" |> serif |> italic |> size 10 |> filled titleColour
          |> move ( -68, mainRectHeight/2.2  )
        , group <|
            List.map2
                (\ss y ->
                    transformString model ss
                        |> text
                        |> fixedwidth
                        |> size 10
                        |> filled black
                        |> notifyTap (Toggle ss)
                        |> move ( -80, 15 )
                        |> time4 model ss 150 10
                        |> move ( -0, y)
                )
                [ Scale, RotateX, RotateY, RotateZ, Move ]
                (List.map (\x -> -10.5 * Basics.toFloat x) (List.range 0 20))
        ]


tweaks model =
  let
    mainRectWidth = 150
    mainRectHeight = 181
  in
    group
        [ rect mainRectWidth mainRectHeight |> filled (rgba 255 255 255 0.5) |> addOutline (solid 1) lightGrey
        , rect 60 12 |> filled white |> addOutline (solid 1) lightGrey
          |> move ( -25, mainRectHeight/2 )
        , text "5. Tweak it!" |> serif |> italic |> size 10 |> filled titleColour
          |> move ( -50, mainRectHeight/2.05 )
        , group <|
            List.map2
                (\( str, msg ) ( x, y ) ->
                    str
                        |> text
                        |> fixedwidth
                        |> size 10
                        |> filled black
                        |> notifyTap msg
                        |> move ( -35 + x, 75 + y )
                )
                [ ( "move(+,_,_)", TransM (\m -> { m | x = m.x + 10 }) )
                , ( "move(-,_,_)", TransM (\m -> { m | x = m.x - 10 }) )
                , ( "move(_,+,_)", TransM (\m -> { m | y = m.y + 10 }) )
                , ( "move(_,-,_)", TransM (\m -> { m | y = m.y - 10 }) )
                , ( "move(_,_,+)", TransM (\m -> { m | z = m.z + 10 }) )
                , ( "move(_,_,-)", TransM (\m -> { m | z = m.z - 10 }) )



                , ( "x-clockwise", TransM (\m -> { m | xAngle = m.xAngle - 30 }) )
                , ( "x-counter", TransM (\m -> { m | xAngle = m.xAngle + 30 }) )
                , ( "y-clockwise", TransM (\m -> { m | yAngle = m.yAngle - 30 }) )
                , ( "y-counter", TransM (\m -> { m | yAngle = m.yAngle + 30 }) )
                , ( "z-clockwise", TransM (\m -> { m | zAngle = m.zAngle - 30 }) )
                , ( "z-counter", TransM (\m -> { m | zAngle = m.zAngle + 30 }) )





            , ( "roughness(+)"
                  , TransM
                        (\m ->
                            { m
                                | roughness =
                                    if m.roughness <= 0.9 then
                                       m.roughness + 0.1

                                    else
                                        1
                            }
                        )
                  )
              , ( "roughness(-)"
                  , TransM
                        (\m ->
                            { m
                                | roughness =
                                    if m.roughness >= 0.1 then
                                         m.roughness - 0.1

                                    else
                                        0
                            }
                        )
                  )



                , ( "bigger"
                  , TransM
                        (\m ->
                            { m
                                | scl =
                                    if m.scl < 3 then
                                        m.scl + 0.25

                                    else
                                        3
                                , sclx =
                                    if m.sclx < 3 then
                                        m.sclx + 0.25

                                    else
                                        3
                                , scly =
                                    if m.scly < 3 then
                                        m.scly + 0.25

                                    else
                                        3
                            }
                        )
                  )
                , ( "smaller"
                  , TransM
                        (\m ->
                            { m
                                | scl =
                                    if m.scl > -3 then
                                        m.scl - 0.25

                                    else
                                        -3
                                , sclx =
                                    if m.sclx > -3 then
                                        m.sclx - 0.25

                                    else
                                        -3
                                , scly =
                                    if m.scly > -3 then
                                        m.scly - 0.25

                                    else
                                        -3
                            }
                        )
                  )


                , ( "x-clockwise", TransM (\m -> { m | xAngle = m.xAngle - 30 }) )
                , ( "x-counter", TransM (\m -> { m | xAngle = m.xAngle + 30 }) )
                , ( "y-clockwise", TransM (\m -> { m | yAngle = m.yAngle - 30 }) )
                , ( "y-counter", TransM (\m -> { m | yAngle = m.yAngle + 30 }) )
                , ( "z-clockwise", TransM (\m -> { m | zAngle = m.zAngle - 30}) )
                , ( "z-counter", TransM (\m -> { m | zAngle = m.zAngle + 30 }) )



                , ( "x-wider"
                  , TransM
                        (\m ->
                            { m
                                | l =
                                    if m.l < 100 then
                                        m.l + 10

                                    else
                                        100
                            }
                        )
                  )
                , ( "x-narrower"
                  , TransM
                        (\m ->
                            { m
                                | l =
                                    if m.l > 10 then
                                        m.l - 10

                                    else
                                        10
                            }
                        )
                  )
                , ( "y-wider"
                    , TransM
                        (\m ->
                            { m
                                | w =
                                    if m.w < 100 then
                                        m.w + 10

                                    else
                                        100
                            }
                        )
                  )
                , ( "y-narrower"
                  , TransM
                        (\m ->
                            { m
                                | w =
                                    if m.w > 10 then
                                        m.w - 10

                                    else
                                        10
                            }
                        )
                  )
                , ( "z-taller"
                  , TransM
                        (\m ->
                            { m
                                | h =
                                    if m.h < 100 then
                                        m.h + 10

                                    else
                                        100
                            }
                        )
                  )
                , ( "z-shorter"
                  , TransM
                        (\m ->
                            { m
                                | h =
                                    if m.h > 10 then
                                        m.h - 10

                                    else
                                        10
                            }
                        )
                  )

                , ( "radius(+)"
                  , TransM
                        (\m ->
                            { m
                                | r =
                                    if m.r < 100 then
                                        m.r + 10

                                    else
                                        100
                            }
                        )
                  )
                , ( "radius(-)"
                  , TransM
                        (\m ->
                            { m
                                | r =
                                    if m.r > 10 then
                                        m.r - 10

                                    else
                                        10
                            }
                        )
                  )

                , ( "thicker"
                  , TransM
                        (\m ->
                            { m
                                | thickness =
                                    if m.thickness < 20 then
                                        m.thickness + 3

                                    else
                                        20
                            }
                        )
                  )
                , ( "thinner"
                  , TransM
                        (\m ->
                            { m
                                | thickness =
                                    if m.thickness > 1 then
                                        m.thickness - 3

                                    else
                                        1
                            }
                        )
                  )


                -- , ( "red(+)"
                --   , TransM
                --         (\m ->
                --             { m
                --                 | red =
                --                     if m.red < 248 then
                --                         m.red + 7

                --                     else
                --                         255
                --             }
                --         )
                --   )
                -- , ( "red(-)"
                --   , TransM
                --         (\m ->
                --             { m
                --                 | red =
                --                     if m.red > 8 then
                --                         m.red - 8

                --                     else
                --                         0
                --             }
                --         )
                --   )
                -- , ( "green(+)"
                --   , TransM
                --         (\m ->
                --             { m
                --                 | green =
                --                     if m.green < 248 then
                --                         m.green + 7

                --                     else
                --                         255
                --             }
                --         )
                --   )
                -- , ( "green(-)"
                --   , TransM
                --         (\m ->
                --             { m
                --                 | green =
                --                     if m.green > 8 then
                --                         m.green - 8

                --                     else
                --                         0
                --             }
                --         )
                --   )
                -- , ( "blue(+)"
                --   , TransM
                --         (\m ->
                --             { m
                --                 | blue =
                --                     if m.blue < 248 then
                --                         m.blue + 7

                --                     else
                --                         255
                --             }
                --         )
                --   )
                -- , ( "blue(-)"
                --   , TransM
                --         (\m ->
                --             { m
                --                 | blue =
                --                     if m.blue > 8 then
                --                         m.blue - 8

                --                     else
                --                         0
                --             }
                --         )
                --   )


                , ( "sides(+)"
                  , TransM
                        (\m ->
                            { m
                                | sides =
                                    if m.sides > 20 then
                                        3

                                    else
                                        m.sides + 1
                            }
                        )
                  )

                ,
                ( "sides(-)"
                  , TransM
                        (\m ->
                            { m
                                | sides =
                                    if m.sides < 3 then
                                        20

                                    else
                                        m.sides - 1
                            }
                        )
                  )
                ]
                (List.concat <| List.map (\idx -> [ ( -30, -(10 * Basics.toFloat idx) ), ( 40, -(10 * Basics.toFloat idx) ) ])
                (List.range 0 20))
                
        ]


yourCode model =
    let
      mainRectWidth = 298
      mainRectHeight = 50
    in
    group
        [ rect mainRectWidth mainRectHeight 
          |> filled (rgba 255 255 255 0.5) |> addOutline (solid 1) lightGrey
        , rect 100 12
          |> filled white |> addOutline (solid 1) lightGrey
          |> move ( -85, mainRectHeight/2 )
        , text "6. Copy Your Code!" |> serif |> italic 
          |> size 10 |> filled titleColour
          |> move ( -125, mainRectHeight/2.4 )
        , move ( -130, 45 ) <|
            group <|
                [ stencilString model model.shape |> copiable model
                  |> move ( -10, -40 )
                , "  |> "
                    ++ stampString model model.draw
                    ++ (if (model.draw == Texture) then (textureString model.textureStyle) else (clrString model model.clr))
                    ++ (if (model.draw == Metal || model.draw == Plastic) then " " ++ ( Round.round 1 model.roughness) else "") --TODO: change this to actual roughness variable
                    |> copiable model
                    |> move ( 0, -52 )

                ]
                    ++ List.map2 (\str y -> str |> copiable model |> move ( 0, y ))
                        (List.concat <|
                            List.map
                                (\( flag, t ) ->
                                    if flag then
                                        [ "  " ++ transformString model t ]

                                    else
                                        []
                                )
                                [ ( model.hasScale, Scale )
                                , ( model.hasRotateX, RotateX )
                                ,  ( model.hasRotateY, RotateY )
                                ,  ( model.hasRotateZ, RotateZ )
                                , ( model.hasMove, Move )
                                ]
                        )
                        [ -62
                          , -70
                          , -80
                          , -90
                          , -100
                          , -110
                          , -120 ]
        ]



-- check if the drawn text is the selected function, and if so group a beating rectangle behind it
-- stagger the heartbeats of selected elements to so that they indicate the order of selection


time1 model ss w h shape =
    if ss == model.shape then
        group [ rect w h |> filled (rgba 255 137 5 (0.6 + 0.4 * sin (5 * model.time)))
                         |> move (0 , 70), shape ]

    else
        shape


time2 model ss w h shape =
    if ss == model.draw then
        group [ rect w h |> filled (rgba 255 137 5 (0.6 + 0.4 * sin (5 * model.time - 0.5)))
                         |> move (0, 13), shape ]

    else
        shape


time3 model ss w h shape =
    if ss == model.clr then
        group [ rect w h |> filled (rgba 255 137 5 (0.6 + 0.4 * sin (5 * model.time - 1)))
                         |> move ( 0, 168 ), shape ]

    else
        shape


time4 model t w h shape =
    if
        case t of
            Move ->
                model.hasMove

            RotateX ->
                model.hasRotateX

            RotateY ->
                model.hasRotateY

            RotateZ ->
                model.hasRotateZ

            Scale ->
                model.hasScale

    then
        group [ rect w h |> filled (rgba 255 137 5 (0.6 + 0.4 * sin (5 * model.time - 1.5)))
                         |> move (0 , 18), shape ]

    else
        shape



-- view helpers


stencilString m shape =
    case shape of
        Cube ->
            "cube " ++ String.fromFloat m.w

        Square3D ->
            "square3D " ++ String.fromFloat m.w

        Rectangle3D ->
            "rectangle3D "  ++ String.fromFloat m.l ++ " " ++ String.fromFloat m.w

        Boxx ->
            "box " ++ String.fromFloat m.l ++ " " ++ String.fromFloat m.w ++ " " ++ String.fromFloat m.h

        Sphere ->
            "sphere " ++ String.fromFloat m.r

        Cone ->
            "cone " ++ String.fromFloat m.r ++ " " ++ String.fromFloat m.h

        Cylinder ->
            "cylinder " ++ String.fromFloat m.r ++ " " ++ String.fromFloat m.h

        Ring ->
            "ring " ++ String.fromFloat m.r ++ " " ++ String.fromFloat m.thickness ++ " model.meshStore"

        PolyCone ->
            "polyCone "  ++ "polyPoints (0,0,25) model.meshStore"


        PolyCylinder ->
            "polyCylinder " ++ "polyPoints " ++ String.fromFloat m.h ++ " model.meshStore"

        Ellipsoid ->
            "ellipsoid " ++ String.fromFloat m.w ++ " " ++ String.fromFloat m.l ++ " " ++ String.fromFloat m.h ++ " model.meshStore"

        Polygon3D ->
            "polygon3D "  ++ String.fromInt m.sides ++ " " ++ String.fromFloat m.w

        CustomPolygon ->
            "customPolygon " ++ "polyPoints"

        TexturedCylinder ->
            "texturedCylinder " ++ String.fromFloat m.r ++ " " ++ String.fromFloat m.h ++ " model.meshStore"

        TruncatedCone ->
            "truncatedCone " ++ String.fromFloat m.r ++ " " ++ String.fromFloat (m.r + 10) ++ " " ++ String.fromFloat m.h ++ " model.meshStore"




stencilFun m =
    case m.shape of
        Cube ->
            cube m.w

        Boxx ->
            box m.l m.w m.h

        Sphere ->
            cube m.r

        Cone ->
            cone m.r m.h

        Cylinder ->
            cylinder m.r m.h


        _ -> cube m.w --not gonna happen

getPolygonDic model =
  case Dict.get model.nextPolygon polygonDict of
    Just polygon -> polygon
    Nothing -> [(-10,-10), (10,10), (10,-10),(-10,-10)]

stencilFunTextured m =
    case m.shape of
        Square3D ->
            square3D m.w

        Rectangle3D ->
            rectangle3D m.l m.w

        Sphere ->
            sphere m.r

        Polygon3D ->
            polygon3D m.sides m.w

        PolyCone -> polyCone (getPolygonDic m) (0,0,25) m.meshStore

        PolyCylinder -> polyCylinder (getPolygonDic m) m.h m.meshStore

        Ellipsoid -> ellipsoid m.w m.l m.h m.meshStore

        CustomPolygon -> customPolygon (getPolygonDic m)

        Ring ->
            ring m.r m.thickness m.meshStore

        TexturedCylinder ->
            texturedCylinder  m.r m.h m.meshStore

        TruncatedCone ->
            truncatedCone m.r (m.r + 10) m.h m.meshStore

        _ -> sphere m.r




stampString m stamp =
    case stamp of
        Metal ->
            "metallic "

        Plastic ->
            "plastic "

        Matte ->
            "matte "

        Texture ->
            "textured "



-- shapeFunTextured : Model -> Object coordinates
shapeFunTextured m =
    (
    case m.draw of
        Metal ->
            metallic (colourFun m) m.roughness (stencilFunTextured m)

        Plastic ->
            plastic (colourFun m) m.roughness  (stencilFunTextured m)

        Matte -> matte (colourFun m) (stencilFunTextured m)

        Texture -> 
            case m.textureStyle of
             Url ->
               textured (getColorTexture "example" m) (constantTexture 0) (constantTexture 0) (stencilFunTextured m)
             Svg ->
               textured (getColorTexture "squares" m) (constantTexture 0) (constantTexture 0) (stencilFunTextured m)


    )
        |> (if m.hasMove then
                move3D ( m.x, m.y, m.z)

            else
                \x -> x
           )
        |> (if m.hasRotateX then
                rotateX3D (degrees m.xAngle)

            else
                \x -> x
           )
        |> (if m.hasRotateY then
                rotateY3D (degrees m.yAngle)

            else
                \y -> y
           )
        |> (if m.hasRotateZ then
                rotateZ3D (degrees m.zAngle)

            else
                \z -> z
           )
        |> (if m.hasScale then
                scale3D m.scl

            else
                \x -> x
           )


shapeFun : Model -> Object coordinates
shapeFun m =
    (
    -- let
    --     stencil = if (List.member m.shape [Cube, Boxx, Cone, Cylinder]) then  stencilFun m else stencilFunTextured m

    -- in

    case m.draw of
        Metal ->
            metallic (colourFun m) m.roughness (stencilFun m)

        Plastic ->
            plastic (colourFun m) m.roughness (stencilFun m)

        Matte -> matte (colourFun m) (stencilFun m)

        _ -> matte (colourFun m) (stencilFun m)


    )
        |> (if m.hasMove then
                move3D ( m.x, m.y, m.z)

            else
                \x -> x
           )
        |> (if m.hasRotateX then
                rotateX3D (degrees m.xAngle) --TODO different angles

            else
                \x -> x
           )
        |> (if m.hasRotateY then
                rotateY3D (degrees m.yAngle) --TODO different angles

            else
                \y -> y
           )
        |> (if m.hasRotateZ then
                rotateZ3D (degrees m.zAngle) --TODO different angles

            else
                \z -> z
           )
        |> (if m.hasScale then
                scale3D m.scl

            else
                \x -> x
           )

textureString texture =
  case texture of
    Url ->
      "(getColorTexture \"example\" model) " ++ "(constantTexture 0) " ++ "(constantTexture 0)"
    Svg ->
      "(getColorTexture \"squares\" model) " ++ "(constantTexture 0) " ++ "(constantTexture 0)"

clrString m clr =
    case clr of
        Black ->
            "Color.black"

        Blue ->
            "Color.blue"

        Brown ->
            "Color.brown"

        Charcoal ->
            "Color.charcoal"

        DarkBlue ->
            "Color.darkBlue"

        DarkBrown ->
            "Color.darkBrown"

        DarkCharcoal ->
            "Color.darkCharcoal"

        DarkGray ->
            "Color.darkGray"

        DarkGreen ->
            "Color.darkGreen"

        DarkGrey ->
            "Color.darkGrey"

        DarkOrange ->
            "Color.darkOrange"

        DarkPurple ->
            "Color.darkPurple"

        DarkRed ->
            "Color.darkRed"

        DarkYellow ->
            "Color.darkYellow"

        Gray ->
            "Color.gray"

        Green ->
            "Color.green"

        Grey ->
            "Color.grey"


        LightBlue ->
            "Color.lightBlue"

        LightBrown ->
            "Color.lightBrown"

        LightCharcoal ->
            "Color.lightCharcoal"

        LightGray ->
            "Color.lightGray"

        LightGreen ->
            "Color.lightGreen"

        LightGrey ->
            "Color.lightGrey"

        LightOrange ->
            "Color.lightOrange"

        LightPurple ->
            "Color.lightPurple"

        LightRed ->
            "Color.lightRed"

        LightYellow ->
            "Color.lightYellow"

        Orange ->
            "Color.orange"


        Purple ->
            "Color.purple"

        Red ->
            "Color.red"

        White ->
            "Color.white"

        Yellow ->
            "Color.yellow"

        RGB ->
            "(Color.rgb255 " ++ String.fromInt m.red ++ " " ++ String.fromInt m.green ++ " " ++ String.fromInt m.blue ++ ")"


colourFun m =
    case m.clr of
        Black ->
            Color.black

        Blue ->
            Color.blue

        Brown ->
            Color.brown

        Charcoal ->
            Color.charcoal

        DarkBlue ->
            Color.darkBlue

        DarkBrown ->
            Color.darkBrown

        DarkCharcoal ->
            Color.darkCharcoal

        DarkGray ->
            Color.darkGray

        DarkGreen ->
            Color.darkGreen

        DarkGrey ->
            Color.darkGrey

        DarkOrange ->
            Color.darkOrange

        DarkPurple ->
            Color.darkPurple

        DarkRed ->
            Color.darkRed

        DarkYellow ->
            Color.darkYellow

        Gray ->
            Color.gray

        Green ->
            Color.green

        Grey ->
           Color.grey

        LightBlue ->
            Color.lightBlue

        LightBrown ->
            Color.lightBrown

        LightCharcoal ->
            Color.lightCharcoal

        LightGray ->
            Color.lightGray

        LightGreen ->
            Color.lightGreen

        LightGrey ->
            Color.lightGrey

        LightOrange ->
            Color.lightOrange


        LightPurple ->
            Color.lightPurple

        LightRed ->
            Color.lightRed

        LightYellow ->
            Color.lightYellow

        Orange ->
            Color.orange

        Purple ->
            Color.purple

        Red ->
            Color.red

        White ->
            Color.white

        Yellow ->
            Color.yellow

        RGB ->
            Color.rgb255 m.red m.green m.blue


transformString m t =
    case t of
        Move ->
            "|> move3D (" ++ String.fromFloat m.x ++ "," ++ String.fromFloat m.y ++ "," ++ String.fromFloat m.z ++ ")"

        RotateX ->
            "|> rotateX3D (degrees " ++ String.fromFloat m.xAngle ++ ")"

        RotateY ->
            "|> rotateY3D (degrees " ++ String.fromFloat m.yAngle ++ ")"

        RotateZ ->
            "|> rotateZ3D (degrees " ++ String.fromFloat m.zAngle ++ ")"

        Scale ->
            "|> scale3D " ++ String.fromFloat m.scl




--


lineStyleFun m =
    case m.style of
        Solid ->
            solid m.lineWidth

        Dotted ->
            dotted m.lineWidth

        Dashed ->
            dashed m.lineWidth

        Longdash ->
            longdash m.lineWidth

        Dotdash ->
            dotdash m.lineWidth



-- format a string as code


titleColour =
    rgb 255 112 0


copiable model str =
    str |> text |> selectable |> fixedwidth |> size 10 |> filled black


code model str =
    str |> text |> fixedwidth |> size 10 |> filled black


colourAmount =
    3

intToFloat : Int -> Float
intToFloat i = case i of
    0 -> 0.0
    1 -> 0.1
    2 -> 0.2
    3 -> 0.3
    4 -> 0.4
    5 -> 0.5
    6 -> 0.6
    7 -> 0.7
    8 -> 0.8
    9 -> 0.9
    _ -> 1.0
