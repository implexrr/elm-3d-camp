{- 3D Slot for MOR. Modified from "3d-elm-camp/WorkshopTemplateBlank.elm".
 -}

port module ColourCube exposing (main)

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
import Rectangle2d
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
import GSVGTexture as GT
import TextureLoader as TL

import Physics.World as World
import Physics.Body as Body

{-header<b>3D Slot!</b>: Make things in 3D! -}


{-editable-}

-- Consider this the equivalent of "myShapes" on the other slots. You start out with a basic shape

port copyToClip : String -> Cmd msg

generate3DCoords n =
  List.filter (\(x,y,z) -> x == 0 || y == 0 || z == 0 || round x == n  || round y == n  || round z == n ) <|
    List.concatMap (\z ->
        List.concatMap (\y ->
            List.map (\x ->
                (toFloat x,toFloat y,toFloat z))
            (List.range 0 n))
        (List.range 0 n))
    (List.range 0 n)

--generateCubesWorld n =
--    List.foldl World.add World.empty <| List.map (createCubeBody n) (generate3DCoords (round n))
--
--createCubeBody n (r, g, b) =
--    let
--        s = 5
--    in
--          Body.block
--              (Block3d.centeredOn
--                  (Frame3d.atPoint (Point3d.fromMeters { x = n * r / 100, y = n * g / 100, z = n * b / 100 }))
--                  ( Length.centimeters s,Length.centimeters s,Length.centimeters s )
--              )
--              (r, g, b)



generateCubes n pointingAt =
  group3D <| List.map (createRGBCube (n-1) pointingAt) (generate3DCoords (n-1))

cubeSize = 5
numberOfCubes = 16

createRGBCube n pointingAt (r, g, b) =
  cube cubeSize
    |> metallic (if pointingAt == Just (r,g,b) then Color.white else Color.rgb (r/toFloat n) (g/toFloat n) (b/toFloat n)) 1
   -- |> rotate
    |> move3D (cubeSize * r, cubeSize * g, cubeSize * b)

myEntities model =
    let
        useTexture = getTexture model.textureLoader.textures
    in
        [ -- Making an ellipsoid requires you to specify the length, width, and height. You also need to pass in the model.
        group3D
            [
              model.colourCube -- |> move3D (-50,-50,-30)
            --, cylinder 1 120
            --    |> matte (Color.rgb 255 0 0)
            --    |> rotateY3D (degrees 90)
            --, cylinder 1 120
            --    |> matte (Color.rgb 0 255 0)
            --    |> rotateX3D (degrees -90)
            --, cylinder 1 120
            --    |> matte (Color.rgb 0 0 255)
            --    |> rotateZ3D (degrees 90)
            , arrow 1 120 (Color.rgb 255 0 0)
                |> rotateY3D (degrees 90)
            , arrow 1 120 (Color.rgb 0 255 0)
                |> rotateX3D (degrees -90)
            , arrow 1 120 (Color.rgb 0 0 255)
                |> rotateZ3D (degrees 90)
            ] --|> move3D (-50, -50, -25)
        ]

arrow r h c =
    group3D
        [
            cylinder r h
                |> matte c
        ,   cone r (r*4)
                |> matte c
                |> move3D (0,0,h)
        ]

-- move / edit the light
light =
    pointLight
        { position = Point3d.centimeters 0 0 100    -- position of the light
        , chromaticity = Light.sunlight             -- the colour of the light (see https://package.elm-lang.org/packages/ianmackenzie/elm-3d-scene/latest/Scene3d-Light#Chromaticity)
        , intensity = LuminousFlux.lumens 10000     -- how intense the light is
        }
showLight = False -- whether to show the light ball or not


-- Put any custom meshes you need generated in here. Make sure the values are identical.
myMeshes =
    [ -- This mesh below will be used for the ellipsoid above. Remember to specify its length, width, and height
      generateEllipsoid 25 10 15
    ]

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

firstJust : List (Maybe a) -> Maybe a
firstJust xs = case xs of
    Nothing :: rest -> firstJust rest
    Just x :: _ -> Just x
    [] -> Nothing

lookingAtCube : Model -> Maybe (Float, Float, Float)
lookingAtCube model =
    case model.pointingAt of
        Just (x, y) ->
            let
                screenRect = Rectangle2d.with
                                {
                                    x1 = Pixels.pixels 0
                                ,   y1 = Pixels.pixels 0
                                ,   x2 = Pixels.float <| toFloat <| Pixels.toInt model.width
                                ,   y2 = Pixels.float <| toFloat <| Pixels.toInt model.height
                                }
                pt = Point2d.fromPixels { x = toFloat (Pixels.toInt model.width) / 2 + x, y = toFloat (Pixels.toInt model.height) / 2 + y }

                spheres = List.map (\(r,g,b) ->
                            let
                                sPt = Point3d.fromMeters { x = cubeSize * (r-1) / 100, y = cubeSize * (g-1) / 100, z = cubeSize * (b-1) / 100 }
                            in
                            (Sphere3d.atPoint sPt (Length.centimeters 5), (r,g,b))
                          ) <| generate3DCoords numberOfCubes

                camera = globalCamera model
                allInLine = List.filterMap (\(sp, rgb) -> Maybe.map (\_ -> (sp,rgb)) <| intersectsPoint camera screenRect pt sp) spheres

                ray = Camera3d.ray camera screenRect pt

                closest =
                    Quantity.minimumBy (sphereDistanceToCamera ray << Tuple.first) allInLine
            in
                Maybe.map Tuple.second closest

        Nothing ->
            Nothing

globalCamera model =
    Camera3d.perspective
        { viewpoint =
            Viewpoint3d.orbitZ
                { focalPoint = Point3d.centimeters 50 50 50
                , azimuth = model.azimuth
                , elevation = model.elevation
                , distance = Length.meters 3
                }
        , verticalFieldOfView = Angle.degrees 45
        }

sphereDistanceToCamera : Axis3d.Axis3d units coordinates -> Sphere3d.Sphere3d units coordinates -> Quantity Float units
sphereDistanceToCamera axis sphere =
    let
        centrePoint = Sphere3d.centerPoint sphere
        --radius = Sphere3d.radius sphere

        axisOrigin =
            Axis3d.originPoint axis

        axisDirection =
            Axis3d.direction axis

        circleCenterToOrigin =
            Vector3d.from centrePoint axisOrigin

        --cto = Vector3d.unwrap circleCenterToOrigin

        --ctoLengthSquared =
        --    cto.x ^ 2 + cto.y ^ 2 + cto.z ^ 2

        dotProduct =
            Vector3d.componentIn axisDirection circleCenterToOrigin

        --r =
        --    Quantity.toFloat radius

        --inRoot =
        --    dotProduct ^ 2 - ctoLengthSquared + r ^ 2
    in
        Quantity.negate dotProduct

intersectsPoint :
    Camera3d.Camera3d units coordinates
    -> Rectangle2d.Rectangle2d screenUnits screenCoordinates
    -> Point2d.Point2d screenUnits screenCoordinates
    -> Sphere3d.Sphere3d units coordinates
    -> Maybe (Point2d.Point2d screenUnits screenCoordinates)
intersectsPoint camera rect pt sphere =
    let
        ray = Camera3d.ray camera rect pt
    in
        case Axis3d.intersectionWithSphere sphere ray of
            Just _ -> Just pt
            _ -> Nothing

-- Put your 2D shapes here, and they will be overlayed on top of the screen!
overlay : Model -> List (Shape Msg)
overlay model =
    let
        txt =
            case model.pointingAtRgb of
                Just (r,g,b) ->
                    "(rgb " ++ String.fromInt (round <| r * 255 / toFloat numberOfCubes) ++ " " ++ String.fromInt (round <| g * 255 / toFloat numberOfCubes) ++ " " ++ String.fromInt (round <| b * 255 / toFloat numberOfCubes) ++ ")"
                Nothing ->
                    ""
        pointingtxt = case model.pointingAtRgb of
            Just (r,g,b) ->
                    group
                    [
                        square 100 |> filled (rgb (numberOfCubes * r) (numberOfCubes * g) (numberOfCubes * b)) |> addOutline (solid 1) black
                    ,   text txt
                            |> centered
                            |> filled black
                            |> move(0,-65)
                    ,   (if model.copied then
                        text "Copied to clipboard!"
                            |> centered
                            |> filled (rgb 0 255 0)
                        else
                        text "Click to copy to clipboard!"
                            |> centered
                            |> filled black)
                            |> move (0,-80)
                    ] |> move (toFloat (Pixels.toInt model.width)/2 - 70, 0)
            Nothing -> group []
    in
    [
        angleDisplay model
    ,   pointingtxt
    ,   rect (toFloat <| Pixels.toInt model.width) (toFloat <| Pixels.toInt model.height)
            |> filled blank
            |> notifyMouseMoveAt PointerAt
            |> notifyMouseDown InitiateCopy
            |> notifyTap (Copy txt)
    ]

-- This colour is used to create the floor. If you want custom colours, use Color.hsl or Color.rgb!
floorColour = Color.green

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
    , colourCube : Object WorldCoordinates
    , pointingAt : Maybe (Float, Float)
    , pointingAtRgb : Maybe (Float, Float, Float)
    , copied : Bool
    , isCopyAction : Bool -- if you rotate and let go, don't copy!
    }

type PointingAt =
    NotPointing | PointingAt (Float, Float, Float)

type Msg
    = Resize (Quantity Int Pixels) (Quantity Int Pixels)
    | Tick Duration
    | MouseDown
    | MouseMove (Quantity Float Pixels) (Quantity Float Pixels)
    | MouseUp
    | VisibilityChange Browser.Events.Visibility
    | GenerateMeshes (List (GeneratedMesh WorldCoordinates))
    -- | GenerateShadows String (List (Mesh.Shadow WorldCoordinates))
    | WidgetMsg Widget.Msg
    | Reset
    | SkyboxMsg GS.Msg
    | GSVGTextureMsg GT.Msg
    | TextureLoadMsg TL.Msg
    | PointerAt (Float, Float)
    | Copy String
    | InitiateCopy


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
            Light.directional (Light.castsShadows False)
                { direction = Direction3d.xyZ (Angle.degrees -90) (Angle.degrees -45)
                , chromaticity = Light.sunlight
                , intensity = Illuminance.lux 100
                }

        -- Add some soft lighting to fill in shadowed areas
        softLighting =
            Light.soft
                { upDirection = Direction3d.positiveZ
                , chromaticity = Light.fluorescent
                , intensityAbove = Illuminance.lux 1000
                , intensityBelow = Illuminance.lux 1000
                }

        -- Create a quad to act as a 'floor'
        plane =
            Scene3d.quad (Material.matte floorColour)
                (Point3d.meters -1 -1 0)
                (Point3d.meters 1 -1 0)
                (Point3d.meters 1 1 0)
                (Point3d.meters -1 1 0)

        -- Define camera as usual
        camera = globalCamera model

        textures = model.textureLoader.textures

        baseEntities =
            (if showLight then [ firstLightBall ] else []) ++
            [ case skyboxType of
                    Skybox.GSVGSkybox _ _ _ _ ->
                        Skybox.skybox [Dict.get "skyB" textures,
                                     Dict.get "skyT" textures,
                                     Dict.get "skyS1" textures,
                                     Dict.get "skyS2" textures,
                                     Dict.get "skyS3" textures,
                                     Dict.get "skyS4" textures]
                                     1000
                    Skybox.URLSkybox _ _ _ _ _ _ ->
                        Skybox.skybox [Dict.get "skyB" textures,
                                     Dict.get "skyT" textures,
                                     Dict.get "skyS1" textures,
                                     Dict.get "skyS2" textures,
                                     Dict.get "skyS3" textures,
                                     Dict.get "skyS4" textures]
                                     1000
                    Skybox.URLSphericalSkybox _ ->
                        Skybox.roundSkybox
                                    (Dict.get "skyT" textures)
                                    500
                    Skybox.GSVGSphericalSkybox _ _ ->
                        Skybox.roundSkybox
                                    (Dict.get "skyT" textures)
                                    500

            ]

    in
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
        text ("azimuth: " ++ String.fromInt (round <| unwrapQ model.azimuth * 180 / pi) ++ "º")
                                |> filled black
                                |> move (toFloat (unwrapQ model.width) / 2 - 160, toFloat (unwrapQ model.height) / 2 - 50)
    ,   text ("elevation: " ++ String.fromInt (round <| unwrapQ model.elevation * 180 / pi) ++ "º")
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
      , orbiting = False
      , azimuth = Angle.degrees 45
      , elevation = Angle.degrees 30
      , meshStore = { generatedMeshes = Dict.empty, generatedShadows = Dict.empty }
      , widget = wModel
      , gSkyboxModel = gSkyboxModel
      , gTextureModel = gTextureModel
      , textureLoader = TL.init
      , colourCube = generateCubes numberOfCubes Nothing
      , pointingAt = Nothing
      , pointingAtRgb = Nothing
      , copied = False
      , isCopyAction = False
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
                TL.fetchTextures  ([
                                ("skyT", texture)
                               ] ++ myTextures) TL.init
            _ -> TL.fetchTextures myTextures TL.init
        , Task.perform (\_ -> GenerateMeshes myMeshes) (Task.succeed True)
        , case skyboxType of
            Skybox.GSVGSkybox _ _ _ _ -> Cmd.map SkyboxMsg gSCmd
            Skybox.GSVGSphericalSkybox _ _ -> Cmd.map SkyboxMsg gSCmd
            _ -> Cmd.none
        -- , Task.perform (\_ -> GenerateShadows name myShadowMeshes) (Task.succeed True)
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
                ( { model | time = timeAsNum }, Cmd.none )
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
                    , isCopyAction = False
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        GenerateMeshes generatedMeshes ->
            case generatedMeshes of
                [] ->
                    (model, Cmd.none)
                (generatedMesh :: rest) ->
                    let
                        updatedMeshes = Dict.insert generatedMesh.name generatedMesh.mesh model.meshStore.generatedMeshes
                        updatedShadows = Dict.insert generatedMesh.name generatedMesh.shadow model.meshStore.generatedShadows

                        updatedMeshStore = { generatedMeshes = updatedMeshes, generatedShadows = updatedShadows }
                    in
                        ( { model | meshStore = updatedMeshStore }, Cmd.batch [ Task.perform (\_ -> GenerateMeshes rest) (Task.succeed True) ])

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

        Reset -> ( { model | azimuth = Angle.degrees 45, elevation = Angle.degrees 30 } , Cmd.none )

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
            (
                { model | textureLoader = tlModel }
            ,   Cmd.map TextureLoadMsg tlCmd
            )

        PointerAt (x,y) ->
            let
                lookingAt = lookingAtCube model
            in
            ( {model | pointingAt = Just (x,y)
                     , pointingAtRgb = lookingAt
                     , copied = if lookingAt == model.pointingAtRgb then model.copied else False
                     {-, colourCube = generateCubes numberOfCubes (lookingAtCube model)-} }
            , Cmd.none
            )

        Copy txt ->
            if model.isCopyAction then ( { model | copied = True }, copyToClip txt ) else ( model, Cmd.none )

        InitiateCopy ->
            ({ model | isCopyAction = True }, Cmd.none )

mouseMoveDecoder : Decoder Msg
mouseMoveDecoder =
    Decode.map2 MouseMove
        (Decode.field "movementX" (Decode.map Pixels.pixels Decode.float))
        (Decode.field "movementY" (Decode.map Pixels.pixels Decode.float))

-- Draw shape from Object. Draw shape and bounding box from Object

drawShape : Object coordinates -> Entity coordinates
drawShape entityBBox = 
  case entityBBox of
    ObjectGroup attributes ->
      Scene3d.group 
        (List.map (\object -> drawShape object) attributes.subObjects)
    Object object -> object.shape


drawShapeBbox : Material.Plain coordinates -> Object coordinates -> Entity coordinates
drawShapeBbox material entityBBox = 
    let
      boxOutLine boundingBox = boxOutline material boundingBox
    in 
     case entityBBox of
       ObjectGroup attributes ->
          case attributes.boundingBox of
            Box boundingBox ->
               [drawShape entityBBox
                 , boxOutLine boundingBox
               ] 
               ++ (List.map 
               (\obj -> drawShapeBbox material obj) 
               attributes.subObjects)
               |> Scene3d.group 
            None ->
               Scene3d.group [drawShape entityBBox]
       Object object ->
          case object.boundingBox of
            Box boundingBox ->
               Scene3d.group [object.shape, boxOutLine boundingBox]
            None ->
               Scene3d.group [object.shape]

-- Fetch textures from textureListSkybox
-- Get a result type as List (Material.Texture Color)
-- Decode the List when we actually are going to load the texture
-- In this example, we decode the list in Skybox.skybox

-- fetchTextures : List (String, String) -> Cmd Msg
-- fetchTextures textDist =
--   let
--     keyList = List.map (\(key, texture) -> key) textDist
--     textureList = List.map (\(key, texture) -> texture) textDist
--   in
--    textureList
--     |> List.map Material.load
--     -- Load the meterial, [Material.load texture, Material.load texture... ]
--     |> Task.sequence -- sequence : List (Task x a) -> Task x (List a)
--     -- Transform a list of the tast to a tast
--     -- Get the result type as Task WebGL.Texture.Error (List (Texture value))
--     |> Task.andThen -- andThen :
--     -- concatenate two tasks
--          (\textures ->
--             case textures of
--               [] ->
--                 Task.fail WebGL.Texture.LoadError
--               textList ->
--                 Task.succeed textList)
--               -- If the list is not empty let the tast succeed
--     |> Task.attempt -- Attempt to update the task here
--        (\result ->
--             case result of
--                 Ok textures ->
--                   LoadTexture (Dict.fromList (List.map2
--                      (\key texture -> (key, texture))
--                       keyList
--                       textures
--                      ))
--                 Err error -> Error error
--         )

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
