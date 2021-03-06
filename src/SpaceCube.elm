{- 3D Slot for MOR. Modified from "3d-elm-camp/WorkshopTemplateBlank.elm".
 -}

module SpaceCube exposing (main)

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
import Density exposing (Density)
import Dict exposing (Dict)
import Direction3d exposing (Direction3d)
import Duration exposing (Duration)
import Force
import Frame3d
import Html exposing (Html)
import Html.Attributes as Attributes exposing (style)
import Illuminance
import Json.Decode as Decode exposing (Decoder)
import Length exposing (Length, Meters)
import LuminousFlux exposing (LuminousFlux)
import Mass exposing (Mass)
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
import Volume
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

import Physics.World as World exposing (World)
import Physics.Body as Body exposing (Body)
import Physics.Coordinates exposing (WorldCoordinates, BodyCoordinates)
import Block3d

{-header<b>3D Slot!</b>: Make things in 3D! -}


{-editable-}

-- Consider this the equivalent of "myShapes" on the other slots. You start out with a basic shape

thrusterLeft = (Direction3d.negativeX, Point3d.centimeters 50 0 0)
thrusterRight = (Direction3d.positiveX, Point3d.centimeters -50 -75 0)

physicsBox : Block3d Meters BodyCoordinates -> Body PhysicsData
physicsBox block3D =
    let
        (w,h,d) = Block3d.dimensions block3D
        centre = Block3d.centerPoint block3D
    in
    Body.block
        block3D
        { object =
            box (Length.inCentimeters w) (Length.inCentimeters h) (Length.inCentimeters d) |> metallic Color.gray 1
        }

ptToXYZ : Point3d Meters b -> (Float, Float, Float)
ptToXYZ pt =
    let
        centre = Point3d.toTuple Length.inCentimeters pt
    in
        centre


myShip : Body PhysicsData
myShip =
    ( physicsBox
        <| Block3d.from
            (Point3d.meters -0.5 -1 -0.5)
            (Point3d.meters 0.5 1 0.5)
    )
    |> Body.withData { object = cylinder 50 200 |> metallic Color.red 0.3 |> rotate3D (degrees 90) 0 0 |> move3D (-100,0,0) }


generate3DCoords n =
  List.filter (\(x,y,z)-> x == 0 || y == 0 || z == 0 || round x == n  || round y == n  || round z == n ) <| List.concatMap (\z -> List.concatMap (\y -> List.map (\x -> (toFloat x,toFloat y,toFloat z)) (List.range 0 n)) (List.range 0 n)) (List.range 0 n)

generateCubes n =
  group3D <| List.map (createRGBCube n) (generate3DCoords n)

createRGBCube n (r, g, b) =
    let
        s = 5
    in
  cube s
    |> metallic (Color.rgb (r/toFloat n) (g/toFloat n) (b/toFloat n)) 1
    |> move3D (s * r, s * g, s *  b)


renderBodies : World PhysicsData -> Object WorldCoordinates
renderBodies world =
    group3D 
        ( List.map 
            (\body ->
            let
                bodyFrame : Frame3d.Frame3d Meters WorldCoordinates { defines : BodyCoordinates }
                bodyFrame = Body.frame body

                obj : Object BodyCoordinates
                obj = (Body.data body).object
            in
            obj
                |> placeObject bodyFrame -- -}move3D (Point3d.toTuple Length.inCentimeters <| Frame3d.originPoint bodyFrame)

                --|> rotateX3D (Frame3d.xDirection)
            ) 
            ( World.bodies world )
        )

renderBodiesAsEntities : World PhysicsData -> Entity WorldCoordinates
renderBodiesAsEntities world =
    Scene3d.group
        ( List.map 
            (\body ->
            let
                bodyFrame : Frame3d.Frame3d Meters WorldCoordinates { defines : BodyCoordinates }
                bodyFrame = Body.frame body
            in
            Scene3d.block
                (Material.matte Color.blue)
                (Block3d.from 
                    (Point3d.centimeters (-50) (-100) (-50))
                    (Point3d.centimeters 50 100 50)
                )
                |> Scene3d.placeIn bodyFrame -- -}move3D (Point3d.toTuple Length.inCentimeters <| Frame3d.originPoint bodyFrame)

                --|> rotateX3D (Frame3d.xDirection)
            ) 
            ( World.bodies world )
        )

myEntities model =  
    let
        useTexture = getTexture model.textureLoader.textures
    in
        [ -- Making an ellipsoid requires you to specify the length, width, and height. You also need to pass in the model.
            renderBodies model.world
        ,   cube 10 |> metallic Color.green 1 |> move3D (ptToXYZ <| Tuple.second thrusterLeft)
        ,   cube 10 |> metallic Color.red 1 |> move3D (ptToXYZ <| Tuple.second thrusterRight)
        ,   sphere 10 |> metallic Color.orange 1
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

-- Put your 2D shapes here, and they will be overlayed on top of the screen!
overlay : Model -> List (Shape Msg)
overlay model =
    [
        angleDisplay model
    ,   rect 20 10 |> filled green |> move(-40, 0) |> notifyTap AccelerateLeft
    ,   rect 20 10 |> filled red |> move (40, 0) |> notifyTap AccelerateRight
    ,   text ("Velocity: " ++ Debug.toString (Maybe.map Body.velocity <| List.head <| World.bodies <| model.world)) |> filled black |> move(0,-20)
    ,   text ("Origin Point: " ++ Debug.toString (Maybe.map (Frame3d.originPoint << Body.frame) <| List.head <| World.bodies <| model.world)) |> filled black |> move(-300,-30)
    ,   text ("Rotational velocity: " ++ Debug.toString (Maybe.map (Body.angularVelocity) <| List.head <| World.bodies <| model.world)) |> filled black |> move(-300,-40)
    ,   text ("x direction: " ++ Debug.toString (Maybe.map (Frame3d.xDirection << Body.frame) <| List.head <| World.bodies <| model.world)) |> filled black |> move(-300,-50)
    ,   text ("y direction: " ++ Debug.toString (Maybe.map (Frame3d.yDirection << Body.frame) <| List.head <| World.bodies <| model.world)) |> filled black |> move(-300,-60)
    ,   text ("z direction: " ++ Debug.toString (Maybe.map (Frame3d.zDirection << Body.frame) <| List.head <| World.bodies <| model.world)) |> filled black |> move(-300,-70)
    , text ("myEntities: " ++ Debug.toString (renderBodies model.world)) |> filled black |> move (0,-90)
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

--type WorldCoordinates
--    = WorldCoordinates

type alias PhysicsData =
    { object : Object BodyCoordinates
    }

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
    , world : World PhysicsData
    }

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
    | AccelerateLeft
    | AccelerateRight


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
                    , entities = 
                        -- renderBodiesAsEntities model.world :: 
                        baseEntities ++ renderEntities (myEntities model)
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
      , azimuth = Angle.degrees 0
      , elevation = Angle.degrees 30
      , meshStore = { generatedMeshes = Dict.empty, generatedShadows = Dict.empty }
      , widget = wModel
      , gSkyboxModel = gSkyboxModel
      , gTextureModel = gTextureModel
      , textureLoader = TL.init
      , colourCube = generateCubes 16
      , world = World.empty |> World.add (myShip |> Body.withBehavior (Body.dynamic (Mass.kilograms 50)))
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

                simulatedWorld =
                    World.simulate (Duration.seconds (1 / 60)) model.world

            in
                ( { model | time = timeAsNum, world = simulatedWorld }, Cmd.none )
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

        AccelerateLeft ->
            let
                accelerateBody b =
                    b
                        |> Body.applyForce (Force.newtons 10) (Tuple.first thrusterLeft) (Tuple.second thrusterLeft)
                newWorld = World.update accelerateBody model.world
            in
                ( {model | world = newWorld }, Cmd.none )

        AccelerateRight ->
            let
                accelerateBody b =
                    b |> Body.applyForce (Force.newtons 10) (Tuple.first thrusterRight) (Tuple.second thrusterRight)
                newWorld = World.update accelerateBody model.world
            in
                ( {model | world = newWorld }, Cmd.none )


mouseMoveDecoder : Decoder Msg
mouseMoveDecoder =
    Decode.map2 MouseMove
        (Decode.field "movementX" (Decode.map Pixels.pixels Decode.float))
        (Decode.field "movementY" (Decode.map Pixels.pixels Decode.float))

-- -- Draw shape from Object. Draw shape and bounding box from Object

-- drawShape : Object coordinates -> Entity coordinates
-- drawShape entityBBox = 
--   case entityBBox of
--     ObjectGroup attributes ->
--       Scene3d.group 
--         (List.map (\object -> drawShape object) attributes.subObjects)
--     Object object -> object.shape


-- drawShapeBbox : Material.Plain coordinates -> Object coordinates -> Entity coordinates
-- drawShapeBbox material entityBBox = 
--     let
--       boxOutLine boundingBox = boxOutline material boundingBox
--     in 
--      case entityBBox of
--        ObjectGroup attributes ->
--           case attributes.boundingBox of
--             Box boundingBox ->
--                [drawShape entityBBox
--                  , boxOutLine boundingBox
--                ] 
--                ++ (List.map 
--                (\obj -> drawShapeBbox material obj) 
--                attributes.subObjects)
--                |> Scene3d.group 
--             NoBox ->
--                Scene3d.group [drawShape entityBBox]
--        Object object ->
--           case object.boundingBox of
--             Box boundingBox ->
--                Scene3d.group [object.shape, boxOutLine boundingBox]
--             NoBox ->
--                Scene3d.group [object.shape]

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
