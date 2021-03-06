{- Testing file for the Collision.elm
   When the cube collide with other objects, the objects will move along z axis
   and will show the objects name
 -}

module CollisionExample exposing (main)

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
import Wrapper3D exposing (..)
import Collision
import Dict exposing (Dict)

import GraphicSVG.Widget as Widget
import GraphicSVG exposing(..)
import Html.Attributes as HA

import GSVGSkybox as GS


{-header<b>3D Slot!</b>: Make things in 3D! -}


{-editable-}

-- Consider this the equivalent of "myShapes" on the other slots. You start out with a basic shape
myEntities model =  
    [ 
    cylinder 10 50
      |> metallic Color.yellow 0.7
      |> rotateY3D (degrees 90)
      |> move3D (40, 20,30)
      |> renderCollider 1
    , group3D [ customPolygon [(-10,-10), (10,10), (10,-10),(-10,-10)]
                |> metallic Color.grey 0.2
                |> move3D (40,50,30)
              , polygon3D 8 20
                |> metallic Color.red 0.3
                |> move3D (0,0,35)
              , group3D [ sphere 10 
                           |> metallic Color.white 0.2 
                           |> move3D (40,0,40)
                         , square3D 20 
                           |> plastic Color.blue 0.5 
                           |> rotateY3D (degrees 90) 
                           |> move3D (40,0,60) 
                         , group3D [ ellipsoid 25 10 15 model.meshStore
                                     |> matte Color.yellow
                                     |> nameObject "This is an example of a named object!"
                                     |> move3D (-10, 50, 50)
                                   , cone 10 20
                                     |> metallic Color.darkGreen 0.7
                                     |> move3D (-25, -25, 40)
                                   ]
                         ]
              ] |> renderCollider 6

    -- This group is just used for debugging the meshes
    -- , Scene3d.group (List.map (Scene3d.mesh (Material.metal {baseColor = Color.red, roughness = 0.3 })) (Dict.values model.generatedMeshes))
    --   |> move (25,25,50)
    ]

-- scrEntity is created for testing collision.
-- Test if scrEntity collide with entities in myEntity
scrEntity model =  cube 20
      |> customMat Color.orange 0.2 (-0.5*cos (model.time*2) + 0.5)
      |> move3D (0, 0, 30)
      |> move3D (50*cos model.time, 50*sin model.time, 0)
      |> renderCollider 1


-- Put any custom meshes you need generated in here. Make sure the values are identical.
myMeshes = 
    [ -- This mesh below will be used for the ellipsoid above. Remember to specify its length, width, and height
      generateEllipsoid 25 10 15
    , generatePolyCylinder 
        [(-32,-13.42),(-41.39,5.1468),(-29.98,20.139),(-14.99,10.069),(-15.44,-6.265),(-21.03,-15.44),(-26.18,-17.23),(-32,-13.42)]
        25
    , generatePolyCone [(-10,-10), (10,10), (10,-10),(-10,-10)] (0,0,25)
    ]

overlay : Model -> List (Object coordinates) -> List (Shape Msg)
overlay model collisionList =
    [
        angleDisplay model
        , collitionInfo model collisionList
    ]

-- This colour is used to create the floor. If you want custom colours, use Color.hsl or Color.rgb!
floorColour = Color.green

-- Here you can specify what images to use to create the skybox. Just replace "todo" with a link to an image. (Keep the quotes, though!)
skyboxType = Skybox.GSVGSkybox True skyboxTop skyboxSides skyBoxBottom  -- Skybox.URLSkybox textureBottom textureTop textureSide1 textureSide2 textureSide3 textureSide4

skyboxTop : Shape msg
skyboxTop =
    group
        [
            square 50 |> filled lightBlue
        ,   circle 10 |> filled yellow
        ]

skyboxSides : Shape msg
skyboxSides =
    group
        [
            rect 200 50 |> filled lightBlue |> move (0,25)
        ,   rect 200 50 |> filled green |> move(0,-25)
        ,   triangle 10 |> filled darkGreen |> rotate (degrees -30) |> move (0,5)
        ,   text "abcdefghijklmnopqrstuvwxyz" |> centered |> size 16 |> filled red
        ]

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
    , textures : Dict String (Material.Texture Color.Color)
    , meshStore : MeshStore WorldCoordinates
    , widget : Widget.Model
    , gSkyboxModel : GS.Model
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
    | LoadTexture (Dict String (Material.Texture Color.Color))
    | Error WebGL.Texture.Error
    | WidgetMsg Widget.Msg
    | Reset
    | SkyboxMsg GS.Msg


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
                , verticalFieldOfView = Angle.degrees 30
                }


        baseEntities = 
            [ firstLightBall
            , plane
            , Skybox.skybox [Dict.get "skyB" model.textures,
                             Dict.get "skyT" model.textures,
                             Dict.get "skyS1" model.textures,
                             Dict.get "skyS2" model.textures,
                             Dict.get "skyS3" model.textures,
                             Dict.get "skyS4" model.textures]
                             1000
            ]
        --- Collision testing
        collisionResultHelper = Collision.getCollisions 
                            (scrEntity model)
                            (myEntities model)
        collisionResult = 
           if (List.length collisionResultHelper) >= 1 then
             List.map (\obj -> obj 
                      |> move3D (0, 0, 30 * (repeatDuration 0.1 1 0 model.time) ) 
                     ) collisionResultHelper
           else collisionResultHelper
        noCollisionResult = Collision.getNonCollidingObjects
                            (scrEntity model)
                            (myEntities model)

    in
        Html.div []
            [   case skyboxType of
                    Skybox.GSVGSkybox debug sT sS sB ->
                        Html.div [style "position" "absolute", style "left" "0px", style "top" (String.fromInt (unwrapQ model.height) ++ "px")]
                        [
                            Html.h1 [] [Html.text "Skybox Debug"]
                        ,   Html.map SkyboxMsg <| GS.drawSkybox debug model.gSkyboxModel sT sS sB
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
                          -- Draw Shape without collision effect
                          {-- baseEntities 
                          ++ renderEntities [(scrEntity model)]
                          ++ renderEntities (myEntities model) --}
                        -- Draw Shape with collision effect
                        baseEntities 
                        ++ renderEntities collisionResult
                        ++ renderEntities noCollisionResult
                        ++ renderEntities [(scrEntity model)]
                    }
                    |> withOverlay (overlay model collisionResult) model
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

--collision Information Display
collitionInfo : Model -> List (Object coordinates) -> Shape Msg
collitionInfo model entityList = 
  let
    objName_list = List.map 
      (\entity ->
        case entity of
          ObjectGroup attributes -> attributes.name
          Object object -> object.name
      ) 
      entityList
    nameHelper = 
      List.map (\name -> text name 
          |> filled black
          |> scale 2
          |> move (-(toFloat (unwrapQ model.width) / 2 - 100), toFloat (unwrapQ model.height) / 2 - 110)) 
          objName_list
  in
   group [
    text ("Collided Object: ")
      |> filled black
      |> scale 2
      |> move (-(toFloat (unwrapQ model.width) / 2 - 100), toFloat (unwrapQ model.height) / 2 - 80)
    , group 
        <| List.map2 (\name adjustPoint -> 
           name |> move (0 , -(toFloat (adjustPoint * 30)))
          )
          nameHelper
          (List.range 0 (List.length nameHelper))
   ]


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
        , case skyboxType of
            Skybox.GSVGSkybox _ _ _ _ ->
                Sub.map SkyboxMsg (GS.subscriptions model.gSkyboxModel)
            _ -> Sub.none
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
        (gSkyboxModel, gSCmd) = GS.initialModelWithLoad
    in
    ( { width = Quantity.zero
      , height = Quantity.zero
      , time = 0
      , orbiting = False
      , azimuth = Angle.degrees 0
      , elevation = Angle.degrees 30
      , textures = Dict.empty 
      , meshStore = { generatedMeshes = Dict.empty, generatedShadows = Dict.empty }
      , widget = wModel
      , gSkyboxModel = gSkyboxModel
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
        , case skyboxType of
            Skybox.URLSkybox top bottom side1 side2 side3 side4 ->
                fetchTextures  [
                                ("skyB", textureBottom),
                                ("skyT", textureTop),
                                ("skyS1", textureSide1),
                                ("skyS2", textureSide2),
                                ("skyS3", textureSide3),
                                ("skyS4", textureSide4)
                               ]
            _ -> Cmd.none
        , Task.perform (\_ -> GenerateMeshes myMeshes) (Task.succeed True)
        , case skyboxType of
            Skybox.GSVGSkybox _ _ _ _ -> Cmd.map SkyboxMsg gSCmd
            _ -> Cmd.none
        -- , Task.perform (\_ -> GenerateShadows name myShadowMeshes) (Task.succeed True)
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

        LoadTexture textures -> 
            ( { model | textures = textures }, Cmd.none)

        Error err ->
            let
              e = Debug.log "err" err
            in
            ( model, Cmd.none)

        -- This is needed for our widget
        WidgetMsg wMsg ->
            let
                (newWModel, wCmd) = Widget.update wMsg model.widget
            in
            ( { model | widget = newWModel }, Cmd.map WidgetMsg wCmd )

        Reset -> ( { model | azimuth = Angle.degrees 0, elevation = Angle.degrees 30 } , Cmd.none )

        SkyboxMsg sMsg ->
            case sMsg of
                GS.AllLoaded allTextures ->
                    ( model, fetchTextures allTextures )
                _ ->
                    let
                        (gSkyboxModel, gSCmd) = GS.update sMsg model.gSkyboxModel
                    in
                        ( { model | gSkyboxModel = gSkyboxModel } , Cmd.map SkyboxMsg gSCmd)

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

fetchTextures : List (String, String) -> Cmd Msg
fetchTextures textDist =
  let 
    keyList = List.map (\(key, texture) -> key) textDist
    textureList = List.map (\(key, texture) -> texture) textDist
  in
   textureList
    |> List.map Material.load
    -- Load the meterial, [Material.load texture, Material.load texture... ]
    |> Task.sequence -- sequence : List (Task x a) -> Task x (List a)
    -- Transform a list of the tast to a tast
    -- Get the result type as Task WebGL.Texture.Error (List (Texture value))
    |> Task.andThen -- andThen :
    -- concatenate two tasks
         (\textures -> 
            case textures of
              [] -> 
                Task.fail WebGL.Texture.LoadError
              textList ->
                Task.succeed textList)
              -- If the list is not empty let the tast succeed
    |> Task.attempt -- Attempt to update the task here
       (\result ->
            case result of
                Ok textures -> 
                  LoadTexture (Dict.fromList (List.map2 
                     (\key texture -> (key, texture))
                      keyList 
                      textures
                     ))
                Err error -> Error error
        )