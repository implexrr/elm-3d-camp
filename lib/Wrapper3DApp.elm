module Wrapper3DApp exposing (graphicScene, world, UserModel, World)

import Angle exposing (Angle)
import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Events as Events
import Browser.Navigation exposing (Key)
import Color
import Dict
import Direction3d exposing (Direction3d)
import Duration exposing (Duration)
import GraphicSVG exposing (Shape)
import GraphicSVG.Widget as Widget
import Html exposing (Html)
import Html.Events.Extra.Wheel as Wheel
import Illuminance
import Length exposing (Length, Meters)
import LuminousFlux exposing (LuminousFlux)
import Pixels exposing (Pixels)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import Scene3d
import Scene3d.Light as Light exposing (Chromaticity, Light)
import Task
import Time
import Url exposing (Url)
import Wrapper3D exposing (..)
import Wrapper3DCamera
import PhysicsWrapper3D
import TextureLoader
import Skybox exposing (SkyboxType)

-- TODO getting SVG textures to work without forcing the presence of SVG textures required some hacky solutions.
-- TODO so, it would be a good idea to clean up this file at some point.

type HiddenMsg userMsg coordinates
    = UserMsg userMsg
    | CameraMsg (Wrapper3DCamera.Msg coordinates)
    | WidgetMsg Widget.Msg
    | TickTime Duration
    | InitTime Duration
    | Resize (Quantity Int Pixels) (Quantity Int Pixels)
    | GenerateMeshes (List (Wrapper3D.Object coordinates))
    | URLRequest Browser.UrlRequest
    | URLChanged Url.Url

type alias HiddenModel userMsg =
    { startTime : Duration
    , time : Float
    , tick : Duration -> userMsg
    , width : Quantity Int Pixels
    , height : Quantity Int Pixels
    , widget : Widget.Model
    }

{-| User-defined models must have the following fields. -}
type alias UserModel coordinates a =
    { a
    | meshStore : MeshStore coordinates
    , cameraModel : Wrapper3DCamera.Model coordinates
    , textureLoader : TextureLoader.Model
    , zoomSpeed : Length
    , width : Quantity Int Pixels
    , height : Quantity Int Pixels
    }

type alias Scene userMsg coordinates a =
    Program () ( UserModel coordinates a, HiddenModel userMsg ) (HiddenMsg userMsg coordinates)

type alias World coordinates userMsg a =
    { objects : UserModel coordinates a -> List (Wrapper3D.Object coordinates)
    , overlay : UserModel coordinates a -> List (Shape userMsg)
    , lightData : LightData coordinates
    , skyboxData : SkyboxType userMsg
    , tickMsg : Duration -> userMsg
    , customHTML : Html userMsg
    }

type alias LightData coordinates =
    { position : Point3d Meters coordinates
    , chromaticity : Chromaticity
    , intensity : LuminousFlux
    , castsShadows : Bool
    , showEntity : Bool
    }

{-| Create a new 3D scene with the given components. The `view` field can be set using the `world` function. -}
graphicScene :
    { init : ( UserModel coordinates a, Cmd userMsg )
    , update : userMsg -> UserModel coordinates a -> ( UserModel coordinates a, Cmd userMsg )
    , view : UserModel coordinates a -> World coordinates userMsg a
    , subscriptions : UserModel coordinates a -> Sub userMsg
    , title : String
    }
    -> Scene userMsg coordinates a
graphicScene input =
    let
        userView =
            { title = input.title, body = input.view }
    in
        Browser.application
            { init =
                \ flags url key ->
                    let
                        userInit =
                            Tuple.first <| input.init

                        userInitCmd =
                            Tuple.second <| input.init
                    in
                        ( ( userInit, initHiddenModel (userView.body userInit).tickMsg ), initialCmd <| Cmd.map UserMsg userInitCmd )
            , update = hiddenAppUpdate userView.body input.update
            , view = hiddenAppView ( \ userModel -> { title = userView.title, body = userView.body userModel } )
            , subscriptions = subs <| input.subscriptions
            , onUrlRequest = URLRequest
            , onUrlChange = URLChanged
            }

{-| Stitches together the data needed for a 3D scene: the 3D objects to be rendered,
an overlay for 2D UI, data for the scene's point light, and your program's tick message which will be called
about 60 times per second. -}
world :
    (UserModel coordinates a -> List (Wrapper3D.Object coordinates))
    -> (UserModel coordinates a -> List (Shape userMsg))
    -> LightData coordinates
    -> SkyboxType userMsg
    -> Html userMsg
    -> (Duration -> userMsg)
    -> (World coordinates userMsg a)
world objects overlay pointLightData skyboxType customHTML tickMsg =
    { objects = objects
    , overlay = overlay
    , lightData = pointLightData
    , skyboxData = skyboxType
    , tickMsg = tickMsg
    , customHTML = customHTML
    }

hiddenAppUpdate :
    (UserModel coordinates a -> World coordinates userMsg a)
    -> (userMsg -> UserModel coordinates a -> ( UserModel coordinates a, Cmd userMsg ))
    -> HiddenMsg userMsg coordinates
    -> ( UserModel coordinates a, HiddenModel userMsg )
    -> ( ( UserModel coordinates a, HiddenModel userMsg ), Cmd (HiddenMsg userMsg coordinates) )
hiddenAppUpdate userWorld userUpdate msg ( userModel, hiddenModel ) =
    case msg of
        UserMsg userMsg ->
            let
                (newModel, userCmds) =
                    userUpdate userMsg userModel
            in
                ( ( newModel, hiddenModel )
                , Cmd.map UserMsg userCmds
                )

        InitTime t ->
            ( ( userModel, { hiddenModel | startTime = t } ), Cmd.none )

        Resize width height ->
            let
                (wModel, wCmd) = Widget.init (toFloat <| Pixels.inPixels width) (toFloat <| Pixels.inPixels height) "widget"
            in
            ( ( { userModel | width = width, height = height }, { hiddenModel | width = width, height = height, widget = wModel }), Cmd.map WidgetMsg wCmd )

        WidgetMsg wMsg ->
            let
                (newWModel, wCmd) = Widget.update wMsg hiddenModel.widget
            in
                ( ( userModel, { hiddenModel | widget = newWModel } ), Cmd.map WidgetMsg wCmd )

        TickTime t ->
            let
                tickRate =
                    Duration.milliseconds 1 |> Quantity.per Duration.second

                updatedTime =
                    Duration.seconds hiddenModel.time |> Quantity.plus (tickRate |> Quantity.for t)

                timeAsNum = Duration.inSeconds updatedTime

                (newModel, userCmds) =
                    userUpdate (hiddenModel.tick t) userModel

            in
                ( ( newModel, { hiddenModel | time = timeAsNum } )
                , Cmd.batch
                    [ Task.perform 
                        ( \ _ -> GenerateMeshes ((userWorld userModel).objects userModel) )
                        (Task.succeed True)
                    , Cmd.map UserMsg userCmds
                    ]
                )

        CameraMsg camMsg ->
            let
                w3cModel = Wrapper3DCamera.update camMsg userModel.cameraModel
            in
                ( ( { userModel | cameraModel = w3cModel }, hiddenModel )
                , Cmd.none
                )

        GenerateMeshes objectList ->
            case objectList of
                [] ->
                    ( ( userModel, hiddenModel ), Cmd.none )
                (object :: rest) ->
                    case object of
                        ObjectGroup attr ->
                            ( ( userModel, hiddenModel ), Cmd.batch [ Task.perform ( \ _ -> GenerateMeshes ( attr.subObjects ++ rest ) ) (Task.succeed True) ])
                        Object attr ->
                            case attr.customMesh of
                                -- Skip generating if the object uses a primitive mesh
                                Primitive ->
                                    ( ( userModel, hiddenModel ), Cmd.batch [ Task.perform (\_ -> GenerateMeshes rest) (Task.succeed True) ])

                                -- Otherwise, check if the mesh already exists, and generate the mesh + shadow and store it if necessary
                                _ ->
                                    let
                                        meshExists =
                                            Dict.member attr.meshHash userModel.meshStore.generatedMeshes
                                    in
                                        if meshExists then
                                            -- Don't regenerate if the mesh already exists
                                            ( (userModel, hiddenModel), Cmd.batch [ Task.perform (\_ -> GenerateMeshes rest) (Task.succeed True) ] )
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
                                                            generateDynamicMesh triMesh doCulling

                                                        -- Since primitives are handled above, this path should never be reached unless
                                                        -- somebody forgets to update it when new meshes are added
                                                        _ ->
                                                            generateEllipsoid 0 0 0

                                                updatedMeshes = Dict.insert generatedMesh.name generatedMesh.mesh userModel.meshStore.generatedMeshes
                                                
                                                updatedShadows = Dict.insert generatedMesh.name generatedMesh.shadow userModel.meshStore.generatedShadows

                                                updatedMeshStore = { generatedMeshes = updatedMeshes, generatedShadows = updatedShadows }
                                            in
                                                ( ( { userModel | meshStore = updatedMeshStore }, hiddenModel ), Cmd.batch [ Task.perform (\_ -> GenerateMeshes rest) (Task.succeed True) ] )

        URLRequest urlreq ->
            case urlreq of
                External url ->
                    ( ( userModel, hiddenModel ), Browser.Navigation.load url )

                _ ->
                    ( ( userModel, hiddenModel ), Cmd.none )

        URLChanged url ->
            ( ( userModel, hiddenModel ), Cmd.none )

hiddenAppView :
    ( UserModel coordinates a -> { title : String, body : World coordinates userMsg a } )
    -> ( UserModel coordinates a, HiddenModel userMsg )
    -> { title : String, body : List (Html (HiddenMsg userMsg coordinates)) }
hiddenAppView userView ( userModel, hiddenModel ) =
    let
        userViewEval =
            userView userModel

        adjustZoom wheelEvent =
            let
                deltaZoom =
                    if wheelEvent.deltaY > 0 then
                        userModel.zoomSpeed
                    else
                        userModel.zoomSpeed |> Quantity.negate
            in
                CameraMsg (Wrapper3DCamera.AdjustZoom deltaZoom)

        -- Incandescent light bulb
        ( firstLight, firstLightBall ) =
            pointLight userViewEval.body.lightData

        -- Add some soft lighting to fill in shadowed areas
        softLighting =
            Light.soft
                { upDirection = Direction3d.positiveZ
                , chromaticity = Light.fluorescent
                , intensityAbove = Illuminance.lux 30
                , intensityBelow = Illuminance.lux 5
                }

        visibleObjects =
            List.filter
                (PhysicsWrapper3D.isVisible userModel.cameraModel.camera (userModel.width |> Pixels.inPixels, userModel.height |> Pixels.inPixels))
                (userViewEval.body.objects userModel)

        textures =
            userModel.textureLoader.colorTextures

        skyboxEntity =
            case userViewEval.body.skyboxData of
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

    in
        { title = userViewEval.title
        , body =
            [ Html.div [ Wheel.onWheel adjustZoom ]
                [ Scene3d.custom
                    -- TODO might want to generalize lights a bit more.
                    { lights = Scene3d.twoLights firstLight softLighting
                    , camera = userModel.cameraModel.camera
                    , clipDepth = Length.centimeters 10
                    , exposure = Scene3d.exposureValue 6
                    , toneMapping = Scene3d.hableFilmicToneMapping
                    , whiteBalance = Light.fluorescent
                    , antialiasing = Scene3d.multisampling
                    , dimensions = ( userModel.width, userModel.height )
                    , background =
                        case userViewEval.body.skyboxData of
                            Skybox.BasicSkybox colour ->
                                Scene3d.backgroundColor colour
                            _ ->
                                Scene3d.transparentBackground
                    , entities = firstLightBall :: skyboxEntity :: renderEntities visibleObjects
                    }
                    |> Wrapper3D.withOverlay (List.map (GraphicSVG.map UserMsg) (userViewEval.body.overlay userModel)) hiddenModel
                ]
            , Html.map UserMsg userViewEval.body.customHTML
            ]
        }

initHiddenModel : (Duration -> userMsg) -> HiddenModel userMsg
initHiddenModel tick =
    let
        (wModel, _) = Widget.init 0 0 "widget"
    in
        { startTime = Quantity.zero
        , time = 0
        , tick = tick
        , width = Quantity.zero
        , height = Quantity.zero
        , widget = wModel
        }

initialCmd :
    Cmd (HiddenMsg userMsg coordinates)
    -> Cmd (HiddenMsg userMsg coordinates)
initialCmd userCmd =
    Cmd.batch
        [ Task.perform
            ( \ { viewport } ->
                Resize
                    (Pixels.int (round viewport.width))
                    (Pixels.int (round viewport.height))
            )
            Browser.Dom.getViewport
        , userCmd
        ]

subs : (UserModel coordinates a -> Sub userMsg) -> ( UserModel coordinates a, gModel ) -> Sub (HiddenMsg userMsg coordinates)
subs userSubs ( userModel, _ ) =
    Sub.batch
        [ Sub.map UserMsg (userSubs userModel)
        , Sub.map CameraMsg (Wrapper3DCamera.subscriptions userModel.cameraModel)
        , Events.onAnimationFrameDelta (Duration.seconds >> TickTime)
        , Events.onResize (\ width height -> Resize (Pixels.int width) (Pixels.int height))
        ]
        