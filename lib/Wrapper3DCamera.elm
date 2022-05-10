module Wrapper3DCamera exposing (..)

import Angle exposing (Angle)
import Browser
import Browser.Dom
import Browser.Events
import Camera3d exposing (Camera3d)
import Direction3d exposing (Direction3d)
import Duration exposing (Duration)
import Json.Decode as Decode exposing (Decoder)
import Length exposing (Length, Meters)
import Pixels exposing (Pixels)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import SketchPlane3d
import Vector3d exposing (Vector3d)
import Viewpoint3d

type alias Model coordinates =
    { allowRotation : Bool
    , azimuth : Angle
    , elevation : Angle
    , cameraPos : Point3d Meters coordinates
    , camera : Camera3d Meters coordinates
    , zoomDistance : Length
    , zoomMin : Length
    , zoomMax : Length
    , fov : Angle
    , dirLR : CameraDirection
    , dirFB : CameraDirection
    , dirUD : CameraDirection
    , cameraMode : CameraMode
    , cameraVelocity : Vector3d Meters coordinates
    }

type Msg coordinates
    = Tick Duration
    | MouseDown
    | MouseMove (Quantity Float Pixels) (Quantity Float Pixels)
    | MouseUp
    | VisibilityChange Browser.Events.Visibility
    | Reset
    | MoveCamera (Direction3d coordinates) Length
    | SetPos (Point3d Meters coordinates)
    | AdjustZoom Length
    | KeyDown String
    | KeyUp String
    | SetCameraMode CameraMode

type CameraMode
    = Orbit
    | OrbitUnlocked
    | OrbitRelative

type CameraDirection
    = Up
    | Down
    | Left
    | Right
    | Forward
    | Backward
    | None

cameraModeToString : CameraMode -> String
cameraModeToString mode =
    case mode of
        Orbit ->
            "Standard Orbit"
        OrbitUnlocked ->
            "Keyboard Orbit"
        OrbitRelative ->
            "Relative Orbit"

{-| Creates a camera with basic settings. -}
basicCamera : Model coordinates
basicCamera =
    { allowRotation = False
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
    , zoomDistance = Length.meters 3
    , zoomMin = Length.meters 0
    , zoomMax = Length.meters 5
    , fov = Angle.degrees 45
    , dirFB = None
    , dirLR = None
    , dirUD = None
    , cameraMode = Orbit
    , cameraVelocity = Vector3d.zero
    }

{-| Creates a camera with some custom settings. -}
customCamera : Point3d Meters coordinates -> (Length, Length, Length) -> Angle -> Model coordinates
customCamera initialPos (zoomInit, zoomMin, zoomMax) fieldOfView =
    { allowRotation = False
    , azimuth = Angle.degrees 180
    , elevation = Angle.degrees 30
    , cameraPos = initialPos
    , camera =
        Camera3d.perspective
            { viewpoint =
                Viewpoint3d.orbitZ
                    { focalPoint = initialPos
                    , azimuth = Angle.degrees 180
                    , elevation = Angle.degrees 30
                    , distance = zoomInit
                    }
            , verticalFieldOfView = fieldOfView
            }
    , zoomDistance = zoomInit
    , zoomMin = zoomMin
    , zoomMax = zoomMax
    , fov = fieldOfView
    , dirFB = None
    , dirLR = None
    , dirUD = None
    , cameraMode = Orbit
    , cameraVelocity = Vector3d.zero
    }

update : Msg coordinates -> Model coordinates -> Model coordinates
update message model =
    case message of
        Tick t ->
            let
                updatedCamera =
                    Camera3d.perspective
                        { viewpoint =
                            Viewpoint3d.orbitZ
                                { focalPoint = model.cameraPos
                                , azimuth = model.azimuth
                                , elevation = model.elevation
                                , distance = model.zoomDistance
                                }
                        , verticalFieldOfView = model.fov
                        }

                cameraSpeed = 0.8

                updatedVelocity =
                    case model.cameraMode of
                        OrbitUnlocked ->
                            model.cameraVelocity
                                |> Vector3d.plus
                                    (if not (model.dirFB == None) then
                                        Vector3d.withLength (Length.centimeters cameraSpeed) (directionConverter model model.dirFB)
                                    else
                                        Vector3d.zero
                                    )
                                |> Vector3d.plus
                                    (if not (model.dirLR == None) then
                                        Vector3d.withLength (Length.centimeters cameraSpeed) (directionConverter model model.dirLR)
                                    else
                                        Vector3d.zero
                                    )
                                |> Vector3d.plus
                                    (if not (model.dirUD == None) then
                                        Vector3d.withLength (Length.centimeters cameraSpeed) (directionConverter model model.dirUD)
                                    else
                                        Vector3d.zero
                                    )
                                |> Vector3d.minus (Vector3d.scaleBy 0.1 model.cameraVelocity)
                        _ ->
                            Vector3d.zero

                updatedPosition =
                    case model.cameraMode of
                        OrbitUnlocked ->
                            model.cameraPos
                                |> Point3d.translateBy model.cameraVelocity
                        _ ->
                            model.cameraPos
            in
                { model | camera = updatedCamera, cameraPos = updatedPosition, cameraVelocity = updatedVelocity }

        MouseDown ->
            { model | allowRotation = True }

        MouseUp ->
            { model | allowRotation = False }

        VisibilityChange Browser.Events.Visible ->
            model

        VisibilityChange Browser.Events.Hidden ->
            { model | allowRotation = False }

        MouseMove dx dy ->
            if model.allowRotation then
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
                    { model
                    | allowRotation = True
                    , azimuth = newAzimuth
                    , elevation = newElevation
                    }

            else
                model

        Reset ->
            { model 
            | azimuth = Angle.degrees 180
            , elevation = Angle.degrees 30
            , cameraPos = Point3d.centimeters 0 0 20
            , cameraVelocity = Vector3d.zero
            , dirUD = None
            , dirFB = None
            , dirLR = None
            }

        MoveCamera direction amount ->
            let
                azi = direction |> Direction3d.azimuthIn SketchPlane3d.xy
                ele = direction |> Direction3d.elevationFrom SketchPlane3d.xy
                adjustedDir =
                    case model.cameraMode of
                        OrbitRelative ->
                            Direction3d.xyZ
                                (model.azimuth |> Quantity.plus azi |> Quantity.plus (Angle.degrees 180))
                                ele
                        _ ->
                            direction
            in
                { model | cameraPos = model.cameraPos |> Point3d.translateBy (Vector3d.withLength amount adjustedDir) }

        SetPos point ->
            { model | cameraPos = point }

        AdjustZoom amount ->
            { model
            | zoomDistance =
                model.zoomDistance
                    |> Quantity.plus amount
                    |> Quantity.clamp model.zoomMin model.zoomMax
            }

        KeyDown key ->
            let
                dir =
                    toDirection key
            in
                case dir of
                    Left ->
                        { model
                            | dirLR = Left
                        }

                    Right ->
                        { model
                            | dirLR = Right
                        }

                    Forward ->
                        { model
                            | dirFB = Forward
                        }

                    Backward ->
                        { model
                            | dirFB = Backward
                        }

                    Up ->
                        { model
                            | dirUD = Up
                        }

                    Down ->
                        { model
                            | dirUD = Down
                        }

                    _ ->
                        model

        KeyUp key ->
            let
                dir =
                    toDirection key
            in
                case dir of
                    Left ->
                        { model | dirLR = None }

                    Right ->
                        { model | dirLR = None }

                    Forward ->
                        { model | dirFB = None }

                    Backward ->
                        { model | dirFB = None }

                    Up ->
                        { model | dirUD = None }

                    Down ->
                        { model | dirUD = None }

                    _ ->
                        model

        SetCameraMode mode ->
            { model | cameraMode = mode }

subscriptions : (Model coordinates) -> Sub (Msg coordinates)
subscriptions model =
    Sub.batch
        [ Browser.Events.onVisibilityChange VisibilityChange
        , Browser.Events.onAnimationFrameDelta (Duration.seconds >> Tick)
        , if model.allowRotation then
            Sub.batch
                [ Browser.Events.onMouseMove mouseMoveDecoder
                , Browser.Events.onMouseUp (Decode.succeed MouseUp)
                ]
          else
            Browser.Events.onMouseDown (Decode.succeed MouseDown)
        -- Listen for key presses
        , Browser.Events.onKeyDown keyDecoder
        , Browser.Events.onKeyUp keyUpDecoder
        ]

{-| Alternates between the different camera modes -}
alternateCameraMode : CameraMode -> CameraMode
alternateCameraMode oldMode =
    case oldMode of
        Orbit ->
            OrbitRelative
        OrbitRelative ->
            OrbitUnlocked
        _ ->
            Orbit

{-| Decodes mouse movement for camera orbiting. -}
mouseMoveDecoder : Decoder (Msg coordinates)
mouseMoveDecoder =
    Decode.map2 MouseMove
        (Decode.field "movementX" (Decode.map Pixels.pixels Decode.float))
        (Decode.field "movementY" (Decode.map Pixels.pixels Decode.float))

-- Converts a key "code" to a Direction
toDirection : String -> CameraDirection
toDirection string =
    case string of
        "ArrowLeft" ->
            Left

        "a" ->
            Left

        "A" ->
            Left

        "ArrowRight" ->
            Right

        "d" ->
            Right

        "D" ->
            Right

        "ArrowUp" ->
            Forward

        "w" ->
            Forward

        "W" ->
            Forward

        "ArrowDown" ->
            Backward

        "s" ->
            Backward

        "S" ->
            Backward

        " " ->
            Up

        "Shift" ->
            Down

        _ ->
            None

{-| Converts a Direction to a Direction3d that is relative to the camera -}
directionConverter : Model coords -> CameraDirection -> Direction3d coords
directionConverter model dir =
    let
        forwardDir =
            Direction3d.xyZ (model.azimuth |> Quantity.plus (Angle.degrees 180)) (Angle.degrees 0)

        backwardDir =
            Direction3d.xyZ (model.azimuth |> Quantity.plus (Angle.degrees 0)) (Angle.degrees 0)

        rightDir =
            Direction3d.xyZ (model.azimuth |> Quantity.plus (Angle.degrees 90)) (Angle.degrees 0)

        leftDir =
            Direction3d.xyZ (model.azimuth |> Quantity.plus (Angle.degrees 270)) (Angle.degrees 0)

        upDir =
            Direction3d.xyZ model.azimuth (Angle.degrees 90)

        downDir =
            Direction3d.xyZ model.azimuth (Angle.degrees -90)
    in
        case dir of
            Forward ->
                forwardDir

            Backward ->
                backwardDir

            Left ->
                leftDir

            Right ->
                rightDir

            Up ->
                upDir

            Down ->
                downDir

            _ ->
                downDir

keyDecoder : Decoder (Msg coordinates)
keyDecoder =
    Decode.map KeyDown (Decode.field "key" Decode.string)

keyUpDecoder : Decoder (Msg coordinates)
keyUpDecoder =
    Decode.map KeyUp (Decode.field "key" Decode.string)