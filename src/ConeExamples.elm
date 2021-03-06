{- COMPILED BUT NOT SHOWN -}

{- This file should provide a good starting point for the snowman. Modified from "3d-elm-camp/BeeMovement.elm".
   This code was originally for the 3D Workshop Snowman on macoutreach.rocks
 -}

module ConeExamples exposing (main)

-- Most of these imports were taken from "3d-elm-camp/BeeMovement.elm", so there may be a lot of unused things
import Angle exposing (Angle)
import Array exposing (Array)
import Axis3d
import Block3d exposing (Block3d)
import Browser
import Browser.Dom
import Browser.Events
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
import Skybox
import Parameter1d

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
init () =
    ( { width = Quantity.zero
      , height = Quantity.zero
      , time = 0
      , orbiting = False
      , azimuth = Angle.degrees 0
      , elevation = Angle.degrees 30
      , textures = Nothing 
      , generatedMeshes = Dict.empty
      , generatedShadows = Dict.empty
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
        , fetchTextures
        , Task.perform (\_ -> GenerateMeshes myMeshes) (Task.succeed True)
        -- , Task.perform (\_ -> GenerateShadows name myShadowMeshes) (Task.succeed True)
        ] 
    )

update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Resize width height ->
            ( { model | width = width, height = height }, Cmd.none )

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
                        updatedMeshes = Dict.insert generatedMesh.name generatedMesh.mesh model.generatedMeshes
                        updatedShadows = Dict.insert generatedMesh.name generatedMesh.shadow model.generatedShadows
                    in
                        ( { model | generatedMeshes = updatedMeshes, generatedShadows = updatedShadows }, Cmd.batch [ Task.perform (\_ -> GenerateMeshes rest) (Task.succeed True) ])

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
            ( { model | textures = Just textures }, Cmd.none)

        Error _ -> 
            ( model, Cmd.none)

mouseMoveDecoder : Decoder Msg
mouseMoveDecoder =
    Decode.map2 MouseMove
        (Decode.field "movementX" (Decode.map Pixels.pixels Decode.float))
        (Decode.field "movementY" (Decode.map Pixels.pixels Decode.float))

-- Fetch textures from textureListSkybox
-- Get a result type as List (Material.Texture Color)
-- Decode the List when we actually are going to load the texture
-- In this example, we decode the list in Skybox.skybox
fetchTextures : Cmd Msg 
fetchTextures =
  textureListSkyBox
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
                Ok texture -> LoadTexture texture
                Err error -> Error error
        )

-- MESHES ARE STORED HERE

ringMesh : Float -> Float -> Mesh.Uniform WorldCoordinates
ringMesh radius thickness = 
    let
        pointList = List.map
                    ( \idx ->
                      let 
                        t = toFloat idx
                        slowx = sin (0.004*t)
                        slowy = cos (0.004*t)
                        fastx = radius + thickness * sin (0.4 *t)
                        fastz =          thickness * cos (0.4 *t)
                      in
                        Point3d.centimeters 
                            (slowx * fastx)
                            (slowy * fastx)
                            (fastz)
                    )
                    <| List.range 0 1572
        triangularMesh =
            TriangularMesh.indexed
                (Array.fromList pointList )
                ((List.concatMap ( \ idx ->  [(idx,idx+1,16+idx)
                                            ,(idx+1,16+idx,16+idx+1)]
                                )
                                <| List.range 0 (1572-16))
                ++
                (List.concatMap ( \ idx ->  [(idx+1572-16,idx+1572-16+1,16+idx)
                                            ,(idx+1572-16+1,16+idx,16+idx+1)]
                                )
                                <| List.range -16 0
                ))
    in
        Mesh.indexedFacets triangularMesh

-- Modified from "sphere" in elm-3d-scene/src/Scene3d/Primitives.elm so might behave a bit strangely
ellipsoidMesh : Float -> Float -> Float -> Mesh.Textured WorldCoordinates
ellipsoidMesh length width height = 
    let
        n =
            72

        len = Length.centimeters length

        wid = Length.centimeters width

        hei = Length.centimeters height

        m =
            ceiling (toFloat n / 2)

        thetaValues =
            Parameter1d.steps n
                (Quantity.interpolateFrom Quantity.zero (Angle.turns 1))

        phiValues =
            Parameter1d.steps m
                (Quantity.interpolateFrom
                    (Angle.degrees 90)
                    (Angle.degrees -90)
                )

        vertices =
            thetaValues
                |> List.map
                    (\theta ->
                        phiValues
                            |> List.map
                                (\phi ->
                                    { position =
                                        Point3d.xyz
                                            (len |> Quantity.multiplyBy (Angle.cos phi * Angle.cos theta))
                                            (wid |> Quantity.multiplyBy (Angle.cos phi * Angle.sin theta))
                                            (hei |> Quantity.multiplyBy (Angle.sin phi))
                                    , normal =
                                        Direction3d.xyZ theta phi |> Direction3d.toVector
                                    , uv =
                                        ( Quantity.ratio theta (Angle.turns 1)
                                        , Quantity.ratio
                                            (phi |> Quantity.plus (Angle.degrees 90))
                                            (Angle.degrees 180)
                                        )

                                    -- , tangent =
                                    --     Direction3d.xy (theta |> Quantity.plus (Angle.degrees 90))
                                    --         |> Direction3d.toVector
                                    }
                                )
                    )
                |> List.concat
                |> Array.fromList

        thetaStartIndices =
            List.range 0 (n - 1)

        phiStartIndices =
            List.range 0 (m - 1)

        linearIndex i j =
            i * (m + 1) + j

        faces =
            thetaStartIndices
                |> List.map
                    (\i ->
                        phiStartIndices
                            |> List.map
                                (\j ->
                                    let
                                        bottomLeftIndex =
                                            linearIndex i (j + 1)

                                        bottomRightIndex =
                                            linearIndex (i + 1) (j + 1)

                                        topLeftIndex =
                                            linearIndex i j

                                        topRightIndex =
                                            linearIndex (i + 1) j
                                    in
                                    [ ( bottomLeftIndex
                                      , bottomRightIndex
                                      , topRightIndex
                                      )
                                    , ( bottomLeftIndex
                                      , topRightIndex
                                      , topLeftIndex
                                      )
                                    ]
                                )
                            |> List.concat
                    )
                |> List.concat
    in
    Mesh.texturedFaces (TriangularMesh.indexed vertices faces)
        |> Mesh.cullBackFaces

defaultEllipsoid : Mesh.Textured WorldCoordinates
defaultEllipsoid = ellipsoidMesh 2 1 1

defaultEllipsoidShadow : Mesh.Shadow WorldCoordinates
defaultEllipsoidShadow = Mesh.shadow defaultEllipsoid

type alias GeneratedMesh = 
    { name : String
    , mesh : Mesh.Textured WorldCoordinates
    , shadow : Mesh.Shadow WorldCoordinates
    }

generateEllipsoid : Float -> Float -> Float -> GeneratedMesh
generateEllipsoid length width height = 
    { name = "ellipsoid" ++ String.fromFloat length ++ String.fromFloat width ++ String.fromFloat height
    , mesh = ellipsoidMesh length width height
    , shadow = Mesh.shadow (ellipsoidMesh length width height)
    }

{- UNEDITABLE -}

--custom type for material 
--type Mat = Metal | NonMetal | Matte 

--custom type for axis
type Axis = X | Y | Z -- Deprecated?

--clean up type for (x,y,z)
type alias Dimension = (Float,Float,Float)

type alias Mold coordinates a = (Material coordinates { a | normals : () } -> Entity coordinates)

type WorldCoordinates
    = WorldCoordinates

type alias Model =
    { width : Quantity Int Pixels
    , height : Quantity Int Pixels
    , time : Float
    , orbiting : Bool
    , azimuth : Angle
    , elevation : Angle
    , textures : Maybe (List (Material.Texture Color))
    , generatedMeshes : Dict String (Mesh.Textured WorldCoordinates)
    , generatedShadows : Dict String (Mesh.Shadow WorldCoordinates)
    }

type Msg
    = Resize (Quantity Int Pixels) (Quantity Int Pixels)
    | Tick Duration
    | MouseDown
    | MouseMove (Quantity Float Pixels) (Quantity Float Pixels)
    | MouseUp
    | VisibilityChange Browser.Events.Visibility
    | GenerateMeshes (List GeneratedMesh)
    -- | GenerateShadows String (List (Mesh.Shadow WorldCoordinates))
    | LoadTexture (List (Material.Texture Color))
    | Error WebGL.Texture.Error

--Drawing basic shapes 

-- Materials
-- "Material coordinates { a | normals : () }" allegedly allows us to not have to bother with textured vs. uniform
metallic : Color.Color -> Float -> Mold WorldCoordinates a-> Entity WorldCoordinates 
metallic colour roughness shapeFunc = (Material.metal { baseColor = colour, roughness = roughness }) |> shapeFunc

plastic : Color.Color -> Float -> Mold WorldCoordinates a-> Entity WorldCoordinates
plastic colour roughness shapeFunc = (Material.nonmetal { baseColor = colour, roughness = roughness }) |> shapeFunc

matte : Color.Color -> Mold WorldCoordinates a -> Entity WorldCoordinates
matte colour shapeFunc = (Material.matte colour) |> shapeFunc

customMat : Color.Color -> Float -> Float -> Mold WorldCoordinates a -> Entity WorldCoordinates
customMat colour roughness metallicity shapeFunc = (Material.pbr { baseColor = colour, roughness = roughness, metallic = metallicity }) |> shapeFunc

--non-metal material
-- myMat : Mat -> Color.Color -> Material.Uniform WorldCoordinates
-- myMat m colour = 
--     case m of 
--         Metal -> Material.nonmetal { baseColor = colour, roughness = 0.2 }
--         NonMetal -> Material.nonmetal { baseColor = colour, roughness = 0.2 }
--         Matte -> Material.matte colour 

--textured non-metal material (some shapes require textured material? )
-- myTexturedMat : Mat -> Color.Color -> Material.Textured WorldCoordinates
-- myTexturedMat m colour = 
--     case m of 
--         Metal -> Material.nonmetal { baseColor = colour, roughness = 0.2 }
--         NonMetal -> Material.nonmetal { baseColor = colour, roughness = 0.2 }
--         Matte -> Material.matte colour 

-- Shapes

cube : Float -> Material.Uniform WorldCoordinates -> Entity WorldCoordinates
cube size material = Scene3d.blockWithShadow material <|
        Block3d.from
            (Point3d.centimeters 0 0 0)
            (Point3d.centimeters size size size)

square : Float -> Material.Textured WorldCoordinates -> Entity WorldCoordinates
square length material = 
    let
        posValue = length / 2
    in
        Scene3d.quadWithShadow material
            (Point3d.centimeters (-posValue) (-posValue) 0)
            (Point3d.centimeters (-posValue) posValue 0)
            (Point3d.centimeters posValue posValue 0)
            (Point3d.centimeters posValue (-posValue) 0)


rectangle : Float -> Float -> Material.Textured WorldCoordinates -> Entity WorldCoordinates
rectangle length width material = 
    let
        lValue = length / 2
        wValue = width / 2

    in
        Scene3d.quadWithShadow material
            (Point3d.centimeters (-lValue) (-wValue) 0)
            (Point3d.centimeters (-lValue) wValue 0)
            (Point3d.centimeters lValue wValue 0)
            (Point3d.centimeters lValue (-wValue) 0)

box : Dimension -> Material.Uniform WorldCoordinates -> Entity WorldCoordinates 
box (x,y,z) material = 
        Scene3d.blockWithShadow material <|
                Block3d.from
                    Point3d.origin
                    (Point3d.centimeters x y z)

sphere : Float -> Material.Textured WorldCoordinates -> Entity WorldCoordinates
sphere r material = 
        (Scene3d.sphereWithShadow material <|
            Sphere3d.withRadius (Length.centimeters r) Point3d.origin)
            |> move (0,0,r)

cone : Float -> Float -> Material.Uniform WorldCoordinates -> Entity WorldCoordinates
cone r h material = 
    Scene3d.coneWithShadow material <|
        Cone3d.along Axis3d.z
            { base = Length.centimeters 0
            , tip = Length.centimeters h
            , radius = Length.centimeters r
            }

polyCone : List (Float,Float) -> (Float,Float,Float) -> Material.Uniform WorldCoordinates -> Entity WorldCoordinates
polyCone points (xtip,ytip,ztip) material =
    let
        tip =
            Point3d.centimeters xtip ytip ztip
        apron = List.map 
            ( \ (x,y) -> 
                Point3d.centimeters x y 0
            )
            points
    in
        TriangularMesh.fan tip apron
          |> Mesh.indexedFacets
          |> Scene3d.mesh material

polyCylinder : List (Float,Float) -> (Float,Float,Float) -> Material.Uniform WorldCoordinates -> Entity WorldCoordinates
polyCylinder points (xpole,ypole,zpole) material =
    let
        bottom = List.map 
            ( \ (x,y) -> 
                Point3d.centimeters x y 0
            )
            points
        top = List.map 
            ( \ (x,y) -> 
                Point3d.centimeters (x+xpole) (y+ypole) zpole
            )
            points
    in
        TriangularMesh.strip top bottom
          |> Mesh.indexedFacets
          |> Scene3d.mesh material


cylinder : Float -> Float -> Material.Uniform WorldCoordinates -> Entity WorldCoordinates
cylinder r h material =
    Scene3d.cylinderWithShadow material <|
        Cylinder3d.along Axis3d.z
            { start = Length.centimeters 0
            , end = Length.centimeters h
            , radius = Length.centimeters r
            }

ring : Float -> Float -> Material.Uniform WorldCoordinates -> Entity WorldCoordinates
ring radius thickness material = Scene3d.mesh material (ringMesh radius thickness)

-- Broken, need to find out how to change the model from a function
ellipsoid : Float -> Float -> Float -> Model -> Material.Textured WorldCoordinates -> Entity WorldCoordinates
ellipsoid length width height model material = 
    let 
        meshName = "ellipsoid" ++ String.fromFloat length ++ String.fromFloat width ++ String.fromFloat height

        mesh = Maybe.withDefault defaultEllipsoid (Dict.get meshName model.generatedMeshes)

        shadow = Maybe.withDefault defaultEllipsoidShadow (Dict.get meshName model.generatedShadows)

    in
        Scene3d.meshWithShadow material mesh shadow

-- ellipsoid : Float -> Material.Textured WorldCoordinates -> Entity WorldCoordinates
-- ellipsoid size material = 
--     Scene3d.meshWithShadow material defaultEllipsoid defaultEllipsoidShadow
--     |> scale size

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

--Translation 
move : Dimension -> Entity coordinates -> Entity coordinates
move (x,y,z) entity = entity |> Scene3d.translateBy (Vector3d.centimeters x y z)      

rotate : Float -> Float -> Float -> Entity coordinates -> Entity coordinates 
rotate pitch yaw roll entity = 
    entity 
        |> Scene3d.rotateAround Axis3d.x (Angle.radians pitch)  
        |> Scene3d.rotateAround Axis3d.y (Angle.radians roll)  
        |> Scene3d.rotateAround Axis3d.z (Angle.radians yaw)

--TODO: Track eneity's position. 
scale : Float -> Entity coordinates -> Entity coordinates 
scale factor entity = entity |> Scene3d.scaleAbout (Point3d.centimeters 0 0 0) factor

-- repeat an animation for a given duration
repeatDuration : Float -> Int -> Float -> Float -> Float
repeatDuration speed duration startPosition time =
  speed * (time - toFloat duration * toFloat (floor time // duration)) + startPosition

textureListSkyBox : List String
textureListSkyBox = 
  [textureBottom, textureTop, textureSide1, textureSide2, textureSide3
    , textureSide4]

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

        -- If the proper textures aren't loaded for whatever reason, the sky will just be light blue
        texturesList = case model.textures of
            Just textures ->
                textures

            Nothing ->
                List.repeat 6 (Material.constant Color.lightBlue)

        baseEntities = 
            [ firstLightBall
            , plane
            , Skybox.skybox texturesList 1000
            ]

    in
    Scene3d.custom
        { lights = Scene3d.threeLights firstLight thirdLight softLighting
        , camera = camera
        , clipDepth = Length.centimeters 10
        , exposure = Scene3d.exposureValue 6
        , toneMapping = Scene3d.hableFilmicToneMapping
        , whiteBalance = Light.fluorescent
        , antialiasing = Scene3d.multisampling
        , dimensions = ( model.width, model.height )
        , background = Scene3d.backgroundColor Color.lightBlue
        , entities = baseEntities ++ myEntities model
        }

{- EDITABLE -}

-- Consider this the equivalent of "myShapes" on the other slots. You start out with some snowflakes
myEntities model =  
    [ sphere 1
      |> metallic Color.red 0.3
    , box (10, 10, 20)
      |> plastic Color.blue 0.4
      |> move (10, 10, 0)
    , cube 20
      |> customMat Color.orange 0.2 (-0.5*cos (model.time*2) + 0.5)  
      |> move (50*cos model.time, 50*sin model.time, 0)
    , cube 10 
      |> matte Color.black
      |> move(-50,-50,0)
    , cone 10 20
      |> metallic Color.darkGreen 0.7
      |> move (25, -25, 0)
    , polyCone 
        [(-35.61,9.6820),(-35.93,0.4923),(-30.68,-7.712),(-17.23,-8.041),(-5.415,0.1641),(-3.446,12.964),(-12.96,21.497),(-27.40,23.138),(-35.61,9.6820)]
         (-5,5,0)
        |> metallic Color.yellow 0.7
        |> move (0,0,10)
    , polyCylinder
        [(-35.61,9.6820),(-35.93,0.4923),(-30.68,-7.712),(-17.23,-8.041),(-5.415,0.1641),(-3.446,12.964),(-12.96,21.497),(-27.40,23.138),(-35.61,9.6820)]
         (0,0,10)
        |> metallic Color.blue 0.7
    , polyCylinder
        [(-35.61,9.6820),(-35.93,0.4923),(-30.68,-7.712),(-17.23,-8.041),(-5.415,0.1641),(-3.446,12.964),(-12.96,21.497),(-27.40,23.138),(-35.61,9.6820)]
         (5,5,10)
        |> metallic Color.yellow 0.7
        |> move (20,20,0)
    , cylinder 10 50
      |> metallic Color.yellow 0.7
      |> move (-25, 25, 0)
    , ring 25 5
      |> metallic Color.lightGray 0.5
      |> move (0, 0, 60)
    , rectangle 25 50
      |> metallic Color.red 0.3
      |> move (-25, -25, 40)
    , ellipsoid 25 10 15 model
      |> matte Color.purple
      |> move (50, 50, 50)
    -- This group is just used for debugging the meshes
    -- , Scene3d.group (List.map (Scene3d.mesh (Material.metal {baseColor = Color.red, roughness = 0.3 })) (Dict.values model.generatedMeshes))
    --   |> move (25,25,50)
    ]

-- Put any custom meshes you need generated in here
myMeshes = 
    [ generateEllipsoid 25 10 15

    ]

floorColour = Color.green

{- Here you can specify what images to use to create the skybox -}

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