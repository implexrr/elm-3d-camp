{- New file for working on the Bee Game -}


module BeeGameNoShadows exposing (main)

-- Most of these imports were taken from "3d-elm-camp/BeeMovement.elm", so there may be a lot of unused things

import Angle exposing (Angle)
import Arc2d
import Arc3d
import Array exposing (Array)
import Axis3d
import Block3d exposing (Block3d)
import BoundingBox3d exposing (BoundingBox3d)
import Browser
import Browser.Dom
import Browser.Events
import Camera3d
import Circle3d
import Collision exposing (noCollide, isColliding, isCollidingByName, getCollisions, nameObject)
import Color exposing (Color)
import Cone3d
import Cylinder3d
import Dict exposing (Dict)
import Direction3d exposing (Direction3d)
import Duration exposing (Duration)
import Frame3d
import GSVGSkybox as GS
import GraphicSVG exposing (..)
import GraphicSVG.Widget as Widget
import Html exposing (Html)
import Html.Attributes as HA exposing (style)
import Illuminance
import Json.Decode as Decode exposing (Decoder)
import Length exposing (Meters)
import LineSegment3d
import LuminousFlux exposing (LuminousFlux)
import Parameter1d
import Pixels exposing (Pixels)
import Point2d
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import Scene3d exposing (Entity)
import Scene3d.Light as Light exposing (Chromaticity, Light)
import Scene3d.Material as Material exposing (Material)
import Scene3d.Mesh as Mesh
import SketchPlane3d
import Skybox
import SolidAngle
import Sphere3d
import Task
import Temperature
import Triangle3d
import TriangularMesh
import Vector3d exposing (Vector3d)
import Viewpoint3d
import WebGL.Texture
import Wrapper3D exposing (..)

import GSVGTexture as GT
import TextureLoader as TL
import Json.Encode exposing (object)

{-header<b>3D Bee Simulator!</b>: Simulate a bee in 3D!-}
{-editable-}
-- Consider this the equivalent of "myShapes" on the other slots. You start out with a basic shape
myEntities model =
    let
        useTexture = getTexture model.textureLoader.textures
    in
    [ group1_forest model
    , farm model                    
    , beebox model
      |> move3D (-2000,-2000,20)
    , beebox model
      |> move3D (-50,-50,0)
    , beebox model
      |> move3D (-1500,-1500,0)
    , cottageFinal model 
         |> move3D (300,0,0)
    ,
    parkBench |> rotateZ3D (degrees -90) |> move3D (-1000,2000,0) 
    ,
    parkBench |> rotateZ3D (degrees -90) |> move3D (1000,2000,0) 
    ,
    cloudsFinal model |> noCollide
    ,
    someHives model |> move3D (100,100,0)
    ,
    box 10000 300 2
        |> matte Color.yellow
        |> move3D (-5000, 2000, 0)
    ,
    box 10000 300 2
        |> matte Color.yellow
        |> move3D (-5000, 2000, 0)
    ,
    box 10000 200 2
        |> matte Color.purple
        |> move3D (-5000,0,0)
    ,
    hill model
        |> move3D (500,3400,0)
    , someHives model |> move3D (500, 3400, 100)
    ,
    treesForPath 4 model |> move3D (0,1950,0)
    ,
    beach model |> move3D (-2800,2500,0)
    ,
    forest
    |> move3D (3000, 3400, 0)
    , someHives model |> move3D (3200, 3200, 0)
    , 
    bee model
    |> scale3D 1
    |> move3D (3900, 2650,190)
    |> move3D (100*sin model.time, 100*cos model.time, 0)
    ,
    theTrees model |> move3D (0,-300, 0)
    ]
 
debugs = group3D [
          cylinder 0.3 10000
           |> matte Color.blue
        , cylinder 0.3 10000
            |> matte Color.red
            |> rotateY3D (degrees 90)
        , cylinder 0.3 10000
            |> matte Color.green
            |> rotateX3D (degrees -90)
      ] |> move3D (0,0,10)
 
--group1-------------------------------
 
farm model = group3D [
                barn model
                |> scale3D 50
                |> move3D (500,-3500,0)
                , animals model
             ] 

animals model = group3D [
            pig model
            |> scale3D 0.90
            |> move3D (500,-2000,150)
          , chiken model
            |> scale3D 3
            |> move3D (600,-1500,50)
          , chiken model
            |> rotateZ3D (degrees 45)
            |> scale3D 3
            |> move3D (800,-1200,50)
          , cow model
            |> scale3D 5
            |> move3D (1300,-1400,90)
          , farmer model
            |> scale3D 5
            |> move3D (400,-800,80)
         ]
 
barn model =group3D  [ cube 15
        |> matte Color.red
        ,
        polyCone [(-10,-10),(10,10),(10,-10),(-10,-10)] (0,0,10) model.meshStore
        |> matte Color.white
        |> scale3D 0.75
        |> move3D (7.50,7.50,15)
        ,
        polyCone [(-10,-10),(10,10),(10,-10),(-10,-10)] (0,0,10) model.meshStore
        |> matte Color.white
        |> scale3D 0.75
        |> rotateZ3D (degrees 180)
        |> move3D (7.50,7.50,15)
        ,
        cube 5
        |> matte Color.black
        |> move3D (10.1,5,0)
        ]
 
 
group1_forest model = group3D [
                        group_tree1 model
                      , group_tree2 model
                      , group_tree3 model
 
                ] |> move3D (-1500,-3000,0)
 
group_tree1 model = group3D (List.map (\ x -> tree1 model |> scale3D 4 |> move3D (x*750,0,0)) (List.map toFloat <| List.range -1 1))
 
group_tree2 model = group3D (List.map (\ x -> tree2 model |> scale3D 4 |> move3D (-250,800,0) |> move3D (x*750,200,0)) (List.map toFloat <| List.range -1 1)) 
 
group_tree3 model = group3D ((List.map (\ x -> tree3 model |> scale3D 7  |> move3D (x*750,1700,800)) (List.map toFloat <| List.range -1 1)) )
 
{- group1Assetsss model useTexture = group3D [
                  tree1 model
                  |> move3D (0,500,0)       
                , tree2 model
                  |> move3D (100,-500,0)
                , tree3 model
                  |> move3D (200,200,120)
                , bee1 model
                  |> move3D (100,0,50)
                , bee22 model
                  |> move3D (150,0,50)
                , bee3 model useTexture
                  |> move3D (200,0,50)
                , queenBee model useTexture
                  |> move3D (250,0,50)
               , crop1 model
                   |> scale3D 2
                   |> move3D (-300,-300,0)
               , crop2
                 |> scale3D 2
                 |> move3D (-300,300,0)

               , pollen1 model
                 |> move3D (50,110,0)
               , pollen2 model
                 |> move3D (50,130,0)
               , farmer model         
                  |> move3D (0,-90,20)
               , cow model      
                 |> move3D (30,-100,20)
               

               , pig model 
                 |> move3D (-90,-150,50)
               , chiken model 
                 |> move3D (-120,-150,30)
               
               , vomitBee model
                 |> move3D (300,0,50)
               , emperorBee model
                 |> move3D (350,0,50)
                ] -}   
 
 
--emperor bee
emperorBee textures meshes time = group3D[
          let
            useTexture = getTexture textures
          in
        sphere 10
        |> textured (useTexture "texture_emperorBee") 0 0
        |> scale3D 4.5
        ,
        eb_thecrown
        |> move3D (0,0,106)
        ,
        eb_eyes
        |> scale3D 3
        |> rotateX3D (degrees 90)
        |> move3D (0,-30,10)
        ,
        eb_wingMove time
        |> scale3D 5
        |> move3D (0,0,20)
        ,
        eb_mouth meshes
        |> rotateX3D (degrees 90)
        |> move3D (10,-25,34)
        ,
        eb_cape meshes
         |> scale3D 2
         |> rotateZ3D (degrees 180)
         |> rotateX3D (degrees -40)
         |> move3D (35,85,15)
 
 
 
                    ] |> rotateZ3D (degrees 90)

eb_cape meshes = group3D        
   [polyCylinder [(4.3146,39.550),(5.4651,38.687),(5.4651,38.687),(5.7528,38.687),(5.7528,38.687),(6.6157,38.112),(6.6157,38.112),(6.6157,38.112),(8.0539,36.961),(8.0539,36.961),(9.7797,36.386),(9.7797,36.386),(12.368,35.811),(12.368,35.811),(12.368,35.811),(18.696,36.098),(18.696,36.098),(25.887,39.550),(25.887,39.550),(29.914,42.426),(29.914,42.426),(50.912,-13.37),(50.912,-13.37),(43.433,-17.97),(25.887,-22.57),(16.107,-22.57),(16.107,-22.57),(4.3146,39.550)] (5) meshes
   |> matte Color.red]

eb_mouth meshes = group3D[        
    polyCylinder [(-37.10,18.552),(12.080,19.703),(-10.06,2.7325),(-37.10,18.552)] (20) meshes
   |> matte Color.black
   ,
 
   polyCylinder [(-29.33,14.813),(5.4651,15.676),(10.930,19.991),(-36.53,18.840),(-29.33,14.813)] (20) meshes
   |> matte Color.white
   ,
 
   polyCylinder [(-17.83,8.4853),(-4.889,8.4853),(-10.35,3.8831),(-17.83,8.4853)] (20) meshes
   |> matte Color.red]
 
 
eb_thecrown = group3D
     [
     ring 10 4
     |> matte Color.darkYellow
     |> scale3D 1.6
     |> move3D (0,0,-6)
     ,
     ring 13 3
     |> matte Color.darkRed
     |> scale3D 1.5
     |> move3D (0,0,-16)
     ,
     eb_crownpoints
     |> move3D (0,0,-6)
     ]
 
 
 
 
eb_crownpoints = group3D
    [cone 10 50
    |> matte Color.darkYellow
    |> scale3D 0.5
    |> move3D (0,15,0)
    |> rotateY3D (degrees 0)
    ,
     cone 10 50
    |> matte Color.darkYellow
    |> scale3D 0.5
    |> move3D (0,15,0)
    |> rotateZ3D (degrees 30)    
 
    ,
    cone 10 50
    |> matte Color.darkYellow
    |> scale3D 0.5
    |> move3D (0,15,0)
    |> rotateZ3D (degrees 60)
    ,
    cone 10 50
    |> matte Color.darkYellow
    |> scale3D 0.5
    |> move3D (0,15,0)
    |> rotateZ3D (degrees 90)
    ,
    cone 10 50
    |> matte Color.darkYellow
    |> scale3D 0.5
    |> move3D (0,15,0)
    |> rotateZ3D (degrees 120)
    ,
    cone 10 50
    |> matte Color.darkYellow
    |> scale3D 0.5
    |> move3D (0,15,0)
    |> rotateZ3D (degrees 150)
    ,
    cone 10 50
    |> matte Color.darkYellow
    |> scale3D 0.5
    |> move3D (0,15,0)
    |> rotateZ3D (degrees 180)
    ,
    cone 10 50
    |> matte Color.darkYellow
    |> scale3D 0.5
    |> move3D (0,15,0)
    |> rotateZ3D (degrees 210)
    ,
    cone 10 50
    |> matte Color.darkYellow
    |> scale3D 0.5
    |> move3D (0,15,0)
    |> rotateZ3D (degrees 240)
    ,
    cone 10 50
    |> matte Color.darkYellow
    |> scale3D 0.5
    |> move3D (0,15,0)
    |> rotateZ3D (degrees 270)
    ,
    cone 10 50
    |> matte Color.darkYellow
    |> scale3D 0.5
    |> move3D (0,15,0)
    |> rotateZ3D (degrees 300)
    ,
    cone 10 50
    |> matte Color.darkYellow
    |> scale3D 0.5
    |> move3D (0,15,0)
    |> rotateZ3D (degrees 330)]

eb_eyes = group3D
    [
    sphere 1.7
    |> matte Color.white
    |> move3D (5,21,0)
    ,
    sphere 1.7
    |> matte Color.white
    |> move3D (-5,21,0)
    ,
    sphere 0.9
    |> matte Color.blue
    |> move3D (5,22,2)
    ,
    sphere 0.9
    |> matte Color.blue
    |> move3D (-5,22,2)
    ]
 
eb_wing time = group3D
            [
            cylinder 15 1 
        --      |> matte Color.white
            |> matte (Color.hsl (1*(-1*sin(time))) 0.5 0.5)
           -- |> matte (Color.hsl (degrees (360*sin(-1*model.time)) 0.5 0.5))
            ]

eb_wingMove time = group3D
    [
    eb_wing time |> move3D ( 20, 0, 0 ) 
  |> rotateY3D (degrees (-15 * sin (10 * time))) 
  |> move3D ( 0, 0, 8 )
  ,
  eb_wing time |> move3D ( -20, 0, 0 ) 
  |> rotateY3D (degrees (15 * sin (10 * time))) 
  |> move3D ( 0, 0, 8 )
    ]

--vomit bee
vb_wing = group3D
            [
            cylinder 15 1 
            |> matte (Color.rgba 0.9 0.9 0.9 0.5)
 
            ]

vb_wingMove time = group3D
    [
    vb_wing |> move3D ( 20, 0, 0 ) 
  |> rotateY3D (degrees (-15 * sin (10 * time))) 
  |> move3D ( 0, 0, 8 )
  ,
  vb_wing |> move3D ( -20, 0, 0 ) 
  |> rotateY3D (degrees (15 * sin (10 * time))) 
  |> move3D ( 0, 0, 8 )
    ]
 
vb_eyesAnd_mouth = group3D
      [
      sphere 10
      |> matte Color.white
      |> scale3D 0.2
      |> move3D (6,25,16)
      ,
      sphere 10
      |> matte Color.white
      |> scale3D 0.2
      |> move3D (-6,25,16)
      ,
      sphere 10
      |> matte Color.black
      |> scale3D 0.2
      |> move3D (0,23,9)
 
      ]

vomitBee textures meshes time = group3D
       [
       vb_wingMove time
       |> move3D (0,0,5)
       ,
       ellipsoid 25 17 15 meshes
       |> matte Color.darkGreen
       |> rotateZ3D (degrees 90)
       |> move3D (0,0,15)
       ,
       vb_eyesAnd_mouth] |>rotateZ3D (degrees 270)
 
tree1 model = group3D [
  polyCylinder[(-3.240,-7.059),(-5.323,-7.291),(-7.869,-7.291),(-11.34,-7.059),(-12.73,-6.365),(-14.35,-4.050),(-13.88,-2.661),(-12.73,-1.273),(-12.26,-0.115),(-13.19,1.5045),(-13.19,1.7359),(-12.96,2.1989),(-11.11,5.4394),(-9.490,6.3652),(-7.638,7.5226),(-6.018,6.8282),(-4.629,6.5967),(-3.471,6.8282),(0.2314,10.763),(3.2405,11.457),(4.3978,11.226),(5.0922,9.1428),(5.5551,8.2169),(7.6383,6.5967),(9.2585,6.5967),(10.878,6.5967),(11.110,5.9023),(12.730,4.2820),(15.276,2.4303),(15.276,0.5786),(16.433,-2.661),(16.202,-4.976),(14.582,-8.448),(11.804,-9.605),(10.647,-9.837),(9.9529,-9.374),(9.0271,-8.448),(6.7124,-7.522),(3.4719,-7.059),(2.7775,-8.216),(2.3146,-8.216),(0.2314,-8.216),(-0.925,-8.679),(-1.851,-8.216),(-1.851,-7.291),(-2.083,-6.596),(-3.934,-6.365),(-3.240,-7.059)] 10 model.meshStore
  |> matte Color.darkGreen
  |> scale3D 1.5
  |> rotateX3D (degrees 90)
  |> move3D (0,0,50)
  ,
  box 20 10 40
            |> matte (Color.hsl (0/360) 0.99 0.07)
            |> move3D (-10,-10,0)
  ] |> scale3D 2
 
 
tree2 model = group3D [
 
 polyCylinder[(-23.60,24.188),(-22.45,25.576),(-21.06,26.965),(-20.13,26.965),(-18.05,26.965),(-17.35,26.271),(-16.43,26.502),(-15.04,26.965),(-14.35,26.271),(-13.88,24.882),(-13.65,24.419),(-13.19,24.188),(-10.41,19.558),(-2.083,-2.430),(-1.388,-6.828),(-1.620,-16.08),(-1.388,-17.47),(-4.397,-26.96),(-6.249,-26.73),(-14.81,-30.90),(-22.22,-29.51),(-30.32,-26.96),(-33.79,-24.65),(-35.64,-20.71),(-37.03,-14.92),(-38.65,-11.22),(-36.80,-9.374),(-36.10,-5.902),(-35.87,-2.430),(-35.41,0.1157),(-34.48,1.9674),(-33.33,4.0506),(-32.40,7.0596),(-31.24,10.068),(-24.30,24.882),(-23.60,26.039),(-21.52,26.271),(-23.60,24.188)] 10 model.meshStore
 |> matte Color.darkGreen
 |> rotateX3D (degrees 90)
 |> move3D (30,0,65)
 ,
  box 20 10 40
            |> matte (Color.hsl (0/360) 0.99 0.07)
            |> rotateX3D (degrees 180)
            |> move3D (0,0,40)
    ]    |> scale3D 2
 
 
 
tree3 model = group3D [    polyCylinder [(-4.629,-34.83),(-4.860,-42.47),(-6.249,-42.93),(-6.249,-35.06),(-4.629,-34.83)] 10 model.meshStore
 
    |> matte Color.brown
 
    ,
 
    polyCylinder [(-4.397,-34.60),(3.2405,-34.60),(-0.462,-28.35),(1.6202,-28.12),(-1.157,-20.71),(0.4629,-20.94),(-5.555,-9.605),(-11.57,-20.71),(-9.952,-20.71),(-12.96,-26.50),(-11.57,-27.66),(-13.42,-34.37),(-6.712,-34.14),(-4.166,-33.44),(-3.934,-33.90),(-3.471,-33.90)] 10 model.meshStore
 
    |> matte Color.darkGreen
 
    ] |> scale3D 3
      |> rotateX3D (degrees 90)
 
 
--bee 1
bee1 textures meshes time = group3D
  [
  ellipsoid 25 10 15 meshes
  |> matte Color.lightOrange
  |> rotateZ3D (degrees 90)
  ,
  bee1_wingMove time
  ,
  bee1_eyes
  ,
  bee1_stripes
  ] |> rotateZ3D (degrees 270)
 
 
bee1_wing = group3D
            [
            cylinder 15 1 
            |> matte (Color.rgba 0.9 0.9 0.9 0.5)
 
            ]
 
bee1_wingMove time = group3D
    [
    bee1_wing |> move3D ( 20, 0, 0 ) 
  |> rotateY3D (degrees (-15 * sin (10 * time))) 
  |> move3D ( 0, 0, 8 )
  ,
  bee1_wing |> move3D ( -20, 0, 0 ) 
  |> rotateY3D (degrees (15 * sin (10 * time))) 
  |> move3D ( 0, 0, 8 )
    ]
 
bee1_eyes = group3D
    [
    sphere 1
    |> matte Color.black
    |> move3D (5,21,0)
    ,
    sphere 1
    |> matte Color.black
    |> move3D (-5,21,0)
    ]
 
bee1_stripes = group3D
    [
    rectangle3D 15 5
  |> matte Color.black
  |> scale3D 0.75
  |> move3D (0,0,15)
  ,
  rectangle3D 15 5
  |> matte Color.black
  |> scale3D 0.75
  |> move3D (0,9,14.5)
  ,
  rectangle3D 15 5
  |> matte Color.black
  |> scale3D 0.75
  |> move3D (0,-9,14.5)
    ]
 
--bee2
bee22 textures meshes time = group3D
  [
  ellipsoid 25 10 15 meshes
  |> matte Color.darkYellow
  |> rotateZ3D (degrees 90)
  ,
  bee2_wingMove time
  ,
  bee2_eyes
  ,
  bee2_stripes
  ,
  bee2_mouth
  ] |> rotateZ3D (degrees 270)
 
 
bee2_wing = group3D
            [
            cylinder 15 1 
            |> matte (Color.rgba 0.9 0.9 0.9 0.5)
 
            ]
 
bee2_wingMove time = group3D
    [
    bee2_wing |> move3D ( 20, 0, 0 ) 
  |> rotateY3D (degrees (-15 * sin (10 * time))) 
  |> move3D ( 0, 0, 8 )
  ,
  bee2_wing |> move3D ( -20, 0, 0 ) 
  |> rotateY3D (degrees (15 * sin (10 * time))) 
  |> move3D ( 0, 0, 8 )
    ]
 
bee2_eyes = group3D
    [
    sphere 1.7
    |> matte Color.white
    |> move3D (5,21,0)
    ,
    sphere 1.7
    |> matte Color.white
    |> move3D (-5,21,0)
    ,
    sphere 0.9
    |> matte Color.black
    |> move3D (5,22,2)
    ,
    sphere 0.9
    |> matte Color.black
    |> move3D (-5,22,2)
    ]
 
bee2_stripes = group3D
    [
    rectangle3D 15 5
  |> matte Color.black
  |> scale3D 0.75
  |> move3D (0,0,15)
  ,
  rectangle3D 15 5
  |> matte Color.black
  |> scale3D 0.75
  |> move3D (0,9,14.5)
  ,
  rectangle3D 15 5
  |> matte Color.black
  |> scale3D 0.75
  |> move3D (0,-9,14.5)
    ]
 
bee2_mouth = group3D
    [
    ring 10 4
  |> plastic Color.lightRed 0.5
  |> scale3D 0.25
  |> move3D (0,1,-25)
  |> rotateX3D (degrees 90)
    ]
 
--bee3
bee3 textures meshes time = 
    let
        useTexture = getTexture textures
    in
        group3D [
        sphere 20 |> textured (useTexture "bee_texture") 0 0 |> rotateX3D (degrees 90) |> move3D (0,20,5)
        ,
        bee3_wingMove time
        ,
        bee3_eyes
        --,
        -- stripes
        ] |> rotateZ3D (degrees 270)
 
 
bee3_wing = group3D
            [
            cylinder 15 1 
            |> matte (Color.rgba 0.9 0.9 0.9 0.5)
 
            ]
 
bee3_wingMove time = group3D
    [
    bee3_wing |> move3D ( 20, 0, 0 ) 
  |> rotateY3D (degrees (-15 * sin (10 * time))) 
  |> move3D ( 0, 0, 8 )
  ,
  bee3_wing |> move3D ( -20, 0, 0 ) 
  |> rotateY3D (degrees (15 * sin (10 * time))) 
  |> move3D ( 0, 0, 8 )
    ]
 
bee3_eyes = group3D
    [
    sphere 1.7
    |> matte Color.white
    |> move3D (5,21,0)
    ,
    sphere 1.7
    |> matte Color.white
    |> move3D (-5,22,0)
    ,
    sphere 0.9
    |> matte Color.black
    |> move3D (5,23,2)
    ,
    sphere 0.9
    |> matte Color.black
    |> move3D (-5,23,2)
    ]
bee3_mouth = group3D
    [
    ring 10 4
  |> plastic Color.lightRed 0.5
  |> scale3D 0.25
  |> move3D (0,31,0)
    ]
 
--queen bee
queenBee textures meshes time = 
    let
        useTexture = getTexture textures
    in
        group3D [
            ellipsoid 25 10 15 meshes |> textured (useTexture "bee_texture") 0 0 |> rotateX3D (degrees 90) |> move3D (0,0,5)
            |> rotateZ3D (degrees 90)
            ,
            qb_wingMove time
            ,
            qb_eyes
            |> move3D (0,2,5)
            ,
            qb_lips
            |> move3D (0,-1,0)
            --  ,
            -- stripes
            
        ] |> rotateZ3D (degrees 270)
 
 
qb_wing = group3D
            [
            cylinder 15 1 
            -- |> matte (Color.rgb 255 10 202)
            |> matte Color.lightPurple
 
            ]
 
qb_wingMove time = group3D
    [
    qb_wing |> move3D ( 20, 0, 0 ) 
  |> rotateY3D (degrees (-15 * sin (10 * time))) 
  |> move3D ( 0, 0, 8 )
  ,
  qb_wing |> move3D ( -20, 0, 0 ) 
  |> rotateY3D (degrees (15 * sin (10 * time))) 
  |> move3D ( 0, 0, 8 )
    ]
 
qb_eyes = group3D
    [
    sphere 1.7
    |> matte Color.white
    |> move3D (5,21,0)
    ,
    sphere 1.7
    |> matte Color.white
    |> move3D (-5,21,0)
    ,
    sphere 0.9
    |> matte Color.blue
    |> move3D (5,22,2)
    ,
    sphere 0.9
    |> matte Color.blue
    |> move3D (-5,22,2)
 
 
 
    ]
 
qb_lips = group3D
   [
 
 
    ring 7 3
  |> plastic Color.darkRed 0.5
  |> scale3D 0.25
  |> move3D (0,1,-25)
  |> rotateX3D (degrees 90)
   ]
 
qb_tiara = group3D
     [
     ring 10 4
     |> matte Color.darkYellow
     |> scale3D 1
     ,
     cone 10 40
    |> matte Color.darkYellow
    |> scale3D 0.5
    |> move3D (0,10,0)
    |> rotateY3D (degrees 0)
    ,
     cone 10 40
    |> matte Color.darkYellow
    |> scale3D 0.5
    |> move3D (0,10,0)
    |> rotateZ3D (degrees 30)    
 
    ,
    cone 10 40
    |> matte Color.darkYellow
    |> scale3D 0.5
    |> move3D (5,10,0)
    |> rotateZ3D (degrees 90)
    ,
    sphere 10
    |> matte Color.lightBlue
    |> scale3D 0.35
    |> move3D (-6,9,5)
 
 
     ]
 
--crops
crop1 model =  group3D [
   ellipsoid 10 10 40 model.meshStore
  |> matte Color.yellow
  |> scale3D 0.25
  |> move3D (0,0,35)
  ,
  cone 10 10
  |> matte Color.darkGreen
  |> scale3D 0.7
  |> rotateY3D (degrees -180)
  |> move3D (0,0,30)
  ,
  cylinder 1 70
  |> matte Color.darkGreen
  |> scale3D 0.5
   ]
 
--crop2
crop2 = group3D [        
       cylinder 1 70
       |> matte Color.lightBrown
       |> scale3D 0.7
       ,
       rectangle3D 70 10
       |> matte Color.lightBrown
       |> scale3D 0.15
       |> move3D (0,0,50)
       |> rotateZ3D (degrees 60)
 
       ] 
 
--rocks
rock = sphere 10
      |> metallic  Color.lightGrey 0.5
 

 
 
--pollen
pollen1 model= group3D 
    [
   sphere 3
   |>  metallic Color.lightYellow 1
   |> scale3D 2
   |> move3D (-10,1,3)
   ] 
 
pollen2 model= group3D 
 
   [
   sphere 2
   |> metallic Color.lightYellow 1
   |> scale3D 2
   ,
   sphere 3
   |>  metallic Color.lightYellow 1
   |> scale3D 2
   |> move3D (-10,1,3)
   ] 
 
--farmer
farmer (model) = group3D [
 
  polyCylinder [(-22.92,36.396),(2.6713,36.396),(3.1165,40.626),(-3.561,40.403),(-3.339,42.852),(-4.229,42.629),(-4.229,43.297),(-6.010,43.297),(-6.010,45.300),(-7.568,44.855),(-7.568,45.523),(-13.13,45.746),(-13.57,45.078),(-14.46,44.855),(-14.91,43.297),(-16.02,43.52),(-15.80,41.293),(-17.14,41.516),(-17.36,39.958),(-23.81,40.403),(-23.59,36.841),(-22.92,36.396)]5.1 model.meshStore
   |>matte Color.lightBrown
   |> move3D (0,0,-2)
   ,
   polyCylinder [(-21.37,13.022),(-21.81,0.5565),(-20.70,0.7791),(-21.14,-0.111),(-19.36,0.1113),(-19.14,-1.001),(-17.58,-1.224),(-0.890,-0.779),(-1.113,0.3339),(0.2226,0.7791),(-0.222,1.4469),(0.4452,1.2243),(0.8904,13.022),(-21.37,13.022)] 5 model.meshStore
 |> matte Color.lightOrange
 ,polyCylinder [(-16.02,12.577),(-22.48,12.8),(-22.70,16.584),(-21.81,16.361),(-21.59,18.142),(-20.92,18.587),(-20.92,20.146),(-20.25,19.923),(-20.25,21.704),(-18.69,21.704),(-18.69,23.707),(-15.58,23.485),(-3.784,23.930),(-3.561,23.04),(-2.671,23.04),(-2.671,22.594),(-1.113,22.372),(-1.558,20.591),(-0.445,21.036),(-0.445,19.033),(1.1130,18.810),(0.8904,17.474),(1.5582,17.697),(1.5582,14.358),(1.7808,13.245),(-16.02,12.577)] 5 model.meshStore
 |>matte Color.darkGrey
 ,
 polyCylinder [(-13.80,23.485),(-15.36,23.930),(-15.36,14.580),(-16.47,14.580),(-15.80,12.8),(-17.80,12.8),(-17.80,-15.47),(-12.02,-15.91),(-11.13,-7.457),(-10.01,-7.012),(-10.46,-5.676),(-9.349,-5.676),(-9.572,-9.906),(-8.459,-9.906),(-8.459,-14.58),(-2.003,-15.02),(-2.226,-3.005),(-2.893,-3.005),(-3.339,13.245),(-4.452,13.022),(-4.452,13.913),(-5.342,13.913),(-5.12,15.693),(-5.787,15.471),(-5.787,23.262),(-7.123,23.485),(-7.791,15.471),(-13.80,15.471),(-13.80,23.485)] 5.5 model.meshStore
 |> matte Color.darkBlue
 ,
 polyCylinder [(-10.90,-15.24),(-18.25,-15.69),(-18.69,-22.59),(-19.81,-22.59),(-20.03,-24.59),(-11.13,-25.48),(-10.90,-15.24)] 5 model.meshStore
 |> matte Color.brown
 ,
 polyCylinder [(-8.459,-14.58),(-2.448,-14.35),(-2.003,-22.59),(-1.113,-22.59),(-0.890,-25.04),(-8.236,-25.48),(-8.459,-14.58)] 5 model.meshStore
 |>matte Color.brown
 ,
 polyCylinder [(-16.02,35.506),(-16.47,32.612),(-18.03,32.834),(-17.58,30.163),(-16.25,30.386),(-16.25,26.379),(-15.36,26.156),(-15.13,25.266),(-14.02,25.266),(-13.80,23.707),(-7.123,23.485),(-7.346,24.598),(-5.565,24.598),(-5.565,25.488),(-4.229,25.266),(-4.452,30.608),(-2.226,29.940),(-2.671,32.834),(-4.006,32.612),(-4.006,35.728),(-16.02,35.506)] 5 model.meshStore
 |>matte Color.lightOrange]
    |> rotateY3D (degrees 180)
    |> rotateX3D (degrees 90)
 
 
cow (model)= group3D [polyCylinder [(-11.13,-9.906),(-18.69,-10.35),(-19.14,-7.457),(-21.59,-7.457),(-21.14,-4.786),(-23.81,-4.786),(-24.04,-2.337),(-27.38,-2.114),(-26.49,3.0052),(-29.38,2.7826),(-29.60,10.796),(-31.83,10.796),(-32.05,15.916),(-36.50,15.916),(-36.73,13.467),(-39.17,13.022),(-39.40,16.139),(-42.07,16.139),(-42.51,18.587),(-44.74,18.365),(-45.18,23.930),(-42.51,23.930),(-42.51,26.824),(-34.72,26.601),(-34.72,29.273),(-27.38,28.382),(-27.60,31.721),(-21.81,31.944),(-21.59,34.838),(-3.561,34.393),(-3.561,37.732),(-1.113,37.509),(-1.113,39.958),(0.6678,40.403),(1.3356,42.406),(6.9008,42.852),(7.3460,39.513),(10.017,39.958),(10.24,37.954),(7.3460,37.064),(7.1234,31.944),(4.4521,31.499),(4.4521,25.933),(38.288,26.379),(38.511,23.707),(40.737,24.153),(40.96,21.036),(43.186,21.481),(42.963,17.92),(45.634,18.365),(45.857,15.693),(50.977,16.584),(51.422,26.601),(48.306,26.824),(48.306,29.273),(46.08,29.495),(45.857,34.615),(48.751,34.170),(48.083,37.064),(54.539,37.286),(54.316,34.838),(56.987,34.393),(56.765,31.944),(59.213,32.166),(59.213,29.273),(56.542,29.718),(56.32,26.601),(54.093,26.379),(53.871,16.584),(51.2,16.139),(50.977,14.358),(48.528,13.913),(48.751,-7.012),(45.634,-7.234),(45.857,-15.02),(44.076,-15.02),(43.408,-20.59),(40.514,-20.81),(40.514,-23.04),(33.168,-23.04),(32.723,-17.69),(30.497,-17.47),(30.274,-20.59),(27.826,-19.92),(28.048,-22.59),(20.257,-22.81),(20.257,-13.02),(15.137,-12.35),(14.914,-15.02),(12.466,-14.80),(12.466,-19.70),(10.017,-19.92),(10.017,-22.81),(-0.890,-23.04),(-0.445,-18.14),(1.7808,-17.92),(1.7808,-12.35),(-0.667,-12.35),(-0.890,-14.80),(-2.893,-14.58),(-3.339,-19.92),(-6.010,-20.14),(-6.233,-23.04),(-13.80,-22.81),(-13.80,-17.47),(-11.13,-17.69),(-11.13,-9.906)] 5 model.meshStore
  |>matte Color.grey
  ,
  polyCylinder [(4.2295,18.365),(1.7808,18.587),(2.2260,21.481),(-11.13,21.481),(-10.90,23.930),(-14.46,24.153),(-13.57,26.601),(-18.47,26.156),(-18.92,31.721),(4.6747,31.499),(4.2295,26.601),(1.7808,26.824),(1.5582,23.707),(4.0069,23.262),(4.2295,18.365)] 5.01 model.meshStore
  |>matte Color.black
  ,
 polyCylinder [(4.4521,23.930),(6.9008,23.707),(7.1234,21.481),(8.9043,21.926),(9.7947,8.5704),(12.020,8.5704),(11.575,6.1217),(14.469,6.1217),(14.469,3.8956),(19.812,3.4504),(19.366,5.8991),(22.706,5.8991),(22.483,8.5704),(25.377,8.3478),(25.154,10.796),(27.158,10.796),(27.826,8.7930),(30.497,8.7930),(30.274,6.1217),(32.946,6.1217),(32.946,3.6730),(38.066,3.2278),(38.288,6.1217),(40.96,5.6765),(40.96,8.3478),(43.408,8.1252),(43.408,13.690),(42.963,17.92),(43.186,21.036),(40.737,21.036),(40.737,23.930),(38.288,23.930),(38.511,26.824),(4.6747,26.379),(4.4521,23.930)] 5.02 model.meshStore
 |>matte Color.black
 ,
 polyCylinder [(-21.59,0.7791),(-21.81,3.6730),(-19.14,3.6730),(-19.58,6.1217),(-14.24,5.6765),(-14.69,8.5704),(-6.233,8.5704),(-6.010,11.686),(2.0034,11.019),(2.2260,9.2382),(4.0069,9.0156),(4.4521,-1.446),(1.5582,-1.224),(1.7808,-4.786),(-0.667,-4.340),(-0.890,-7.457),(-8.904,-7.234),(-19.14,-7.457),(-18.69,-5.008),(-21.14,-4.786),(-21.59,0.7791)] 5.03 model.meshStore
 |>matte Color.lightBrown
 ,
 polyCylinder [(-34.72,29.495),(-34.72,36.841),(-32.27,36.841),(-32.05,39.958),(-29.60,39.958),(-29.16,36.619),(-26.71,36.841),(-26.49,31.499),(-27.82,31.499),(-27.15,28.16),(-34.50,28.605),(-34.72,29.495)] 5.04 model.meshStore
 |>matte Color.lightBrown
 ,
 polyCylinder [(-16.69,34.393),(-16.69,42.406),(-14.02,42.406),(-13.80,44.855),(-11.35,45.078),(-10.90,42.629),(-8.681,42.184),(-8.904,40.626),(-5.565,39.735),(-5.787,34.838),(-16.69,34.393)] 5.05 model.meshStore
 |>matte Color.lightBrown
 ,
 polyCylinder [(-11.13,-2.337),(-16.02,-2.114),(-16.02,-3.895),(-10.90,-4.563),(-11.13,-2.337)] 5.06 model.meshStore
 |>matte Color.black
 ,
 polyCylinder [(-0.445,1.2243),(-1.113,3.4504),(-5.342,3.0052),(-5.787,1.0017),(-0.445,1.2243)] 5.07 model.meshStore
 |>matte Color.black
 ,
 polyCylinder [(6.9008,-18.14),(-0.445,-17.92),(-1.113,-21.48),(9.5721,-22.37),(9.3495,-20.14),(7.1234,-19.92),(6.9008,-18.14)] 5.08 model.meshStore
 |>matte Color.black
 ,
 polyCylinder [(-3.784,-17.25),(-12.91,-17.92),(-14.46,-21.70),(-6.233,-22.14),(-6.010,-20.36),(-3.561,-20.36),(-3.784,-17.25)] 5.09 model.meshStore
 |>matte Color.black
 ,
 polyCylinder [(20.48,-18.58),(30.052,-17.92),(29.829,-20.59),(27.603,-20.36),(27.158,-22.14),(20.257,-22.81),(20.48,-18.58)] 5.10 model.meshStore
 |>matte Color.black
 ,
 polyCylinder [(43.408,-17.92),(43.631,-19.70),(40.737,-19.92),(41.182,-22.37),(32.946,-22.81),(32.946,-17.92),(43.408,-17.92)] 5.11 model.meshStore
 |>matte Color.black
 ,
 polyCylinder [(-18.92,13.690),(-18.92,8.3478),(-24.26,8.1252),(-24.26,14.358),(-18.92,13.690)] 5.12 model.meshStore
 |>matte Color.black
 ,
 polyCylinder [(-8.681,19.033),(-3.339,18.810),(-3.561,14.135),(-7.791,13.913),(-8.681,19.033)] 5.13 model.meshStore
 |>matte Color.black]    
   |> rotateY3D (degrees 180)
   |> rotateX3D (degrees 90)
 
 
 
--beehive
beebox model = group3D 
            [polyCone [(-20,15),(20,15),(20,-15),(-20,-15),(-20,15)] (0, 0, 40) model.meshStore
                    |> matte (Color.rgb255 153 153 0)
                    |> rotateY3D (degrees 180)
                    |> move3D (0,0,15)
                    , 
                    box 55 40 3
                  --  |> matte (Color.rgb255 153 153 0)
                    |> matte (Color.hsl (abs <| cos model.time) 1 0.5)
                    |> move3D (-25,-20,15) --(frontandback,leftandright,-)
                    ,
                    box 45 35 20
                    |> matte (Color.rgb255 153 153 0)
                    |> move3D (-20,-17.5,18)
                    , 
                    box 55 40 3
                 --   |> matte (Color.rgb255 153 153 0)
                    |> matte (Color.hsl (abs <| cos model.time) 1 0.5)
                    |> move3D (-25,-20,23)
                    , 
                    box 55 40 3
                 --   |> matte (Color.rgb255 153 153 0)
                    |> matte (Color.hsl (abs <| cos model.time) 1 0.5)
                    |> move3D (-25,-20,31)
                    , 
                    box 55 40 3
               --   |> matte (Color.rgb255 153 153 0)
                    |> matte (Color.hsl (abs <| cos model.time) 1 0.5)
                    |> move3D (-25,-20,37)
                    ,
                    box 37 25 10
                 --   |> matte (Color.rgb255 153 153 0)
                    |> matte (Color.rgb255 153 153 0)
                    |> move3D (-16,-12.5,40)
                    , 
                    box 45 30 3
                 --   |> matte (Color.rgb255 153 153 0)
                   |> matte (Color.hsl (abs <| cos model.time) 1 0.5)
                    |> move3D (-20,-15,50)
                    ,
                    box 27 15 30
                    |> matte (Color.rgb255 153 153 0)
                    |> move3D (-10,-8,42)
                    ,
                    hive model
                    |> scale3D 0.3
                    |> move3D (0,0,71)
                    ,
                    hive model
                    |> scale3D 0.3
                    |> rotateX3D (degrees 30)
                    |> rotateZ3D (degrees 90)
                    |> move3D (0,-11,55)
                    ,
                    hive model
                    |> scale3D 0.3
                    |> rotateX3D (degrees 30)
                    |> rotateZ3D (degrees 90)
                    |> move3D (0,7,55)
            ] |> nameObject "beehive"
 
killerBeeHive model = group3D
                [ polyCone [(-0.838,39.406),(-33.95,20.960),(-34.16,-17.18),(-1.048,-36.47),(32.279,-17.18),(32.279,20.751),(-0.838,39.196)] (0, 0, 90) model.meshStore
                    |> matte (Color.rgb255 255 255 0)
                    |> rotateY3D (degrees 180)
                    |> move3D (0,0,60)
                    ,
                    hive model
                    |> scale3D 1
                    |> rotateX3D (degrees 30)
                    |> rotateY3D (degrees 90)
                    |> move3D (-25,15,66)
                    ,
                    hivemini model
                    |> scale3D 0.3
                    |> rotateY3D (degrees 90)
                    |> move3D (-16,11,68)
                    ,
                    hivemini model
                    |> scale3D 0.3
                    |> rotateY3D (degrees 90)
                    |> move3D (22,11.5,68)
                ]  
 
hive model = group3D (
          List.map (\(x,y,z) -> myHexagon model |> move3D (x,y,z))
            [ (0,0,0)
            , (0,19,11)
            , (0,19,-11)
            , (0,0,22)
            , (0,0,-22)
            , (0,-19,11)
            , (0,-19,-11)
            ] )
 
myHexagon model = polyCylinder  [(-0.838,39.406),(-33.95,20.960),(-34.16,-17.18),(-1.048,-36.47),(32.279,-17.18),(32.279,20.751),(-0.838,39.196)] 30 model.meshStore
              |> plastic Color.lightYellow 0.5
              |> scale3D 0.3
              |> rotateY3D (degrees 90)
              |> move3D (0,0,30)
 
myHexagonFlat model = polygon3D 6 10
                   |> matte Color.black
                  -- |> rotateX3D (degrees 30)
                   |> rotateY3D (degrees 90)
 
hivemini model = group3D (
          List.map (\(x,y,z) -> myHexagonFlat model |> move3D (x,y,z))
            [ (0,0,0)
            , (0,19,11)
            , (0,19,-11)
            , (0,0,22)
            , (0,0,-22)
            , (0,-19,11)
            , (0,-19,-11)
            ] )
 
 
--pig
pig (model) = group3D [
 polyCylinder [(-36.50,-39.73),(-42.51,-39.73),(-42.74,-37.28),(-48.52,-37.28),(-48.97,-41.51),(-54.98,-41.51),(-55.20,-35.50),(-57.43,-35.28),(-57.43,-29.27),(-59.65,-29.27),(-59.65,-24.82),(-61.21,-25.04),(-61.44,-8.570),(-59.43,-8.570),(-59.88,-6.344),(-57.43,-6.344),(-57.65,-3.895),(-55.87,-4.118),(-55.20,2.3373),(-57.65,1.8921),(-58.10,5.8991),(-59.65,6.1217),(-59.43,19.478),(-57.43,19.033),(-57.65,21.481),(-55.65,21.036),(-55.65,23.707),(-53.20,23.262),(-53.20,25.043),(-56.54,25.043),(-56.76,27.046),(-59.43,27.269),(-59.65,29.050),(-61.66,29.273),(-61.21,37.286),(-59.21,37.064),(-59.43,39.513),(-57.65,39.513),(-57.43,41.961),(-55.87,41.739),(-55.42,43.742),(-53.42,43.742),(-53.64,46.191),(-44.74,46.191),(-44.96,43.742),(-42.51,43.742),(-42.29,41.516),(-36.50,41.739),(-36.28,39.735),(-24.26,39.735),(-24.04,37.732),(-11.79,37.286),(-12.02,39.958),(-5.12,39.290),(-5.342,37.954),(-3.561,37.954),(-2.893,33.947),(-1.335,33.725),(-1.113,31.944),(0.2226,31.944),(1.3356,20.813),(-0.890,21.036),(-0.667,19.033),(-4.897,18.587),(-4.897,-1.001),(-7.346,-0.333),(-7.346,-3.673),(-9.349,-3.895),(-9.349,-6.121),(-11.79,-6.344),(-12.24,-8.570),(-14.02,-8.570),(-14.24,-11.90),(-11.57,-12.35),(-11.35,-26.82),(-13.80,-27.93),(-13.35,-30.38),(-16.25,-30.38),(-16.02,-36.61),(-17.36,-37.06),(-17.80,-39.51),(-24.04,-39.73),(-24.04,-41.73),(-26.71,-41.51),(-26.26,-43.52),(-28.27,-43.52),(-28.27,-47.52),(-36.28,-47.74),(-36.50,-39.73)] 5 model.meshStore
 |> matte (Color.hsl (318/360) 1 0.50)
 ,
 polyCylinder [(-28.04,0.1113),(-36.28,-0.333),(-36.50,1.8921),(-42.51,1.8921),(-42.74,4.3408),(-44.96,4.3408),(-44.74,10.351),(-42.51,10.351),(-43.18,12.8),(-28.04,12.132),(-28.27,10.128),(-26.49,10.128),(-25.82,1.8921),(-28.04,2.1147),(-28.04,0.1113)]5.01 model.meshStore
 |> matte (Color.hsl (318/360) 1 0.83)
 ,
 polyCylinder [(-46.97,16.584),(-46.97,18.810),(-50.97,18.587),(-51.2,16.361),(-53.20,16.361),(-53.42,12.8),(-50.97,12.8),(-51.2,10.796),(-46.74,10.128),(-46.97,12.577),(-44.29,12.577),(-44.96,16.584),(-46.97,16.584)]5.02 model.meshStore
 |> matte Color.black
 |> move3D (-34,-8,0)
 ,
 polyCylinder [(-21.81,5.8991),(-23.81,6.1217),(-23.81,10.351),(-22.48,10.351),(-21.81,12.577),(-18.25,12.354),(-17.80,10.796),(-16.02,10.796),(-15.80,6.5669),(-17.80,6.5669),(-18.03,4.3408),(-21.14,4.3408),(-21.81,5.8991)]5.03 model.meshStore
 |> matte Color.black
 ,
 polyCylinder [(-18.03,-39.06),(-15.36,-39.73),(-16.02,-44.63),(-18.25,-45.96),(-19.58,-42.18),(-17.58,-40.84),(-18.69,-39.51),(-18.03,-39.06)] 5.04 model.meshStore
 |> matte (Color.hsl (318/360) 1 0.50)
 ,
 polyCylinder [(-31.83,9.6834),(-34.28,9.6834),(-34.50,3.2278),(-32.50,3.4504),(-31.83,9.6834)] 5.05 model.meshStore
 |> matte Color.black
 ,
 polyCylinder [(-37.84,9.6834),(-40.06,9.6834),(-40.29,4.1182),(-38.06,4.3408),(-37.84,9.6834)] 5.06 model.meshStore
 |> matte Color.black
 ,
 polyCylinder [(-17.80,10.351),(-19.36,10.573),(-19.36,8.7930),(-18.03,9.0156),(-17.80,10.351)]5.07 model.meshStore
 |> matte Color.white 
 
 
  ]  |> rotateY3D (degrees 180)
     |> rotateX3D (degrees 90)
     |> scale3D 3
 
 
 
 
--chicken
chiken (model) = group3D  [ polyCylinder [(8.6817,10.351),(-2.448,10.128),(-2.671,11.686),(-4.452,11.909),(-4.452,13.467),(-6.455,13.913),(-6.233,17.252),(-8.236,17.697),(-8.236,20.146),(-19.36,19.700),(-19.58,8.3478),(-17.36,8.1252),(-17.80,5.0086),(-16.02,4.7860),(-15.80,0.7791),(-13.80,0.7791),(-14.02,-4.563),(-11.79,-4.563),(-12.02,-6.789),(-9.794,-7.234),(-10.24,-7.902),(-6.678,-8.570),(-6.233,-10.35),(-4.452,-10.57),(-4.452,-12.35),(6.9008,-12.8),(6.9008,-11.01),(10.017,-11.01),(10.685,-8.793),(12.466,-8.570),(12.243,-7.012),(14.024,-7.234),(14.469,-5.008),(15.805,-4.786),(16.027,0.5565),(18.253,1.2243),(18.031,6.3443),(20.034,6.1217),(20.034,13.690),(18.253,13.690),(17.808,16.361),(16.027,15.916),(16.027,13.913),(14.246,14.135),(14.246,12.132),(12.466,11.909),(12.020,10.351),(10.685,10.573),(10.462,8.5704),(8.4591,8.7930),(8.6817,10.351)] 5 model.meshStore
 |> matte Color.grey
 ,
 polyCylinder [(-19.14,9.6834),(-19.58,19.033),(-20.92,19.255),(-21.14,17.252),(-22.48,17.252),(-22.48,15.471),(-25.15,15.693),(-25.37,14.135),(-23.37,14.135),(-23.37,11.909),(-21.81,12.132),(-21.37,10.573),(-19.14,9.6834)]5.01 model.meshStore
 |> matte Color.orange 
 ,
 polyCylinder [(-9.794,19.478),(-9.794,21.259),(-12.24,21.481),(-12.02,25.266),(-13.80,25.266),(-14.02,23.485),(-15.36,23.485),(-15.58,25.266),(-17.14,25.043),(-17.80,23.930),(-19.36,23.262),(-19.14,20.368),(-9.794,19.478)]5.02 model.meshStore
 |> matte Color.red
 ,
 polyCylinder [(-15.36,15.916),(-17.58,15.693),(-17.14,17.474),(-15.58,17.474),(-15.36,15.916)] 5.03 model.meshStore
 |> matte Color.black
 
 ]
  |> rotateY3D (degrees 180)
   |> rotateX3D (degrees 90)
 
 
--simple bee hive
simplehive model = group3D 
           [ellipsoid 10 10 5 model.meshStore
            |> matte Color.yellow
            |> move3D (0, 0, 5)
            , 
            ellipsoid 15 15 5 model.meshStore
            |> matte Color.yellow
            |> move3D (0, 0, 11)
            , 
            ellipsoid 20 20 5 model.meshStore
            |> matte Color.yellow
            |> move3D (0, 0, 17)
            , 
            ellipsoid 25 25 5 model.meshStore
            |> matte Color.yellow
            |> move3D (0, 0, 23)
            ,
            ellipsoid 10 10 5 model.meshStore
            |> matte Color.yellow
            |> move3D (0, 0, 47)
            , 
            ellipsoid 15 15 5 model.meshStore
            |> matte Color.yellow
            |> move3D (0, 0, 41)
            , 
            ellipsoid 20 20 5 model.meshStore
            |> matte Color.yellow
            |> move3D (0, 0, 35)
            , 
            ellipsoid 25 25 5 model.meshStore
            |> matte Color.yellow
            |> move3D (0, 0, 29)
            , 
            ellipsoid 3 5 8 model.meshStore
            |> matte Color.black
            |> move3D (20, 0, 32)
            ] |> renderCollider 5 |> nameObject "beehive"
 
hill model = group3D [
       ellipsoid 1000 1000 150 model.meshStore
         |> matte Color.darkGreen
    --    ,
    --    beehiveTree 
    --      |> move3D (0,0,100)
         ]
theTrees model = group3D [
                          pineTree |> move3D (-3500, 1000, 0)
                          ,
                          mapleTree |> move3D (-2500, 1100, 0)
                          ,
                          pineTree |> move3D (-500, 1300, 0)
                          ,
                          tree2 model |> move3D (2500, 1650, 0)
                          ]
                          
treesForPath n model = group3D [  
                          tree1 model |> move3D (800*n, 0, 0)
                          , 
                        --   beehiveTree |> move3D (0, 0, 0)
                        --   ,
                          pineTree |> move3D (400*n, 0, 0)
                          ,
                          mapleTree |> move3D (-400*n, 0, 0)
                          ,
                          tree2 model |> move3D (-800*n, 0, 0) 
                        ]

forest = group3D [ 
 
              oakTree
             |> move3D (-270, 0, 0)
              ,
              ashTree
              |> move3D (-300,-660,0)
                ,
              ashTree
              |> move3D (-1000, 100, 0)
              ,
              mapleTree
              |> move3D (50, 100, 0)
              |> scale3D 4
              ,
              pineTree
              |> move3D (-300, -150, 0)
              |> scale3D 3
               ,
              pineTree
              |> move3D (200, -50, 0)
              |> scale3D 3
              ,
              pineTree
              |> scale3D 4.5
              |> move3D (300, -620, 0)
              ,
              mapleTree
              |> move3D (-340, -200, 0)
              |> scale3D 4
            --   ,
            --   beehiveTree 
            --   |> move3D (200, -200,0)
            --   |> scale3D 4
         ]

jpbee textures meshes time = 
    let
        useTexture = getTexture textures
    in
        group3D 
            [
                ellipsoid 10 20 15 meshes
                |> matte Color.blue
                |> move3D (0,0,20)
                ,
                ellipsoid 1 20 15 meshes
                |> matte Color.white
                |> rotateY3D (degrees 90)
                |> rotateY3D (0.5 * sin (10 *time)) 
                |> move3D (-15,-5,20)
                ,
                ellipsoid 1 20 15 meshes
                |> matte Color.white
                |> rotateY3D (degrees 90)
                |> rotateY3D (-0.5 * sin (10 *time)) 
                |> move3D (15,-5,20)
            ] |> rotateZ3D (degrees 270)

    
cloudsFinal model = group3D (List.map (\ y -> clouds model |> move3D (-5000, y*2000, 0)) (List.map toFloat <| List.range -1 1))
    
clouds model = group3D (List.map3
                      (\ x z y -> cloud |> move3D (x*2000 + sin(model.time)*50,y*2000 + sin(model.time)*50,3000))
                      (List.map toFloat <| List.range -3 3)
                      (List.map toFloat <| List.range -2 3)
                      (List.map toFloat <| List.range 0 5))
 
-- Please put all of your flowers here, and NOT in myEntities
myFlowers model =
    let
        useTexture = getTexture model.textureLoader.textures
       
        
    in
    [ 
        flower1 Color.red
        |> move3D (895, -987,0)
        ,
        flower1 Color.orange  
        |> move3D (1895, -1597,0)
        ,
        flower1 Color.purple
        |> move3D (1783, -1037,0)
        ,
        flower1 Color.blue
        |> move3D (-1289, -1597,0)
        ,
        flower1 (Color.rgb255 143 255 218)
        |> move3D (-1803, -1480, 0)   
        ,
        flower1 (Color.rgb255 251 189 255)
        |> move3D (-1092, -2802, 0)
    ] ++ rowOfFlowerForest ++ rowOfFlower
 
beach model = group3D[
      ellipsoid 3000 1200 10 model.meshStore
       |> matte (Color.rgb255 220 192 139)
       |> move3D (-70,1000,3)
      ,
      box 1500 1000 15
        |> matte Color.blue
        |> move3D (-1200,500,0)
       ,
       pomtree model |> scale3D 2 |> move3D (-250,0,0)
       ]

pomtree model =
    let
        useTexture = getTexture model.textureLoader.textures
    in         
         group3D [ sphere 10 |> textured (useTexture "Wood") 0 0 |> move3D (0,0,0)
         ,
         sphere 10 |> textured (useTexture "Wood") 0 0 |> move3D (0,0,30)
         ,
         sphere 10 |> textured (useTexture "Wood") 0 0 |> move3D (0,0,60)
         ,
         sphere 10 |> textured (useTexture "Wood") 0 0 |> move3D (0,0,90)
         ,
         sphere 10 |> textured (useTexture "Leaf") 0 0 |> move3D (20,-10,90)
         ,                                                          
         sphere 10 |> textured (useTexture "Leaf") 0 0 |> move3D (40,-26,90)
         ,
         sphere 10 |> textured (useTexture "Leaf") 0 0 |> move3D (60,-40,90)
         ,
         sphere 10 |> textured (useTexture "Leaf") 0 0 |> move3D (80,-60,90)
         ,
         sphere 10 |> textured (useTexture "Leaf") 0 0 |> move3D (80,60,90)
         ,
         sphere 10 |> textured (useTexture "Leaf") 0 0 |> move3D (60,40,90)
         ,
         sphere 10 |> textured (useTexture "Leaf") 0 0 |> move3D (40,26,90)
         ,
         sphere 10 |> textured (useTexture "Leaf") 0 0 |> move3D (20,10,90)
         ,
         sphere 10 |> textured (useTexture "Leaf") 0 0 |> move3D (-20,-10,90)
         ,
         sphere 10 |> textured (useTexture "Leaf") 0 0 |> move3D (-40,-26,90)
         ,
         sphere 10 |> textured (useTexture "Leaf") 0 0 |> move3D (-60,-40,90)
         ,
         sphere 10 |> textured (useTexture "Leaf") 0 0 |> move3D (-80,-60,90)
         ,
         sphere 10 |> textured (useTexture "Leaf") 0 0 |> move3D (-20,10,90)
         ,
         sphere 10 |> textured (useTexture "Leaf") 0 0 |> move3D (-40,26,90)
         ,
         sphere 10 |> textured (useTexture "Leaf") 0 0 |> move3D (-60,40,90)
         ,
         sphere 10 |> textured (useTexture "Leaf") 0 0 |> move3D (-80,60,90)
         ,
         sphere 10 |> textured (useTexture "Cocanut") 0 0 |> move3D (17,1,80)
         ,
         sphere 10 |> textured (useTexture "Cocanut") 0 0 |> move3D (-17,1,80)
         ,
         sphere 10 |> textured (useTexture "Cocanut") 0 0 |> move3D (0,15,80)
         ,
         sphere 10 |> textured (useTexture "Cocanut") 0 0 |> move3D (0,-15,80)
         ,
         sphere 10 |> textured (useTexture "Cocanut") 0 0 |> move3D (-50,50,0)
         ,
         sphere 10 |> textured (useTexture "Cocanut") 0 0 |> move3D (50,50,0)
         ,
         sphere 10 |> textured (useTexture "Cocanut") 0 0 |> move3D (50,-50,0)
         ,
         sphere 10 |> textured (useTexture "Cocanut") 0 0 |> move3D (-50,-50,0)]
      
honeycomb =
       group 
       [
      square 55
         |> filled brown
         |> move (2.5,0)
         ,
        row1
         ,
         row2
         |> move (-1,-5)
         ,
         row3
         |> move (-2,0)
         ,
         row4
         |> move (-3, -5)
         ,
         row5
         |> move (-4, 0)
         ,
         row5
         |> move (5, 5)
         ,
         ngon 6 5
           |> filled yellow
           |> move (-11, 25)
           ,
         ngon 6 5
           |> filled yellow
           |> move (7, 25)
          ,
          ngon 6 5
           |> filled yellow
           |> move (25, -25)
       ]            



window = group3D [
  square3D 100 |> matte Color.white |> rotateX3D(degrees 90) |> rotateY3D(0) |> rotateZ3D(110) |> move3D(90,3490,400)
  ,square3D 33 |> matte Color.lightBlue |> rotateX3D(degrees 90) |> rotateY3D(degrees 0) |> rotateZ3D(110) |> move3D(110,3490,420)
  ,square3D 33 |> matte Color.lightBlue |> rotateX3D(degrees 90) |> rotateY3D(degrees 0) |> rotateZ3D(110) |> move3D(70,3480,420)
  ,square3D 33 |> matte Color.lightBlue |> rotateX3D(degrees 90) |> rotateY3D(degrees 0) |> rotateZ3D(110) |> move3D(110,3490,380)
  ,square3D 33 |> matte Color.lightBlue |> rotateX3D(degrees 90) |> rotateY3D(degrees 0) |> rotateZ3D(110) |> move3D(70,3480,380)]

door = group3D [
  rectangle3D 100 200 |> matte Color.white |> rotateX3D(degrees 90) |> rotateZ3D(110) {--|> move3D(90,3490,0)--}]

roof model = group3D [
    polyCone [(-55.30,-29.20),(8.3262,-33.67),(15.533,15.285),(-46.10,25.724),(-55.05,-29.20)] (0,0,25) model.meshStore |> matte Color.red  |> scale3D 13 |> move3D(650,1790,500)]
  

table = group3D [
  box 150 100 100 |> matte Color.lightBrown]

book col = group3D [
  box 10 10 2 |> matte col]


cottage model = 
  let
     useTexture = getTexture model.textureLoader.textures
  in group3D [
    box 50 1500 500 |> matte Color.brown |> move3D(0,1500,0)
    , box 800 50 500 |> matte Color.brown |> move3D(0,1500,180)
    , box 800 50 500 |> matte Color.brown |> move3D(0,1500,180)
    , box 700 50 200 |> matte Color.brown |> move3D(50,3000,300) -- Back wall assistance wall
    , box 600 50 300 |> matte Color.brown |> move3D(50,3000,0) -- Back wall
    , box 650 50 300 |> matte Color.brown |> move3D(150,1500,0) -- Left wall assistance wall
    , box 50 1550 500 |> matte Color.brown |> move3D(750,1500,0) -- Right wall assistance wall
    , door |> rotateZ3D(90*sin(0.01*model.time)) |> move3D (70,1490,80) -- Right Side wall
    , window |> move3D(0,-2000,0)
    , window |> move3D(250,-2000,0)
    , window |> move3D(350,-2000,0)
    , window |> move3D(600,-2000,0)
    , window |> move3D(250,-2000,-300)
    , roof model |> move3D (0,0,100)
    , river model |> move3D (0,0,15)
    , rectangle3D 140 80 |> textured(useTexture "treasure chest") 0 0 |> rotateX3D(degrees 90) |> rotateY3D(degrees -90) |> move3D(500,1800,80)
    , beehivecottage model |> move3D(50,1300,200) |> scale3D(3)]

river model = group3D [
  polyCylinder [(-33.42,-44.36),(-28.45,-45.11),(-13.54,-14.04),(-13.79,-3.106),(-31.19,26.221),(-38.4,50.330),(-43.12,49.335),(-34.92,25.724),(-16.77,-2.361),(-16.52,-12.8),(-33.42,-44.11)] 0.02 model.meshStore |> matte Color.blue |> scale3D(55) |> rotateZ3D(degrees 310)]
  
cottageFinal model = group3D [
    --,rectangle3D 100 300 |> matte Color.white |> rotateX3D(degrees 90) |> rotateY3D(0) |> rotateZ3D(110)
    cottage model |> rotateZ3D (degrees -30) |> move3D (-3000,2200,0) 
    ,chair |> scale3D 1.5 |> rotateZ3D (degrees 120) |> move3D (500,3400,0)
    ]
    
rowOfFlower = 
    (List.map2
        (\ x col -> flower1 col |> move3D (x*700 + 50, 0, 0))
        (List.map toFloat <| List.range -4 4)
        ([Color.red, Color.blue, Color.lightBlue, Color.purple, Color.blue, Color.green, Color.orange, Color.yellow, Color.brown, Color.darkGreen, Color.darkBrown, Color.red, Color.blue, Color.orange]))
    ++ (List.map2
        (\ x col -> flowerthing col |> move3D (x*700,550,20))
        (List.map toFloat <| List.range -4 4)
        ([Color.red, Color.blue, Color.lightBlue, Color.purple, Color.blue, Color.green, Color.orange, Color.yellow, Color.brown, Color.darkGreen, Color.darkBrown, Color.red, Color.blue, Color.orange]))
                

rowOfFlowerForest = 
    List.map2
        (\ x col -> flower1 col |> move3D (x*500 + 50, 0, 0))
        (List.map toFloat <| List.range -1 1)
        ([Color.red, Color.blue, Color.lightBlue, Color.purple, Color.blue, Color.green, Color.orange, Color.yellow, Color.brown, Color.darkGreen, Color.darkBrown, Color.red, Color.blue, Color.orange])
    ++ List.map2
        (\ x col -> flowerthing col |> move3D (x*500,550,20))
        (List.map toFloat <| List.range -1 1)
        ([Color.red, Color.blue, Color.lightBlue, Color.purple, Color.blue, Color.green, Color.orange, Color.yellow, Color.brown, Color.darkGreen, Color.darkBrown, Color.red, Color.blue, Color.orange])

-- move / edit the light
light =
    pointLight
        { position = Point3d.centimeters 0 0 100    -- position of the light
        , chromaticity = Light.sunlight             -- the colour of the light (see https://package.elm-lang.org/packages/ianmackenzie/elm-3d-scene/latest/Scene3d-Light#Chromaticity)
        , intensity = LuminousFlux.lumens 10000     -- how intense the light is
        }
showLight = True -- whether to show the light ball or not

-- Set to False to turn off shadows
drawShadows = False
 
-- Put any custom meshes you need generated in here. Make sure the values are identical.
myMeshes =
    [ generateEllipsoid 20 10 10 -- This is used for the default bee model. It's probably best to leave it here 
      ,
      generateEllipsoid 1000 1000 150
      , generateEllipsoid 10 20 15
      , generateEllipsoid 1 20 15
    ]
 
    ++
    --group1
    [ generateEllipsoid 20 10 10 -- This is used for the default bee model. It's probably best to leave it here
    , generatePolyCylinder [(-3.240,-7.059),(-5.323,-7.291),(-7.869,-7.291),(-11.34,-7.059),(-12.73,-6.365),(-14.35,-4.050),(-13.88,-2.661),(-12.73,-1.273),(-12.26,-0.115),(-13.19,1.5045),(-13.19,1.7359),(-12.96,2.1989),(-11.11,5.4394),(-9.490,6.3652),(-7.638,7.5226),(-6.018,6.8282),(-4.629,6.5967),(-3.471,6.8282),(0.2314,10.763),(3.2405,11.457),(4.3978,11.226),(5.0922,9.1428),(5.5551,8.2169),(7.6383,6.5967),(9.2585,6.5967),(10.878,6.5967),(11.110,5.9023),(12.730,4.2820),(15.276,2.4303),(15.276,0.5786),(16.433,-2.661),(16.202,-4.976),(14.582,-8.448),(11.804,-9.605),(10.647,-9.837),(9.9529,-9.374),(9.0271,-8.448),(6.7124,-7.522),(3.4719,-7.059),(2.7775,-8.216),(2.3146,-8.216),(0.2314,-8.216),(-0.925,-8.679),(-1.851,-8.216),(-1.851,-7.291),(-2.083,-6.596),(-3.934,-6.365),(-3.240,-7.059)] 10
    , generatePolyCylinder [(-23.60,24.188),(-22.45,25.576),(-21.06,26.965),(-20.13,26.965),(-18.05,26.965),(-17.35,26.271),(-16.43,26.502),(-15.04,26.965),(-14.35,26.271),(-13.88,24.882),(-13.65,24.419),(-13.19,24.188),(-10.41,19.558),(-2.083,-2.430),(-1.388,-6.828),(-1.620,-16.08),(-1.388,-17.47),(-4.397,-26.96),(-6.249,-26.73),(-14.81,-30.90),(-22.22,-29.51),(-30.32,-26.96),(-33.79,-24.65),(-35.64,-20.71),(-37.03,-14.92),(-38.65,-11.22),(-36.80,-9.374),(-36.10,-5.902),(-35.87,-2.430),(-35.41,0.1157),(-34.48,1.9674),(-33.33,4.0506),(-32.40,7.0596),(-31.24,10.068),(-24.30,24.882),(-23.60,26.039),(-21.52,26.271),(-23.60,24.188)] 10
    , generatePolyCylinder  [(-4.629,-34.83),(-4.860,-42.47),(-6.249,-42.93),(-6.249,-35.06),(-4.629,-34.83)] 10
    , generatePolyCylinder [(-4.397,-34.60),(3.2405,-34.60),(-0.462,-28.35),(1.6202,-28.12),(-1.157,-20.71),(0.4629,-20.94),(-5.555,-9.605),(-11.57,-20.71),(-9.952,-20.71),(-12.96,-26.50),(-11.57,-27.66),(-13.42,-34.37),(-6.712,-34.14),(-4.166,-33.44),(-3.934,-33.90),(-3.471,-33.90)] 10
    , generateEllipsoid 25 10 15
    , generateEllipsoid 10 10 40 
    , generatePolyCylinder [(-22.92,36.396),(2.6713,36.396),(3.1165,40.626),(-3.561,40.403),(-3.339,42.852),(-4.229,42.629),(-4.229,43.297),(-6.010,43.297),(-6.010,45.300),(-7.568,44.855),(-7.568,45.523),(-13.13,45.746),(-13.57,45.078),(-14.46,44.855),(-14.91,43.297),(-16.02,43.52),(-15.80,41.293),(-17.14,41.516),(-17.36,39.958),(-23.81,40.403),(-23.59,36.841),(-22.92,36.396)] 5.1
    , generatePolyCylinder [(-21.37,13.022),(-21.81,0.5565),(-20.70,0.7791),(-21.14,-0.111),(-19.36,0.1113),(-19.14,-1.001),(-17.58,-1.224),(-0.890,-0.779),(-1.113,0.3339),(0.2226,0.7791),(-0.222,1.4469),(0.4452,1.2243),(0.8904,13.022),(-21.37,13.022)] 5 
    , generatePolyCylinder [(-16.02,12.577),(-22.48,12.8),(-22.70,16.584),(-21.81,16.361),(-21.59,18.142),(-20.92,18.587),(-20.92,20.146),(-20.25,19.923),(-20.25,21.704),(-18.69,21.704),(-18.69,23.707),(-15.58,23.485),(-3.784,23.930),(-3.561,23.04),(-2.671,23.04),(-2.671,22.594),(-1.113,22.372),(-1.558,20.591),(-0.445,21.036),(-0.445,19.033),(1.1130,18.810),(0.8904,17.474),(1.5582,17.697),(1.5582,14.358),(1.7808,13.245),(-16.02,12.577)] 5
    , generatePolyCylinder [(-13.80,23.485),(-15.36,23.930),(-15.36,14.580),(-16.47,14.580),(-15.80,12.8),(-17.80,12.8),(-17.80,-15.47),(-12.02,-15.91),(-11.13,-7.457),(-10.01,-7.012),(-10.46,-5.676),(-9.349,-5.676),(-9.572,-9.906),(-8.459,-9.906),(-8.459,-14.58),(-2.003,-15.02),(-2.226,-3.005),(-2.893,-3.005),(-3.339,13.245),(-4.452,13.022),(-4.452,13.913),(-5.342,13.913),(-5.12,15.693),(-5.787,15.471),(-5.787,23.262),(-7.123,23.485),(-7.791,15.471),(-13.80,15.471),(-13.80,23.485)] 5.5
    , generatePolyCylinder  [(-10.90,-15.24),(-18.25,-15.69),(-18.69,-22.59),(-19.81,-22.59),(-20.03,-24.59),(-11.13,-25.48),(-10.90,-15.24)] 5
    , generatePolyCylinder [(-8.459,-14.58),(-2.448,-14.35),(-2.003,-22.59),(-1.113,-22.59),(-0.890,-25.04),(-8.236,-25.48),(-8.459,-14.58)] 5
    , generatePolyCylinder [(-16.02,35.506),(-16.47,32.612),(-18.03,32.834),(-17.58,30.163),(-16.25,30.386),(-16.25,26.379),(-15.36,26.156),(-15.13,25.266),(-14.02,25.266),(-13.80,23.707),(-7.123,23.485),(-7.346,24.598),(-5.565,24.598),(-5.565,25.488),(-4.229,25.266),(-4.452,30.608),(-2.226,29.940),(-2.671,32.834),(-4.006,32.612),(-4.006,35.728),(-16.02,35.506)] 5
    , generatePolyCylinder  [(-11.13,-9.906),(-18.69,-10.35),(-19.14,-7.457),(-21.59,-7.457),(-21.14,-4.786),(-23.81,-4.786),(-24.04,-2.337),(-27.38,-2.114),(-26.49,3.0052),(-29.38,2.7826),(-29.60,10.796),(-31.83,10.796),(-32.05,15.916),(-36.50,15.916),(-36.73,13.467),(-39.17,13.022),(-39.40,16.139),(-42.07,16.139),(-42.51,18.587),(-44.74,18.365),(-45.18,23.930),(-42.51,23.930),(-42.51,26.824),(-34.72,26.601),(-34.72,29.273),(-27.38,28.382),(-27.60,31.721),(-21.81,31.944),(-21.59,34.838),(-3.561,34.393),(-3.561,37.732),(-1.113,37.509),(-1.113,39.958),(0.6678,40.403),(1.3356,42.406),(6.9008,42.852),(7.3460,39.513),(10.017,39.958),(10.24,37.954),(7.3460,37.064),(7.1234,31.944),(4.4521,31.499),(4.4521,25.933),(38.288,26.379),(38.511,23.707),(40.737,24.153),(40.96,21.036),(43.186,21.481),(42.963,17.92),(45.634,18.365),(45.857,15.693),(50.977,16.584),(51.422,26.601),(48.306,26.824),(48.306,29.273),(46.08,29.495),(45.857,34.615),(48.751,34.170),(48.083,37.064),(54.539,37.286),(54.316,34.838),(56.987,34.393),(56.765,31.944),(59.213,32.166),(59.213,29.273),(56.542,29.718),(56.32,26.601),(54.093,26.379),(53.871,16.584),(51.2,16.139),(50.977,14.358),(48.528,13.913),(48.751,-7.012),(45.634,-7.234),(45.857,-15.02),(44.076,-15.02),(43.408,-20.59),(40.514,-20.81),(40.514,-23.04),(33.168,-23.04),(32.723,-17.69),(30.497,-17.47),(30.274,-20.59),(27.826,-19.92),(28.048,-22.59),(20.257,-22.81),(20.257,-13.02),(15.137,-12.35),(14.914,-15.02),(12.466,-14.80),(12.466,-19.70),(10.017,-19.92),(10.017,-22.81),(-0.890,-23.04),(-0.445,-18.14),(1.7808,-17.92),(1.7808,-12.35),(-0.667,-12.35),(-0.890,-14.80),(-2.893,-14.58),(-3.339,-19.92),(-6.010,-20.14),(-6.233,-23.04),(-13.80,-22.81),(-13.80,-17.47),(-11.13,-17.69),(-11.13,-9.906)] 5 
    , generatePolyCylinder [(4.2295,18.365),(1.7808,18.587),(2.2260,21.481),(-11.13,21.481),(-10.90,23.930),(-14.46,24.153),(-13.57,26.601),(-18.47,26.156),(-18.92,31.721),(4.6747,31.499),(4.2295,26.601),(1.7808,26.824),(1.5582,23.707),(4.0069,23.262),(4.2295,18.365)] 5.01
    , generatePolyCylinder [(4.4521,23.930),(6.9008,23.707),(7.1234,21.481),(8.9043,21.926),(9.7947,8.5704),(12.020,8.5704),(11.575,6.1217),(14.469,6.1217),(14.469,3.8956),(19.812,3.4504),(19.366,5.8991),(22.706,5.8991),(22.483,8.5704),(25.377,8.3478),(25.154,10.796),(27.158,10.796),(27.826,8.7930),(30.497,8.7930),(30.274,6.1217),(32.946,6.1217),(32.946,3.6730),(38.066,3.2278),(38.288,6.1217),(40.96,5.6765),(40.96,8.3478),(43.408,8.1252),(43.408,13.690),(42.963,17.92),(43.186,21.036),(40.737,21.036),(40.737,23.930),(38.288,23.930),(38.511,26.824),(4.6747,26.379),(4.4521,23.930)] 5.02
    , generatePolyCylinder [(-21.59,0.7791),(-21.81,3.6730),(-19.14,3.6730),(-19.58,6.1217),(-14.24,5.6765),(-14.69,8.5704),(-6.233,8.5704),(-6.010,11.686),(2.0034,11.019),(2.2260,9.2382),(4.0069,9.0156),(4.4521,-1.446),(1.5582,-1.224),(1.7808,-4.786),(-0.667,-4.340),(-0.890,-7.457),(-8.904,-7.234),(-19.14,-7.457),(-18.69,-5.008),(-21.14,-4.786),(-21.59,0.7791)] 5.03
    , generatePolyCylinder [(-34.72,29.495),(-34.72,36.841),(-32.27,36.841),(-32.05,39.958),(-29.60,39.958),(-29.16,36.619),(-26.71,36.841),(-26.49,31.499),(-27.82,31.499),(-27.15,28.16),(-34.50,28.605),(-34.72,29.495)] 5.04
    , generatePolyCylinder [(-16.69,34.393),(-16.69,42.406),(-14.02,42.406),(-13.80,44.855),(-11.35,45.078),(-10.90,42.629),(-8.681,42.184),(-8.904,40.626),(-5.565,39.735),(-5.787,34.838),(-16.69,34.393)] 5.05 
    , generatePolyCylinder [(-11.13,-2.337),(-16.02,-2.114),(-16.02,-3.895),(-10.90,-4.563),(-11.13,-2.337)] 5.06 
    , generatePolyCylinder [(-0.445,1.2243),(-1.113,3.4504),(-5.342,3.0052),(-5.787,1.0017),(-0.445,1.2243)] 5.07 
    , generatePolyCylinder [(6.9008,-18.14),(-0.445,-17.92),(-1.113,-21.48),(9.5721,-22.37),(9.3495,-20.14),(7.1234,-19.92),(6.9008,-18.14)] 5.08
    , generatePolyCylinder [(-3.784,-17.25),(-12.91,-17.92),(-14.46,-21.70),(-6.233,-22.14),(-6.010,-20.36),(-3.561,-20.36),(-3.784,-17.25)] 5.09
    , generatePolyCylinder [(20.48,-18.58),(30.052,-17.92),(29.829,-20.59),(27.603,-20.36),(27.158,-22.14),(20.257,-22.81),(20.48,-18.58)] 5.10
    , generatePolyCylinder [(43.408,-17.92),(43.631,-19.70),(40.737,-19.92),(41.182,-22.37),(32.946,-22.81),(32.946,-17.92),(43.408,-17.92)] 5.11 
    , generatePolyCylinder  [(-18.92,13.690),(-18.92,8.3478),(-24.26,8.1252),(-24.26,14.358),(-18.92,13.690)] 5.12
    , generatePolyCylinder [(-8.681,19.033),(-3.339,18.810),(-3.561,14.135),(-7.791,13.913),(-8.681,19.033)] 5.13
    , generatePolyCone [(-20,15),(20,15),(20,-15),(-20,-15),(-20,15)] (0, 0, 40) 
    , generatePolyCone [(-0.838,39.406),(-33.95,20.960),(-34.16,-17.18),(-1.048,-36.47),(32.279,-17.18),(32.279,20.751),(-0.838,39.196)] (0, 0, 90)
    , generatePolyCylinder  [(-0.838,39.406),(-33.95,20.960),(-34.16,-17.18),(-1.048,-36.47),(32.279,-17.18),(32.279,20.751),(-0.838,39.196)] 30 
    , generatePolyCylinder [(-36.50,-39.73),(-42.51,-39.73),(-42.74,-37.28),(-48.52,-37.28),(-48.97,-41.51),(-54.98,-41.51),(-55.20,-35.50),(-57.43,-35.28),(-57.43,-29.27),(-59.65,-29.27),(-59.65,-24.82),(-61.21,-25.04),(-61.44,-8.570),(-59.43,-8.570),(-59.88,-6.344),(-57.43,-6.344),(-57.65,-3.895),(-55.87,-4.118),(-55.20,2.3373),(-57.65,1.8921),(-58.10,5.8991),(-59.65,6.1217),(-59.43,19.478),(-57.43,19.033),(-57.65,21.481),(-55.65,21.036),(-55.65,23.707),(-53.20,23.262),(-53.20,25.043),(-56.54,25.043),(-56.76,27.046),(-59.43,27.269),(-59.65,29.050),(-61.66,29.273),(-61.21,37.286),(-59.21,37.064),(-59.43,39.513),(-57.65,39.513),(-57.43,41.961),(-55.87,41.739),(-55.42,43.742),(-53.42,43.742),(-53.64,46.191),(-44.74,46.191),(-44.96,43.742),(-42.51,43.742),(-42.29,41.516),(-36.50,41.739),(-36.28,39.735),(-24.26,39.735),(-24.04,37.732),(-11.79,37.286),(-12.02,39.958),(-5.12,39.290),(-5.342,37.954),(-3.561,37.954),(-2.893,33.947),(-1.335,33.725),(-1.113,31.944),(0.2226,31.944),(1.3356,20.813),(-0.890,21.036),(-0.667,19.033),(-4.897,18.587),(-4.897,-1.001),(-7.346,-0.333),(-7.346,-3.673),(-9.349,-3.895),(-9.349,-6.121),(-11.79,-6.344),(-12.24,-8.570),(-14.02,-8.570),(-14.24,-11.90),(-11.57,-12.35),(-11.35,-26.82),(-13.80,-27.93),(-13.35,-30.38),(-16.25,-30.38),(-16.02,-36.61),(-17.36,-37.06),(-17.80,-39.51),(-24.04,-39.73),(-24.04,-41.73),(-26.71,-41.51),(-26.26,-43.52),(-28.27,-43.52),(-28.27,-47.52),(-36.28,-47.74),(-36.50,-39.73)] 5
    , generatePolyCylinder [(-28.04,0.1113),(-36.28,-0.333),(-36.50,1.8921),(-42.51,1.8921),(-42.74,4.3408),(-44.96,4.3408),(-44.74,10.351),(-42.51,10.351),(-43.18,12.8),(-28.04,12.132),(-28.27,10.128),(-26.49,10.128),(-25.82,1.8921),(-28.04,2.1147),(-28.04,0.1113)] 5.01
    , generatePolyCylinder [(-46.97,16.584),(-46.97,18.810),(-50.97,18.587),(-51.2,16.361),(-53.20,16.361),(-53.42,12.8),(-50.97,12.8),(-51.2,10.796),(-46.74,10.128),(-46.97,12.577),(-44.29,12.577),(-44.96,16.584),(-46.97,16.584)] 5.02
    , generatePolyCylinder [(-21.81,5.8991),(-23.81,6.1217),(-23.81,10.351),(-22.48,10.351),(-21.81,12.577),(-18.25,12.354),(-17.80,10.796),(-16.02,10.796),(-15.80,6.5669),(-17.80,6.5669),(-18.03,4.3408),(-21.14,4.3408),(-21.81,5.8991)]5.03
    , generatePolyCylinder [(-18.03,-39.06),(-15.36,-39.73),(-16.02,-44.63),(-18.25,-45.96),(-19.58,-42.18),(-17.58,-40.84),(-18.69,-39.51),(-18.03,-39.06)] 5.04
    , generatePolyCylinder [(-31.83,9.6834),(-34.28,9.6834),(-34.50,3.2278),(-32.50,3.4504),(-31.83,9.6834)] 5.05
    , generatePolyCylinder [(-37.84,9.6834),(-40.06,9.6834),(-40.29,4.1182),(-38.06,4.3408),(-37.84,9.6834)] 5.06
    , generatePolyCylinder [(-17.80,10.351),(-19.36,10.573),(-19.36,8.7930),(-18.03,9.0156),(-17.80,10.351)]5.07
    , generatePolyCylinder [(-17.80,10.351),(-19.36,10.573),(-19.36,8.7930),(-18.03,9.0156),(-17.80,10.351)] 5.08
    , generatePolyCylinder [(8.6817,10.351),(-2.448,10.128),(-2.671,11.686),(-4.452,11.909),(-4.452,13.467),(-6.455,13.913),(-6.233,17.252),(-8.236,17.697),(-8.236,20.146),(-19.36,19.700),(-19.58,8.3478),(-17.36,8.1252),(-17.80,5.0086),(-16.02,4.7860),(-15.80,0.7791),(-13.80,0.7791),(-14.02,-4.563),(-11.79,-4.563),(-12.02,-6.789),(-9.794,-7.234),(-10.24,-7.902),(-6.678,-8.570),(-6.233,-10.35),(-4.452,-10.57),(-4.452,-12.35),(6.9008,-12.8),(6.9008,-11.01),(10.017,-11.01),(10.685,-8.793),(12.466,-8.570),(12.243,-7.012),(14.024,-7.234),(14.469,-5.008),(15.805,-4.786),(16.027,0.5565),(18.253,1.2243),(18.031,6.3443),(20.034,6.1217),(20.034,13.690),(18.253,13.690),(17.808,16.361),(16.027,15.916),(16.027,13.913),(14.246,14.135),(14.246,12.132),(12.466,11.909),(12.020,10.351),(10.685,10.573),(10.462,8.5704),(8.4591,8.7930),(8.6817,10.351)] 5
    , generatePolyCylinder [(-19.14,9.6834),(-19.58,19.033),(-20.92,19.255),(-21.14,17.252),(-22.48,17.252),(-22.48,15.471),(-25.15,15.693),(-25.37,14.135),(-23.37,14.135),(-23.37,11.909),(-21.81,12.132),(-21.37,10.573),(-19.14,9.6834)]5.01
    , generatePolyCylinder [(-9.794,19.478),(-9.794,21.259),(-12.24,21.481),(-12.02,25.266),(-13.80,25.266),(-14.02,23.485),(-15.36,23.485),(-15.58,25.266),(-17.14,25.043),(-17.80,23.930),(-19.36,23.262),(-19.14,20.368),(-9.794,19.478)]5.02
    , generatePolyCylinder [(-15.36,15.916),(-17.58,15.693),(-17.14,17.474),(-15.58,17.474),(-15.36,15.916)] 5.03
    , generateEllipsoid 10 10 5
    , generateEllipsoid 15 15 5
    , generateEllipsoid 20 20 5
    , generateEllipsoid  25 25 5
    , generateEllipsoid 10 10 5
    , generateEllipsoid 15 15 5
    , generateEllipsoid 3 5 8
    , generateEllipsoid 25 17 15
    , generatePolyCylinder [(4.3146,39.550),(5.4651,38.687),(5.4651,38.687),(5.7528,38.687),(5.7528,38.687),(6.6157,38.112),(6.6157,38.112),(6.6157,38.112),(8.0539,36.961),(8.0539,36.961),(9.7797,36.386),(9.7797,36.386),(12.368,35.811),(12.368,35.811),(12.368,35.811),(18.696,36.098),(18.696,36.098),(25.887,39.550),(25.887,39.550),(29.914,42.426),(29.914,42.426),(50.912,-13.37),(50.912,-13.37),(43.433,-17.97),(25.887,-22.57),(16.107,-22.57),(16.107,-22.57),(4.3146,39.550)] (5)
    , generatePolyCylinder  [(-37.10,18.552),(12.080,19.703),(-10.06,2.7325),(-37.10,18.552)] (20)
    , generatePolyCylinder [(-29.33,14.813),(5.4651,15.676),(10.930,19.991),(-36.53,18.840),(-29.33,14.813)] (20)
    , generatePolyCylinder [(-17.83,8.4853),(-4.889,8.4853),(-10.35,3.8831),(-17.83,8.4853)] (20)
    , generatePolyCone [(-10,-10),(10,10),(10,-10),(-10,-10)] (0,0,10)
 
    ]
    ++
    [generateEllipsoid 1000 1000 150
      , generatePolyCylinder [(-3.240,-7.059),(-5.323,-7.291),(-7.869,-7.291),(-11.34,-7.059),(-12.73,-6.365),(-14.35,-4.050),(-13.88,-2.661),(-12.73,-1.273),(-12.26,-0.115),(-13.19,1.5045),(-13.19,1.7359),(-12.96,2.1989),(-11.11,5.4394),(-9.490,6.3652),(-7.638,7.5226),(-6.018,6.8282),(-4.629,6.5967),(-3.471,6.8282),(0.2314,10.763),(3.2405,11.457),(4.3978,11.226),(5.0922,9.1428),(5.5551,8.2169),(7.6383,6.5967),(9.2585,6.5967),(10.878,6.5967),(11.110,5.9023),(12.730,4.2820),(15.276,2.4303),(15.276,0.5786),(16.433,-2.661),(16.202,-4.976),(14.582,-8.448),(11.804,-9.605),(10.647,-9.837),(9.9529,-9.374),(9.0271,-8.448),(6.7124,-7.522),(3.4719,-7.059),(2.7775,-8.216),(2.3146,-8.216),(0.2314,-8.216),(-0.925,-8.679),(-1.851,-8.216),(-1.851,-7.291),(-2.083,-6.596),(-3.934,-6.365),(-3.240,-7.059)] 10
    , generatePolyCylinder [(-23.60,24.188),(-22.45,25.576),(-21.06,26.965),(-20.13,26.965),(-18.05,26.965),(-17.35,26.271),(-16.43,26.502),(-15.04,26.965),(-14.35,26.271),(-13.88,24.882),(-13.65,24.419),(-13.19,24.188),(-10.41,19.558),(-2.083,-2.430),(-1.388,-6.828),(-1.620,-16.08),(-1.388,-17.47),(-4.397,-26.96),(-6.249,-26.73),(-14.81,-30.90),(-22.22,-29.51),(-30.32,-26.96),(-33.79,-24.65),(-35.64,-20.71),(-37.03,-14.92),(-38.65,-11.22),(-36.80,-9.374),(-36.10,-5.902),(-35.87,-2.430),(-35.41,0.1157),(-34.48,1.9674),(-33.33,4.0506),(-32.40,7.0596),(-31.24,10.068),(-24.30,24.882),(-23.60,26.039),(-21.52,26.271),(-23.60,24.188)] 10
    , generatePolyCylinder  [(-4.629,-34.83),(-4.860,-42.47),(-6.249,-42.93),(-6.249,-35.06),(-4.629,-34.83)] 10
    , generatePolyCylinder [(-4.397,-34.60),(3.2405,-34.60),(-0.462,-28.35),(1.6202,-28.12),(-1.157,-20.71),(0.4629,-20.94),(-5.555,-9.605),(-11.57,-20.71),(-9.952,-20.71),(-12.96,-26.50),(-11.57,-27.66),(-13.42,-34.37),(-6.712,-34.14),(-4.166,-33.44),(-3.934,-33.90),(-3.471,-33.90)] 10
    , generateEllipsoid 25 10 15
    , generateEllipsoid 10 10 40 
    , generatePolyCylinder [(-22.92,36.396),(2.6713,36.396),(3.1165,40.626),(-3.561,40.403),(-3.339,42.852),(-4.229,42.629),(-4.229,43.297),(-6.010,43.297),(-6.010,45.300),(-7.568,44.855),(-7.568,45.523),(-13.13,45.746),(-13.57,45.078),(-14.46,44.855),(-14.91,43.297),(-16.02,43.52),(-15.80,41.293),(-17.14,41.516),(-17.36,39.958),(-23.81,40.403),(-23.59,36.841),(-22.92,36.396)] 5.1
    , generatePolyCylinder [(-21.37,13.022),(-21.81,0.5565),(-20.70,0.7791),(-21.14,-0.111),(-19.36,0.1113),(-19.14,-1.001),(-17.58,-1.224),(-0.890,-0.779),(-1.113,0.3339),(0.2226,0.7791),(-0.222,1.4469),(0.4452,1.2243),(0.8904,13.022),(-21.37,13.022)] 5 
    , generatePolyCylinder [(-16.02,12.577),(-22.48,12.8),(-22.70,16.584),(-21.81,16.361),(-21.59,18.142),(-20.92,18.587),(-20.92,20.146),(-20.25,19.923),(-20.25,21.704),(-18.69,21.704),(-18.69,23.707),(-15.58,23.485),(-3.784,23.930),(-3.561,23.04),(-2.671,23.04),(-2.671,22.594),(-1.113,22.372),(-1.558,20.591),(-0.445,21.036),(-0.445,19.033),(1.1130,18.810),(0.8904,17.474),(1.5582,17.697),(1.5582,14.358),(1.7808,13.245),(-16.02,12.577)] 5
    , generatePolyCylinder [(-13.80,23.485),(-15.36,23.930),(-15.36,14.580),(-16.47,14.580),(-15.80,12.8),(-17.80,12.8),(-17.80,-15.47),(-12.02,-15.91),(-11.13,-7.457),(-10.01,-7.012),(-10.46,-5.676),(-9.349,-5.676),(-9.572,-9.906),(-8.459,-9.906),(-8.459,-14.58),(-2.003,-15.02),(-2.226,-3.005),(-2.893,-3.005),(-3.339,13.245),(-4.452,13.022),(-4.452,13.913),(-5.342,13.913),(-5.12,15.693),(-5.787,15.471),(-5.787,23.262),(-7.123,23.485),(-7.791,15.471),(-13.80,15.471),(-13.80,23.485)] 5.5
    , generatePolyCylinder  [(-10.90,-15.24),(-18.25,-15.69),(-18.69,-22.59),(-19.81,-22.59),(-20.03,-24.59),(-11.13,-25.48),(-10.90,-15.24)] 5
    , generatePolyCylinder [(-8.459,-14.58),(-2.448,-14.35),(-2.003,-22.59),(-1.113,-22.59),(-0.890,-25.04),(-8.236,-25.48),(-8.459,-14.58)] 5
    , generatePolyCylinder [(-16.02,35.506),(-16.47,32.612),(-18.03,32.834),(-17.58,30.163),(-16.25,30.386),(-16.25,26.379),(-15.36,26.156),(-15.13,25.266),(-14.02,25.266),(-13.80,23.707),(-7.123,23.485),(-7.346,24.598),(-5.565,24.598),(-5.565,25.488),(-4.229,25.266),(-4.452,30.608),(-2.226,29.940),(-2.671,32.834),(-4.006,32.612),(-4.006,35.728),(-16.02,35.506)] 5
    , generatePolyCylinder  [(-11.13,-9.906),(-18.69,-10.35),(-19.14,-7.457),(-21.59,-7.457),(-21.14,-4.786),(-23.81,-4.786),(-24.04,-2.337),(-27.38,-2.114),(-26.49,3.0052),(-29.38,2.7826),(-29.60,10.796),(-31.83,10.796),(-32.05,15.916),(-36.50,15.916),(-36.73,13.467),(-39.17,13.022),(-39.40,16.139),(-42.07,16.139),(-42.51,18.587),(-44.74,18.365),(-45.18,23.930),(-42.51,23.930),(-42.51,26.824),(-34.72,26.601),(-34.72,29.273),(-27.38,28.382),(-27.60,31.721),(-21.81,31.944),(-21.59,34.838),(-3.561,34.393),(-3.561,37.732),(-1.113,37.509),(-1.113,39.958),(0.6678,40.403),(1.3356,42.406),(6.9008,42.852),(7.3460,39.513),(10.017,39.958),(10.24,37.954),(7.3460,37.064),(7.1234,31.944),(4.4521,31.499),(4.4521,25.933),(38.288,26.379),(38.511,23.707),(40.737,24.153),(40.96,21.036),(43.186,21.481),(42.963,17.92),(45.634,18.365),(45.857,15.693),(50.977,16.584),(51.422,26.601),(48.306,26.824),(48.306,29.273),(46.08,29.495),(45.857,34.615),(48.751,34.170),(48.083,37.064),(54.539,37.286),(54.316,34.838),(56.987,34.393),(56.765,31.944),(59.213,32.166),(59.213,29.273),(56.542,29.718),(56.32,26.601),(54.093,26.379),(53.871,16.584),(51.2,16.139),(50.977,14.358),(48.528,13.913),(48.751,-7.012),(45.634,-7.234),(45.857,-15.02),(44.076,-15.02),(43.408,-20.59),(40.514,-20.81),(40.514,-23.04),(33.168,-23.04),(32.723,-17.69),(30.497,-17.47),(30.274,-20.59),(27.826,-19.92),(28.048,-22.59),(20.257,-22.81),(20.257,-13.02),(15.137,-12.35),(14.914,-15.02),(12.466,-14.80),(12.466,-19.70),(10.017,-19.92),(10.017,-22.81),(-0.890,-23.04),(-0.445,-18.14),(1.7808,-17.92),(1.7808,-12.35),(-0.667,-12.35),(-0.890,-14.80),(-2.893,-14.58),(-3.339,-19.92),(-6.010,-20.14),(-6.233,-23.04),(-13.80,-22.81),(-13.80,-17.47),(-11.13,-17.69),(-11.13,-9.906)] 5 
    , generatePolyCylinder [(4.2295,18.365),(1.7808,18.587),(2.2260,21.481),(-11.13,21.481),(-10.90,23.930),(-14.46,24.153),(-13.57,26.601),(-18.47,26.156),(-18.92,31.721),(4.6747,31.499),(4.2295,26.601),(1.7808,26.824),(1.5582,23.707),(4.0069,23.262),(4.2295,18.365)] 5.01
    , generatePolyCylinder [(4.4521,23.930),(6.9008,23.707),(7.1234,21.481),(8.9043,21.926),(9.7947,8.5704),(12.020,8.5704),(11.575,6.1217),(14.469,6.1217),(14.469,3.8956),(19.812,3.4504),(19.366,5.8991),(22.706,5.8991),(22.483,8.5704),(25.377,8.3478),(25.154,10.796),(27.158,10.796),(27.826,8.7930),(30.497,8.7930),(30.274,6.1217),(32.946,6.1217),(32.946,3.6730),(38.066,3.2278),(38.288,6.1217),(40.96,5.6765),(40.96,8.3478),(43.408,8.1252),(43.408,13.690),(42.963,17.92),(43.186,21.036),(40.737,21.036),(40.737,23.930),(38.288,23.930),(38.511,26.824),(4.6747,26.379),(4.4521,23.930)] 5.02
    , generatePolyCylinder [(-21.59,0.7791),(-21.81,3.6730),(-19.14,3.6730),(-19.58,6.1217),(-14.24,5.6765),(-14.69,8.5704),(-6.233,8.5704),(-6.010,11.686),(2.0034,11.019),(2.2260,9.2382),(4.0069,9.0156),(4.4521,-1.446),(1.5582,-1.224),(1.7808,-4.786),(-0.667,-4.340),(-0.890,-7.457),(-8.904,-7.234),(-19.14,-7.457),(-18.69,-5.008),(-21.14,-4.786),(-21.59,0.7791)] 5.03
    , generatePolyCylinder [(-34.72,29.495),(-34.72,36.841),(-32.27,36.841),(-32.05,39.958),(-29.60,39.958),(-29.16,36.619),(-26.71,36.841),(-26.49,31.499),(-27.82,31.499),(-27.15,28.16),(-34.50,28.605),(-34.72,29.495)] 5.04
    , generatePolyCylinder [(-16.69,34.393),(-16.69,42.406),(-14.02,42.406),(-13.80,44.855),(-11.35,45.078),(-10.90,42.629),(-8.681,42.184),(-8.904,40.626),(-5.565,39.735),(-5.787,34.838),(-16.69,34.393)] 5.05 
    , generatePolyCylinder [(-11.13,-2.337),(-16.02,-2.114),(-16.02,-3.895),(-10.90,-4.563),(-11.13,-2.337)] 5.06 
    , generatePolyCylinder [(-0.445,1.2243),(-1.113,3.4504),(-5.342,3.0052),(-5.787,1.0017),(-0.445,1.2243)] 5.07 
    , generatePolyCylinder [(6.9008,-18.14),(-0.445,-17.92),(-1.113,-21.48),(9.5721,-22.37),(9.3495,-20.14),(7.1234,-19.92),(6.9008,-18.14)] 5.08
    , generatePolyCylinder [(-3.784,-17.25),(-12.91,-17.92),(-14.46,-21.70),(-6.233,-22.14),(-6.010,-20.36),(-3.561,-20.36),(-3.784,-17.25)] 5.09
    , generatePolyCylinder [(20.48,-18.58),(30.052,-17.92),(29.829,-20.59),(27.603,-20.36),(27.158,-22.14),(20.257,-22.81),(20.48,-18.58)] 5.10
    , generatePolyCylinder [(43.408,-17.92),(43.631,-19.70),(40.737,-19.92),(41.182,-22.37),(32.946,-22.81),(32.946,-17.92),(43.408,-17.92)] 5.11 
    , generatePolyCylinder  [(-18.92,13.690),(-18.92,8.3478),(-24.26,8.1252),(-24.26,14.358),(-18.92,13.690)] 5.12
    , generatePolyCylinder [(-8.681,19.033),(-3.339,18.810),(-3.561,14.135),(-7.791,13.913),(-8.681,19.033)] 5.13
    , generatePolyCone [(-20,15),(20,15),(20,-15),(-20,-15),(-20,15)] (0, 0, 40) 
    , generatePolyCone [(-0.838,39.406),(-33.95,20.960),(-34.16,-17.18),(-1.048,-36.47),(32.279,-17.18),(32.279,20.751),(-0.838,39.196)] (0, 0, 90)
    , generatePolyCylinder  [(-0.838,39.406),(-33.95,20.960),(-34.16,-17.18),(-1.048,-36.47),(32.279,-17.18),(32.279,20.751),(-0.838,39.196)] 30 
    , generatePolyCylinder [(-36.50,-39.73),(-42.51,-39.73),(-42.74,-37.28),(-48.52,-37.28),(-48.97,-41.51),(-54.98,-41.51),(-55.20,-35.50),(-57.43,-35.28),(-57.43,-29.27),(-59.65,-29.27),(-59.65,-24.82),(-61.21,-25.04),(-61.44,-8.570),(-59.43,-8.570),(-59.88,-6.344),(-57.43,-6.344),(-57.65,-3.895),(-55.87,-4.118),(-55.20,2.3373),(-57.65,1.8921),(-58.10,5.8991),(-59.65,6.1217),(-59.43,19.478),(-57.43,19.033),(-57.65,21.481),(-55.65,21.036),(-55.65,23.707),(-53.20,23.262),(-53.20,25.043),(-56.54,25.043),(-56.76,27.046),(-59.43,27.269),(-59.65,29.050),(-61.66,29.273),(-61.21,37.286),(-59.21,37.064),(-59.43,39.513),(-57.65,39.513),(-57.43,41.961),(-55.87,41.739),(-55.42,43.742),(-53.42,43.742),(-53.64,46.191),(-44.74,46.191),(-44.96,43.742),(-42.51,43.742),(-42.29,41.516),(-36.50,41.739),(-36.28,39.735),(-24.26,39.735),(-24.04,37.732),(-11.79,37.286),(-12.02,39.958),(-5.12,39.290),(-5.342,37.954),(-3.561,37.954),(-2.893,33.947),(-1.335,33.725),(-1.113,31.944),(0.2226,31.944),(1.3356,20.813),(-0.890,21.036),(-0.667,19.033),(-4.897,18.587),(-4.897,-1.001),(-7.346,-0.333),(-7.346,-3.673),(-9.349,-3.895),(-9.349,-6.121),(-11.79,-6.344),(-12.24,-8.570),(-14.02,-8.570),(-14.24,-11.90),(-11.57,-12.35),(-11.35,-26.82),(-13.80,-27.93),(-13.35,-30.38),(-16.25,-30.38),(-16.02,-36.61),(-17.36,-37.06),(-17.80,-39.51),(-24.04,-39.73),(-24.04,-41.73),(-26.71,-41.51),(-26.26,-43.52),(-28.27,-43.52),(-28.27,-47.52),(-36.28,-47.74),(-36.50,-39.73)] 5
    , generatePolyCylinder [(-28.04,0.1113),(-36.28,-0.333),(-36.50,1.8921),(-42.51,1.8921),(-42.74,4.3408),(-44.96,4.3408),(-44.74,10.351),(-42.51,10.351),(-43.18,12.8),(-28.04,12.132),(-28.27,10.128),(-26.49,10.128),(-25.82,1.8921),(-28.04,2.1147),(-28.04,0.1113)] 5.01
    , generatePolyCylinder [(-46.97,16.584),(-46.97,18.810),(-50.97,18.587),(-51.2,16.361),(-53.20,16.361),(-53.42,12.8),(-50.97,12.8),(-51.2,10.796),(-46.74,10.128),(-46.97,12.577),(-44.29,12.577),(-44.96,16.584),(-46.97,16.584)] 5.02
    , generatePolyCylinder [(-21.81,5.8991),(-23.81,6.1217),(-23.81,10.351),(-22.48,10.351),(-21.81,12.577),(-18.25,12.354),(-17.80,10.796),(-16.02,10.796),(-15.80,6.5669),(-17.80,6.5669),(-18.03,4.3408),(-21.14,4.3408),(-21.81,5.8991)]5.03
    , generatePolyCylinder [(-18.03,-39.06),(-15.36,-39.73),(-16.02,-44.63),(-18.25,-45.96),(-19.58,-42.18),(-17.58,-40.84),(-18.69,-39.51),(-18.03,-39.06)] 5.04
    , generatePolyCylinder [(-31.83,9.6834),(-34.28,9.6834),(-34.50,3.2278),(-32.50,3.4504),(-31.83,9.6834)] 5.05
    , generatePolyCylinder [(-37.84,9.6834),(-40.06,9.6834),(-40.29,4.1182),(-38.06,4.3408),(-37.84,9.6834)] 5.06
    , generatePolyCylinder [(-17.80,10.351),(-19.36,10.573),(-19.36,8.7930),(-18.03,9.0156),(-17.80,10.351)]5.07
    , generatePolyCylinder [(-17.80,10.351),(-19.36,10.573),(-19.36,8.7930),(-18.03,9.0156),(-17.80,10.351)] 5.08
    , generatePolyCylinder [(8.6817,10.351),(-2.448,10.128),(-2.671,11.686),(-4.452,11.909),(-4.452,13.467),(-6.455,13.913),(-6.233,17.252),(-8.236,17.697),(-8.236,20.146),(-19.36,19.700),(-19.58,8.3478),(-17.36,8.1252),(-17.80,5.0086),(-16.02,4.7860),(-15.80,0.7791),(-13.80,0.7791),(-14.02,-4.563),(-11.79,-4.563),(-12.02,-6.789),(-9.794,-7.234),(-10.24,-7.902),(-6.678,-8.570),(-6.233,-10.35),(-4.452,-10.57),(-4.452,-12.35),(6.9008,-12.8),(6.9008,-11.01),(10.017,-11.01),(10.685,-8.793),(12.466,-8.570),(12.243,-7.012),(14.024,-7.234),(14.469,-5.008),(15.805,-4.786),(16.027,0.5565),(18.253,1.2243),(18.031,6.3443),(20.034,6.1217),(20.034,13.690),(18.253,13.690),(17.808,16.361),(16.027,15.916),(16.027,13.913),(14.246,14.135),(14.246,12.132),(12.466,11.909),(12.020,10.351),(10.685,10.573),(10.462,8.5704),(8.4591,8.7930),(8.6817,10.351)] 5
    , generatePolyCylinder [(-19.14,9.6834),(-19.58,19.033),(-20.92,19.255),(-21.14,17.252),(-22.48,17.252),(-22.48,15.471),(-25.15,15.693),(-25.37,14.135),(-23.37,14.135),(-23.37,11.909),(-21.81,12.132),(-21.37,10.573),(-19.14,9.6834)]5.01
    , generatePolyCylinder [(-9.794,19.478),(-9.794,21.259),(-12.24,21.481),(-12.02,25.266),(-13.80,25.266),(-14.02,23.485),(-15.36,23.485),(-15.58,25.266),(-17.14,25.043),(-17.80,23.930),(-19.36,23.262),(-19.14,20.368),(-9.794,19.478)]5.02
    , generatePolyCylinder [(-15.36,15.916),(-17.58,15.693),(-17.14,17.474),(-15.58,17.474),(-15.36,15.916)] 5.03
    , generateEllipsoid 10 10 5
    , generateEllipsoid 15 15 5
    , generateEllipsoid 20 20 5
    , generateEllipsoid  25 25 5
    , generateEllipsoid 10 10 5
    , generateEllipsoid 15 15 5
    , generateEllipsoid 3 5 8
    , generateEllipsoid 25 17 15
    , generatePolyCylinder [(4.3146,39.550),(5.4651,38.687),(5.4651,38.687),(5.7528,38.687),(5.7528,38.687),(6.6157,38.112),(6.6157,38.112),(6.6157,38.112),(8.0539,36.961),(8.0539,36.961),(9.7797,36.386),(9.7797,36.386),(12.368,35.811),(12.368,35.811),(12.368,35.811),(18.696,36.098),(18.696,36.098),(25.887,39.550),(25.887,39.550),(29.914,42.426),(29.914,42.426),(50.912,-13.37),(50.912,-13.37),(43.433,-17.97),(25.887,-22.57),(16.107,-22.57),(16.107,-22.57),(4.3146,39.550)] (5)
    , generatePolyCylinder  [(-37.10,18.552),(12.080,19.703),(-10.06,2.7325),(-37.10,18.552)] (20)
    , generatePolyCylinder [(-29.33,14.813),(5.4651,15.676),(10.930,19.991),(-36.53,18.840),(-29.33,14.813)] (20)
    , generatePolyCylinder [(-17.83,8.4853),(-4.889,8.4853),(-10.35,3.8831),(-17.83,8.4853)] (20)
    , generateEllipsoid 90 155 15
    , generateEllipsoid 3000 1200 10]
 
-- Put all of your bee models in here. Make sure that they are all grouped properly.
myBees : List (TextureStore -> MeshStore WorldCoordinates -> Float -> Object WorldCoordinates)
myBees =
    [ defaultBee
    , emperorBee
    , vomitBee
    , bee1
    , bee22
    , bee3
    , queenBee
    , jpbee
    ]
 
overlay : Model -> List (Shape Msg)
overlay model =
    [ angleDisplay model
    , beePicker model
    , beeUI model
    ]
 
-- Use "loadTexture [name] [url]" to load in texture images from the Internet!
myTextures =
    [ loadTexture "example" "Put an image URL here!"
    ]
 
-- Usage: `svgTexture "name" "name`, where shape is any 2D shape or group
svgTextures =
    [ svgTexture "squares" squares
    ] 
 
    ++
 
    --group1
        [ 
     svgTexture "flower2_texture" flower2_texture
    , svgTexture "texture_emperorBee" texture_emperorBee
    , svgTexture "bee_texture" bee_texture
    ]
 
 
 
flower2_texture =
    group
    [
      square 50 |> filled blue
 
       , rect 50 15 |> filled darkBlue |> move (0,10)
     , rect 50 15 |> filled yellow |> move (0,20)
    ]
 
texture_emperorBee =
    group
    [
      square 50 |> filled black
       , rect 50 5 |> filled blue |> move (0,20)
       , rect 50 5 |> filled blue |> move (0,10)
       , rect 50 5 |> filled blue 
       , rect 50 5 |> filled blue |> move (0,-10)
       , rect 50 5 |> filled blue |> move (0,-20)
    ]
 
bee_texture =
    group
    [
        square 50 |> filled yellow
       , rect 50 5 |> filled black |> move (0,20)
       , rect 50 5 |> filled black |> move (0,10)
       , rect 50 5 |> filled black 
       , rect 50 5 |> filled black |> move (0,-10)
       , rect 50 5 |> filled black |> move (0,-20)
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
 
-- Here you can specify whether you want a skybox generated from your own 2D shapes, or one from pictures on the Internet
skyboxType =
    Skybox.URLSkybox textureBottom textureTop textureSide1 textureSide2 textureSide3 textureSide4
    -- Some other options (comment in the one above and comment one of these out)
    -- Skybox.GSVGSkybox False skyboxTop skyboxSides skyBoxBottom
    -- Skybox.GSVGSphericalSkybox False skyboxTop
    -- Skybox.URLSphericalSkybox "https://cschank.github.io/img/milky.jpg"
 
-- This is 50 by 50
skyboxTop : Shape msg
skyboxTop =
    group
        [ square 50 |> filled lightBlue
        , circle 10 |> filled yellow
        ]
 
-- This is 200 by 50
skyboxSides : Shape msg
skyboxSides =
    group
        [ rect 200 50 |> filled lightBlue |> move ( 0, 25 )
        , rect 200 50 |> filled green |> move ( 0, -25 )
        , triangle 10 |> filled darkGreen |> rotate (degrees -30) |> move ( 0, 5 )
        , text "abcdefghijklmnopqrstuvwxyz" |> centered |> size 16 |> filled red
        ]
 
-- This is 50 by 50
skyBoxBottom : Shape msg
skyBoxBottom =
    group
        [ square 50 |> filled green
        ]
 
-- This colour is used to create the floor. If you want custom colours, use Color.hsl or Color.rgb!
floorColour =
    Color.green
 
-- Here you can specify what images to use to create the skybox. Just replace "todo" with a link to an image. (Keep the quotes, though!)
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

flower1 (col) = group3D
       [sphere 10
       |> matte Color.yellow
       ,
       --top left
       cylinder 10 1
       |> matte col
       |>move3D (-10,5,10)
       ,
       -- middle
       cylinder 10 1
       |> matte col
       |>move3D (0,10,10)
       , 
       -- top right
       cylinder 10 1
       |> matte col
       |>move3D (10,5,10)
       ,
       -- bottom left
       cylinder 10 1
       |> matte col
       |>move3D (-8,-7,10)
       ,
       -- bottom right
       cylinder 10 1
       |> matte col
       |>move3D (8,-7,10)]
 
 
flower2 ( useTexture) = group3D
       [ sphere 10
         |> textured (useTexture "flower2_texture") 0 0
         |> move3D (0,0,10)
         ,
         --top left
       cylinder 10 1
       |> matte Color.blue
       |>move3D (-10,5,20)
       ,
       -- middle
       cylinder 10 1
       |> matte Color.blue
       |>move3D (0,10,20)
       , 
       -- top right
       cylinder 10 1
       |> matte Color.blue
       |>move3D (10,5,20)
       ,
       -- bottom left
       cylinder 10 1
       |> matte Color.blue
       |>move3D (-8,-7,20)
       ,
       -- bottom right
       cylinder 10 1
       |> matte Color.blue
       |>move3D (8,-7,20)
        ]
 
flowerthing (col) = group3D
        [cylinder 15 1
         |> matte col
         |> move3D (0,12,0)
      ,   cylinder 15 1
         |> matte col
         |> move3D (-11.4,3.7,0)
      ,  cylinder 15 1
         |> matte col
         |> move3D (-7.05,-9.7,0)
      ,  cylinder 15 1
         |> matte col
         |> move3D (7.05,-9.7,0)
      ,   cylinder 15 1
         |> matte col
         |> move3D (11.4,3.7,0)
      ,  cylinder 100 1
         |> matte Color.lightYellow                
         |> scale3D 0.1
         |> move3D (0,0,1)]
         
parkBench = group3D
    [
    box 20 10 10
    |> plastic Color.blue 1
    |> rotateY3D (degrees -90)
    |> move3D (20,30,0)
    ,
    box 20 10 10
    |> plastic Color.green 1
    |> rotateY3D (degrees -90)
    |> move3D (50,30,0)
    ,
    box 20 10 10
    |> plastic Color.yellow 1
    |> rotateY3D (degrees -90)
    |> move3D (20,-30,0)
    ,
    box 20 10 10
    |> plastic Color.red 1
    |> rotateY3D (degrees -90)
    |> move3D (50,-30,0)
    ,
    box 70 50 10
    |> plastic Color.lightBrown 1
    |> rotateZ3D (degrees 90)
    |> move3D (50,-30,20)
    ,
    box 70 60 10
    |> plastic Color.lightBrown 1
    |> rotateX3D (degrees 90)
    |> rotateZ3D (degrees 90)
    |> move3D (40,-30,20)
    ] 
    
cloud = group3D
    [
        sphere 10
        |> matte Color.white
        ,
        sphere 10
        |> matte Color.white
        |> move3D (-10,0,0)
        ,
        sphere 10
        |> matte Color.white
        |> move3D (-20,0,0)
        ,
        sphere 10
        |> matte Color.white
        |> move3D (-5,0,10)
        ,
        sphere 10
        |> matte Color.white
        |> move3D (-15,0,10)
    ] |> scale3D 20


bee2 model = group3D -- a bee model
             [
          sphere 11
        |> matte Color.yellow
        ,
           cylinder 14 1 
        |> matte (Color.rgba 0.9 0.9 0.9 0.5) 
        |> move3D ( 20, 0, 9 ) 
        |> rotateY3D (degrees (-15 * sin (10 * model.time)))
        ,  cylinder 14 1 
        |> matte (Color.rgba 0.9 0.9 0.9 0.5)        
        |> move3D ( -20, 0, 9 ) 
        |> rotateY3D (degrees (15 * sin (10 * model.time)))
             ]     
             
bee model = group3D --another bee model beacause for some reason when using the same bee model more then twice it started to glitch
             [
          sphere 11
        |> matte Color.yellow
        ,
           cylinder 14 1 
        |> matte (Color.rgba 0.9 0.9 0.9 0.5) 
        |> move3D ( 20, 0, 9 ) 
        |> rotateY3D (degrees (-15 * sin (10 * model.time)))
        ,  cylinder 14 1 
        |> matte (Color.rgba 0.9 0.9 0.9 0.5)        
        |> move3D ( -20, 0, 9 ) 
        |> rotateY3D (degrees (15 * sin (10 * model.time)))
             ]
   
hiveShort = group3D --the short hive
         [
          box 30 40 26
        |> matte Color.white
        |> move3D (3,3,0)
        ,
        box 35 45 5
        |> matte Color.brown
        |> move3D (0,0,26)
        ,
        box 32 42 1
        |> matte Color.black
        |> move3D (2,2,12)
         ] |> nameObject "beehive"
         
hiveTall = group3D --the tall hive
         [
         box 28 48 60
        |> matte Color.white
        |> move3D (3,3,0)
        ,
        box 32 52 4
        |> matte Color.brown
        |> move3D (1,1,60)
        ,
        box 30 50 1
        |> matte Color.black
        |> move3D (2,2,13)
         ,
        box 30 50 1
        |> matte Color.black
        |> move3D (2,2,30)
        ,
        box 30 50 1
        |> matte Color.black
        |> move3D (2,2,48)
        
         ] |> nameObject "beehive"
hiveSquare = group3D --the square hive
        [
        box 30 30 40
        |> matte Color.white
        |> move3D (3,3,0)
        ,
        box 36 36 5
        |> matte Color.brown
        |> move3D (0,0,40)
        ,
        box 32 32 1
        |> matte Color.black
        |> move3D (2,2,13)
         ,
        box 32 32 1
        |> matte Color.black
        |> move3D (2,2,28)

        ] |> nameObject "beehive"

      
row5 = group -- these are rows for the honeycomb pattern
          [
          ngon 6 5
           |> filled yellow
           |> move (20, 20)
            ,
           ngon 6 5
           |> filled yellow
           |> move (20, -10)
            ,
           ngon 6 5
           |> filled yellow
           |> move (20, 0)
            ,
           ngon 6 5
           |> filled yellow
           |> move (20, 10)
            ,
           ngon 6 5
           |> filled yellow
           |> move (20, -20)
          ]
          
row4 = group
        [
           ngon 6 5
           |> filled yellow
           |> move (10, 0)
           ,
           ngon 6 5
           |> filled yellow
           |> move (10, -20)
             ,
           ngon 6 5
           |> filled yellow
           |> move (10, -10)
             ,
           ngon 6 5
           |> filled yellow
           |> move (10, 20)
             ,
           ngon 6 5
           |> filled yellow
           |> move (10, 10)
        ]
        
row3 = group
          [
           ngon 6 5
           |> filled yellow
           |> move (0, -20)
             ,
           ngon 6 5
           |> filled yellow
           |> move (0, -10)
            ,
           ngon 6 5
           |> filled yellow
           |> move (0, 0)
            ,
           ngon 6 5
           |> filled yellow
           |> move (0, 10)
            ,
           ngon 6 5
           |> filled yellow
           |> move (0, 20)
          ]
          
row2 = group
        [
         ngon 6 5
           |> filled yellow
           |> move (-10, 20)
           ,
           ngon 6 5
           |> filled yellow
           |> move (-10, -20)
            ,
           ngon 6 5
           |> filled yellow
           |> move (-10, 10)
           ,
           ngon 6 5
           |> filled yellow
           |> move (-10, -10)
           ,
           ngon 6 5
           |> filled yellow
           |> move (-10, 0)
        ]
        
row1 = group
             [
           ngon 6 5
           |> filled yellow
           |> move (-20, 20)
                       ,
           ngon 6 5
           |> filled yellow
           |> move (-20, 10)
           ,
           ngon 6 5
           |> filled yellow
           |> move (-20, -20)
           ,
           ngon 6 5
           |> filled yellow
           |> move (-20, -10)
            ,
           ngon 6 5
           |> filled yellow
           |> move (-20, 0)
             ]
someHives model =  
              let
                useTexture = getTexture model.textureLoader.textures
              in group3D [
                     hiveSquare -- the square hive
                     ,
                     hiveTall -- the tall hive
                     |> move3D (-60,0,0.1)
                     ,
                     hiveShort --the short hive
                     |> move3D (-60, -70, 0.1)
                     ,
                     square3D 32 --honey comb texture on top of tall hive
                     |> textured (useTexture "honeycomb") 0 0 |> move3D (-43,16,64.5)
                      ,
                     square3D 32 --honey comb texture on top of tall hive
                     |> textured (useTexture "honeycomb") 0 0 |> move3D (-43,39,64.6)
                        ,
                     square3D 32 -- honey comb texture on top of square hive
                     |> textured (useTexture "honeycomb") 0 0 |> move3D (18,19,45.1)
                        ,
                     square3D 32 -- hoey comb texture on top of short hive
                     |> textured (useTexture "honeycomb") 0 0 |> move3D (-42.5,-42,31.5)
                             ,
                      bee model --square hive bee
                     |> move3D (130*sin model.time, 130*cos model.time, 0) |> scale3D 0.22 |> move3D (18,18,10)
                       ,
                      bee model--short hive bee left
                     |> scale3D 0.22 |> move3D (-53,-64,42) |> move3D (0,0, 10 * sin (1.8*model.time))
                          ,
                      bee2 model--short hive bee right
                     |> scale3D 0.22 |> move3D (-32,-64,42)|> move3D (0,0, 10 * sin (2.2*model.time))
                          ,
                      bee2 model--tall hive bee top
                     |> scale3D 0.22 |> move3D (-42,27,66)|> move3D (10*sin model.time, 22*cos model.time, 0)] |> nameObject "beehive"
chair = group3D
       [
       box 5 5 20
            |> matte Color.darkCharcoal
            ,
            box 5 5 20
            |> matte Color.darkCharcoal
            |> move3D (25,0,0)
            ,
            box 5 5 20
            |> matte Color.darkCharcoal
            |> move3D (0,50,0)
            ,
            box 5 5 20
            |> matte Color.darkCharcoal
            |> move3D (25,50,0)
            ,
            rectangle3D 30 58
            |> matte Color.darkOrange
            |> move3D (15,27,20.5)
            ,
            rectangle3D 30 30
            |> matte Color.darkOrange
            |> move3D (15,-29,15)
            |> rotateX3D (degrees 318)
            ]

oakTree = group3D
          [
          cylinder 10 105 |>matte Color.lightBrown|>move3D(0,0,0),
          box 30 30 20 |>matte Color.lightGreen|>move3D(0-15,0-15,0+105),
          box 60 60 20 |>matte Color.lightGreen|>move3D(0-30,0-30,0+85),
          box 90 90 20 |>matte Color.lightGreen|>move3D(0-45,0-45,0+65)
          
          
          ] |> scale3D 4
          
ashTree = group3D
          [
          cylinder 10 105 |>matte Color.darkBrown|>move3D(0,0,0),
          box 30 30 20 |>matte Color.darkGreen|>move3D(0-15,0-15,0+105),
          box 60 60 20 |>matte Color.darkGreen|>move3D(0-30,0-30,0+85),
          box 90 90 20 |>matte Color.darkGreen|>move3D(0-45,0-45,0+65)
          
          
          ] |> scale3D 4
mapleTree = group3D
          [
          cylinder 10 105 |>matte Color.lightBrown|>move3D(0,0,0),
          box 30 30 20 |>matte Color.lightOrange|>move3D(-15,-15,105),
          box 60 60 20 |>matte Color.lightOrange|>move3D(-30,-30,85),
          box 90 90 20 |>matte Color.lightOrange|>move3D(-45,-45,65)
          
           ] |> scale3D 3
beehiveTree = group3D
          [
          cylinder 10 105 |>matte Color.lightBrown|>move3D(0,0,0),
          box 30 30 20 |>matte Color.lightGreen|>move3D(0-15,0-15,0+105),
          box 60 60 20 |>matte Color.lightGreen|>move3D(0-30,0-30,0+85),
          box 90 90 20 |>matte Color.lightGreen|>move3D(0-45,0-45,0+65),
          box 30 30 20 |>matte Color.yellow|>move3D(0+10,0,0+45),
          sphere 10 |>matte Color.black|>move3D(0+25,0,0+45)
          
          ]|> scale3D 3 
          
pineTree = group3D [
      
        cylinder 10 205|>matte Color.darkBrown|>move3D(0,0,0),
        box 50 50 30|>matte Color.darkGreen|>move3D(0-25,0-25,0+105),
        box 120 120 30|>matte Color.darkGreen|>move3D(0-60,0-60,0+65),
        box 10 30 30|>matte Color.darkGreen|>move3D(0,0,0+155),
        box 30 10 30|>matte Color.darkGreen|>move3D(0,0,0+155),
        box 10 -30 30|>matte Color.darkGreen|>move3D(0,0,0+155),
        box -30 10 30|>matte Color.darkGreen|>move3D(0,0,0+155) ] |> scale3D 2 
        
beehivecottage model= 
  let
          useTexture = getTexture model.textureLoader.textures
      in group3D
          [ -- Making an ellipsoid requires you to specify the length, width, and height. You also need to pass in the model.
         hiveShort --the short hive
         |> move3D (-60, -70, 0.1)
         ,
         square3D 32 -- hoey comb texture on top of short hive
         |> textured (useTexture "honeycomb") 0 0 |> move3D (-42.5,-42,31.5)
                 ,
          bee model--short hive bee left
         |> scale3D 0.22 |> move3D (-53,-64,42) |> move3D (0,0, 10 * sin (1.8*model.time))
              ,
          bee2 model--short hive bee right
         |> scale3D 0.22 |> move3D (-32,-64,42)|> move3D (0,0, 10 * sin (2.2*model.time))
          ]
{-endeditable-}

{-extra-}

-- The default bee model that's loaded when something goes wrong.
-- You can use this as a base when making your own bees!
defaultBee textures meshes time =
    let
        useTexture = getTexture textures

        wing =
            cylinder 15 1 |> matte (Color.rgba 0.9 0.9 0.9 0.5) |> noCollide
    in
    group3D
        [ ellipsoid 20 10 10 meshes
            |> matte Color.yellow
        , wing |> move3D ( 0, 20, 0 ) |> rotateX3D (degrees (-15 * sin (10 * time))) |> move3D ( 0, 0, 8 )
        , wing |> move3D ( 0, -20, 0 ) |> rotateX3D (degrees (15 * sin (10 * time))) |> move3D ( 0, 0, 8 )
        ]
        |> move3D ( 0, 0, 20 )

type WorldCoordinates
    = WorldCoordinates

type Direction
    = Up
    | Down
    | Left
    | Right
    | Forward
    | Backward
    | RotLeft
    | RotRight
    | None

type alias TextureStore = Dict String (Material.Texture Color.Color)

-- Game Constants

-- How fast the bee moves
speed : Float
speed = 0.8

-- Degrees per tick
rotSpeed : Float
rotSpeed = 2.5

-- Collision detection is turned off for this many seconds after a collision
-- This is done because the bee will almost always still be colliding with something for
-- a few frames after being bounced away.
collisionCooldown : Float
collisionCooldown = 0.25

-- The amount of pollen that is turned into honey on each tick
honeyCreationRate : Float
honeyCreationRate = 0.015

-- Bee can carry this much pollen before having to drop it off
maxPollen : Int
maxPollen = 20

-- Time, in seconds, before the bee can collect pollen again
pollenCooldown : Float
pollenCooldown = 30

type alias Model =
    { width : Quantity Int Pixels
    , height : Quantity Int Pixels
    , time : Float
    , lastCollisionTime : Float
    , orbiting : Bool
    , azimuth : Angle
    , elevation : Angle
    , meshStore : MeshStore WorldCoordinates
    , beePos : Point3d Meters WorldCoordinates
    , camPos : Point3d Meters WorldCoordinates
    , beeRot : Angle
    , velocity : Vector3d Meters WorldCoordinates
    , rotVelocity : Float
    , dirLR : Direction
    , dirFB : Direction
    , dirUD : Direction
    , rotLR : Direction
    , bee : TextureStore -> MeshStore WorldCoordinates -> Float -> Object WorldCoordinates
    , beeModels : Array (TextureStore -> MeshStore WorldCoordinates -> Float -> Object WorldCoordinates)
    , beeIndex : Int
    , beePollen : Int
    , storedPollen : Float
    , storedHoney : Float
    , choosingBee : Bool
    , mapFlowers : List (Object WorldCoordinates)
    , flowerCollisionTimes : Dict String Float
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
    | KeyDown String
    | KeyUp String
    | VisibilityChange Browser.Events.Visibility
    | GenerateMeshes (List (GeneratedMesh WorldCoordinates))
    | InitializeFlowers
    | WidgetMsg Widget.Msg
    | Reset
    | SkyboxMsg GS.Msg
    | ChangeBee Bool
    | ToggleBeePicker
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
    ( Light.point (Light.castsShadows drawShadows) properties
    , Scene3d.sphere sphereMaterial lightsphere
    )

view : Model -> Html Msg
view model =
    let
        -- Incandescent light bulb
        ( firstLight, firstLightBall ) =
            light

        -- Rough approximation of sunlight
        sun =
            Light.directional (Light.castsShadows drawShadows)
                { direction = Direction3d.xyZ (Angle.degrees -30) (Angle.degrees -50)
                , chromaticity = Light.sunlight
                , intensity = Illuminance.lux 250
                }

        -- Add some soft lighting to fill in shadowed areas
        softLighting =
            Light.soft
                { upDirection = Direction3d.positiveZ
                , chromaticity = Light.fluorescent
                , intensityAbove = Illuminance.lux 50
                , intensityBelow = Illuminance.lux 5
                }

        -- Create a quad to act as a 'floor'
        plane =
            square3D 8000
                |> matte floorColour

        -- Define camera as usual
        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.orbitZ
                        { focalPoint = model.camPos
                        , azimuth = model.beeRot |> Quantity.plus (Angle.degrees 180)
                        , elevation = model.elevation
                        , distance = Length.meters 3
                        }
                , verticalFieldOfView = Angle.degrees 45
                }

        textures = model.textureLoader.textures

        -- Things that are always shown
        baseEntities =
            let
                rotatedBee =
                    if model.choosingBee then
                        model.bee model.textureLoader.textures model.meshStore model.time

                    else
                        model.bee model.textureLoader.textures model.meshStore model.time
                            |> rotateZ3D (Angle.inRadians model.beeRot)

                beeShape =
                    let
                        position =
                            let
                                coords =
                                    Point3d.toRecord Length.inCentimeters model.beePos
                            in
                            ( coords.x, coords.y, coords.z )
                    in
                        rotatedBee
                        |> move3D position
                        |> move3D ( 0, 0, 5 * sin model.time )

            in
            (if showLight then [firstLightBall] else []) ++
                ( Skybox.skybox
                    [ Dict.get "skyB" textures
                    , Dict.get "skyT" textures
                    , Dict.get "skyS1" textures
                    , Dict.get "skyS2" textures
                    , Dict.get "skyS3" textures
                    , Dict.get "skyS4" textures
                    ]
                    4000
                :: renderEntities [ plane, beeShape ] )
    in
    Html.div []
        [ case skyboxType of
            Skybox.GSVGSkybox debug sT sS sB ->
                Html.div [ style "position" "absolute", style "left" "0px", style "top" (String.fromInt (unwrapQ model.height) ++ "px") ]
                    [ -- Html.h1 [] [Html.text "Skybox Debug"]
                      Html.map SkyboxMsg <| GS.drawSkybox debug model.gSkyboxModel sT sS sB
                    ]

            _ ->
                Html.span [] []
        , Scene3d.custom
            { lights = Scene3d.threeLights firstLight sun softLighting
            , camera = camera
            , clipDepth = Length.centimeters 10
            , exposure = Scene3d.exposureValue 6
            , toneMapping = Scene3d.hableFilmicToneMapping
            , whiteBalance = Light.fluorescent
            , antialiasing = Scene3d.multisampling
            , dimensions = ( model.width, model.height )
            , background = Scene3d.backgroundColor Color.lightBlue
            , entities = baseEntities ++ renderEntities (myEntities model) ++ renderEntities (myFlowers model)
            }
            |> withOverlay (overlay model) model
        , Html.map GSVGTextureMsg <| GT.drawTextures False model.gTextureModel
        ]

-- Displays information about camera rotation, and allows you to reset it
angleDisplay : Model -> Shape Msg
angleDisplay model =
    group
        [ text ("azimuth: " ++ String.fromInt (round <| unwrapQ model.azimuth * 180 / pi) ++ "º")
            |> filled black
            |> move ( -(toFloat (unwrapQ model.width) / 2) + 95, toFloat (unwrapQ model.height) / 2 - 50 )
        , text ("elevation: " ++ String.fromInt (round <| unwrapQ model.elevation * 180 / pi) ++ "º")
            |> filled black
            |> move ( -(toFloat (unwrapQ model.width) / 2) + 95, toFloat (unwrapQ model.height) / 2 - 60 )
        , group
            [ roundedRect 60 40 10
                |> filled green
            , text "Reset"
                |> size 16
                |> centered
                |> filled black
                |> move ( 0, -5 )
            ]
            |> move ( -(toFloat (unwrapQ model.width) / 2) + 125, toFloat (unwrapQ model.height) / 2 - 90 )
            |> notifyTap Reset
        ]

-- Consists of the pollen and honey meters
beeUI : Model -> Shape Msg
beeUI model = 
    let
        barHeight = toFloat (unwrapQ model.height) * 0.75

        barScale = toFloat model.beePollen / toFloat maxPollen

        pollenMeter = 
            group [ roundedRect 60 barHeight 10
                    |> filled gray
                    |> move ( toFloat (unwrapQ model.width) / 2 - 125, -50 )
                  , roundedRect 60 barHeight 10
                    |> filled yellow
                    |> scaleY barScale
                    |> move ( toFloat (unwrapQ model.width) / 2 - 125, -50 )
                  , text "Pollen Meter"
                    |> sansserif
                    |> size 24
                    |> centered
                    |> filled black
                    |> move ( toFloat (unwrapQ model.width) / 2 - 125, (toFloat (unwrapQ model.height) * 0.75) / 2 - 40 )
                  , text (String.fromInt model.beePollen)
                    |> sansserif
                    |> size 24
                    |> centered
                    |> filled black
                    |> move ( toFloat (unwrapQ model.width) / 2 - 125, -58 )
                  ]

        honeyMeter = 
            group [ text ("Stored Honey: " ++ String.fromInt (round model.storedHoney) ++ " mL")
                    |> sansserif
                    |> size 24
                    |> centered
                    |> filled black
                    |> move ( toFloat (unwrapQ model.width) / 2 - 125, (toFloat (unwrapQ model.height) * 0.75) / 2 + 40 )
                  ]
    in
        group [ pollenMeter
              , honeyMeter
              ]

-- The Bee Picker allows you to swap between different bee models, defined in "myBees"
beePicker : Model -> Shape Msg
beePicker model =
    if model.choosingBee then
        group
            [ group
                [ roundedRect 200 80 10 |> filled (hsl (degrees 200) 1 0.5)
                , text "Done Choosing" |> size 24 |> centered |> filled black |> move ( 0, -8 )
                ]
                |> move ( 0, -(toFloat (unwrapQ model.height) / 2 - 100) )
                |> notifyTap ToggleBeePicker
            , group
                [ roundedRect 200 80 10 |> filled (hsl (degrees 135) 1 0.35)
                , text "Previous Bee" |> size 24 |> centered |> filled black |> move ( 0, -8 )
                ]
                |> move ( -250, -(toFloat (unwrapQ model.height) / 2 - 100) )
                |> notifyTap (ChangeBee False)
            , group
                [ roundedRect 200 80 10 |> filled (hsl (degrees 135) 1 0.35)
                , text "Next Bee" |> size 24 |> centered |> filled black |> move ( 0, -8 )
                ]
                |> move ( 250, -(toFloat (unwrapQ model.height) / 2 - 100) )
                |> notifyTap (ChangeBee True)
            ]

    else
        group
            [ roundedRect 200 80 10 |> filled (hsl (degrees 200) 1 0.5)
            , text "Change Bee Model" |> size 24 |> centered |> filled black |> move ( 0, -8 )
            ]
            |> move ( 0, -(toFloat (unwrapQ model.height) / 2 - 100) )
            |> notifyTap ToggleBeePicker

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

        -- Listen for key presses
        , Browser.Events.onKeyDown keyDecoder
        , Browser.Events.onKeyUp keyUpDecoder

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

            _ ->
                Sub.none
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
init () =
    let
        ( wModel, _ ) =
            Widget.init 0 0 "widget"


        (gSkyboxModel, gSCmd) = GS.initialModel
        (gTextureModel, gTCmd) = GT.initialModel svgTextures
    in
    ( { width = Quantity.zero
      , height = Quantity.zero
      , time = 0
      , lastCollisionTime = 0
      , orbiting = False
      , azimuth = Angle.degrees 0
      , elevation = Angle.degrees 30
      , meshStore = { generatedMeshes = Dict.empty, generatedShadows = Dict.empty }
      , widget = wModel
      , gSkyboxModel = gSkyboxModel
      , beePos = Point3d.origin
      , camPos = Point3d.origin
      , beeRot = Angle.degrees 0
      , velocity = Vector3d.zero
      , rotVelocity = 0
      , dirFB = None
      , dirLR = None
      , dirUD = None
      , rotLR = None
      , bee = defaultBee
      , beeModels = Array.fromList myBees
      , beeIndex = 0
      , beePollen = 0
      , storedPollen = 0
      , storedHoney = 0
      , choosingBee = True
      , mapFlowers = []
      , flowerCollisionTimes = Dict.empty
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
        , Task.perform (\_ -> InitializeFlowers) (Task.succeed True)
        , case skyboxType of
            Skybox.GSVGSkybox _ _ _ _ ->
                Cmd.map SkyboxMsg gSCmd

            _ ->
                Cmd.none
        , Cmd.map GSVGTextureMsg gTCmd
        -- , Task.perform (\_ -> GenerateShadows name myShadowMeshes) (Task.succeed True)
        ]
    )

update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Resize width height ->
            let
                ( wModel, wCmd ) =
                    Widget.init (toFloat <| unwrapQ width) (toFloat <| unwrapQ height) "widget"
            in
            ( { model | width = width, height = height, widget = wModel }, Cmd.map WidgetMsg wCmd )

        Tick t ->
            let
                tickRate =
                    Duration.milliseconds 1 |> Quantity.per Duration.second

                updatedTime =
                    Duration.seconds model.time |> Quantity.plus (tickRate |> Quantity.for t)

                timeAsNum =
                    Duration.inSeconds updatedTime

                -- This should only be used for collision detection
                updatedBee =
                    let
                        rotatedBee =
                            if model.choosingBee then
                                model.bee model.textureLoader.textures model.meshStore model.time

                            else
                                model.bee model.textureLoader.textures model.meshStore model.time
                                    |> rotateZ3D (Angle.inRadians model.beeRot)

                        position =
                            let
                                coords =
                                    Point3d.toRecord Length.inCentimeters model.beePos
                            in
                                ( coords.x, coords.y, coords.z )
                    in
                        rotatedBee
                        |> move3D position
                        |> move3D ( 0, 0, 5 * sin model.time )

                collided = isColliding updatedBee (myEntities model)

                collidedWithFlower = isColliding updatedBee model.mapFlowers

                collidedWithBeehive = isCollidingByName updatedBee (myEntities model) "beehive"

                collisionOnCooldown = model.time < model.lastCollisionTime + collisionCooldown

                collidingFlowers = getCollisions updatedBee model.mapFlowers

                -- This updates the last collision time of each flower that was bumped into, leaving the rest of them alone
                updatedFlowerTimes = 
                    if not collidedWithFlower then
                        model.flowerCollisionTimes
                    else
                        Dict.fromList (updatedFlowerTimesHelper collidingFlowers)  

                updatedFlowerTimesHelper flowers = 
                    case flowers of
                        [] -> []
                        flower :: rest ->
                            let
                                dictAsList = Dict.toList model.flowerCollisionTimes
                            in
                                case flower of
                                    Object attr ->
                                        List.map 
                                            ( \ (index, origTime) -> 
                                                if index == attr.name && not (pollenOnCooldown [flower]) then 
                                                    (index, timeAsNum) 
                                                else 
                                                    (index, origTime) 
                                            ) 
                                            dictAsList 
                                        ++ updatedFlowerTimesHelper rest
                                    ObjectGroup attr ->
                                        List.map 
                                            ( \ (index, origTime) -> 
                                                if index == attr.name && not (pollenOnCooldown [flower]) then 
                                                    (index, timeAsNum) 
                                                else 
                                                    (index, origTime) 
                                            ) 
                                            dictAsList 
                                        ++ updatedFlowerTimesHelper rest

                -- For simplicity, if one flower in the group that you collided with is off cooldown, then you get pollen
                pollenOnCooldown flowers = 
                    case flowers of
                        [] -> True
                        flower :: rest ->
                            let
                                flowerName =
                                    case flower of
                                        ObjectGroup attr -> attr.name
                                        Object attr -> attr.name
                                timeFromDict = Maybe.withDefault 0 (Dict.get flowerName model.flowerCollisionTimes)
                            in
                                not (timeFromDict < 1e-6) && (model.time < timeFromDict + pollenCooldown) && pollenOnCooldown rest

                updatedStoredPollen = 
                    if not collisionOnCooldown && collidedWithBeehive && model.storedPollen > honeyCreationRate then
                        model.storedPollen + toFloat model.beePollen - honeyCreationRate
                    else if not collisionOnCooldown && collidedWithBeehive then
                        model.storedPollen + toFloat model.beePollen
                    else if model.storedPollen > honeyCreationRate then
                        model.storedPollen - honeyCreationRate
                    else
                        model.storedPollen

                updatedStoredHoney = 
                    if model.storedPollen > honeyCreationRate then
                        model.storedHoney + honeyCreationRate
                    else
                        model.storedHoney

                updatedBeePollen = 
                    if not collisionOnCooldown && collidedWithBeehive then
                        0
                    else if not (pollenOnCooldown collidingFlowers) && collidedWithFlower && model.beePollen < maxPollen then
                        model.beePollen + 1
                    else
                        model.beePollen

                -- Bee moves by adding different vectors to its velocity depending on which keys are being pressed
                -- This allows for movement in more than 1 direction at a time
                updatedVelocity =
                    -- If the player is choosing bee models, the bee will stop moving
                    if model.choosingBee then
                        model.velocity
                            |> Vector3d.minus (Vector3d.scaleBy 0.1 model.velocity)
                    -- If the bee bumps into an object, it should bounce backwards a little
                    else if not collisionOnCooldown && (collided || collidedWithFlower) then
                        let
                            -- This scaling is done in order to prevent the bee from just being able
                            -- to phase through any objects by moving through them slowly.
                            -- The specific numbers were just obtained through trial and error.
                            bounceFactor =
                                -- This first check should prevent the bee from being launched into orbit in some edge cases
                                if Length.inCentimeters (Vector3d.length model.velocity) < 1e-6 then
                                    0
                                else if Length.inCentimeters (Vector3d.length model.velocity) < 3 then
                                    6 / Length.inCentimeters (Vector3d.length model.velocity)
                                else if Length.inCentimeters (Vector3d.length model.velocity) < 6 then
                                    15 / Length.inCentimeters (Vector3d.length model.velocity)
                                else if Length.inCentimeters (Vector3d.length model.velocity) < 10 then
                                    20 / Length.inCentimeters (Vector3d.length model.velocity)
                                else
                                    2.5
                        in
                            model.velocity
                            |> Vector3d.minus (Vector3d.scaleBy bounceFactor model.velocity)
                    -- Otherwise it works as normal
                    else
                        model.velocity
                            |> Vector3d.plus
                                (if not (model.dirFB == None) then
                                    Vector3d.withLength (Length.centimeters speed) (directionConverter model model.dirFB)
                                else
                                    Vector3d.zero
                                )
                            |> Vector3d.plus
                                (if not (model.dirLR == None) then
                                    Vector3d.withLength (Length.centimeters speed) (directionConverter model model.dirLR)
                                else
                                    Vector3d.zero
                                )
                            |> Vector3d.plus
                                (if not (model.dirUD == None) then
                                    if model.dirUD == Down && hitGround then
                                        Vector3d.zero
                                    else
                                        Vector3d.withLength (Length.centimeters speed) (directionConverter model model.dirUD)
                                else
                                    Vector3d.zero
                                )
                            |> Vector3d.minus (Vector3d.scaleBy 0.1 model.velocity)

                updatedBeePos =
                    if goingDown && hitGround then
                        let
                            x =
                                Vector3d.xComponent model.velocity

                            y =
                                Vector3d.yComponent model.velocity

                            velocityNoZ =
                                Vector3d.xyz x y (Length.centimeters 0)
                        in
                        model.beePos
                            |> Point3d.translateBy velocityNoZ
                    else
                        model.beePos
                            |> Point3d.translateBy model.velocity

                newBeeRot =
                    case model.rotLR of
                        RotLeft -> 
                            model.beeRot |> Quantity.plus (Angle.degrees rotSpeed)

                        RotRight ->
                            model.beeRot |> Quantity.minus (Angle.degrees rotSpeed)

                        _ -> 
                            model.beeRot

                goingDown =
                    not (Quantity.greaterThanOrEqualTo (Length.centimeters 0) (Vector3d.zComponent model.velocity))

                hitGround =
                    not (Quantity.greaterThanOrEqualTo (Length.centimeters 0) (Point3d.zCoordinate model.beePos))

            in
            ( { model
                | time = timeAsNum
                , camPos = updatedBeePos |> Point3d.translateIn Direction3d.z (Length.centimeters 30)
                , velocity = updatedVelocity
                , beePos = updatedBeePos
                , beeRot = newBeeRot
                , azimuth = newBeeRot
                , beePollen = updatedBeePollen
                , storedPollen = updatedStoredPollen
                , storedHoney = updatedStoredHoney
                , lastCollisionTime = 
                    if not collisionOnCooldown && (collided || collidedWithFlower) then
                        model.time
                    else
                        model.lastCollisionTime
                , flowerCollisionTimes = updatedFlowerTimes
              }
            , Cmd.none
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
                        model.beeRot
                            |> Quantity.minus (dx |> Quantity.at rotationRate)

                    newElevation =
                        model.elevation
                            |> Quantity.plus (dy |> Quantity.at rotationRate)
                            |> Quantity.clamp (Angle.degrees -5) (Angle.degrees 85)
                in
                ( { model
                    | orbiting = True
                    , azimuth = newAzimuth
                    , beeRot = newAzimuth
                    , elevation = newElevation
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        KeyDown key ->
            let
                dir =
                    toDirection key
            in
            case dir of
                Left ->
                    ( { model
                        | dirLR = Left
                      }
                    , Cmd.none
                    )

                Right ->
                    ( { model
                        | dirLR = Right
                      }
                    , Cmd.none
                    )

                Forward ->
                    ( { model
                        | dirFB = Forward
                      }
                    , Cmd.none
                    )

                Backward ->
                    ( { model
                        | dirFB = Backward
                      }
                    , Cmd.none
                    )

                Up ->
                    ( { model
                        | dirUD = Up
                      }
                    , Cmd.none
                    )

                Down ->
                    ( { model
                        | dirUD = Down
                      }
                    , Cmd.none
                    )

                RotLeft ->
                    ( { model
                        | rotLR = RotLeft
                      }
                    , Cmd.none
                    )

                RotRight ->
                    ( { model
                        | rotLR = RotRight
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        KeyUp key ->
            let
                dir =
                    toDirection key
            in
            case dir of
                Left ->
                    ( { model | dirLR = None }, Cmd.none )

                Right ->
                    ( { model | dirLR = None }, Cmd.none )

                Forward ->
                    ( { model | dirFB = None }, Cmd.none )

                Backward ->
                    ( { model | dirFB = None }, Cmd.none )

                Up ->
                    ( { model | dirUD = None }, Cmd.none )

                Down ->
                    ( { model | dirUD = None }, Cmd.none )

                RotLeft ->
                    ( { model | rotLR = None }, Cmd.none )

                RotRight ->
                    ( { model | rotLR = None }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GenerateMeshes generatedMeshes ->
            case generatedMeshes of
                [] ->
                    ( model, Cmd.none )

                generatedMesh :: rest ->
                    let
                        updatedMeshes =
                            Dict.insert generatedMesh.name generatedMesh.mesh model.meshStore.generatedMeshes

                        updatedShadows =
                            Dict.insert generatedMesh.name generatedMesh.shadow model.meshStore.generatedShadows

                        updatedMeshStore =
                            { generatedMeshes = updatedMeshes, generatedShadows = updatedShadows }
                    in
                    ( { model | meshStore = updatedMeshStore }, Cmd.batch [ Task.perform (\_ -> GenerateMeshes rest) (Task.succeed True) ] )

        -- This is needed for our widget
        WidgetMsg wMsg ->
            let
                ( newWModel, wCmd ) =
                    Widget.update wMsg model.widget
            in
            ( { model | widget = newWModel }, Cmd.map WidgetMsg wCmd )

        Reset ->
            ( { model | azimuth = Angle.degrees 0, elevation = Angle.degrees 30 }, Cmd.none )

        SkyboxMsg sMsg ->
            let
                (gSkyboxModel, gSCmd) = GS.update sMsg model.gSkyboxModel
            in
                ( { model | gSkyboxModel = gSkyboxModel } , Cmd.map SkyboxMsg gSCmd)

        ChangeBee increasing ->
            let
                newIndex =
                    if increasing then
                        if model.beeIndex + 1 >= Array.length model.beeModels then
                            0

                        else
                            model.beeIndex + 1

                    else if model.beeIndex - 1 < 0 then
                        Array.length model.beeModels - 1

                    else
                        model.beeIndex - 1

                arrElem =
                    Array.get newIndex model.beeModels

                newBee =
                    case arrElem of
                        Just theBee ->
                            theBee

                        Nothing ->
                            defaultBee

                -- Make sure a default bee is defined somewhere!
            in
            ( { model | beeIndex = newIndex, bee = newBee }, Cmd.none )

        ToggleBeePicker ->
            if model.choosingBee then
                ( { model | choosingBee = False }, Cmd.none )

            else
                ( { model | choosingBee = True }, Cmd.none )

        InitializeFlowers ->
            let
                listFlowers = List.indexedMap nameFlower (myFlowers model)
                nameFlower index obj = 
                    case obj of
                        Object _ ->
                            obj 
                            |> nameObject ("flower" ++ String.fromInt index)
                        ObjectGroup attr ->
                            ObjectGroup
                                { attr 
                                | subObjects = List.map (nameFlower index) attr.subObjects
                                , name = "flower" ++ String.fromInt index 
                                }
                makeCollisionTimes =
                    \ object ->
                        case object of
                            ObjectGroup attr ->
                                (attr.name, 0)
                            Object attr ->
                                (attr.name, 0)

            in
                ( { model 
                  | flowerCollisionTimes = Dict.fromList (List.map makeCollisionTimes listFlowers)
                  , mapFlowers = listFlowers
                  }
                , Cmd.none 
                )

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

mouseMoveDecoder : Decoder Msg
mouseMoveDecoder =
    Decode.map2 MouseMove
        (Decode.field "movementX" (Decode.map Pixels.pixels Decode.float))
        (Decode.field "movementY" (Decode.map Pixels.pixels Decode.float))

keyDecoder : Decoder Msg
keyDecoder =
    Decode.map KeyDown (Decode.field "key" Decode.string)

keyUpDecoder : Decoder Msg
keyUpDecoder =
    Decode.map KeyUp (Decode.field "key" Decode.string)

-- Converts a key "code" to a Direction
toDirection : String -> Direction
toDirection string =
    case string of
        "ArrowLeft" ->
            RotLeft

        "a" ->
            RotLeft

        "A" ->
            RotLeft

        "ArrowRight" ->
            RotRight

        "d" ->
            RotRight

        "D" ->
            RotRight

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

        "q" ->
            Left

        "Q" ->
            Left

        "e" ->
            Right

        "E" ->
            Right

        " " ->
            Up

        "Shift" ->
            Down

        _ ->
            None

-- Converts a Direction to a Direction3d that is relative to the bee
directionConverter : Model -> Direction -> Direction3d coords
directionConverter model dir =
    let
        forwardDir =
            Direction3d.xyZ model.beeRot (Angle.degrees 0)

        backwardDir =
            Direction3d.xyZ (model.beeRot |> Quantity.plus (Angle.degrees 180)) (Angle.degrees 0)

        rightDir =
            Direction3d.xyZ (model.beeRot |> Quantity.plus (Angle.degrees 270)) (Angle.degrees 0)

        leftDir =
            Direction3d.xyZ (model.beeRot |> Quantity.plus (Angle.degrees 90)) (Angle.degrees 0)

        upDir =
            Direction3d.xyZ model.beeRot (Angle.degrees 90)

        downDir =
            Direction3d.xyZ model.beeRot (Angle.degrees -90)
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

-- Fetch textures from from the Internet and store them
-- fetchTextures : List ( String, String ) -> Cmd Msg
-- fetchTextures textDist =
--     let
--         keyList =
--             List.map (\( key, texture ) -> key) textDist

--         textureList =
--             List.map (\( key, texture ) -> texture) textDist
--     in
--     textureList
--         |> List.map Material.load
--         -- Load the meterial, [Material.load texture, Material.load texture... ]
--         |> Task.sequence
--         -- sequence : List (Task x a) -> Task x (List a)
--         -- Transform a list of the tast to a tast
--         -- Get the result type as Task WebGL.Texture.Error (List (Texture value))
--         |> Task.andThen
--             -- andThen :
--             -- concatenate two tasks
--             (\textures ->
--                 case textures of
--                     [] ->
--                         Task.fail WebGL.Texture.LoadError

--                     textList ->
--                         Task.succeed textList
--             )
--         -- If the list is not empty let the tast succeed
--         |> Task.attempt
--             -- Attempt to update the task here
--             (\result ->
--                 case result of
--                     Ok textures ->
--                         LoadTexture
--                             (Dict.fromList
--                                 (List.map2
--                                     (\key texture -> ( key, texture ))
--                                     keyList
--                                     textures
--                                 )
--                             )

--                     Err error ->
--                         Error error
--             )

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
