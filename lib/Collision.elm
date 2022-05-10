module Collision exposing 
  ( noCollide
  , nameObject
  , getCollisions, getNonCollidingObjects
  , isColliding
  , isCollidingByName, getCollisionsByName
  )

import Angle
import Axis3d
import Array
import BoundingBox3d exposing (BoundingBox3d)
import Direction3d
import Length exposing (Meters)
import Wrapper3D exposing (..)
import Point3d exposing (Point3d)
import WebGL exposing (entity)
import Block3d
import Cone3d
import Cylinder3d
import Sphere3d
import Quantity
import Vector3d

{-| Removes the bounding box of an object, so it won't collide with anything. -}
noCollide : Object coordinates -> Object coordinates
noCollide entityBBox = 
    case entityBBox of
      ObjectGroup attributes ->
          ObjectGroup
            { attributes
            | boundingBox = None
            , subObjects = List.map noCollide attributes.subObjects
            }
      Object object -> 
          Object 
              { object 
              | shape = object.shape
              , boundingBox = None
              , approxShape = EmptyShape
              }

{-| Give a name to an `Object`, which can be useful with collision detection -}
nameObject : String -> Object coordinates -> Object coordinates
nameObject newName object =
    case object of
        ObjectGroup attributes ->
            ObjectGroup 
                { attributes 
                | name = newName
                , subObjects = List.map (nameObject newName) attributes.subObjects
                }
        Object attributes ->
            Object { attributes | name = newName }

{-| Returns a list of all objects in `entity_list` that collide with `src` -}
getCollisions : Object coordinates
           -> List (Object coordinates)
           -> List (Object coordinates)
getCollisions src entity_list = 
  case src of 
    ObjectGroup attributes ->
      case attributes.boundingBox of
        Box _ -> 
          getCollisionsHelper src entity_list
        None -> []
    Object object ->
      case object.boundingBox of 
        Box _ ->
          getCollisionsHelper src entity_list
        None -> []

getCollisionsHelper : Object coordinates
           -> List (Object coordinates)
           -> List (Object coordinates)

getCollisionsHelper src entity_list = 
  let
    srcBbox = 
      case src of
        ObjectGroup attributes ->
          case attributes.boundingBox of
            Box bBox ->
              bBox
            -- This case shouldn't be reached here since the main function already handles it
            _ ->
              BoundingBox3d.singleton Point3d.origin
        Object attributes ->
          case attributes.boundingBox of
            Box bBox ->
              bBox
            -- This case shouldn't be reached here since the main function already handles it
            _ ->
              BoundingBox3d.singleton Point3d.origin
  in
    case src of
      ObjectGroup attributes ->
        getCollisionsBetweenObjects attributes.subObjects entity_list
      Object _ ->
        List.foldl (\entity otherEntity-> 
          case entity of
            ObjectGroup attributes ->
              case attributes.boundingBox of 
                Box entity_bbox -> 
                  -- Check if two boxs are collided
                  if (BoundingBox3d.intersects srcBbox entity_bbox) then
                    otherEntity ++ getCollisionsHelper src attributes.subObjects
                  else otherEntity
                None -> otherEntity
            Object object ->
              case object.boundingBox of 
                Box entity_bbox ->
                  -- Check if two boxs are collided
                  if BoundingBox3d.intersects srcBbox entity_bbox && isCollidingPrecise src entity then
                    otherEntity ++ [Object object]
                  else otherEntity
                None -> otherEntity
            ) [] entity_list

{-| Given two lists of objects, returns only the objects from `possibleCollisions` that are colliding with objects from `sourceObjects` -}
getCollisionsBetweenObjects : List (Object coordinates) -> List (Object coordinates) -> List (Object coordinates)
getCollisionsBetweenObjects sourceObjects possibleCollisions = 
   case sourceObjects of
        [] ->
            []
        (object :: rest) ->
            getCollisions object possibleCollisions ++ getCollisionsBetweenObjects rest possibleCollisions


{-| Returns a list of all objects in `entity_list` that do not collide with `src` -}
getNonCollidingObjects : Object coordinates
           -> List (Object coordinates)
           -> List (Object coordinates)
getNonCollidingObjects src entity_list = 
  case src of 
    ObjectGroup attributes ->
      case attributes.boundingBox of
        Box _ -> 
          getNonCollidingObjectsHelper src entity_list
        None -> entity_list
    Object object ->
      case object.boundingBox of 
        Box _ ->
          getNonCollidingObjectsHelper src entity_list
        None -> entity_list

getNonCollidingObjectsHelper : Object coordinates 
           -> List (Object coordinates)
           -> List (Object coordinates)
getNonCollidingObjectsHelper src entity_list =
  let
    srcBbox = 
      case src of
        ObjectGroup attributes ->
          case attributes.boundingBox of
            Box bBox ->
              bBox
            -- This case shouldn't be reached here since the main function already handles it
            _ ->
              BoundingBox3d.singleton Point3d.origin
        Object attributes ->
          case attributes.boundingBox of
            Box bBox ->
              bBox
            -- This case shouldn't be reached here since the main function already handles it
            _ ->
              BoundingBox3d.singleton Point3d.origin
  in
    List.foldl (\entity otherEntity-> 
      case entity of
        ObjectGroup attributes ->
          case attributes.boundingBox of 
            Box entity_bbox -> 
              if (BoundingBox3d.intersects srcBbox entity_bbox) then
                otherEntity ++ getNonCollidingObjectsHelper src attributes.subObjects
              else otherEntity ++ attributes.subObjects
            None -> otherEntity
        Object object ->
          case object.boundingBox of 
            Box entity_bbox ->
              -- Check if two boxs are collided
              if (BoundingBox3d.intersects srcBbox entity_bbox && isCollidingPrecise src entity) then
                otherEntity
              else Object object :: otherEntity
            None -> otherEntity
        ) [] entity_list

{-| Checks if any objects in `entity_list` collide with `src` and returns the result, as a `Bool` -}
isColliding : Object coordinates
           -> List (Object coordinates)
           -> Bool
isColliding src entity_list = 
  (List.length (getCollisions src entity_list)) >= 1 

{-| Checks whether two objects are colliding using their approxShape, not bounding boxes. Should not be used with groups. -}
isCollidingPrecise : Object coordinates -> Object coordinates -> Bool
isCollidingPrecise a b =
  let
    foldOR xs val =
      if val then
        True
      else
        case xs of
          [] ->
            val
          (y :: ys) ->
            foldOR ys y
    movePointCm x y z (xRot, yRot, zRot) point =
      point
        |> Point3d.translateBy (Vector3d.centimeters x y z)
        |> Point3d.rotateAround Axis3d.x xRot
        |> Point3d.rotateAround Axis3d.y yRot
        |> Point3d.rotateAround Axis3d.z zRot
    getRotation obj =
      case obj of
        ObjectGroup _ ->
          (Quantity.zero, Quantity.zero, Quantity.zero)
        Object attr ->
          (attr.rotation.roll, attr.rotation.pitch, attr.rotation.yaw)
    getPoints obj =
      case obj of
        ObjectGroup _ ->
          []
        Object attr ->
          case attr.approxShape of
            EmptyShape ->
              []
            Block shape ->
              shape |> Block3d.vertices
            RingShape outer inner ->
              let
                centre = inner |> Cylinder3d.centerPoint
                innerRad = inner |> Cylinder3d.radius |> Length.inCentimeters
                outerRad = outer |> Cylinder3d.radius |> Length.inCentimeters
                r = (innerRad + outerRad) / 2
                angleDegrees = 360 / 8

                getPos n = Point3d.translateBy (Vector3d.withLength (Length.centimeters r) (Direction3d.xy (Angle.degrees (angleDegrees * n)))) centre
              in
                List.map getPos (List.range 0 7 |> List.map toFloat)
            Sphere shape ->
              let
                centre = shape |> Sphere3d.centerPoint
                radius = shape |> Sphere3d.radius |> Length.inCentimeters
              in
                [ centre
                , centre |> movePointCm radius 0 0 (getRotation obj)
                , centre |> movePointCm (-radius) 0 0 (getRotation obj)
                , centre |> movePointCm 0 radius 0 (getRotation obj)
                , centre |> movePointCm 0 (-radius) 0 (getRotation obj)
                , centre |> movePointCm 0 0 radius (getRotation obj)
                , centre |> movePointCm 0 0 (-radius) (getRotation obj)
                ]
            EllipsoidShape shape (x, y, z) ->
              let
                centre = shape |> Cylinder3d.centerPoint
              in
                [ centre
                , centre |> movePointCm x 0 0 (getRotation obj)
                , centre |> movePointCm (-x) 0 0 (getRotation obj)
                , centre |> movePointCm 0 y 0 (getRotation obj)
                , centre |> movePointCm 0 (-y) 0 (getRotation obj)
                , centre |> movePointCm 0 0 z (getRotation obj)
                , centre |> movePointCm 0 0 (-z) (getRotation obj)
                ]
            Cone shape ->
              let
                tip = shape |> Cone3d.tipPoint
                baseCentre = shape |> Cone3d.basePoint
                midPoint = baseCentre |> movePointCm 0 0 ((shape |> Cone3d.length |> Length.inCentimeters) / 2) (getRotation obj)
                r = shape |> Cone3d.radius |> Length.inCentimeters
              in
                [ tip
                , midPoint
                , baseCentre
                , baseCentre |> movePointCm r 0 0 (getRotation obj)
                , baseCentre |> movePointCm (-r) 0 0 (getRotation obj)
                , baseCentre |> movePointCm 0 r 0 (getRotation obj)
                , baseCentre |> movePointCm 0 (-r) 0 (getRotation obj)
                ]
            Cylinder shape ->
              let
                centre = shape |> Cylinder3d.centerPoint
                startCentre = shape |> Cylinder3d.startPoint
                endCentre = shape |> Cylinder3d.endPoint
                r = shape |> Cylinder3d.radius |> Length.inCentimeters
              in
                [ centre
                , centre |> movePointCm r 0 0 (getRotation obj)
                , centre |> movePointCm (-r) 0 0 (getRotation obj)
                , centre |> movePointCm 0 r 0 (getRotation obj)
                , centre |> movePointCm 0 (-r) 0 (getRotation obj)
                , startCentre
                , startCentre |> movePointCm r 0 0 (getRotation obj)
                , startCentre |> movePointCm (-r) 0 0 (getRotation obj)
                , startCentre |> movePointCm 0 r 0 (getRotation obj)
                , startCentre |> movePointCm 0 (-r) 0 (getRotation obj)
                , endCentre
                , endCentre |> movePointCm r 0 0 (getRotation obj)
                , endCentre |> movePointCm (-r) 0 0 (getRotation obj)
                , endCentre |> movePointCm 0 r 0 (getRotation obj)
                , endCentre |> movePointCm 0 (-r) 0 (getRotation obj)
                ]
  in
    case (a, b) of
      (Object aAttr, Object bAttr) ->
        case (aAttr.approxShape, bAttr.approxShape) of
          (Sphere x, Sphere y) ->
            let
              distance =
                x |> Sphere3d.centerPoint |> Point3d.distanceFrom (y |> Sphere3d.centerPoint)
            in
              distance
                |> Quantity.lessThanOrEqualTo (x |> Sphere3d.radius |> Quantity.plus (y |> Sphere3d.radius))
          _ ->
            if (getVolume a |> Quantity.greaterThan (getVolume b)) then
              case aAttr.approxShape of
                EmptyShape ->
                  False
                Block shape ->
                  foldOR (List.map ( \ p -> shape |> Block3d.contains p) (getPoints b)) False
                Sphere shape ->
                  foldOR (List.map ( \ p -> shape |> Sphere3d.contains p) (getPoints b)) False
                EllipsoidShape shape _ ->
                  foldOR (List.map ( \ p -> shape |> Cylinder3d.contains p) (getPoints b)) False
                RingShape outer inner ->
                  foldOR (List.map ( \ p -> (outer |> Cylinder3d.contains p) && not (inner |> Cylinder3d.contains p)) (getPoints b)) False
                Cone shape ->
                  foldOR (List.map ( \ p -> shape |> Cone3d.contains p) (getPoints b)) False
                Cylinder shape ->
                  foldOR (List.map ( \ p -> shape |> Cylinder3d.contains p) (getPoints b)) False
            else
              case bAttr.approxShape of
                EmptyShape ->
                  False
                Block shape ->
                  foldOR (List.map ( \ p -> shape |> Block3d.contains p) (getPoints a)) False
                Sphere shape ->
                  foldOR (List.map ( \ p -> shape |> Sphere3d.contains p) (getPoints a)) False
                EllipsoidShape shape _ ->
                  foldOR (List.map ( \ p -> shape |> Cylinder3d.contains p) (getPoints a)) False
                RingShape outer inner ->
                  foldOR (List.map ( \ p -> (outer |> Cylinder3d.contains p) && not (inner |> Cylinder3d.contains p)) (getPoints a)) False
                Cone shape ->
                  foldOR (List.map ( \ p -> shape |> Cone3d.contains p) (getPoints a)) False
                Cylinder shape ->
                  foldOR (List.map ( \ p -> shape |> Cylinder3d.contains p) (getPoints a)) False
      _ ->
        False

{-| Checks if any objects in `entity_list` called `targetName` collide with `src` and returns the result, as a `Bool` -}
isCollidingByName : Object coordinates
           -> List (Object coordinates)
           -> String
           -> Bool
isCollidingByName src entity_list targetName = 
  List.any (\collided_obj -> 
     case collided_obj of
       ObjectGroup attributes -> attributes.name == targetName
       Object object -> object.name == targetName
     )
    <| getCollisions src entity_list

{-| Returns a list of all objects named `targetName` in `entity_list` that collide with `src -}
getCollisionsByName : Object coordinates
           -> List (Object coordinates)
           -> String
           -> List (Object coordinates)
getCollisionsByName src entity_list targetName = 
  List.filter (\collided_obj -> 
     case collided_obj of
       ObjectGroup attributes -> attributes.name == targetName
       Object object -> object.name == targetName
     ) 
    <| getCollisions src entity_list
