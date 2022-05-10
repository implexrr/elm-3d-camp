module SpaceshipPart exposing (..)

import Angle exposing (Angle)
import Area exposing (Area)
import Axis3d
import Browser.Events
import Collision exposing (noCollide)
import Color exposing (red, white)
import Density exposing (Density)
import Dict exposing (Dict)
import Direction3d exposing (Direction3d)
import Duration exposing (Duration, Seconds)
import Force exposing (Force)
import Frame3d exposing (Frame3d)
import GraphicSVG exposing (Color, Shape, customFont, filled, group, move, orange, outlined, rect, solid, text, white)
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Length exposing (Length, Meters)
import Mass exposing (Kilograms, Mass)
import Physics.Body as Body exposing (Body, applyForce, applyImpulse, withDamping)
import Physics.Coordinates exposing (BodyCoordinates, WorldCoordinates)
import Physics.World as World exposing (World)
import PhysicsWrapper3D exposing (PhysicsData, autoMass, constrainBodies, customDamping, getMass, linkTo, moveBody, moveBodyTo, usePhysics, withMass, withMassU)
import Pixels exposing (Pixels)
import Point3d exposing (Point3d)
import Pressure exposing (Pascals, Pressure)
import Quantity exposing (Product, Quantity, Rate)
import Round
import Scene3d.Material as Material
import Set
import Speed exposing (MetersPerSecond, Speed)
import Vector3d
import Volume exposing (CubicMeters)
import Wrapper3D exposing (MeshStore, Object, cylinder, group3D, matte, metallic, move3D, move3DU, rotate3D, rotate3DU, textured, texturedCylinder, constantTexture)
import RocketParts exposing (engine)

type Key =
      UpArrow
    | DownArrow
    | LeftArrow
    | RightArrow
    | Spacebar
    | Key String

type alias Name = String

type Part =
    Part
    {
        position : Point3d Meters BodyCoordinates
    ,   angle : Direction3d BodyCoordinates
    ,   roll : Angle
    ,   pitch : Angle
    ,   yaw : Angle
    ,   shipPart : ShipPart
    ,   name : Name
    ,   decor : PartDecoration
    }
    | PartGroup (List Part)

partGroup = PartGroup

type alias ThrusterParameters =
    {
        fuelVelocity : Speed
    ,   fuelFlowRate : Quantity Float (Rate Kilograms Seconds)
    ,   diameter : Length
    ,   length : Length
    ,   mass : Mass
    }

type alias FuelTankParameters =
    {
        height : Length
    ,   radius : Length
    ,   fuel : Mass
    }

type alias StabilizerParameters =
    {
        stability : Float
    ,   diameter : Length
    ,   height : Length
    ,   mass : Mass
    }

type ShipPart =
      Thruster ThrusterParameters
    | FuelTank FuelTankParameters
    | Stabilizer StabilizerParameters
    | CustomPart (Object BodyCoordinates)

addThruster : Name -> Speed -> Quantity Float (Rate Kilograms Seconds) -> Length -> Length -> Mass -> (Color.Color, Float) -> Part
addThruster name fuelVelocity fuelFlowRate diameter length mass (colour,roughness) =
    Part
    {
        position = Point3d.origin
    ,   angle = Direction3d.z
    ,   roll = Quantity.zero
    ,   pitch = Quantity.zero
    ,   yaw = Quantity.zero
    ,   shipPart =
            Thruster
            { fuelVelocity = fuelVelocity
            , fuelFlowRate = fuelFlowRate
            , diameter = diameter
            , length = length
            , mass = mass
            }
    ,   name = name
    ,   decor = Painted colour roughness
    }

jetFuelDensity : Density
jetFuelDensity = Density.kilogramsPerCubicMeter 900

paintPart : Color.Color -> Part -> Part
paintPart colour p =
    case p of
        Part part ->
            Part { part | decor = Painted colour 0.3 }
        PartGroup parts ->
            PartGroup <|
                List.map (paintPart colour) parts

texturePart : Material.Texture Color.Color -> Part -> Part
texturePart texture p =
    case p of
        Part part ->
            Part { part | decor = Textured texture }
        PartGroup parts ->
            PartGroup <|
                List.map (texturePart texture) parts

addFuelTank : Name -> Length -> Length -> Float -> Part
addFuelTank name radius height percentFull =
    Part
    {
        position = Point3d.origin
    ,   angle = Direction3d.z
    ,   roll = Quantity.zero
    ,   pitch = Quantity.zero
    ,   yaw = Quantity.zero
    ,   shipPart =
            let
                fuelMassOnBoard =
                    Quantity.squared radius
                       |> Quantity.multiplyBy pi
                       |> Quantity.multiplyBy ((clamp 0 100 percentFull) / 100)
                       |> Quantity.times height
                       |> Quantity.at jetFuelDensity
            in
            FuelTank
                {
                    height = height
                ,   radius = radius
                ,   fuel = fuelMassOnBoard
                }
    , name = name
    , decor = Painted Color.orange 0.3
    }

addStabilizer : Name -> Length -> Length -> Part
addStabilizer name radius height =
    Part
    {
        position = Point3d.origin
    ,   angle = Direction3d.z
    ,   roll = Quantity.zero
    ,   pitch = Quantity.zero
    ,   yaw = Quantity.zero
    ,   shipPart =
            Stabilizer
                { stability = 0.025
                , diameter = radius |> Quantity.multiplyBy 2
                , height = height
                , mass = Mass.kilograms 10000
                }
    , name = name
    , decor = Painted Color.darkCharcoal 0.3
    }

rotatePart : Angle -> Angle -> Angle -> Part -> Part
rotatePart pitch yaw roll p =
    case p of
        Part part ->
            Part
            { part |
                angle =
                    part.angle
                    |> Direction3d.rotateAround Axis3d.x roll
                    |> Direction3d.rotateAround Axis3d.y pitch
                    |> Direction3d.rotateAround Axis3d.z yaw
                , position =
                    part.position
                    |> Point3d.rotateAround Axis3d.x roll
                    |> Point3d.rotateAround Axis3d.y pitch
                    |> Point3d.rotateAround Axis3d.z yaw
                , yaw = part.yaw |> Quantity.plus yaw
                , pitch = part.pitch |> Quantity.plus pitch
                , roll = part.roll |> Quantity.plus roll
            }
        PartGroup parts ->
            PartGroup <|
            List.map (rotatePart pitch yaw roll) parts

movePart : (Length, Length, Length) -> Part -> Part
movePart (x, y, z) p =
    case p of
        Part part ->
           Part
            { part |
                position =
                    part.position
                    |> Point3d.translateBy (Vector3d.fromTuple Length.meters (Length.inMeters x, Length.inMeters y, Length.inMeters z))
            }
        PartGroup parts ->
            PartGroup
                <| List.map (movePart (x,y,z)) parts

type alias Percent = Float

type Action =
      EnableThruster Name Percent
    | DisableThruster Name
    | EnableStabilizer Name
    | DisableStabilizer Name
    | DecouplePart Name
    | DumpFuel Name Mass
    | DoNothing
    | ManyActions (List Action)

customPart : Name -> Object BodyCoordinates -> Part
customPart name object =
    Part
    {
        position = Point3d.origin
    ,   angle = Direction3d.x
    ,   roll = Quantity.zero
    ,   pitch = Quantity.zero
    ,   yaw = Quantity.zero
    ,   shipPart =
            CustomPart object
    ,   name = name
    ,   decor = Painted Color.black 0-- not used
    }

type KeyAction =
    Released | Pressed

type alias Spaceship =
    {
        commandModule : Object BodyCoordinates
    ,   parts : List Part                           -- a list of parts in your spaceship
    ,   keyActions : Key -> KeyAction -> Action     -- a list of keys and actions they do
    }

updateSpaceship : Action -> SpaceshipState -> SpaceshipState
updateSpaceship action spaceshipState =
    case action of
        EnableThruster name percent ->
            { spaceshipState | thrusterStates = enableThruster name percent spaceshipState.thrusterStates }
        DisableThruster name ->
            { spaceshipState | thrusterStates = disableThruster name spaceshipState.thrusterStates }
        EnableStabilizer name ->
            { spaceshipState | stabilizerStates = enableStabilizer name spaceshipState.stabilizerStates }
        DisableStabilizer name ->
            { spaceshipState | stabilizerStates = disableStabilizer name spaceshipState.stabilizerStates }
        DecouplePart name ->
            spaceshipState -- doesn't work yet
        DumpFuel name mass ->
            { spaceshipState | fuelTankStates = dumpFuel name mass spaceshipState.fuelTankStates }
        DoNothing -> spaceshipState
        ManyActions actions ->
            List.foldl updateSpaceship spaceshipState actions

type alias ThrusterState =
    { name : Name
    , parameters : ThrusterParameters
    , thrustPercent : Percent
    , position : Point3d Meters BodyCoordinates
    , angle : Direction3d BodyCoordinates
    }

initThrusterState : Name -> Point3d Meters BodyCoordinates -> Direction3d BodyCoordinates -> ThrusterParameters -> ThrusterState
initThrusterState name position direction tParams =
    {
        parameters = tParams
    ,   name = name
    ,   position = position
    ,   angle = direction
    ,   thrustPercent = 0
    }

initFuelTankState : Name -> FuelTankParameters -> FuelTankState
initFuelTankState name fParams =
    {
        parameters = fParams
    ,   fuelLeft = fParams.fuel
    ,   name = name
    }

initStabilizerState : Name -> StabilizerParameters -> StabilizerState
initStabilizerState name sParams =
    {
        parameters = sParams
    ,   enabled = False
    ,   name = name
    }

type alias StabilizerState =
    { parameters : StabilizerParameters, enabled : Bool, name : Name }

type alias FuelTankState =
    { parameters : FuelTankParameters, fuelLeft : Mass, name : Name }

enableThruster :
    Name ->
    Percent ->
    Dict Name ThrusterState ->
    Dict Name ThrusterState
enableThruster name percent =
    Dict.update name
        (Maybe.map <| \th -> { th | thrustPercent = clamp 0 100 percent })

disableThruster :
    Name ->
    Dict Name ThrusterState ->
    Dict Name ThrusterState
disableThruster name =
    Dict.update name
        (Maybe.map <| \th -> { th | thrustPercent = 0 })

enableStabilizer :
    Name ->
    Dict Name StabilizerState ->
    Dict Name StabilizerState
enableStabilizer name =
    Dict.update name
        (Maybe.map <| \th -> { th | enabled = True })

disableStabilizer :
    Name ->
    Dict Name StabilizerState ->
    Dict Name StabilizerState
disableStabilizer name =
    Dict.update name
        (Maybe.map <| \th -> { th | enabled = False })

dumpFuel :
    Name ->
    Mass ->
    Dict Name FuelTankState ->
    Dict Name FuelTankState
dumpFuel name mass =
    Dict.update name
        (Maybe.map <| \th -> { th | fuelLeft = Quantity.clamp (Mass.kilograms 0) th.fuelLeft (th.fuelLeft |> Quantity.minus mass) })

spaceshipFrame : Model -> Frame3d Meters WorldCoordinates { defines : BodyCoordinates }
spaceshipFrame model =
    let
        frame = Maybe.withDefault Frame3d.atOrigin
                <| Maybe.map  Body.frame <| List.head <| World.bodies model.world
    in
        frame
--type alias Force =
--    Quantity Float (Product (Rate Kilograms Seconds) MetersPerSecond)

-- compute a thrust from a given fuel velocity and flow rate
computeThrust : Speed -> Quantity Float (Rate Kilograms Seconds) -> Force
computeThrust fuelVelocity flowRate =
    let
        speedMpS = Speed.inMetersPerSecond fuelVelocity
        rateKGpS = flowRate
                    |> Quantity.for Duration.second
                    |> Mass.inKilograms
    in
        Force.newtons <| speedMpS * rateKGpS

-- calculates a thruster's thrust as well as how much fuel is expended during a given duration of firing this thruster
calculateThrusterThrust :
    Mass ->
    Duration -> -- the duration of the tick
    ThrusterState ->   -- the parameters for the thruster
    ( ThrusterState    -- the new parameters for the thruster
    , Force                                                           -- the force produced by the thruster
    , Mass                                                            -- the mass of fuel expelled
    )
calculateThrusterThrust fuelRemaining dur th =
    let
        adjustedFlowRate =
            th.parameters.fuelFlowRate
                |> Quantity.multiplyBy (th.thrustPercent / 100)

        force =
            if fuelRemaining |> Quantity.greaterThan Quantity.zero then computeThrust th.parameters.fuelVelocity adjustedFlowRate else Quantity.zero

        fuelExpelled =
            adjustedFlowRate
                |> Quantity.for dur
    in
        ( th, force, fuelExpelled )

type alias SpaceshipState =
    {
        thrusterStates : Dict Name ThrusterState
    ,   fuelTankStates : Dict Name FuelTankState
    ,   stabilizerStates : Dict Name StabilizerState
    ,   spaceship : Spaceship
    }

type alias Model =
    {
        world : World PhysicsData
    ,   spaceshipState : SpaceshipState
    }

init : MeshStore BodyCoordinates -> Spaceship -> Model
init meshStore spaceship =
    let
        (freshSpaceshipState, freshWorld) = initSpaceshipWorld meshStore spaceship
    in
        {
            world = freshWorld
        ,   spaceshipState = freshSpaceshipState
        }

updateBodyData : (data -> data) -> Body data -> Body data
updateBodyData f body =
    body
        |> Body.withData (f <| Body.data body)

updateSpaceshipKey : Key -> KeyAction -> SpaceshipState -> SpaceshipState
updateSpaceshipKey key keyAction spaceshipState =
    spaceshipState
        |> updateSpaceship (spaceshipState.spaceship.keyActions key keyAction)

update : MeshStore BodyCoordinates -> (Action, Bool) -> Msg -> Model -> (Model, Cmd Msg)
update meshStore (autoPilotAction, useAutoPilot) msg model =
    case msg of
        Tick t ->
            let
                (newSpaceshipState, newWorld) =
                    if useAutoPilot then
                        simulateStep
                            meshStore
                            t
                            ( updateSpaceship autoPilotAction model.spaceshipState )
                            model.world
                    else
                        simulateStep meshStore t model.spaceshipState model.world
                --m = Debug.log "world" (List.map (.mass << Body.data) <| World.bodies model.world)
            in
            ( { model | world = newWorld, spaceshipState = newSpaceshipState
              } ,
            Cmd.none )
        KeyPressed k ->
            ( { model | spaceshipState = updateSpaceshipKey k Pressed model.spaceshipState }, Cmd.none )
        KeyReleased k ->
            ( { model | spaceshipState = updateSpaceshipKey k Released model.spaceshipState }, Cmd.none )

-- http://www.totalmateria.com/Article73.htm#:~:text=The%20effect%20of%20alloying%20elements,manganese%20and%20high%2Diron%20contents.
metalDensity : Density
metalDensity = Density.kilogramsPerCubicMeter 2700

fuelTankMass : FuelTankState -> Mass
fuelTankMass fts =
    let
        endArea =
            fts.parameters.radius |> Quantity.squared |> Quantity.multiplyBy pi

        circumference =
            fts.parameters.radius |> Quantity.multiplyBy (2 * pi)

        sideArea =
            circumference
                |> Quantity.times fts.parameters.height

        surfaceArea =
            endArea
                |> Quantity.multiplyBy 2
                |> Quantity.plus sideArea

        metalVolume =
            surfaceArea
                |> Quantity.times (Length.millimeter)

        metalMass =
            metalVolume
                |> Quantity.at metalDensity
    in
        metalMass
            |> Quantity.plus fts.fuelLeft

applyForces : List (Force, Point3d Meters BodyCoordinates, Direction3d BodyCoordinates) -> Body data -> Body data
applyForces forceList body =
    List.foldl (\ (f,p,d) intermBody ->
        let
            frame = Body.frame body
            globalPos = p |> Point3d.placeIn frame
            globalDir = d |> Direction3d.placeIn frame
        in
        intermBody |>
            applyForce f globalDir globalPos) body forceList

-- simulate one step
simulateStep : MeshStore BodyCoordinates -> Duration -> SpaceshipState -> World PhysicsData -> (SpaceshipState, World PhysicsData)
simulateStep meshStore duration spaceshipState world =
    let
        -- simulate the spaceship for a given amount of time
        (newSSState, forces) = simulateSpaceship duration spaceshipState

        {- This modifies the bodies in the simulation depending on how things may have changed (e.g. forces applied, or
         - fuel being used up
        -}
        modifyBodies : Body PhysicsData -> Body PhysicsData
        modifyBodies body =
            let
                data = Body.data body
                frame = Body.frame body
                globalDir dir = dir |> Direction3d.placeIn frame
                name = data.name

                -- the mass of the fuel tank may have changed
                -- if it's not a fuel tank, keep the mass the same
                --newMass = Maybe.withDefault identity
                --            <| Maybe.map (withMassU << fuelTankMass)
                --            <| Dict.get name spaceshipState.fuelTankStates

                (objects, masses) = List.unzip <| (spaceshipState.spaceship.commandModule, spaceshipState.spaceship.commandModule |> getMass metalDensity) :: List.map (createObject meshStore) spaceshipState.spaceship.parts

                spaceshipBody = group3D objects

                mass = List.foldl Quantity.plus Quantity.zero masses |> Quantity.minus (fuelCapacity spaceshipState |> Quantity.minus (fuelLeft spaceshipState))

                angularDampening = List.sum <| List.map (\ss -> ss.parameters.stability) <| List.filter (\ss -> ss.enabled) <| Dict.values spaceshipState.stabilizerStates

                -- lookup if there's a force to be applied, otherwise don't apply one
            in
                body
                    |> withMassU mass
                    |> applyForces forces
                    |> customDamping 0 angularDampening

        newWorld =
            World.constrain constrainBodies world
                |> World.update modifyBodies -- this simulates the rocket
                |> World.simulate duration   -- this simulates the movement

    in
        (newSSState, newWorld)


initSpaceshipState : Spaceship -> SpaceshipState
initSpaceshipState spaceship =
    {
        thrusterStates = Dict.empty
    ,   fuelTankStates = Dict.empty
    ,   stabilizerStates = Dict.empty
    ,   spaceship = spaceship
    }

-- https://en.wikipedia.org/wiki/Rocket_engine#Mechanical_issues
enginePressure : Pressure
enginePressure = Pressure.megapascals 10

fuelExhaustVelocity : Speed
fuelExhaustVelocity =
    Speed.metersPerSecond 1000

type PartDecoration =
    Painted Color.Color Float | Textured (Material.Texture Color.Color)

-- create a world with the spaceship
createObject : MeshStore BodyCoordinates -> Part -> (Object BodyCoordinates, Mass)
createObject meshStore p =
    case p of
        Part part ->
            case part.shipPart of
                Thruster tParams ->
                     ( engine { meshStore = meshStore } (tParams.diameter |> Quantity.divideBy 2 |> Length.inCentimeters) (tParams.length |> Length.inCentimeters)
                                    |> (case part.decor of
                                            Painted colour roughness -> metallic colour roughness
                                            Textured texture -> textured texture (constantTexture 0.3) (constantTexture 0.3)
                                       )
                                    |> rotate3DU part.pitch part.yaw part.roll
                                    |> move3D (Point3d.toTuple Length.inCentimeters part.position)
                     , tParams.mass
                     )

                FuelTank tParams ->
                     (
                        (case part.decor of
                            Painted colour roughness ->
                                cylinder (Length.inCentimeters tParams.radius) (Length.inCentimeters tParams.height)
                                    |> metallic colour roughness
                            Textured texture ->
                                texturedCylinder (Length.inCentimeters tParams.radius) (Length.inCentimeters tParams.height) meshStore
                                    |> textured texture (constantTexture 0.3) (constantTexture 1)
                        )
                                    |> rotate3DU part.pitch part.yaw part.roll
                                    |> move3D (0, 0, Length.inCentimeters tParams.height / 2)
                                    |> move3D (Point3d.toTuple Length.inCentimeters part.position)
                     , fuelTankMass <| initFuelTankState part.name tParams
                     )

                Stabilizer sParams ->
                     ( (case part.decor of
                               Painted colour roughness ->
                                   cylinder (Length.inCentimeters (sParams.diameter |> Quantity.divideBy 2)) (Length.inCentimeters sParams.height)
                                       |> metallic colour roughness
                               Textured texture ->
                                   texturedCylinder (Length.inCentimeters (sParams.diameter |> Quantity.divideBy 2)) (Length.inCentimeters sParams.height) meshStore
                                       |> textured texture (constantTexture 0.3) (constantTexture 1)
                     )
                                    |> rotate3DU part.pitch part.yaw part.roll
                                    |> move3D (Point3d.toTuple Length.inCentimeters part.position)
                     , sParams.mass
                     )

                CustomPart object ->
                     ( object
                        |> rotate3DU part.pitch part.yaw part.roll
                        |> move3D (Point3d.toTuple Length.inCentimeters part.position)
                     , getMass metalDensity object
                     )
        PartGroup parts ->
            let
                (obs,masses) = List.unzip <| List.map (createObject meshStore) parts
                totalMass = List.foldl Quantity.plus Quantity.zero masses
            in
                (group3D obs, totalMass)

initSpaceshipWorld : MeshStore BodyCoordinates -> Spaceship -> (SpaceshipState, World PhysicsData)
initSpaceshipWorld meshStore spaceship =
    let
        -- create the spaceship state
        createSpaceshipState : Part -> SpaceshipState -> SpaceshipState
        createSpaceshipState p spaceshipState =
            case p of
                Part part ->
                    case part.shipPart of
                        Thruster tParams ->
                            { spaceshipState | thrusterStates =
                                Dict.insert part.name
                                    (initThrusterState part.name part.position part.angle tParams)
                                    spaceshipState.thrusterStates
                            }
                        FuelTank tParams ->
                            { spaceshipState | fuelTankStates =
                                Dict.insert part.name
                                    (initFuelTankState part.name tParams)
                                    spaceshipState.fuelTankStates
                            }
                        Stabilizer sParams ->
                            { spaceshipState | stabilizerStates =
                                Dict.insert part.name
                                    (initStabilizerState part.name sParams)
                                    spaceshipState.stabilizerStates
                            }
                        CustomPart object ->
                            spaceshipState
                PartGroup parts ->
                    List.foldl createSpaceshipState spaceshipState parts

        freshSpaceshipState = List.foldl createSpaceshipState (initSpaceshipState spaceship) spaceship.parts


        (objects, masses) = List.unzip <| (spaceship.commandModule, spaceship.commandModule |> getMass metalDensity) :: List.map (createObject meshStore) spaceship.parts

        spaceshipBody = group3D objects

        mass = List.foldl Quantity.plus Quantity.zero masses

        freshWorld =
            World.empty
                |> World.add (spaceshipBody
                                |> usePhysics "spaceship"
                                |> withMassU mass
                                |> withDamping { linear = 0, angular = 0 }
                             )

    in
        (freshSpaceshipState, freshWorld)

-- determine the forces on the spaceship
simulateSpaceship : Duration -> SpaceshipState -> ( SpaceshipState, List ( Force, Point3d Meters BodyCoordinates , Direction3d BodyCoordinates ) )
simulateSpaceship duration spaceshipState =
    let
        thrusterCalculations = List.map (calculateThrusterThrust (fuelLeft spaceshipState) duration) (Dict.values spaceshipState.thrusterStates)
        thrusterStates = List.map (\(a,_,_) -> a) thrusterCalculations
        forces = List.map (\(tS,force,_) -> (force, tS.position, tS.angle) ) thrusterCalculations
        -- TODO: take fuel away from the tanks
        fuelMasses = List.map (\(_,_,c) -> c) thrusterCalculations
        totalFuelMass = List.foldl Quantity.plus Quantity.zero fuelMasses
    in
    (
        useUpFuel totalFuelMass spaceshipState
    ,   forces
    )

progressBar : Color -> (Float, Float) -> Float -> Float -> Shape msg
progressBar color (width, height) total left =
    let
        percent = left / total
    in
    group
        [
            rect width height
                        |> outlined (solid 1) color
                        |> move (0, 0)
        ,   rect (width * percent) 15
                        |> filled color
                        |> move (-width * (1 - percent) / 2, 0)
        ]

overlay : Quantity Int Pixels -> Quantity Int Pixels -> GraphicSVG.Color -> Model -> Shape Msg
overlay width height colour model =
    let
        mass = List.sum <| List.map (Mass.inMetricTons << .mass << Body.data) <| World.bodies model.world
        velocity = Maybe.withDefault 0 <| Maybe.map (Speed.inMetersPerSecond << Vector3d.length << Body.velocity) <| List.head <| World.bodies model.world
        distance = Maybe.withDefault 0 <| Maybe.map (Length.inKilometers << Vector3d.length << Vector3d.from Point3d.origin << Frame3d.originPoint << Body.frame) <| List.head <| World.bodies model.world
        fuelRemaining = Mass.inMetricTons <| fuelLeft model.spaceshipState
        fuelOriginalCap = Mass.inMetricTons <| fuelCapacity model.spaceshipState
        fuelPercent = fuelRemaining / fuelOriginalCap * 100
    in group
    [   text ("Mass : " ++ Round.round 2 mass ++ " tonnes") |> GraphicSVG.customFont "Audiowide" |> filled colour |> move (toFloat (Pixels.toInt width) / 2 - 250, 20)
    ,   text ("Velocity : " ++ Round.round 3 velocity ++ " m/s") |> GraphicSVG.customFont "Audiowide" |> filled colour |> move (toFloat (Pixels.toInt width) / 2 - 250, 0)
    ,   text ("Distance from space station : " ++ Round.round 3 distance ++ " km") |> GraphicSVG.customFont "Audiowide" |> filled colour |> move (toFloat (Pixels.toInt width) / 2 - 250, -20)
    ,   text ("Fuel : " ++ Round.round 2 fuelRemaining ++ " / " ++ Round.round 2 fuelOriginalCap ++ " tonnes (" ++ Round.round 2 fuelPercent ++ "%)")
            |> GraphicSVG.customFont "Audiowide"
            |> filled colour
            |> move (toFloat (Pixels.toInt width) / 2 - 250, -40)
    ,   progressBar (if fuelRemaining / fuelOriginalCap < 0.10 then GraphicSVG.red else if fuelRemaining / fuelOriginalCap < 0.20 then orange else colour) (200,15) fuelOriginalCap fuelRemaining
            |> move (toFloat (Pixels.toInt width) / 2 - 110, -60)
    ]

fuelCapacity : SpaceshipState -> Mass
fuelCapacity spaceshipState =
    let
        fuelMasses = List.map (\f -> f.parameters.fuel) (Dict.values spaceshipState.fuelTankStates)

        totalFuelMass = List.foldl Quantity.plus Quantity.zero fuelMasses
    in
        totalFuelMass

fuelLeft : SpaceshipState -> Mass
fuelLeft spaceshipState =
    let
        fuelMasses = List.map .fuelLeft (Dict.values spaceshipState.fuelTankStates)

        totalFuelMass = List.foldl Quantity.plus Quantity.zero fuelMasses
    in
        totalFuelMass

-- use up a given mass of fuel evenly amongst all the fuel tanks
useUpFuel : Mass -> SpaceshipState -> SpaceshipState
useUpFuel mass spaceshipState =
    let
        nonZeroFuelTanks =
            Set.fromList <| List.map Tuple.first
            <|
            List.filter
                (Tuple.second >> .fuelLeft >> Quantity.greaterThan Quantity.zero)
                    <| Dict.toList spaceshipState.fuelTankStates

        massPerTank =
            mass
                |> Quantity.divideBy (toFloat <| Set.size nonZeroFuelTanks)

        -- tanks that cannot provide the full massPerTank
        fractionalFuelTanks =
            Set.fromList <| List.map Tuple.first
            <|
            List.filter
                (\(_,ft) -> Quantity.lessThan massPerTank ft.fuelLeft && Quantity.greaterThan Quantity.zero ft.fuelLeft)
                    <| Dict.toList spaceshipState.fuelTankStates

        useFuelInTank : Name -> Mass -> SpaceshipState -> (SpaceshipState, Mass)
        useFuelInTank name massToUse ssState =
            let
                tankState = Dict.get name ssState.fuelTankStates
                fLeft = Maybe.withDefault Quantity.zero <| Maybe.map .fuelLeft tankState
                clampedMass = massToUse |> Quantity.clamp Quantity.zero fLeft
            in
                ({ ssState |
                    fuelTankStates = Dict.update name
                        (Maybe.map <| \ fts -> { fts | fuelLeft = fts.fuelLeft |> Quantity.minus clampedMass } ) ssState.fuelTankStates
                }, clampedMass
                )

        useFractional : (Mass, SpaceshipState)
        useFractional =
            List.foldl (\ftName (m, sss) ->
                let
                    (ssState, fUsedUp) = useFuelInTank ftName massPerTank sss
                in
                    (m |> Quantity.minus fUsedUp, ssState)
                ) (mass, spaceshipState) <| Set.toList fractionalFuelTanks

        useRest : (Mass, SpaceshipState)
        useRest =
            let
                remainingTanks = Set.diff nonZeroFuelTanks fractionalFuelTanks
            in
            List.foldl (\ftName (m, sss) ->
                let
                    (remainingMass,_) = useFractional
                    newMassPerTank = remainingMass |> Quantity.divideBy (toFloat <| Set.size remainingTanks)
                    (ssState, fUsedUp) = useFuelInTank ftName newMassPerTank sss
                in
                    (m |> Quantity.minus fUsedUp, ssState)
                ) useFractional (Set.toList remainingTanks)
    in
        Tuple.second useRest

toKey : String -> Key
toKey k =
    case k of
        " " -> Spacebar
        "ArrowLeft" -> LeftArrow
        "ArrowRight" -> RightArrow
        "ArrowUp" -> UpArrow
        "ArrowDown" -> DownArrow
        a -> Key (String.toLower a)

type Msg =
      Tick Duration
    | KeyPressed Key
    | KeyReleased Key

-- some rockets

-- https://en.wikipedia.org/wiki/SpaceX_Merlin#Merlin_1D
merlin : Name -> Part
merlin name =
    let
        fuelVelocity = Speed.metersPerSecond 3050
        fuelRate =
            Mass.kilograms 140
                |> Quantity.per (Duration.second)
        diameter = Length.meters 3.3
        length = Length.meters 3
        mass = Mass.kilograms 470
    in
    addThruster
        name fuelVelocity fuelRate diameter length mass (Color.rgb255 222 218 211, 0.5)

saturnV : Name -> Part
saturnV name =
    let
        fuelVelocity = Speed.metersPerSecond 2580
        fuelRate =
            Mass.kilograms 2607.38
                |> Quantity.per (Duration.second)
        diameter = Length.meters 3.7
        length = Length.meters 5.6
        mass = Mass.kilograms 8700
    in
    addThruster
        name fuelVelocity fuelRate diameter length mass (Color.rgb255 222 218 211, 0.5)

-- The space shuttle engine: https://en.wikipedia.org/wiki/RS-25
shuttleEngine : Name -> Part
shuttleEngine name =
    let
        fuelVelocity = Speed.metersPerSecond 4432.5
        fuelRate =
            Mass.kilograms 514.16
                |> Quantity.per (Duration.second)
        diameter = Length.meters 2.4
        length = Length.meters 4.3
        mass = Mass.kilograms 3527
    in
    addThruster
        name fuelVelocity fuelRate diameter length mass (Color.hsl 0 0 0.25, 0.3)

-- RD-191: https://en.wikipedia.org/wiki/RD-191
antaresEngine : Name -> Part
antaresEngine name =
    let
        fuelVelocity = Speed.metersPerSecond 3303
        fuelRate =
            Mass.kilograms 632.76
                |> Quantity.per (Duration.second)
        diameter = Length.meters 1.45
        length = Length.meters 4
        mass = Mass.kilograms 2290
    in
    addThruster
        name fuelVelocity fuelRate diameter length mass (Color.hsl 0 0 0.9, 0.1)

vulcainTwo : Name -> Part
vulcainTwo name =
    let
        fuelVelocity = Speed.metersPerSecond 4204
        fuelRate =
            Mass.kilograms 323.26
                |> Quantity.per (Duration.second)
        diameter = Length.meters 2.09
        length = Length.meters 3.44
        mass = Mass.kilograms 1800
    in
    addThruster
        name fuelVelocity fuelRate diameter length mass (Color.hsl 0 0 0.7, 0.5)

-- Fictional rocket engines

-- Balanced between thrust and fuel efficiency
balancedEngine : Name -> Part
balancedEngine name =
    let
        fuelVelocity = Speed.metersPerSecond 3000
        fuelRate =
            Mass.kilograms 1000
                |> Quantity.per (Duration.second)
        diameter = Length.meters 3.5
        length = Length.meters 5
        mass = Mass.kilograms 7500
    in
    addThruster
        name fuelVelocity fuelRate diameter length mass (Color.hsl (225 / 360) 0.15 0.25, 0.3)

-- More efficient, but heavier and less thrust
efficientEngine : Name -> Part
efficientEngine name =
    let
        fuelVelocity = Speed.metersPerSecond 4500
        fuelRate =
            Mass.kilograms 500
                |> Quantity.per (Duration.second)
        diameter = Length.meters 3.5
        length = Length.meters 5
        mass = Mass.kilograms 9500
    in
    addThruster
        name fuelVelocity fuelRate diameter length mass (Color.hsl (40 / 360) 0.35 0.40, 0.3)

-- More thrust, but heavier and uses more fuel
heavyEngine : Name -> Part
heavyEngine name =
    let
        fuelVelocity = Speed.metersPerSecond 3250
        fuelRate =
            Mass.kilograms 1500
                |> Quantity.per (Duration.second)
        diameter = Length.meters 3.5
        length = Length.meters 5
        mass = Mass.kilograms 10000
    in
    addThruster
        name fuelVelocity fuelRate diameter length mass (Color.hsl 0 0.3 0.15, 0.3)

keyDecoder : Decoder Msg
keyDecoder =
    Decode.map KeyPressed (Decode.map toKey <| Decode.field "key" Decode.string)

keyUpDecoder : Decoder Msg
keyUpDecoder =
    Decode.map KeyReleased (Decode.map toKey <| Decode.field "key" Decode.string)

subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [
            Browser.Events.onAnimationFrameDelta (Duration.milliseconds >> Tick)
        ,   Browser.Events.onKeyDown keyDecoder
        ,   Browser.Events.onKeyUp keyUpDecoder
        ]