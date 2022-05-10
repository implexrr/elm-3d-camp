module TextureLoader exposing (..)

import Scene3d.Material as Material
import Dict exposing(Dict)
import Color
import WebGL.Texture
import Task

type TextureType
    = TexColor
    | TexRoughness
    | TexMetallicity

type alias URL = String

type alias TextureRequest =
    { name : String
    , textureType : TextureType
    , url : String
    }

type alias Model =
    { requestedURLs : Dict String URL
    , colorTextures : Dict String (Material.Texture Color.Color)
    , roughnessTextures : Dict String (Material.Texture Float)
    , metallicityTextures : Dict String (Material.Texture Float)
    , numberFinished : Int
    }

{-| Function for abstracting texture requests in texture lists -}
loadTexture : String -> TextureType -> URL -> TextureRequest
loadTexture name texType url =
    { name = name
    , textureType = texType
    , url = url
    }

{-| Attempts to read a string as a `TextureType`. -}
readTextureType : String -> Maybe TextureType
readTextureType val =
    case val of
        "TexColor" ->
            Just TexColor
        "TexRoughness" ->
            Just TexRoughness
        "TexMetallicity" ->
            Just TexMetallicity
        _ ->
            Nothing

{-| Shows a `TextureType` as a string. -}
showTextureType : TextureType -> String
showTextureType val =
    case val of
        TexColor ->
            "TexColor"
        TexRoughness ->
            "TexRoughness"
        TexMetallicity ->
            "TexMetallicity"

{-| Given a model containing a texture loader, return the color texture associated with that index from the texture loader.
If that texture doesn't exist, a constant black texture is returned -}
getColorTexture : String -> { a | textureLoader : Model } -> Material.Texture Color.Color
getColorTexture index model =
    let
        default = Material.constant Color.black

        lookupResult = Dict.get index model.textureLoader.colorTextures

    in
        Maybe.withDefault default lookupResult

{-| Given a model containing a texture loader, return the roughness texture associated with that index from the texture loader.
If that texture doesn't exist, a constant value of 0.5 is returned -}
getRoughnessTexture : String -> { a | textureLoader : Model } -> Material.Texture Float
getRoughnessTexture index model =
    let
        default = Material.constant 0.5

        lookupResult = Dict.get index model.textureLoader.roughnessTextures

    in
        Maybe.withDefault default lookupResult

{-| Given a model containing a texture loader, return the metallicity texture associated with that index from the texture loader.
If that texture doesn't exist, a constant value of 0.5 is returned -}
getMetallicityTexture : String -> { a | textureLoader : Model } -> Material.Texture Float
getMetallicityTexture index model =
    let
        default = Material.constant 0.5

        lookupResult = Dict.get index model.textureLoader.metallicityTextures

    in
        Maybe.withDefault default lookupResult

allTexturesLoaded : Model -> Bool
allTexturesLoaded model =
    Dict.size model.requestedURLs == model.numberFinished

type alias TextureName = String

type Msg
    = RequestNew TextureRequest
    | ColorLoaded String (Result WebGL.Texture.Error (Material.Texture Color.Color))
    | RoughnessLoaded String (Result WebGL.Texture.Error (Material.Texture Float))
    | MetallicityLoaded String (Result WebGL.Texture.Error (Material.Texture Float))

init : Model
init =
    { requestedURLs = Dict.empty
    , colorTextures = Dict.empty
    , roughnessTextures = Dict.empty
    , metallicityTextures = Dict.empty
    , numberFinished = 0
    }

fetchTexture : TextureRequest -> Model -> Cmd Msg
fetchTexture request model =
    Task.perform identity (Task.succeed <| RequestNew request)

fetchTextures : List TextureRequest -> Model -> Cmd Msg
fetchTextures requests model =
    Cmd.batch <| List.map (Task.perform identity << Task.succeed << RequestNew) requests

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        RequestNew request ->
            ( { model | requestedURLs = Dict.insert request.name request.url model.requestedURLs }
            , case request.textureType of
                TexColor ->
                    Task.attempt (ColorLoaded request.name) (Material.load request.url)
                TexRoughness ->
                    Task.attempt (RoughnessLoaded request.name) (Material.load request.url)
                TexMetallicity ->
                    Task.attempt (MetallicityLoaded request.name) (Material.load request.url)
            )
        ColorLoaded name res ->
            case res of
                Ok texture -> ( { model | colorTextures = Dict.insert name texture model.colorTextures, numberFinished = model.numberFinished + 1 }, Cmd.none)
                _ -> ( { model | numberFinished = model.numberFinished + 1 } , Cmd.none)
        RoughnessLoaded name res ->
            case res of
                Ok texture -> ( { model | roughnessTextures = Dict.insert name texture model.roughnessTextures, numberFinished = model.numberFinished + 1 }, Cmd.none)
                _ -> ( { model | numberFinished = model.numberFinished + 1 } , Cmd.none)
        MetallicityLoaded name res ->
            case res of
                Ok texture -> ( { model | metallicityTextures = Dict.insert name texture model.metallicityTextures, numberFinished = model.numberFinished + 1 }, Cmd.none)
                _ -> ( { model | numberFinished = model.numberFinished + 1 } , Cmd.none)