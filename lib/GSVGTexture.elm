module GSVGTexture exposing (..)

import Html exposing(Html,div,button)
import Html.Events exposing(onClick)
import Html.Attributes exposing (style)
import Browser
import GraphicSVG exposing(..)
import GraphicSVG.Widget as Widget
import Html.Events exposing (onClick)

import Dict exposing (Dict)

import Task

import SVGPorts exposing (..)

import Scene3d.Material as Material exposing (Material)
import Color
import TextureLoader exposing (TextureType)
import Set exposing (Set)



type alias WidgetID = String

type alias URL = String

type alias SVGRequest =
    { name : String
    , textureType : String
    , shape : Shape Never
    }

{-| Function for abstracting texture requests in texture lists -}
svgTexture : String -> TextureType -> Shape Never -> SVGRequest
svgTexture name texType shape =
    { name = name
    , textureType = TextureLoader.showTextureType texType
    , shape = shape
    }

svgTextures =
    []


type alias Model =
    { widgets : Dict String Widget.Model
    , created : Set String
    , shapes : Dict String (Shape Never)
    , textures : Dict String URL
    }

initialModel : List SVGRequest -> (Model, Cmd Msg)
initialModel svgRequests =
    let
        shapes =
            List.map ( \ request -> (request.name ++ request.textureType, request.shape) ) svgRequests
        widgetsPlusCmd = List.map (\(n, _) -> Widget.init 50 50 n) shapes
        widgetModels = List.map (\(wM, _) -> (wM.id, wM)) widgetsPlusCmd
        widgetCmds = List.map (Tuple.mapFirst .id) widgetsPlusCmd
    in
    ({
        widgets = Dict.fromList <| widgetModels
    ,   created = Set.empty
    ,   shapes = Dict.fromList shapes
    ,   textures = Dict.empty
    }
    , Cmd.batch
        [
            Cmd.batch <| List.map (\(id, cmd) -> Cmd.map (WidgetMsg id) cmd) widgetCmds
        ,   Cmd.batch <| List.map (\(id, _) -> createPNG (id, 1024, 1024)) shapes
        ]
    )


--initialModelWithLoad : (Model, Cmd Msg)
--initialModelWithLoad =
--    let
--        (topWidget, topWCmd) = Widget.init 50 50 "skyT"
--        (botWidget, botWCmd) = Widget.init 50 50 "skyB"
--        (side1Widget, s1WCmd) = Widget.init 50 50 "skyS1"
--        (side2Widget, s2WCmd) = Widget.init 50 50 "skyS2"
--        (side3Widget, s3WCmd) = Widget.init 50 50 "skyS3"
--        (side4Widget, s4WCmd) = Widget.init 50 50 "skyS4"
--    in
--    ({ topWidget = topWidget
--    , botWidget = botWidget
--    , side1Widget = side1Widget
--    , side2Widget = side2Widget
--    , side3Widget = side3Widget
--    , side4Widget = side4Widget
--    , showBorders = False
--    , topPNG = ""
--    , botPNG = ""
--    , side1PNG = ""
--    , side2PNG = ""
--    , side3PNG = ""
--    , side4PNG = ""
--    }
--    ,Cmd.batch [Cmd.map TopWidgetMsg topWCmd
--               ,Cmd.map BotWidgetMsg botWCmd
--               ,Cmd.map Side1Widget s1WCmd
--               ,Cmd.map Side2Widget s2WCmd
--               ,Cmd.map Side3Widget s3WCmd
--               ,Cmd.map Side4Widget s4WCmd
--               ,createPNG ("skyT", 1024, 1024)
--               ,createPNG ("skyB", 1024, 1024)
--               ,createPNG ("skyS1", 1024, 1024)
--               ,createPNG ("skyS2", 1024, 1024)
--               ,createPNG ("skyS3", 1024, 1024)
--               ,createPNG ("skyS4", 1024, 1024)
--               ]
--    )

type Msg =
      WidgetMsg String Widget.Msg
    | LoadNew (String, Shape Never)
    | GeneratedPNG (String, URL)

generateNew : (String, Shape Never) -> Cmd Msg
generateNew shapePlusName =
    Task.perform LoadNew <| Task.succeed shapePlusName

generateMany : List (String, Shape Never) -> Cmd Msg
generateMany =
    Cmd.batch << List.map generateNew

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        WidgetMsg wId wMsg ->
            case Maybe.map (Widget.update wMsg) <| Dict.get wId model.widgets of
                Just (newWState, wCmd) ->
                    ({ model | widgets = Dict.insert wId newWState model.widgets
                                }
                    , Cmd.map (WidgetMsg wId) wCmd)
                Nothing -> ( model, Cmd.none )

        LoadNew (name, shape) ->
            let
                (newWidget, newWCmd) = Widget.init 50 50 name
            in
                (
                    { model |
                                widgets = Dict.insert name newWidget model.widgets
                            ,   shapes = Dict.insert name shape model.shapes
                   }
                ,   Cmd.batch
                    [
                        Cmd.map (WidgetMsg name) newWCmd
                    ,   createPNG (name, 1024, 1024)
                    ]
                )

        GeneratedPNG (widgetID, imageURL) ->
            let
                newTextures = Dict.insert widgetID imageURL model.textures
            in
            ( { model | textures = newTextures
                      , widgets = Dict.remove widgetID model.widgets -- remove generated SVG to save computation
              }
            , Cmd.none
            )



showBorder : String -> Bool -> List (Shape msg) -> List (Shape msg)
showBorder name show sh =
    if show then
        sh ++ [
           square 50
                |> outlined (solid 1) black
        , text name
            |> fixedwidth
            |> size 2
            |> filled black
            |> move (-23,21)
        ]
    else sh

view : Model -> Html Msg
view model =
    drawTextures True model


drawTextures : Bool -> Model -> Html Msg
drawTextures show model =
    div ((if show then [style "display" "block"] else [style "display" "none"])) <|
        List.map (Html.map never) <|
        (List.filterMap (\(id, shapes) ->
            if show || not (Set.member id model.created) then -- display all if show, or just the ones not created yet
                Maybe.map (\widgetModel ->
                     Widget.view widgetModel
                                    ([shapes] |> showBorder id show)) (Dict.get id model.widgets)
            else
                Nothing
            ) <| Dict.toList model.shapes
        )


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> initialModel svgTextures
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [
            Sub.batch <| List.map (\(id,_) -> Sub.map (WidgetMsg id) Widget.subscriptions) <| Dict.toList model.widgets
        ,   receivePNG GeneratedPNG
        ]