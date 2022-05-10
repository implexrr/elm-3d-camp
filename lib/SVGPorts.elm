port module SVGPorts exposing (..)

type alias WidgetID = String

-- tell JavaScript to create a PNG with a given name (must match the id of the widget)
port createPNG : (WidgetID, Int, Int) -> Cmd msg

type alias URL = String

port receivePNG : ((WidgetID, URL) -> msg) -> Sub msg