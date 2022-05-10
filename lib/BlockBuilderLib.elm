module BlockBuilderLib exposing (Block, basicBlock, blockGroup, moveBlock, toMesh)

import Array
import Length exposing (Length, Meters)
import Point3d exposing (Point3d)
import TriangularMesh exposing (TriangularMesh)
import Vector3d


type alias Scene3DVertex coordinates =
    { position : Point3d Meters coordinates, uv : ( Float, Float ) }


type alias Block coordinates =
    { mesh : TriangularMesh (Scene3DVertex coordinates)
    , codeString : String
    }


listToString : (a -> String) -> List a -> String
listToString toString xs =
    case xs of
        [] ->
            "[]"

        _ ->
            "[ " ++ String.join ", " (List.map toString xs) ++ " ]"


vertexToString : Scene3DVertex coordinates -> String
vertexToString vert =
    let
        ( x, y, z ) =
            vert.position
                |> Point3d.toTuple Length.inMeters

        ( u, v ) =
            vert.uv
    in
    "{ position = Point3d.meters "
        ++ String.fromFloat x
        ++ " "
        ++ String.fromFloat y
        ++ " "
        ++ String.fromFloat z
        ++ ", uv = ( "
        ++ String.fromFloat u
        ++ ", "
        ++ String.fromFloat v
        ++ " ) }"


vertex : Point3d Meters coordinates -> ( Float, Float ) -> Scene3DVertex coordinates
vertex pos uv =
    { position = pos, uv = uv }


tripleToString : ( Float, Float, Float ) -> String
tripleToString ( a, b, c ) =
    "( " ++ String.join ", " (List.map String.fromFloat [ a, b, c ]) ++ " )"


blockGroup : List (Block coordinates) -> Block coordinates
blockGroup blocks =
    let
        combinedMesh =
            List.map .mesh blocks
                |> TriangularMesh.combine

        adjustedSubCode =
            List.map
                (\block ->
                    String.split "\n" block.codeString
                        |> List.map String.trim
                        |> String.join " "
                )
                blocks

        combinedCode =
            case adjustedSubCode of
                first :: rest ->
                    "TriangularMesh.combine\n"
                        ++ "    [ "
                        ++ first
                        ++ (List.map (\s -> "\n    , " ++ s) rest |> String.join "")
                        ++ "\n    ]"

                _ ->
                    "TriangularMesh.combine []"
    in
    { mesh = combinedMesh
    , codeString = combinedCode
    }


basicBlock : Float -> Float -> Float -> Block coordinates
basicBlock l w h =
    let
        vertices =
            [ vertex (Point3d.centimeters (-l / 2) (-w / 2) (-h / 2)) ( 0, 1 )
            , vertex (Point3d.centimeters (l / 2) (-w / 2) (-h / 2)) ( 1, 1 )
            , vertex (Point3d.centimeters (l / 2) (w / 2) (-h / 2)) ( 1, 0 )
            , vertex (Point3d.centimeters (-l / 2) (w / 2) (-h / 2)) ( 0, 0 )
            , vertex (Point3d.centimeters (-l / 2) (-w / 2) (h / 2)) ( 0, 0 )
            , vertex (Point3d.centimeters (l / 2) (-w / 2) (h / 2)) ( 1, 0 )
            , vertex (Point3d.centimeters (l / 2) (w / 2) (h / 2)) ( 1, 1 )
            , vertex (Point3d.centimeters (-l / 2) (w / 2) (h / 2)) ( 0, 1 )
            ]

        faces =
            [ ( 0, 2, 1 )
            , ( 0, 3, 2 )
            , ( 4, 5, 6 )
            , ( 4, 6, 7 )
            , ( 1, 2, 6 )
            , ( 1, 6, 5 )
            , ( 0, 7, 3 )
            , ( 0, 4, 7 )
            , ( 0, 1, 5 )
            , ( 0, 5, 4 )
            , ( 3, 6, 2 )
            , ( 3, 7, 6 )
            ]

        mesh =
            TriangularMesh.indexed (Array.fromList vertices) faces
    in
    { mesh = mesh
    , codeString =
        "TriangularMesh.indexed (Array.fromList "
            ++ (vertices |> listToString vertexToString)
            ++ ") "
            ++ (faces |> listToString tripleToString)
    }


toMesh : Block coordinates -> TriangularMesh (Scene3DVertex coordinates)
toMesh block =
    block.mesh


moveBlock : ( Float, Float, Float ) -> Block coordinates -> Block coordinates
moveBlock ( x, y, z ) block =
    let
        move v =
            { v | position = v.position |> Point3d.translateBy (Vector3d.centimeters x y z) }
    in
    { block
        | mesh =
            block.mesh
                |> TriangularMesh.mapVertices move
        , codeString =
            block.codeString
                ++ " |> TriangularMesh.mapVertices ( \\ v -> { v | position = v.position |> Point3d.translateBy (Vector3d.centimeters "
                ++ String.fromFloat x
                ++ " "
                ++ String.fromFloat y
                ++ " "
                ++ String.fromFloat z
                ++ " )} )"
    }

-- TODO rotation