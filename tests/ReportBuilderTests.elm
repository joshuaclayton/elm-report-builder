module ReportBuilderTests exposing (tests)

import Expect exposing (Expectation)
import Fuzz
import ReportBuilder
import ReportBuilder.Field as Field
import Test exposing (..)


tests : Test
tests =
    let
        records =
            [ { name = "Jane Doe", age = 42 }
            , { name = "John Doe", age = 43 }
            , { name = "Jimmy Doe", age = 15 }
            , { name = "Ginny Doe", age = 11 }
            ]

        reportBuilder =
            ReportBuilder.new { records = records }
    in
    describe "ReportBuilder"
        [ test "returns an empty CSV when no fields are defined" <|
            \() ->
                reportBuilder
                    |> ReportBuilder.toCsv
                    |> String.trim
                    |> Expect.equal ""
        , test "supports multiple fields" <|
            \() ->
                [ Field.new "Name" (Field.string .name)
                , Field.new "Age" (Field.int .age)
                ]
                    |> List.foldl ReportBuilder.addField reportBuilder
                    |> ReportBuilder.toCsv
                    |> Expect.equal
                        (String.join "\u{000D}\n"
                            [ "Name,Age"
                            , "Jane Doe,42"
                            , "John Doe,43"
                            , "Jimmy Doe,15"
                            , "Ginny Doe,11"
                            ]
                        )
        ]
