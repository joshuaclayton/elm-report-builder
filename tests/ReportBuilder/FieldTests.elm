module ReportBuilder.FieldTests exposing (tests)

import Expect exposing (Expectation)
import Fuzz
import ReportBuilder.Field as Field exposing (Field)
import Test exposing (..)


tests : Test
tests =
    describe "ReportBuilder.Field"
        [ test "with an accessor that returns a static value" <|
            \() ->
                Field.new "header" (Field.int (always 42))
                    |> Field.encode "inconsequential"
                    |> Expect.equal "42"
        , test "with a record" <|
            \() ->
                Field.new "header" (Field.string .foo)
                    |> Field.encode { foo = "bar" }
                    |> Expect.equal "bar"
        , lengthOfTests
        , boolTests
        , intTests
        , fieldConstructorTests
        , optionalTests
        ]


lengthOfTests : Test
lengthOfTests =
    describe "lengthOf"
        [ fuzz (Fuzz.list Fuzz.string) "always returns the length of the list" <|
            \list ->
                Field.new "header" (Field.lengthOf .records)
                    |> Field.encode { records = list }
                    |> Expect.equal (String.fromInt (List.length list))
        ]


optionalTests : Test
optionalTests =
    describe "optional"
        [ fuzz (Fuzz.pair Fuzz.string Fuzz.bool) "returns the value when Just or empty string when Nothing" <|
            \( string, isNothing ) ->
                Field.new "header" (Field.optional Field.string .value)
                    |> Field.encode
                        { value =
                            if isNothing then
                                Nothing

                            else
                                Just string
                        }
                    |> Expect.equal
                        (if isNothing then
                            ""

                         else
                            string
                        )
        ]


boolTests : Test
boolTests =
    describe "bool"
        [ fuzz (Fuzz.pair Fuzz.string Fuzz.string) "returns the truthy value when the predicate is True" <|
            \( trueCase, falseCase ) ->
                Field.new "header" (Field.bool { true = trueCase, false = falseCase } (always True))
                    |> Field.encode 1234
                    |> Expect.equal trueCase
        , fuzz (Fuzz.pair Fuzz.string Fuzz.string) "returns the false value when the predicate is True" <|
            \( trueCase, falseCase ) ->
                Field.new "header" (Field.bool { true = trueCase, false = falseCase } (always False))
                    |> Field.encode 1234
                    |> Expect.equal falseCase
        ]


intTests : Test
intTests =
    describe "int"
        [ fuzz Fuzz.int "returns the integer value as a string" <|
            \value ->
                Field.new "header" (Field.int identity)
                    |> Field.encode value
                    |> Expect.equal (String.fromInt value)
        ]


fieldConstructorTests : Test
fieldConstructorTests =
    describe "Field.new"
        [ describe "header"
            [ fuzz Fuzz.string "returns the value as a string" <|
                \headerValue ->
                    Field.new headerValue (Field.int (always 1))
                        |> Field.header
                        |> Expect.equal headerValue
            ]
        ]
