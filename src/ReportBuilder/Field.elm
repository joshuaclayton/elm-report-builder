module ReportBuilder.Field exposing
    ( Field, FieldAccess
    , new
    , bool, int, string
    , optional
    , lengthOf
    , encode, header
    , isNumeric, numeric
    )

{-| `ReportBuilder.Field` provides a way to define fields that can be used to
generate reports.


# Definitions

@docs Field, FieldAccess


# Constructor

@docs new


# Field Types

@docs bool, int, string


# Field Modifiers

@docs optional


# Computed FieldAccess

@docs lengthOf


# Generate values

@docs encode, header


# Annotate numeric values

@docs isNumeric, numeric

-}


type FieldType
    = String
    | Numeric


{-| A field describes a column in a report. It has a name, an accessor function
and a type.
-}
type Field a
    = Field
        { name : String
        , accessor : FieldAccess a
        , fieldType : FieldType
        }


{-| A field accessor is a function that takes a record and returns a string.
-}
type FieldAccess a
    = FieldAccess
        { access : a -> String
        }


{-| Create a new field with a name and an accessor function.
-}
new : String -> FieldAccess a -> Field a
new name f =
    Field { name = name, accessor = f, fieldType = String }


{-| Mark a field as numeric.
-}
numeric : Field a -> Field a
numeric (Field record) =
    Field { record | fieldType = Numeric }


{-| Check if a field is numeric.
-}
isNumeric : Field a -> Bool
isNumeric (Field { fieldType }) =
    fieldType == Numeric


{-| Get the header value of a field.
-}
header : Field a -> String
header (Field { name }) =
    name


{-| Given a field and a corresponding record, return the value of the field.
-}
encode : a -> Field a -> String
encode record (Field { accessor }) =
    let
        (FieldAccess { access }) =
            accessor
    in
    access record


{-| Define field access for a string.
-}
string : (a -> String) -> FieldAccess a
string accessor =
    FieldAccess { access = accessor }


{-| Define field access for an integer.
-}
int : (a -> Int) -> FieldAccess a
int accessor =
    FieldAccess { access = String.fromInt << accessor }


{-| Mark a field as optional.
-}
optional : ((a -> b) -> FieldAccess a) -> (a -> Maybe b) -> FieldAccess a
optional f1 f2 =
    FieldAccess
        { access =
            \record ->
                case f2 record of
                    Just value ->
                        let
                            (FieldAccess { access }) =
                                f1 (always value)
                        in
                        access record

                    Nothing ->
                        ""
        }


{-| Define field access for the length of a list.
-}
lengthOf : (a -> List b) -> FieldAccess a
lengthOf accessor =
    FieldAccess { access = String.fromInt << List.length << accessor }


{-| Define field access for a boolean.
-}
bool : { true : String, false : String } -> (a -> Bool) -> FieldAccess a
bool { true, false } accessor =
    FieldAccess
        { access =
            \value ->
                if accessor value then
                    true

                else
                    false
        }
