module ReportBuilder exposing
    ( ReportBuilder
    , addField
    , downloadCsv
    , new
    , toCsv
    , withName
    )

{- | A report builder is a data structure that allows you to build a report
      with a list of records and a list of fields. You can add fields to the
      report and then generate a CSV representation of the report.

   # Definition
   @docs ReportBuilder

   # Constructor
   @docs new

   # Modification
   @docs addField, withName

   # Output
   @docs toCsv

   # Commands
   @docs downloadCsv
-}

import Csv.Encode as CsvEncode
import File.Download as Download
import ReportBuilder.Field as Field exposing (Field)


{-| A report builder is a data structure that allows you to build a report
with a list of records and a list of fields. You can add fields to the
report and then generate a CSV representation of the report.
-}
type ReportBuilder a
    = ReportBuilder
        { records : List a
        , fields : List (Field a)
        , name : Maybe String
        }


{-| Create a new report builder with a list of records.
-}
new : { records : List a } -> ReportBuilder a
new { records } =
    ReportBuilder
        { records = records
        , fields = []
        , name = Nothing
        }


{-| Add a field to the report builder.
-}
addField : Field a -> ReportBuilder a -> ReportBuilder a
addField field (ReportBuilder reportBuilder) =
    ReportBuilder
        { reportBuilder
            | fields = reportBuilder.fields ++ [ field ]
        }


{-| Set the name of the report builder.

This is used for generating the filename when downloading the CSV.

-}
withName : String -> ReportBuilder a -> ReportBuilder a
withName name (ReportBuilder reportBuilder) =
    ReportBuilder
        { reportBuilder
            | name = Just name
        }


headers : ReportBuilder a -> List String
headers (ReportBuilder { fields }) =
    List.map Field.header fields


{-| Convert a report builder to a CSV string.
-}
toCsv : ReportBuilder a -> String
toCsv ((ReportBuilder { fields, records }) as reportBuilder) =
    let
        generatedRows =
            List.map
                (\record ->
                    List.map (Field.encode record) fields
                )
                records
    in
    CsvEncode.encode
        { encoder = CsvEncode.withoutFieldNames identity
        , fieldSeparator = ','
        }
        (headers reportBuilder :: generatedRows)


{-| Generate a command to download the CSV representation of the report
builder.
-}
downloadCsv : ReportBuilder a -> Cmd msg
downloadCsv reportBuilder =
    let
        (ReportBuilder { name }) =
            reportBuilder
    in
    Download.string
        (Maybe.withDefault "report.csv" name)
        "text/csv"
        (toCsv reportBuilder)
