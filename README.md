# elm-report-builder

A library for generating CSV reports in Elm.

## Example Usage

```elm
import ReportBuilder
import ReportBuilder.Field as Field

type alias BlogPost =
    { title : String
    , author : Author
    , date : String
    , content : String
    , comments : List Comment
    }

type alias Author =
    { name : String
    , email : String
    }

type alias Comment =
    { author : Author
    , date : String
    , content : String
    }

blogPostReport : List BlogPost -> ReportBuilder BlogPost
blogPostReport blogPosts =
    let
        fields =
            [ Field.new "Title" (Field.string .title)
            , Field.new "Author" (Field.string (.author >> .name))
            , Field.new "Author Email" (Field.string (.author >> .email))
            , Field.new "Comments Count" (Field.lengthOf .comments)
            ]

        reportBuilder =
            ReportBuilder.new { records = blogPosts }
                |> ReportBuilder.withName "blog-posts.csv"
    in
    List.foldl ReportBuilder.addField reportBuilder fields
```

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE)
file for details.
