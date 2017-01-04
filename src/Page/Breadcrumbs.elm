module Page.Breadcrumbs exposing (Model, Msg, init, update, view)

import Dict
import Html
import Html.Attributes exposing (class, href)


-- Bloggero modules

import Page.PageInfo as PageInfo


type alias Model =
    { path : String
    , pageInfo : Dict.Dict String PageInfo.PageInfo
    }


type Msg
    = PageInfoMsg PageInfo.Msg


init : String -> ( Model, Cmd Msg )
init path =
    ( { path = path, pageInfo = Dict.empty }
    , Cmd.batch
        [ Cmd.map PageInfoMsg <| PageInfo.init "/home"
        , Cmd.map PageInfoMsg <| PageInfo.init path
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PageInfoMsg msg ->
            case PageInfo.update msg of
                PageInfo.Success path pageInfoJson pageInfo ->
                    ( { model | pageInfo = Dict.insert path pageInfo model.pageInfo }
                    , Cmd.none
                    )

                PageInfo.FetchFail path httpError ->
                    ( model, Cmd.none )

                PageInfo.BadJson path pageInfoJson errorInfo ->
                    ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Html.ul [ class "breadcrumbs" ]
        [ Html.li
            []
            [ Html.a
                [ href "/#!/home" ]
                [ Html.text <|
                    Maybe.withDefault "Home" <|
                        Maybe.map
                            .title
                            (Dict.get
                                "/home"
                                model.pageInfo
                            )
                ]
            ]
        , Html.li
            []
            [ Html.text <|
                Maybe.withDefault "Unknown page title" <|
                    Maybe.map
                        .title
                        (Dict.get
                            model.path
                            model.pageInfo
                        )
            ]
        ]



--Maybe.withDefault (Html.text "") <|
--    Maybe.map
--        (\pageInfo ->
--             Html.text pageInfo.title
--        )
--        model.pageInfo
