module Page.Breadcrumbs exposing (Model, Msg, init, update, view)

import Dict
import Html
import Html.Attributes exposing (class, href)
import List
import List.Extra
import String


-- Bloggero modules

import Page.PageInfo as PageInfo


type alias Model =
    { path : String
    , pageInfo : Dict.Dict String PageInfo.PageInfo
    , paths : List String
    }


type Msg
    = PageInfoMsg PageInfo.Msg


init : String -> ( Model, Cmd Msg )
init path =
    let
        paths =
            path
                |> String.split "/"
                |> List.filter (not << String.isEmpty)
                |> List.Extra.inits
                |> List.drop 1
                |> List.map (((++) "/") << (String.join "/"))
                |> (::) "/home"
    in
        ( { path = path, pageInfo = Dict.empty, paths = paths }
        , Cmd.batch <|
            List.map (Cmd.map PageInfoMsg << PageInfo.init) paths
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
    case List.reverse model.paths of
        [] ->
            Html.text ""

        pagePath :: reversedPaths ->
            Html.ul [ class "breadcrumbs" ]
                (List.reverse
                    ((Maybe.withDefault (Html.text "")
                        (Maybe.map
                            (\pageInfo ->
                                Html.li
                                    []
                                    [ Html.text pageInfo.title
                                    ]
                            )
                            (Dict.get pagePath model.pageInfo)
                        )
                     )
                        :: (List.map
                                (\path ->
                                    Maybe.withDefault (Html.text "")
                                        (Maybe.map
                                            (\pageInfo ->
                                                Html.li
                                                    []
                                                    [ Html.a
                                                        [ href <| "/#!" ++ path ]
                                                        [ Html.text pageInfo.title ]
                                                    ]
                                            )
                                            (Dict.get path model.pageInfo)
                                        )
                                )
                                reversedPaths
                           )
                    )
                )
