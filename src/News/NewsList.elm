module News.NewsList exposing (Model, Msg, Config, defaultConfig, init, update, view)

import Dict
import Html
import Json.Decode exposing (..)
import Maybe.Extra


-- Material Design Lite modules

import Material.List as MdlList


-- Bloggero modules

import Alert.AlertLevel as AlertLevel
import Alert.InPlaceAlert as InPlaceAlert
import Link
import Page.PageInfo as PageInfo


type alias Model =
    { config : Config
    , newsIds : List NewsId
    , newsListLinks : Dict.Dict NewsId Link.Model
    , inPlaceAlert : Maybe InPlaceAlert.Model
    }


type alias NewsId =
    String


type Msg
    = PageInfoMsg PageInfo.Msg
    | LinkMsg NewsId Link.Msg


type alias Config =
    { root : String
    , title : String
    }


defaultConfig : Config
defaultConfig =
    { root = "/news"
    , title = "News List"
    }


init : Config -> ( Model, Cmd Msg )
init config =
    let
        inPlaceAlert =
            InPlaceAlert.init AlertLevel.InfoLevel "News list loading..."
    in
        ( { config = config
          , newsIds = []
          , newsListLinks = Dict.empty
          , inPlaceAlert = Just inPlaceAlert
          }
        , Cmd.map PageInfoMsg (PageInfo.init config.root)
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PageInfoMsg msg ->
            case PageInfo.update msg of
                PageInfo.Success path json pageInfo ->
                    let
                        newsIdsListDecoder =
                            maybe
                                (field "news" <|
                                    Json.Decode.map identity <|
                                        field "news" <|
                                            Json.Decode.list string
                                )

                        newsIdsListDecodeResult =
                            decodeString newsIdsListDecoder json
                    in
                        case newsIdsListDecodeResult of
                            Ok newsIds ->
                                let
                                    newsIdsUnwraped =
                                        newsIds |> Maybe.Extra.unwrap [] identity
                                in
                                    if List.isEmpty newsIdsUnwraped then
                                        let
                                            inPlaceAlert =
                                                InPlaceAlert.init
                                                    AlertLevel.WarningLevel
                                                    "News list is empty."
                                        in
                                            ( { model
                                                | inPlaceAlert = Just inPlaceAlert
                                              }
                                            , Cmd.none
                                            )
                                    else
                                        let
                                            inPlaceAlert =
                                                InPlaceAlert.init
                                                    AlertLevel.SuccessLevel
                                                    "News list loaded."

                                            ( links, linkCmds ) =
                                                List.unzip <|
                                                    List.map
                                                        (\newsId ->
                                                            let
                                                                ( link, linkCmds ) =
                                                                    Link.init <| model.config.root ++ "/" ++ newsId
                                                            in
                                                                ( ( newsId, link ), ( newsId, linkCmds ) )
                                                        )
                                                        newsIdsUnwraped
                                        in
                                            ( { model
                                                | newsIds = newsIdsUnwraped
                                                , newsListLinks = Dict.fromList links
                                                , inPlaceAlert = Just inPlaceAlert
                                              }
                                            , Cmd.batch <|
                                                List.map
                                                    (\( newsId, linkCmds ) -> Cmd.map (LinkMsg newsId) linkCmds)
                                                    linkCmds
                                            )

                            Err info ->
                                let
                                    inPlaceAlert =
                                        InPlaceAlert.init AlertLevel.DangerLevel info
                                in
                                    ( { model
                                        | inPlaceAlert = Just inPlaceAlert
                                      }
                                    , Cmd.none
                                    )

                _ ->
                    ( model, Cmd.none )

        LinkMsg newsId linkMsg ->
            Dict.get newsId model.newsListLinks
                |> Maybe.map
                    (\link ->
                        let
                            ( linkUpdated, linkCmds ) =
                                Link.update linkMsg link
                        in
                            ( { model
                                | inPlaceAlert = Nothing
                                , newsListLinks =
                                    Dict.update newsId
                                        (\item ->
                                            case item of
                                                Just _ ->
                                                    Just linkUpdated

                                                Nothing ->
                                                    Nothing
                                        )
                                        model.newsListLinks
                              }
                            , Cmd.map (LinkMsg newsId) linkCmds
                            )
                    )
                |> Maybe.withDefault ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.h2 [] [ Html.text model.config.title ]
        , model.inPlaceAlert
            |> Maybe.map InPlaceAlert.view
            |> Maybe.withDefault (Html.text "")
        , MdlList.ul [] <|
            List.map
                (\newsId ->
                    MdlList.li []
                        [ Dict.get newsId model.newsListLinks
                            |> Maybe.map (Html.map (LinkMsg newsId) << Link.view)
                            |> Maybe.withDefault (Html.text "")
                        ]
                )
                model.newsIds
        ]
