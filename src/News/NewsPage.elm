module News.NewsPage exposing (Model, Msg, OutMsg(..), init, update, view)

import Html
import Html.Attributes exposing (..)
import Http
import Json.Decode exposing (..)
import Markdown
import Maybe.Extra
import Navigation
import Task


-- Bloggero modules

import Alert.AlertLevel as AlertLevel
import Alert.InPlaceAlert as InPlaceAlert
import News.NewsInfo exposing (..)
import Page.PageInfo as PageInfo
import Utils


type alias Model =
    { inPlaceAlert : Maybe InPlaceAlert.Model
    , location : Navigation.Location
    , pageInfo : Maybe PageInfo.PageInfo
    , newsInfo : Maybe NewsInfo
    , content : Maybe String
    }


type Msg
    = PageInfoMsg PageInfo.Msg
    | PageContentFetchSucceed String
    | PageContentFetchFail Http.Error


type OutMsg
    = NoneOutMsg
    | AlertOutMsg AlertLevel.Level String


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        path =
            Utils.pagePath location

        inPlaceAlert =
            InPlaceAlert.init
                AlertLevel.InfoLevel
                ("Loading news [" ++ path ++ "] ...")
    in
        ( { inPlaceAlert = Just inPlaceAlert
          , location = location
          , pageInfo = Nothing
          , newsInfo = Nothing
          , content = Nothing
          }
        , Cmd.map PageInfoMsg <| PageInfo.init path
        )


loadContent : Model -> Cmd Msg
loadContent model =
    Task.attempt
        (\result ->
            case result of
                Ok content ->
                    PageContentFetchSucceed content

                Err error ->
                    PageContentFetchFail error
        )
        (Http.toTask <|
            Http.getString <|
                (Utils.pagePath model.location)
                    ++ "/index.markdown"
        )


update : Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update msg model =
    case msg of
        PageInfoMsg pageInfoMsg ->
            case PageInfo.update pageInfoMsg of
                PageInfo.Success path pageInfoJson pageInfo ->
                    case decodeString newsInfoDecoder pageInfoJson of
                        Ok newsInfo ->
                            ( { model
                                | pageInfo = Just pageInfo
                                , newsInfo = newsInfo
                              }
                            , loadContent model
                            , NoneOutMsg
                            )

                        Err error ->
                            let
                                inPlaceAlert =
                                    InPlaceAlert.init AlertLevel.DangerLevel error
                            in
                                ( { model
                                    | pageInfo = Just pageInfo
                                    , inPlaceAlert = Just inPlaceAlert
                                  }
                                , Cmd.none
                                , NoneOutMsg
                                )

                PageInfo.BadJson path pageInfoJson errorInfo ->
                    let
                        inPlaceAlert =
                            InPlaceAlert.init AlertLevel.DangerLevel errorInfo
                    in
                        ( { model | inPlaceAlert = Just inPlaceAlert }
                        , Cmd.none
                        , NoneOutMsg
                        )

                PageInfo.FetchFail path httpError ->
                    let
                        inPlaceAlert =
                            InPlaceAlert.init AlertLevel.DangerLevel
                                (Utils.toHumanReadable httpError)
                    in
                        ( { model | inPlaceAlert = Just inPlaceAlert }
                        , Cmd.none
                        , NoneOutMsg
                        )

        PageContentFetchSucceed content ->
            ( { model | content = Just content, inPlaceAlert = Nothing }
            , Cmd.none
            , NoneOutMsg
            )

        PageContentFetchFail error ->
            case error of
                Http.BadStatus response ->
                    if response.status.code == 404 then
                        let
                            inPlaceAlert =
                                InPlaceAlert.init AlertLevel.DangerLevel
                                    "News content fetch fail: content file not found."
                        in
                            ( { model | inPlaceAlert = Just inPlaceAlert }
                            , Cmd.none
                            , NoneOutMsg
                            )
                    else
                        let
                            inPlaceAlert =
                                InPlaceAlert.init
                                    AlertLevel.DangerLevel
                                    ("Page content fetch: ["
                                        ++ (toString response.status.code)
                                        ++ "] "
                                        ++ response.status.message
                                    )
                        in
                            ( { model | inPlaceAlert = Just inPlaceAlert }
                            , Cmd.none
                            , NoneOutMsg
                            )

                _ ->
                    let
                        inPlaceAlert =
                            InPlaceAlert.init
                                AlertLevel.DangerLevel
                                ("Page content fetch error.")
                    in
                        ( { model | inPlaceAlert = Just inPlaceAlert }
                        , Cmd.none
                        , NoneOutMsg
                        )


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ model.inPlaceAlert
            |> Maybe.Extra.unwrap (Html.text "") InPlaceAlert.view
        , model.newsInfo
            |> Maybe.map
                (\newsInfo ->
                    Html.footer [ class "news-info" ]
                        [ Html.p [] <|
                            (newsInfo.author
                                |> Maybe.map
                                    (\author ->
                                        [ Html.span [ class "news-author" ]
                                            [ Html.text "Author: " ]
                                        , Html.text author
                                        ]
                                    )
                                |> Maybe.withDefault []
                            )
                                ++ [ Html.text " " ]
                                ++ (newsInfo.date
                                        |> Maybe.map
                                            (\date ->
                                                [ Html.span [ class "news-date" ]
                                                    [ Html.text "Date: " ]
                                                , Html.text date
                                                ]
                                            )
                                        |> Maybe.withDefault []
                                   )
                        ]
                )
            |> Maybe.withDefault (Html.text "")
        , model.content
            |> Maybe.map (Markdown.toHtml [])
            |> Maybe.withDefault (Html.text "")
        ]
