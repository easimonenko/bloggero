module Blog.PostPage exposing (Model, Msg, init, update, view)

import Html
import Html.Attributes exposing (..)
import HtmlParser
import HtmlParser.Util
import Http
import Json.Decode exposing (..)
import Markdown
import Navigation
import Task


-- Bloggero modules

import Alert.AlertLevel as AlertLevel
import Alert.InPlaceAlert as InPlaceAlert
import Page.PageInfo as PageInfo
import Utils


type alias Model =
    { location : Navigation.Location
    , pageInfo : Maybe PageInfo.PageInfo
    , postInfo : Maybe PostInfo
    , rawContent : Maybe String
    , rawContentType : RawContentType
    , inPlaceAlert : Maybe InPlaceAlert.Model
    }


type alias PostInfo =
    { author : Maybe String
    , abstract : Maybe String
    , date : Maybe String
    }


type Msg
    = PageInfoMsg PageInfo.Msg
    | PageInfoFetchSucceed String
    | PageInfoFetchFail Http.Error
    | PageContentFetchSucceed String RawContentType
    | PageContentFetchFail Http.Error RawContentType
    | InPlaceAlertMsg InPlaceAlert.Msg


type RawContentType
    = UnknownContentType
    | MarkdownContentType
    | HtmlContentType
    | FailContentType


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        ( inPlaceAlert, inPlaceAlertCmds ) =
            InPlaceAlert.init
                AlertLevel.InfoLevel
                ("Loading post page [" ++ (Utils.pagePath location) ++ "] ...")
    in
        ( { location = location
          , pageInfo = Nothing
          , postInfo = Nothing
          , rawContent = Nothing
          , rawContentType = UnknownContentType
          , inPlaceAlert = Just inPlaceAlert
          }
        , Cmd.batch
            [ Cmd.map PageInfoMsg <| PageInfo.init (Utils.pagePath location)
            , Cmd.map InPlaceAlertMsg inPlaceAlertCmds
            ]
        )


nextContentType : RawContentType -> RawContentType
nextContentType contentType =
    case contentType of
        UnknownContentType ->
            MarkdownContentType

        MarkdownContentType ->
            HtmlContentType

        HtmlContentType ->
            FailContentType

        FailContentType ->
            FailContentType


loadContentType model contentType =
    let
        extension =
            case contentType of
                UnknownContentType ->
                    ""

                MarkdownContentType ->
                    "markdown"

                HtmlContentType ->
                    "html"

                FailContentType ->
                    ""
    in
        Task.attempt
            (\result ->
                case result of
                    Ok content ->
                        PageContentFetchSucceed content contentType

                    Err error ->
                        PageContentFetchFail error contentType
            )
            (Http.toTask <|
                Http.getString <|
                    (Utils.pagePath model.location)
                        ++ "/index."
                        ++ extension
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PageInfoMsg pageInfoMsg ->
            case PageInfo.update pageInfoMsg of
                PageInfo.Success path pageInfoJson pageInfo ->
                    let
                        postDecoder =
                            maybe
                                (field "post"
                                    (map3 PostInfo
                                        (maybe (field "author" string))
                                        (maybe (field "abstract" string))
                                        (maybe (field "date" string))
                                    )
                                )
                    in
                        case decodeString postDecoder pageInfoJson of
                            Ok postInfo ->
                                ( { model
                                    | pageInfo = Just pageInfo
                                    , postInfo = postInfo
                                  }
                                , loadContentType model (nextContentType model.rawContentType)
                                )

                            Err error ->
                                let
                                    ( inPlaceAlert, inPlaceAlertCmds ) =
                                        InPlaceAlert.init AlertLevel.DangerLevel error
                                in
                                    ( { model
                                        | pageInfo = Just pageInfo
                                        , inPlaceAlert = Just inPlaceAlert
                                      }
                                    , Cmd.map InPlaceAlertMsg inPlaceAlertCmds
                                    )

                PageInfo.BadJson path pageInfoJson errorInfo ->
                    let
                        ( inPlaceAlert, inPlaceAlertCmds ) =
                            InPlaceAlert.init AlertLevel.DangerLevel errorInfo
                    in
                        ( { model | inPlaceAlert = Just inPlaceAlert }
                        , Cmd.map InPlaceAlertMsg inPlaceAlertCmds
                        )

                PageInfo.FetchFail path httpError ->
                    let
                        ( inPlaceAlert, inPlaceAlertCmds ) =
                            InPlaceAlert.init
                                AlertLevel.DangerLevel
                                (Utils.toHumanReadable httpError)
                    in
                        ( { model | inPlaceAlert = Just inPlaceAlert }
                        , Cmd.map InPlaceAlertMsg inPlaceAlertCmds
                        )

        PageContentFetchSucceed pageContent contentType ->
            ( { model
                | rawContent = Just pageContent
                , rawContentType = contentType
                , inPlaceAlert = Nothing
              }
            , Cmd.none
            )

        PageContentFetchFail error contentType ->
            case error of
                Http.BadStatus response ->
                    if response.status.code == 404 then
                        let
                            otherContentType =
                                nextContentType contentType
                        in
                            case otherContentType of
                                FailContentType ->
                                    let
                                        ( inPlaceAlert, inPlaceAlertCmds ) =
                                            InPlaceAlert.init
                                                AlertLevel.DangerLevel
                                                "Page content fetch fail: content file not found."
                                    in
                                        ( { model | inPlaceAlert = Just inPlaceAlert }
                                        , Cmd.map InPlaceAlertMsg inPlaceAlertCmds
                                        )

                                UnknownContentType ->
                                    let
                                        ( inPlaceAlert, inPlaceAlertCmds ) =
                                            InPlaceAlert.init
                                                AlertLevel.DangerLevel
                                                "Page content fetch fail: internal error."
                                    in
                                        ( { model | inPlaceAlert = Just inPlaceAlert }
                                        , Cmd.map InPlaceAlertMsg inPlaceAlertCmds
                                        )

                                _ ->
                                    ( model
                                    , loadContentType model otherContentType
                                    )
                    else
                        let
                            ( inPlaceAlert, inPlaceAlertCmds ) =
                                InPlaceAlert.init
                                    AlertLevel.DangerLevel
                                    ("Page content fetch: ["
                                        ++ (toString response.status.code)
                                        ++ "] "
                                        ++ response.status.message
                                    )
                        in
                            ( { model | inPlaceAlert = Just inPlaceAlert }
                            , Cmd.map InPlaceAlertMsg inPlaceAlertCmds
                            )

                _ ->
                    let
                        ( inPlaceAlert, inPlaceAlertCmds ) =
                            InPlaceAlert.init
                                AlertLevel.DangerLevel
                                ("Page content fetch error.")
                    in
                        ( { model | inPlaceAlert = Just inPlaceAlert }
                        , Cmd.map InPlaceAlertMsg inPlaceAlertCmds
                        )

        InPlaceAlertMsg inPlaceAlertMsg ->
            case model.inPlaceAlert of
                Just inPlaceAlert ->
                    let
                        ( inPlaceAlertUpdated, inPlaceAlertCmds ) =
                            InPlaceAlert.update inPlaceAlertMsg inPlaceAlert
                    in
                        ( { model | inPlaceAlert = Just inPlaceAlertUpdated }
                        , Cmd.map InPlaceAlertMsg inPlaceAlertCmds
                        )

                Nothing ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Maybe.withDefault (Html.text "") <|
            Maybe.map
                (\inPlaceAlert ->
                    Html.map InPlaceAlertMsg <| InPlaceAlert.view inPlaceAlert
                )
                model.inPlaceAlert
        , Maybe.withDefault (Html.text "") <|
            Maybe.map
                (\postInfo ->
                    Html.footer [ class "post-info" ]
                        [ Html.p [] <|
                            (Maybe.withDefault [] <|
                                Maybe.map
                                    (\author ->
                                        [ Html.span [ class "post-author" ]
                                            [ Html.text "Author: " ]
                                        , Html.text author
                                        ]
                                    )
                                    postInfo.author
                            )
                                ++ [ Html.text " " ]
                                ++ (Maybe.withDefault [] <|
                                        Maybe.map
                                            (\date ->
                                                [ Html.span [ class "post-date" ]
                                                    [ Html.text "Date: " ]
                                                , Html.text date
                                                ]
                                            )
                                            postInfo.date
                                   )
                        , Maybe.withDefault (Html.text "") <|
                            Maybe.map
                                (\abstract ->
                                    Html.p [ class "post-abstract" ]
                                        [ Html.text abstract
                                        ]
                                )
                                postInfo.abstract
                        ]
                )
                model.postInfo
        , Maybe.withDefault (Html.text "") <|
            Maybe.map
                (\rawContent ->
                    Html.article [] <|
                        case model.rawContentType of
                            MarkdownContentType ->
                                [ Markdown.toHtml [] rawContent
                                ]

                            HtmlContentType ->
                                HtmlParser.Util.toVirtualDom <| HtmlParser.parse rawContent

                            _ ->
                                []
                )
                model.rawContent
        ]
