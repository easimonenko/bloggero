module News.NewsPage exposing (Model, Msg, OutMsg(..), init, update, view)

import Html
import Http
import Markdown
import Maybe.Extra
import Navigation
import Task


-- Bloggero modules

import Alert.AlertLevel as AlertLevel
import Alert.InPlaceAlert as InPlaceAlert
import Page.PageInfo as PageInfo
import Utils


type alias Model =
    { inPlaceAlert : Maybe InPlaceAlert.Model
    , location : Navigation.Location
    , pageInfo : Maybe PageInfo.PageInfo
    , content : Maybe String
    }


type alias NewsInfo =
    { author : Maybe String
    , date : Maybe String
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
                    ( { model | pageInfo = Just pageInfo }
                    , loadContent model
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
        , model.content
            |> Maybe.map (Markdown.toHtml [])
            |> Maybe.withDefault (Html.text "")
        ]
