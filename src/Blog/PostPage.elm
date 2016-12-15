module Blog.PostPage exposing (Model, Msg, init, update, view)

import Date
import Html
import Http
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Markdown
import Navigation
import String.Extra
import Task


-- Bloggero modules

import Alert.AlertLevel as AlertLevel
import Alert.InPlaceAlert as InPlaceAlert
import Utils


type alias Model =
    { location : Navigation.Location
    , rawContent : String
    , rawContentType : String
    , post : Post
    , inPlaceAlert : Maybe InPlaceAlert.Model
    }


type alias Post =
    { author :
        String
        --, abstract : String
        --, date : Date.Date
    }


type Msg
    = PageInfoFetchSucceed String
    | PageInfoFetchFail Http.Error
    | PageContentFetchSucceed String
    | PageContentFetchFail Http.Error
    | InPlaceAlertMsg InPlaceAlert.Msg


defaultPost : Post
defaultPost =
    { author = ""
    }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    ( { location = location
      , rawContent = ""
      , rawContentType = "markdown"
      , post = defaultPost
      , inPlaceAlert = Nothing
      }
    , Task.attempt
        (\result ->
            case result of
                Ok pageInfo ->
                    PageInfoFetchSucceed pageInfo

                Err error ->
                    PageInfoFetchFail error
        )
        (Http.toTask <| Http.getString <| (Utils.pagePath location) ++ "/index.json")
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PageInfoFetchSucceed pageInfoJson ->
            let
                postDecoder =
                    decode identity
                        |> optional "post"
                            (decode Post
                                |> optional "author" string ""
                             -- |> optional "abstract" string ""
                            )
                            defaultPost
            in
                case decodeString postDecoder pageInfoJson of
                    Ok post ->
                        ( { model | post = post }
                        , Task.attempt
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
                        )

                    Err error ->
                        let
                            ( inPlaceAlert, inPlaceAlertCmds ) =
                                InPlaceAlert.init AlertLevel.DangerLevel error
                        in
                            ( { model | inPlaceAlert = Just inPlaceAlert }
                            , Cmd.map InPlaceAlertMsg inPlaceAlertCmds
                            )

        PageContentFetchSucceed pageContent ->
            ( { model | rawContent = pageContent }, Cmd.none )

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
    let
        title =
            String.dropLeft 2 <| String.Extra.leftOf "\n\n" model.rawContent

        body =
            String.dropLeft (String.length title + 2) model.rawContent
    in
        Html.article []
            [ Html.header []
                [ case model.inPlaceAlert of
                    Just inPlaceAlert ->
                        Html.map InPlaceAlertMsg <| InPlaceAlert.view inPlaceAlert

                    Nothing ->
                        Html.text ""
                , Html.h1 [] [ Html.text title ]
                , Html.p []
                    [ Html.text <|
                        if not (String.isEmpty model.post.author) then
                            "Author: " ++ model.post.author
                        else
                            ""
                    ]
                ]
            , Markdown.toHtml [] body
            ]
