module Link.Link exposing (Model, Msg, init, update, view)

import Html
import Html.Attributes exposing (..)


-- Bloggero modules

import Alert.InPlaceAlert as InPlaceAlert
import Alert.AlertLevel as AlertLevel
import Page.PageInfo as PageInfo
import Utils


type alias Model =
    { pagePath : String
    , pageTitle : String
    , inPlaceAlert : Maybe InPlaceAlert.Model
    }


type Msg
    = PageInfoMsg PageInfo.Msg


init : String -> ( Model, Cmd Msg )
init pagePath =
    ( { pagePath = pagePath, pageTitle = "", inPlaceAlert = Nothing }
    , Cmd.map PageInfoMsg (PageInfo.init pagePath)
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PageInfoMsg msg ->
            case PageInfo.update msg of
                PageInfo.Success path pageInfoJson pageInfo ->
                    if String.isEmpty pageInfo.title then
                        ( { model
                            | inPlaceAlert =
                                Just <|
                                    InPlaceAlert.init AlertLevel.WarningLevel "Page title is empty."
                          }
                        , Cmd.none
                        )
                    else
                        ( { model | pageTitle = pageInfo.title }
                        , Cmd.none
                        )

                PageInfo.BadJson path pageInfoJson errorInfo ->
                    let
                        inPlaceAlert =
                            InPlaceAlert.init AlertLevel.DangerLevel errorInfo
                    in
                        ( { model | inPlaceAlert = Just inPlaceAlert }
                        , Cmd.none
                        )

                PageInfo.FetchFail path httpError ->
                    let
                        inPlaceAlert =
                            InPlaceAlert.init
                                AlertLevel.DangerLevel
                                (Utils.toHumanReadable httpError)
                    in
                        ( { model | inPlaceAlert = Just inPlaceAlert }
                        , Cmd.none
                        )


view : Model -> Html.Html Msg
view model =
    case model.inPlaceAlert of
        Nothing ->
            Html.a [ href <| "/#!" ++ model.pagePath ] [ Html.text model.pageTitle ]

        Just inPlaceAlert ->
            InPlaceAlert.view inPlaceAlert
