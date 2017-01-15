module News.NewsSectionPage exposing (Model, Msg, OutMsg(..), init, update, view)

import Html
import Navigation


-- Bloggero modules

import Alert.AlertLevel as AlertLevel
import Alert.InPlaceAlert as InPlaceAlert
import Alert.InPlaceAlertCloseable as InPlaceAlertCloseable
import News.NewsList as NewsList
import Page.PageInfo as PageInfo
import Utils


type alias Model =
    { pageInfo : Maybe PageInfo.PageInfo
    , inPlaceAlert : Maybe InPlaceAlert.Model
    , inPlaceAlertCloseable : InPlaceAlertCloseable.Model
    , newsList : NewsList.Model
    }


type Msg
    = PageInfoMsg PageInfo.Msg
    | InPlaceAlertCloseableMsg InPlaceAlertCloseable.Msg
    | NewsListMsg NewsList.Msg


type OutMsg
    = NoneOutMsg
    | AlertOutMsg AlertLevel.Level String


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        inPlaceAlertCloseable =
            InPlaceAlertCloseable.init AlertLevel.InfoLevel "The news section is in development."

        newsListConfig =
            NewsList.defaultConfig

        ( newsList, newsListCmds ) =
            NewsList.init { newsListConfig | root = (Utils.pagePath location) }
    in
        ( { pageInfo = Nothing
          , inPlaceAlert = Nothing
          , inPlaceAlertCloseable = inPlaceAlertCloseable
          , newsList = newsList
          }
        , Cmd.batch
            [ Cmd.map PageInfoMsg <| PageInfo.init <| Utils.pagePath location
            , Cmd.map NewsListMsg newsListCmds
            ]
        )


update : Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update msg model =
    case msg of
        PageInfoMsg msg ->
            case PageInfo.update msg of
                PageInfo.Success path json pageInfo ->
                    ( { model | pageInfo = Just pageInfo }
                    , Cmd.none
                    , NoneOutMsg
                    )

                _ ->
                    ( model, Cmd.none, NoneOutMsg )

        InPlaceAlertCloseableMsg msg ->
            let
                ( inPlaceAlertUpdated, inPlaceAlertCmds ) =
                    InPlaceAlertCloseable.update msg model.inPlaceAlertCloseable
            in
                ( { model | inPlaceAlertCloseable = inPlaceAlertUpdated }
                , Cmd.map InPlaceAlertCloseableMsg inPlaceAlertCmds
                , NoneOutMsg
                )

        NewsListMsg msg ->
            let
                ( newsList, newsListCmds ) =
                    NewsList.update msg model.newsList
            in
                ( { model | newsList = newsList }
                , Cmd.map NewsListMsg newsListCmds
                , NoneOutMsg
                )


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ model.inPlaceAlertCloseable
            |> Html.map InPlaceAlertCloseableMsg
            << InPlaceAlertCloseable.view
        , Html.h1 []
            [ model.pageInfo
                |> Maybe.map .title
                |> Maybe.withDefault ""
                |> Html.text
            ]
        , model.inPlaceAlert
            |> Maybe.map (InPlaceAlert.view)
            |> Maybe.withDefault (Html.text "")
        , model.newsList
            |> Html.map NewsListMsg
            << NewsList.view
        ]
