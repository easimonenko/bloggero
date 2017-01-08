module Config exposing (Config, Mode(..), Section, Placement(..), Msg, OutMsg(..), init, update)

import Http
import Json.Decode exposing (..)
import Task


type alias Config =
    { title : String
    , mode : Mode
    , sections : List Section
    , copyright : Maybe Copyright
    , links : Maybe (List Link)
    }


type Mode
    = DevelopmentMode
    | ProductionMode
    | UnknownMode


type alias Section =
    { id : String
    , title : String
    , route : String
    , icon : Maybe String
    , placement : List Placement
    }


type Placement
    = HeaderPlacement
    | DrawerPlacement
    | FooterPlacement
    | SiteMapPlacement
    | UnknownPlacement


type alias Copyright =
    { text : String
    , url : Maybe String
    , email : Maybe String
    }


type alias Link =
    { title : String
    , url : String
    , icon : Maybe String
    }


type Msg
    = ConfigFetchSucceed String
    | ConfigFetchFail Http.Error


type OutMsg
    = Success String Config
    | FetchFail Http.Error
    | BadJson String String


init : Cmd Msg
init =
    Task.attempt
        (\result ->
            case result of
                Err msg ->
                    ConfigFetchFail msg

                Ok msg ->
                    ConfigFetchSucceed msg
        )
        (Http.toTask <| Http.getString "/config.json")


update : Msg -> OutMsg
update msg =
    case msg of
        ConfigFetchFail httpError ->
            FetchFail httpError

        ConfigFetchSucceed json ->
            let
                modeDecoder =
                    Json.Decode.map
                        (\item ->
                            case item of
                                "development" ->
                                    DevelopmentMode

                                "production" ->
                                    ProductionMode

                                _ ->
                                    UnknownMode
                        )
                        string

                placementItemListDecoder =
                    list
                        (Json.Decode.map
                            (\item ->
                                case item of
                                    "header" ->
                                        HeaderPlacement

                                    "drawer" ->
                                        DrawerPlacement

                                    "footer" ->
                                        FooterPlacement

                                    "sitemap" ->
                                        SiteMapPlacement

                                    _ ->
                                        UnknownPlacement
                            )
                            string
                        )

                sectionItemListDecoder =
                    list
                        (map5 Section
                            (field "id" string)
                            (field "title" string)
                            (field "route" string)
                            (maybe (field "icon" string))
                            (field "placement" placementItemListDecoder)
                        )

                copyrightDecoder =
                    Json.Decode.map3 Copyright
                        (field "text" string)
                        (maybe (field "url" string))
                        (maybe (field "email" string))

                linkDecoder =
                    Json.Decode.map3 Link
                        (field "title" string)
                        (field "url" string)
                        (maybe (field "icon" string))

                configDecoder =
                    Json.Decode.map5 Config
                        (field "title" string)
                        (field "mode" modeDecoder)
                        (field "sections" sectionItemListDecoder)
                        (maybe (field "copyright" copyrightDecoder))
                        (maybe (field "links" (list linkDecoder)))
            in
                case decodeString configDecoder json of
                    Ok config ->
                        Success json config

                    Err info ->
                        BadJson json info
