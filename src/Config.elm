module Config exposing (Config, Mode(..), Section, Placement(..), Msg, OutMsg(..), init, update)

import Http
import Json.Decode exposing (..)
import Task


type alias Config =
    { title : String
    , mode : Mode
    , sections : List Section
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
                blogTitle =
                    decodeString (field "title" string) json

                blogMode =
                    decodeString
                        (field "mode" <|
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
                        )
                        json

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

                blogSections =
                    decodeString (field "sections" sectionItemListDecoder) json
            in
                case Result.map3 Config blogTitle blogMode blogSections of
                    Ok config ->
                        Success json config

                    Err info ->
                        BadJson json info
