module Page.PageInfo exposing (PageInfo, Msg, OutMsg(..), init, update)

import Http
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Task


type alias PageInfo =
    { title : String
    , driver : String
    }


type alias Path =
    String


type Msg
    = PageInfoFetchSucceed Path String
    | PageInfoFetchFail Path Http.Error


type OutMsg
    = Success Path String PageInfo
    | FetchFail Path Http.Error
    | BadJson Path String String


init : String -> Cmd Msg
init path =
    Task.attempt
        (\result ->
            case result of
                Ok msg ->
                    PageInfoFetchSucceed path msg

                Err msg ->
                    PageInfoFetchFail path msg
        )
        (Http.toTask <| Http.getString <| path ++ "/index.json")


update : Msg -> OutMsg
update msg =
    case msg of
        PageInfoFetchSucceed path pageInfoJson ->
            let
                pageInfoDecoder =
                    decode PageInfo
                        |> required "title" string
                        |> optional "driver" string "markdown"
            in
                case decodeString pageInfoDecoder pageInfoJson of
                    Ok pageInfo ->
                        Success path pageInfoJson pageInfo

                    Err errorInfo ->
                        BadJson path pageInfoJson errorInfo

        PageInfoFetchFail path httpError ->
            FetchFail path httpError
