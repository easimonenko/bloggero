module Page.PageInfo exposing (PageInfo, Msg, OutMsg(..), init, update)

import Http
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Task


type alias PageInfo =
    { title : String
    , driver : String
    }


type Msg
    = PageInfoFetchSucceed String
    | PageInfoFetchFail Http.Error


type OutMsg
    = Success String PageInfo
    | FetchFail Http.Error
    | BadJson String String


init : String -> Cmd Msg
init path =
    Task.attempt
        (\result ->
            case result of
                Ok msg ->
                    PageInfoFetchSucceed msg

                Err msg ->
                    PageInfoFetchFail msg
        )
        (Http.toTask <| Http.getString <| path ++ "/index.json")


update : Msg -> OutMsg
update msg =
    case msg of
        PageInfoFetchSucceed pageInfoJson ->
            let
                pageInfoDecoder =
                    decode PageInfo
                        |> required "title" string
                        |> optional "driver" string "markdown"
            in
                case decodeString pageInfoDecoder pageInfoJson of
                    Ok pageInfo ->
                        Success pageInfoJson pageInfo

                    Err errorInfo ->
                        BadJson pageInfoJson errorInfo

        PageInfoFetchFail httpError ->
            FetchFail httpError
