module Blog.PostPage exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Markdown


import Material


type alias Model =
  {
    mdl : Material.Model,
    content : String
  }

type Msg =
  Mdl Material.Msg


init root path query =
  (
    {
      mdl = Material.model,
      content = ""
    },
    Cmd.none
  )


update msg model =
  case msg of
    _ -> ( model, Cmd.none )


view model =
  article [] [ Markdown.toHtml [] model.content ]
