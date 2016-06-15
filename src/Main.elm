module Main exposing (main)

import Html exposing (..)
import Html.App exposing (program)

type alias Model =
  {
    title : String
  }

init =
  (
    {
      title = "My Blog"
    },
    Cmd.none
  )

view model = header []
  [
    h1 [] [ text model.title ]
  ]

update action model =
  (
    model,
    Cmd.none
  )

subscriptions model = Sub.none

main = program
  {
    init = init,
    view = view,
    subscriptions = subscriptions,
    update = update
  }
