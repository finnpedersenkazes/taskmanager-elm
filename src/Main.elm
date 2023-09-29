module Main exposing (main)

import Browser
import Model.Model exposing (Model, Msg, init)
import Subscriptions.Subscriptions exposing (subscriptions)
import Update.Update exposing (update)
import View.View exposing (view)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
