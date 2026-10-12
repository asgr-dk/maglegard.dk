module Main exposing (main)

import Browser
import Browser.Navigation
import Element exposing (Element, el)
import Element.Input
import Url exposing (Url)


type alias Flags =
    {}


type alias Model =
    {}


type Msg
    = DoNothing


init : Flags -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    ( {}, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DoNothing ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Maglegard"
    , body =
        [ Element.layout
            [ Element.width Element.fill
            , Element.height Element.fill
            ]
            (el
                [ Element.centerX
                , Element.centerY
                ]
                (Element.text "Hello World!!!")
            )
        ]
    }


subs : Model -> Sub Msg
subs model =
    Sub.none


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subs
        , onUrlChange = always DoNothing
        , onUrlRequest = always DoNothing
        }
