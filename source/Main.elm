module Main exposing (main)

import Browser
import Browser.Navigation
import Element exposing (Element, el)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Lazy
import Time
import Url exposing (Url)


image : List { src : String, description : String }
image =
    [ { src = "/image/farm_close.jpeg", description = "Maglegard, få kilometer udenfor Ribe." }
    , { src = "/image/horsewagon_ribe.jpeg", description = "Turistkørsel i Ribes smukke gader." }
    , { src = "/image/horsewagon_wedding_front.jpeg", description = "Bryllupskørsel med originale uniformer, lygter, og seletøj." }
    , { src = "/image/horsewagon_wedding_back.jpeg", description = "Bryllupskørsel med originale uniformer, lygter, og seletøj." }
    , { src = "/image/horsewagon_wedding_close.jpeg", description = "Bryllupskørsel med originale uniformer, lygter, og seletøj." }
    , { src = "/image/horsewagon_harness.jpeg", description = "Bryllupskørsel med originale uniformer, lygter, og seletøj." }
    , { src = "/image/horsewagon_horses.jpeg", description = "Oldenborgere på gårdens marker." }
    , { src = "/image/workshop_wagonwheel.jpeg", description = "Reparation af hjul til hestevogn." }
    , { src = "/image/workshop_lantern4.jpeg", description = "Reparation af originalt vognlygte." }
    , { src = "/image/workshop_lantern5.jpeg", description = "Reparation af originalt vognlygte." }
    , { src = "/image/workshop_lantern6.jpeg", description = "Reparation af originalt vognlygte." }
    , { src = "/image/workshop_lantern2.jpeg", description = "Diverse originale lygter til reservedele, reparation og salg." }
    , { src = "/image/workshop_lantern3.jpeg", description = "Diverse originale lygter til reservedele, reparation og salg." }
    , { src = "/image/workshop_lantern7.jpeg", description = "Diverse originale lygter til reservedele, reparation og salg." }
    , { src = "/image/workshop_lantern8.jpeg", description = "Diverse originale lygter til reservedele, reparation og salg." }
    , { src = "/image/workshop_lantern9.jpeg", description = "Diverse originale lygter til reservedele, reparation og salg." }
    , { src = "/image/workshop_lantern10.jpeg", description = "Diverse originale lygter til reservedele, reparation og salg." }
    , { src = "/image/workshop_lantern11.jpeg", description = "Diverse originale lygter til reservedele, reparation og salg." }
    , { src = "/image/workshop_tools.jpeg", description = "Diverse originale lygter til reservedele, reparation og salg." }
    ]


type alias Flags =
    ()


type alias Model =
    { currentGalleryIndex : Int
    , isGalleryActive : Bool
    }


type Msg
    = DoNothing
    | SelectGalleryIndex Int
    | ToggleGalleryActive
    | RequestUrl Browser.UrlRequest


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subs
        , onUrlRequest = RequestUrl
        , onUrlChange = always DoNothing
        }


init : Flags -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { currentGalleryIndex = 0
      , isGalleryActive = True
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DoNothing ->
            ( model, Cmd.none )

        SelectGalleryIndex index ->
            ( { model | currentGalleryIndex = index }, Cmd.none )

        ToggleGalleryActive ->
            ( { model | isGalleryActive = not model.isGalleryActive }, Cmd.none )

        RequestUrl request ->
            case request of
                Browser.Internal url ->
                    ( model, Browser.Navigation.load (Url.toString url) )

                Browser.External url ->
                    ( model, Browser.Navigation.load url )


subs : Model -> Sub Msg
subs model =
    if model.isGalleryActive then
        Time.every 5000
            (always (SelectGalleryIndex (modBy (List.length image) (model.currentGalleryIndex + 1))))

    else
        Sub.none


view : Model -> Browser.Document Msg
view model =
    { title = "Hestevognskørsel, Vognlygter, og Antikke Møbler"
    , body =
        [ Element.layout
            [ Element.padding 30 ]
            (Element.column
                [ Element.centerX
                , Element.spacing 30
                , Element.width (Element.maximum 960 Element.fill)
                ]
                [ viewGallery model
                , viewHorsewagon
                , viewLantern
                , viewFurniture
                , viewDetails
                ]
            )
        ]
    }


viewGallery : Model -> Element Msg
viewGallery { currentGalleryIndex, isGalleryActive } =
    Element.column [ Element.width Element.fill, Element.spacing 20 ]
        [ Element.paragraph [ Font.size 30 ] [ Element.text "Velkommen til Maglegard" ]
        , case List.drop currentGalleryIndex image of
            [] ->
                Element.none

            currentImage :: _ ->
                Element.column [ Element.width Element.fill ]
                    [ Element.image
                        [ Element.centerX
                        , Element.width (Element.maximum 960 Element.fill)
                        , Element.inFront (viewGalleryToggleButton isGalleryActive)
                        ]
                        currentImage
                    , Element.paragraph [ Font.size 15 ] [ Element.text currentImage.description ]
                    , Element.wrappedRow
                        [ Element.spacing 5
                        , Element.paddingEach
                            { top = 15
                            , bottom = 0
                            , right = 0
                            , left = 0
                            }
                        ]
                        (List.indexedMap (viewGalleryButton currentGalleryIndex) image)
                    ]
        ]


viewGalleryButton : Int -> Int -> { src : String, description : String } -> Element Msg
viewGalleryButton currentGalleryIndex imageIndex image =
    Input.button
        [ Element.height (Element.px 50)
        , Element.width (Element.px 50)
        , Element.clip
        ]
        { onPress = Just (SelectGalleryIndex imageIndex)
        , label =
            Element.image
                [ Element.height (Element.px 50)
                ]
                image
        }


viewGalleryToggleButton : Bool -> Element Msg
viewGalleryToggleButton isGalleryActive =
    Input.button
        [ Element.alignBottom
        , Element.alignLeft
        , Element.padding 15
        , Font.color white
        , Font.size 15
        , Font.underline
        ]
        { onPress = Just ToggleGalleryActive
        , label =
            Element.text
                (if isGalleryActive then
                    "pause"

                 else
                    "start"
                )
        }


viewHorsewagon : Element msg
viewHorsewagon =
    Element.wrappedRow
        [ Element.width Element.fill
        , Element.spacing 15
        ]
        [ Element.column
            [ Element.width (Element.minimum 300 Element.fill)
            , Element.alignTop
            , Element.spacing 15
            ]
            [ Element.paragraph [ Font.size 30 ] [ Element.text "Kørsel med Hestevogn" ]
            , Element.paragraph [ Font.size 20 ] [ Element.text "Der kan bestilles kørsel med hestevogn til diverse begivenheder såsom bryllup, jubilum, familie-udflugt, studenter-tur, osv. For eksempel igennem de gamle idylliske gader i Ribe." ]
            , Element.paragraph [ Font.size 20 ] [ Element.text "Vognene bliver ført af flotte oldenborgere, som er opkaldet efter det gamle Tyske hertugdømme \"Oldenburg\"." ]
            , Element.paragraph [ Font.size 20 ] [ Element.text "Ved særlig anledning vil der blive brugt original uniformer, vognlygter og seletøj." ]
            ]
        , Element.column [ Element.alignTop ]
            [ Element.image [ Element.width (Element.px 300) ] { src = "/image/horsewagon_sled.jpeg", description = "..." }
            , Element.paragraph [ Font.size 15 ] [ Element.text "Der er mange originale vogne til forskellige anledninger." ]
            ]
        ]


viewLantern : Element msg
viewLantern =
    Element.wrappedRow
        [ Element.width Element.fill
        , Element.spacing 15
        ]
        [ Element.column
            [ Element.width (Element.minimum 300 Element.fill)
            , Element.alignTop
            , Element.spacing 15
            ]
            [ Element.paragraph [ Font.size 30 ] [ Element.text "Lygter til Hestevogn" ]
            , Element.paragraph [ Font.size 20 ] [ Element.text "Niels har over 30 års erfaring i reparation af vognlygter, og har et rigtig stort lager af reservedele." ]
            , Element.paragraph [ Font.size 20 ] [ Element.text "Han opkøber, reparerer, og sælger dem igen. Dette gælder ikke kun lygter, men også komplette hestevogne." ]
            , Element.paragraph [ Font.size 20 ] [ Element.text "Det flotte håndværk betyder at lygterne kan vedligeholdes tro mod deres historiske rødder." ]
            ]
        , Element.column [ Element.alignTop ]
            [ Element.image [ Element.width (Element.px 300) ] { src = "/image/workshop_lantern.jpeg", description = "..." }
            , Element.paragraph [ Font.size 15 ] [ Element.text "Flotte vognlygter bliver købt, solgt og repareret." ]
            ]
        ]


viewFurniture : Element msg
viewFurniture =
    Element.wrappedRow
        [ Element.width Element.fill
        , Element.spacing 15
        ]
        [ Element.column
            [ Element.width (Element.minimum 300 Element.fill)
            , Element.alignTop
            , Element.spacing 15
            ]
            [ Element.paragraph [ Font.size 30 ] [ Element.text "Antikke Møbler" ]
            , Element.paragraph [ Font.size 20 ] [ Element.text "Gården rummer et utal af flotte antikke møbler som aktivt købes, repareres, og sælges." ]
            , Element.paragraph [ Font.size 20 ] [ Element.text "Hvis du er på jagt efter noget særligt kan du besøge Niels på gården og gå på opdagelse i det store udvalg af originale møbler." ]
            ]
        , Element.column [ Element.alignTop ]
            [ Element.image [ Element.width (Element.px 300) ] { src = "/image/workshop_woodwork.jpeg", description = "..." }
            , Element.paragraph [ Font.size 15 ] [ Element.text "Der handles med flotte antikke møbler." ]
            ]
        ]


viewDetails : Element msg
viewDetails =
    Element.wrappedRow
        [ Element.width Element.fill
        , Element.spacing 15
        , Font.size 20
        ]
        [ Element.column []
            [ el [ Element.padding 5 ] (Element.text "Niels Nielsen")
            , el [ Element.padding 5 ] (Element.text "Toftlundvej 6, 6760 Ribe")
            , Element.link [ Element.padding 5, Font.underline, Font.color blue ] { label = Element.text "mail@maglegard.dk", url = "mailto:mail@maglegard.dk" }
            , Element.link [ Element.padding 5, Font.underline, Font.color blue ] { label = Element.text "+45 52 24 44 30", url = "tel:+4552244430" }
            ]
        ]


blue : Element.Color
blue =
    Element.rgb255 65 105 225


white : Element.Color
white =
    Element.rgb255 255 255 255
