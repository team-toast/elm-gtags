port module GTag exposing (GTagData, gTagOut)

import Json.Decode
import Json.Encode
import List.Extra
import Set exposing (Set)


type alias GTagHistory =
    { sentOnlyOnce : Set String
    , sentOnlyOnChange : List GTagData
    }


type alias GTagData =
    { event : String
    , category : Maybe String
    , label : Maybe String
    , value : Maybe Int
    }


encodeGTag :
    GTagData
    -> Json.Decode.Value
encodeGTag gtag =
    Json.Encode.object
        [ ( "event", Json.Encode.string gtag.event )
        , ( "category"
          , gtag.category
                |> Maybe.withDefault "[none]"
                -- Crucial note: empty strings for category seems to silently invalidate the gtag completely
                |> Json.Encode.string
          )
        , ( "label"
          , gtag.label
                |> Maybe.withDefault ""
                |> Json.Encode.string
          )
        , ( "value"
          , gtag.value
                |> Maybe.withDefault 0
                |> Json.Encode.int
          )
        ]


gTagOutOnlyOnLabelOrValueChange : GTagHistory -> GTagData -> ( GTagHistory, Cmd msg )
gTagOutOnlyOnLabelOrValueChange gtagHistory gtag =
    let
        hasEvent event =
            .event >> (==) event

        maybeLastLogged =
            gtagHistory.sentOnlyOnChange
                |> List.filter (hasEvent gtag.event)
                |> List.head

        -- updateAndSend gtag =
    in
    case maybeLastLogged of
        Nothing ->
            ( { gtagHistory
                | sentOnlyOnChange =
                    gtagHistory.sentOnlyOnChange
                        |> List.append
                            [ gtag ]
              }
            , gTagOut gtag
            )

        Just lastLogged ->
            if gtag.label == lastLogged.label && gtag.value == lastLogged.value then
                ( gtagHistory
                , Cmd.none
                )

            else
                ( { gtagHistory
                    | sentOnlyOnChange =
                        gtagHistory.sentOnlyOnChange
                            |> List.Extra.setIf
                                (hasEvent gtag.event)
                                gtag
                  }
                , gTagOut gtag
                )


gTagOutOnlyOnceForEvent : GTagHistory -> GTagData -> ( GTagHistory, Cmd msg )
gTagOutOnlyOnceForEvent gtagHistory gtag =
    if Set.member gtag.event gtagHistory.sentOnlyOnce then
        ( gtagHistory, Cmd.none )

    else
        ( { gtagHistory
            | sentOnlyOnce =
                gtagHistory.sentOnlyOnce
                    |> Set.insert gtag.event
          }
        , gTagOut gtag
        )


gTagOut : GTagData -> Cmd msg
gTagOut =
    encodeGTag >> gTagOutPort


port gTagOutPort : Json.Encode.Value -> Cmd msg
