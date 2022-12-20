module TimeUtil exposing
    ( DateFormat
    , dateTimeFormat, defaultDateFormat
    , encodePosix, posixDecoder, posixCodec
    , format, formatDate, formatDate_
    , shortDateFormat, shortTimeFormat, timeAttrFormat
    , isSameDay, difference
    , localToUtc, utcToLocal
    , setNoon
    , stringFormatToStrftimeFormats
    )

{-| Utilities we use to format times / dates wraps Time.Extra and other funzies.

@docs DateFormat
@docs dateTimeFormat, defaultDateFormat
@docs encodePosix, posixDecoder, posixCodec
@docs format, formatDate, formatDate_
@docs shortDateFormat, shortTimeFormat, timeAttrFormat
@docs isSameDay, difference
@docs localToUtc, utcToLocal
@docs setNoon
@docs stringFormatToStrftimeFormats

-}

import Codec exposing (Codec)
import Duration exposing (Duration)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Strftime
import Time exposing (Posix, Zone)
import Time.Extra as TimeX


{-| -}
format : Zone -> Posix -> String
format tz time =
    let
        fromZoneAndTime : Zone -> Posix -> String
        fromZoneAndTime =
            Strftime.format "%-I:%M %p %b %d %Y"
    in
    if Time.utc == tz then
        fromZoneAndTime Time.utc time ++ " UTC"

    else
        fromZoneAndTime tz time


{-| -}
formatDate : Posix -> String
formatDate =
    formatDate_ Time.utc


{-| -}
formatDate_ : Zone -> Posix -> String
formatDate_ =
    Strftime.format "%Y-%m-%d"


{-| -}
stringFormatToStrftimeFormats : Maybe String -> DateFormat
stringFormatToStrftimeFormats configDateFormat =
    case configDateFormat of
        Just "M/d/yy h:mm a" ->
            { date = "%-m/%-d/%y", time = "%-I:%M %p" }

        Just "dd.MM.yyyy h:mm a" ->
            { date = "%d.%m.%Y", time = "%-I:%M %p" }

        Just "MMM d, yyyy h:mm a" ->
            { date = "%b %-d, %Y", time = "%-I:%M %p" }

        Just "dd/MM/yyyy h:mm a" ->
            { date = "%d/%m/%Y", time = "%-I:%M %p" }

        Just "dd.MM.yy h:mm a" ->
            { date = "%d.%m.%y", time = "%-I:%M %p" }

        Just "MM/dd/yyyy h:mm a" ->
            { date = "%m/%d/%Y", time = "%-I:%M %p" }

        Just "M/d/yyyy h:mm a" ->
            { date = "%-m/%-d/%Y", time = "%-I:%M %p" }

        _ ->
            defaultDateFormat


{-| -}
defaultDateFormat : DateFormat
defaultDateFormat =
    { date = "%-m/%-d/%y", time = "%-I:%M %p" }


{-| -}
type alias DateFormat =
    { date : String
    , time : String
    }


{-| -}
isSameDay : Zone -> Posix -> Posix -> Bool
isSameDay timezone p1 p2 =
    let
        p1Parts : TimeX.Parts
        p1Parts =
            TimeX.posixToParts timezone p1

        p2Parts : TimeX.Parts
        p2Parts =
            TimeX.posixToParts timezone p2
    in
    (p1Parts.year == p2Parts.year)
        && (p1Parts.month == p2Parts.month)
        && (p1Parts.day == p2Parts.day)


{-| -}
shortDateFormat : Zone -> Posix -> String
shortDateFormat timezone posix =
    Strftime.format "%b %d" timezone posix


{-| -}
shortTimeFormat : Zone -> Posix -> String
shortTimeFormat timezone posix =
    --TODO locale
    -- e.g. 12:05 PM
    Strftime.format "%-I:%M %p" timezone posix


{-| -}
dateTimeFormat : Zone -> Posix -> String
dateTimeFormat timezone posix =
    --TODO locale
    -- e.g. 2020-05-13 04:17 PM
    Strftime.format "%Y-%m-%d %H:%M %p" timezone posix


{-| -}
timeAttrFormat : Zone -> Posix -> String
timeAttrFormat timezone posix =
    Strftime.format "%Y-%m-%dT%H:%M:%S" timezone posix


{-| -}
difference : Posix -> Posix -> Int
difference t0 t1 =
    abs (Time.posixToMillis t0 - Time.posixToMillis t1)


{-| -}
posixDecoder : Decoder Posix
posixDecoder =
    Decode.map Time.millisToPosix Decode.int


{-| -}
encodePosix : Posix -> Encode.Value
encodePosix =
    Time.posixToMillis >> Encode.int


{-| -}
posixCodec : Codec Posix
posixCodec =
    Codec.build encodePosix posixDecoder


{-| -}
localToUtc : Zone -> Posix -> Posix
localToUtc zone posix =
    let
        tzOffset : Duration
        tzOffset =
            TimeX.toOffset zone posix
                |> toFloat
                |> Duration.minutes
    in
    Duration.addTo posix tzOffset


{-| -}
utcToLocal : Zone -> Posix -> Posix
utcToLocal zone posix =
    let
        tzOffset : Duration
        tzOffset =
            TimeX.toOffset zone posix
                |> toFloat
                |> Duration.minutes
    in
    Duration.subtractFrom posix tzOffset


{-| -}
setNoon : Zone -> Posix -> Posix
setNoon zone posix =
    Duration.addTo
        (TimeX.floor TimeX.Day zone posix)
        (Duration.hours 12)
