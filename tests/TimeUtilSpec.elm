module TimeUtilSpec exposing (..)

import Expect
import Test exposing (Test)
import Time exposing (Posix)
import TimeUtil exposing (..)
import TimeZone


tests : Test
tests =
    let
        psx : Posix
        psx =
            Time.millisToPosix
                1652342400000
    in
    Test.describe "Util.Time tests"
        [ Test.describe "localToUtc"
            [ Test.test "positive timezone" <|
                \_ ->
                    localToUtc (TimeZone.asia__shanghai ()) psx
                        |> Expect.equal
                            (Time.millisToPosix 1652371200000)
            , Test.test "neutral timezone" <|
                \_ ->
                    localToUtc Time.utc psx
                        |> Expect.equal psx
            , Test.test "negative timezone" <|
                \_ ->
                    localToUtc (TimeZone.america__nome ()) psx
                        |> Expect.equal
                            (Time.millisToPosix 1652313600000)
            ]
        , Test.describe "utcToLocal"
            [ Test.test "positive timezone" <|
                \_ ->
                    utcToLocal (TimeZone.asia__shanghai ())
                        (Time.millisToPosix 1652371200000)
                        |> Expect.equal psx
            , Test.test "neutral timezone" <|
                \_ ->
                    utcToLocal Time.utc psx
                        |> Expect.equal psx
            , Test.test "negative timezone" <|
                \_ ->
                    utcToLocal (TimeZone.america__nome ())
                        (Time.millisToPosix 1652313600000)
                        |> Expect.equal psx
            ]
        ]
