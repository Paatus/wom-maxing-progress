module Utils exposing (..)

import FormatNumber.Locales exposing (usLocale)


locale : FormatNumber.Locales.Locale
locale =
    { usLocale | decimals = FormatNumber.Locales.Exact 2 }


decimalLocale : Int -> FormatNumber.Locales.Locale
decimalLocale decimals =
    { usLocale | decimals = FormatNumber.Locales.Exact decimals }
