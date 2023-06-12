module Utils exposing (decimalLocale)

import FormatNumber.Locales exposing (usLocale)


decimalLocale : Int -> FormatNumber.Locales.Locale
decimalLocale decimals =
    { usLocale | decimals = FormatNumber.Locales.Exact decimals }
