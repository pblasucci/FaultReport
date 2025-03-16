namespace Demo.ForEx

open System
open System.Text.RegularExpressions
open MulberryLabs.FaultReport


type DatePeriod = { StartDate : DateOnly; EndDate : DateOnly }

type ExchangeRateParameters = {
  Base : string
  Quotes : string list
  Period : DatePeriod
}

type ExchangeRate = { Base : string; Quote : string; Rate : float }

type ExchangeRateSummary = {
  Period : DatePeriod
  Rates : Map<DateOnly, ExchangeRate list>
}


type ForExFault =
  | InvalidBase of currency : string
  | InvalidQuote of currency : string
  | InvalidPeriod of startDate : DateOnly * endDate : DateOnly
  | QuoteIsBase of currency : string
  interface IFault with
    member _.Cause = None
    member me.Message =
      match me with
      | InvalidBase ccy -> $"Base currency, '%s{ccy}', is invalid."
      | InvalidQuote ccy -> $"Quote currency, '%s{ccy}', is invalid."
      | InvalidPeriod(from, until) -> $"%A{until} occurs after %A{from}."
      | QuoteIsBase ccy ->
        $"Cannot use the same currency (%s{ccy}) for base and quote."


[<RequireQualifiedAccess>]
module ForEx =

  open FSharp.Data

  [<Literal>]
  let ForExHistorySample =
    "KEY,FREQ,CURRENCY,CURRENCY_DENOM,EXR_TYPE,EXR_SUFFIX,TIME_PERIOD,OBS_VALUE
    EXR.D.CAD.EUR.SP00.A,D,CAD,EUR,SP00,A,2024-11-20,1.4767
    EXR.D.CHF.EUR.SP00.A,D,CHF,EUR,SP00,A,2024-11-20,0.9342
    EXR.D.GBP.EUR.SP00.A,D,GBP,EUR,SP00,A,2024-11-20,0.8338
    EXR.D.USD.EUR.SP00.A,D,USD,EUR,SP00,A,2024-11-20,1.0562"

  type ForExHistory =
    CsvProvider<
      ForExHistorySample,
      Schema="string,string,string,string,string,string,date,float"
     >

  let buildQuery (input : ExchangeRateParameters) =
    let startDate = input.Period.StartDate.ToString("o")
    let endDate = input.Period.EndDate.ToString("o")
    let quotes = input.Quotes |> String.concat "+"
    let period = $"?startPeriod={startDate}&endPeriod={endDate}"
    $"https://data-api.ecb.europa.eu/service/data/EXR/D.{quotes}.{input.Base}.SP00.A{period}&detail=dataonly&format=csvdata"

  let supportedCurrencies =
    Regex(
      "\\A(AUD|BGN|BRL|CAD|CHF|CNY|CZK|DKK|EUR|GBP|HKD|HUF|IDR|ILS|INR|ISK|JPY|KRW|MXN|MYR|NOK|NZD|PHP|PLN|RON|SEK|SGD|THB|TRY|USD|ZAR)\\z",
      RegexOptions.Compiled ||| RegexOptions.IgnoreCase,
      TimeSpan.FromSeconds 1.
    )

  let inline (|SupportedCcy|_|) (currency : string) =
    try
      if supportedCurrencies.IsMatch currency then
        Some(currency.ToUpperInvariant())
      else
        None
    with
    | :? ArgumentNullException
    | :? RegexMatchTimeoutException -> None

  let inline (|EqOrdCI|_|) target value =
    if String.Equals(value, target, StringComparison.OrdinalIgnoreCase) then
      Some()
    else
      None

  let inline (|Quote|) (row : ForExHistory.Row) = Quote(row.CURRENCY)

  let inline (|Rate|) (row : ForExHistory.Row) = Rate(row.OBS_VALUE)

  let validateBase ({ Base = baseCcy } as input : ExchangeRateParameters) =
    match baseCcy with
    | SupportedCcy baseCcy -> Pass { input with Base = baseCcy }
    | _ -> Fail(InvalidBase baseCcy)

  let validateQuote (baseCcy : string) (quoteCcy : string) =
    match quoteCcy with
    | SupportedCcy quoteCcy & EqOrdCI baseCcy -> Fail(QuoteIsBase quoteCcy)
    | SupportedCcy quoteCcy -> Pass quoteCcy
    | _ -> Fail(InvalidQuote quoteCcy)

  let validateQuotes ({ Base = baseCcy; Quotes = quotes } as input) =
    let passed, failed =
      quotes |> List.map (validateQuote baseCcy) |> List.divide

    if 0 < List.length passed then
      Pass { input with Quotes = quotes }
    else
      Fail(List.head failed)

  let validatePeriod (input : ExchangeRateParameters) =
    let { StartDate = startDate; EndDate = endDate } = input.Period

    if startDate <= endDate then
      Pass input
    else
      Fail(InvalidPeriod(startDate, endDate))

  let lookupExchangeRates (input : ExchangeRateParameters) =
    let validated =
      input
      |> validateBase
      |> Report.bind validateQuotes
      |> Report.bind validatePeriod

    match validated with
    | Pass { Base = baseCcy; Period = period } ->
      async {
        let query = buildQuery input
        let! data = ForExHistory.AsyncLoad(query)
        let summary = {
          Period = period
          Rates =
            data.Rows
            |> Seq.groupBy _.TIME_PERIOD
            |> Seq.map (fun (key, values) ->
              let key = DateOnly.FromDateTime key
              let values = [
                for Quote quoteCcy & Rate closeRate in values ->
                  { Base = baseCcy; Quote = quoteCcy; Rate = closeRate }
              ]
              (key, values)
            )
            |> Map.ofSeq
        }
        return Pass summary
      }
    | Fail fault -> async { return Fail fault }
