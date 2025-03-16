module Demo.TravelMap.TravelMap

open System
open System.Text.RegularExpressions
open NodaTime
open MulberryLabs.FaultReport


type VisitFault =
  | UnknownIso3 of currency : string
  | InvalidIso2 of country : string
  | EmptyCountryName
  | InvalidStay of from : LocalDate * until : LocalDate
  interface IFault with
    member _.Cause = None
    member me.Message = string me // Note: In practice, this would likely be more detailed

[<Struct>]
type Iso3Currency =
  | AUD
  | BGN
  | BRL
  | CAD
  | CHF
  | CNY
  | CZK
  | DKK
  | EUR
  | GBP
  | HKD
  | HUF
  | IDR
  | ILS
  | INR
  | ISK
  | JPY
  | KRW
  | MXN
  | MYR
  | NOK
  | NZD
  | PHP
  | PLN
  | RON
  | SEK
  | SGD
  | THB
  | TRY
  | USD
  | ZAR
  static member Parse(value) =
    match value with
    | nameof AUD -> Pass AUD
    | nameof BGN -> Pass BGN
    | nameof BRL -> Pass BRL
    | nameof CAD -> Pass CAD
    | nameof CHF -> Pass CHF
    | nameof CNY -> Pass CNY
    | nameof CZK -> Pass CZK
    | nameof DKK -> Pass DKK
    | nameof EUR -> Pass EUR
    | nameof GBP -> Pass GBP
    | nameof HKD -> Pass HKD
    | nameof HUF -> Pass HUF
    | nameof IDR -> Pass IDR
    | nameof ILS -> Pass ILS
    | nameof INR -> Pass INR
    | nameof ISK -> Pass ISK
    | nameof JPY -> Pass JPY
    | nameof KRW -> Pass KRW
    | nameof MXN -> Pass MXN
    | nameof MYR -> Pass MYR
    | nameof NOK -> Pass NOK
    | nameof NZD -> Pass NZD
    | nameof PHP -> Pass PHP
    | nameof PLN -> Pass PLN
    | nameof RON -> Pass RON
    | nameof SEK -> Pass SEK
    | nameof SGD -> Pass SGD
    | nameof THB -> Pass THB
    | nameof TRY -> Pass TRY
    | nameof USD -> Pass USD
    | nameof ZAR -> Pass ZAR
    | _ -> Fail(UnknownIso3 value)

type CurrencyPair = {
  Base : Iso3Currency
  Quote : Iso3Currency
} with
  static member Parse(baseCcy, quoteCcy) =
    let baseCcy = Iso3Currency.Parse baseCcy
    let quoteCcy = Iso3Currency.Parse quoteCcy
    match (baseCcy, quoteCcy) with
    | Pass baseCcy, Pass quoteCcy -> Pass { Base = baseCcy; Quote = quoteCcy }
    | Fail fault, _
    | _, Fail fault -> Fail fault

[<Struct>]
type Iso2Country = private {
  Iso2' : string
} with
  override me.ToString() = me.Iso2'
  static let spec = Regex("[A-Z]{2}", RegexOptions.Compiled)
  static member Parse(value : string) =
    try
      if spec.IsMatch value then
        Pass { Iso2' = value.ToUpperInvariant() }
      else
        Fail(InvalidIso2 value)
    with
    | :? ArgumentNullException
    | :? RegexMatchTimeoutException -> Fail(InvalidIso2 value)

type Country = private {
  Code' : Iso2Country
  Name' : string
} with
  member me.Code = me.Code'
  member me.Name = me.Name'
  static member Of(code, name) =
    match (Iso2Country.Parse code, name) with
    | Pass code, NotEmpty name -> Pass { Code' = code; Name' = name }
    | Fail fault, _ -> Fail fault
    | _, _ -> Fail EmptyCountryName

[<NoComparison>]
type Visit = {
  Place : Country
  Stay : DateInterval
  Notes : string option
  Rates : Map<CurrencyPair, decimal>
}

[<RequireQualifiedAccess>]
module Visit =
  let assemble
    (place : {| Code : string; Name : string |})
    (stay : {| From : LocalDate; Until : LocalDate |})
    (notes : string option)
    (rates : {| Base : string; Quote : string; Rate : decimal |} list)
    =
    let place = Country.Of(place.Code, place.Name)

    let stay =
      try
        Pass(DateInterval(stay.From, stay.Until))
      with _ ->
        Fail(InvalidStay(stay.From, stay.Until))

    let rates =
      rates
      |> List.traverse (fun spot ->
        match CurrencyPair.Parse(spot.Base, spot.Quote) with
        | Pass pair -> Pass {| Pair = pair; Rate = spot.Rate |}
        | Fail fault -> Fail fault
      )
      |> Report.map (fun rates ->
        rates
        |> List.groupBy _.Pair
        |> List.map (fun (pair, values) ->
          (pair, Math.Round(values |> List.averageBy _.Rate, decimals = 4))
        )
        |> Map.ofList
      )

    match (place, stay, rates) with
    | Pass place, Pass stay, Pass rates ->
      Pass { Place = place; Stay = stay; Notes = notes; Rates = rates }
    | Fail fault, _, _
    | _, Fail fault, _
    | _, _, Fail fault -> Fail fault
