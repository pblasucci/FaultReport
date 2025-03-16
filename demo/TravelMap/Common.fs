namespace Demo.TravelMap

open System
open System.Data
open Demo.ForEx
open MulberryLabs.FaultReport

[<AutoOpen>]
module Patterns =
  let inline (|Trimmed|) (value : string) =
    Trimmed(if String.IsNullOrWhiteSpace value then "" else value.Trim())

  let inline (|Empty|_|) (Trimmed value) = if value = "" then Some() else None

  let inline (|NotEmpty|_|) (Trimmed value) =
    if value = "" then None else Some value

  let inline (|StringEq|_|)
    (mode : StringComparison)
    (Trimmed target)
    (Trimmed subject)
    =
    if String.Equals(target, subject, mode) then Some subject else None

  let inline (|EqOrdCI|_|) target subject =
    (|StringEq|_|) StringComparison.OrdinalIgnoreCase target subject
  let inline (|EqOrdCS|_|) target subject =
    (|StringEq|_|) StringComparison.Ordinal target subject

  let inline (|EqInvCI|_|) target subject =
    (|StringEq|_|) StringComparison.InvariantCultureIgnoreCase target subject
  let inline (|EqInvCS|_|) target subject =
    (|StringEq|_|) StringComparison.InvariantCulture target subject

  let inline (|EqCurCI|_|) target subject =
    (|StringEq|_|) StringComparison.CurrentCultureIgnoreCase target subject
  let inline (|EqCurCS|_|) target subject =
    (|StringEq|_|) StringComparison.CurrentCulture target subject


type FetchExchangeRates =
  ExchangeRateParameters -> Async<Report<ExchangeRateSummary, ForExFault>>


type ConnectToStore = unit -> IDbConnection

