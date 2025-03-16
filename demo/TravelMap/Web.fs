[<RequireQualifiedAccess>]
module Demo.TravelMap.Web

open System
open System.ComponentModel.DataAnnotations
open System.Net
open System.Threading
open System.Threading.Tasks

open FsToolkit.ErrorHandling
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Logging
open MulberryLabs.FaultReport
open NodaTime
open NodaTime.Extensions

open Demo.Atlas
open Demo.ForEx
open Demo.TravelMap.TravelMap


type Web = ``HACK to improve logging -- because modules are not types!``


[<CLIMutable>]
type Defaults = {
  [<Required>]
  [<Range(minimum = 1, maximum = 100)>]
  RecentVisitsCount : int
} with
  static member MinimumVisits = 1
  static member MaximumVisits = 100
  static member Key = nameof Defaults


type AddVisitInputs = {
  Country : {|
    Identity : {| Code : string; Name : string |}
    Currencies : string list
  |}
  Duration : {| From : LocalDate; Until : LocalDate |}
  Notes : string option
  SpotRates : {| Base : string; Quote : string; Rate : decimal |} list
} with
  static member Empty = {
    Country = {| Identity = {| Code = ""; Name = "" |}; Currencies = [] |}
    Duration = {| From = LocalDate.MaxIsoValue; Until = LocalDate.MinIsoValue |}
    Notes = None
    SpotRates = List.empty
  }

type AddVisitRequest = {
  Place : string
  From : DateOnly
  Until : DateOnly
  Notes : string
}

[<NoComparison>]
type AddVisitFault =
  | InvalidDateRange of from : DateOnly * until : DateOnly
  | ProtocolError of details : GraphError array
  | TransportError of status : HttpStatusCode
  | UnknownCountry of countryName : string

  interface IFault with
    member _.Cause = None
    member me.Message = string me // Note: In practice, this would likely be more detailed

  interface AtlasFault<AddVisitFault> with
    static member ProtocolError(details) = ProtocolError(details)
    static member TransportError(status, _) = TransportError(status)
    static member UnknownCountry(countryName) = UnknownCountry(countryName)


let checkNotes { Notes = notes } (inputs : AddVisitInputs) =
  Pass {
    inputs with
        Notes =
          match notes with
          | NotEmpty notes -> Some notes
          | _ -> None
  }

let checkStay { From = fromDate; Until = untilDate } inputs =
  if untilDate < fromDate then
    (fromDate, untilDate) |> InvalidDateRange |> Report.ofFault
  else
    Pass {
      inputs with
          Duration = {|
            From = fromDate.ToLocalDate()
            Until = untilDate.ToLocalDate()
          |}
    }

let checkPlace (atlas : AtlasClient) cancel { Place = place } inputs =
  backgroundTask {
    match! atlas.FindCountry(place, cancel) with
    | Pass country ->
      let validated = {
        inputs with
            Country = {|
              Identity = {| Code = country.Code; Name = country.Name |}
              Currencies = List.ofArray country.Currencies
            |}
      }
      return Pass validated
    | Fail(fault : AddVisitFault) -> return Report.ofFault fault
  }

let lookupRates (forex : FetchExchangeRates) currencies cancel request inputs =
  let getRates =
    forex {
      Base = "EUR"
      Period = { StartDate = request.From; EndDate = request.Until }
      Quotes = currencies |> List.filter ((<>) "EUR")
    }

  backgroundTask {
    match! Async.StartAsTask(getRates, cancellationToken = cancel) with
    | Pass { ExchangeRateSummary.Rates = rates } ->
      let rates =
        rates
        |> Map.values
        |> List.ofSeq
        |> List.collect (fun rates ->
          rates
          |> List.map (fun rate -> {|
            Base = rate.Base
            Quote = rate.Quote
            Rate = decimal rate.Rate
          |})
        )
      return Pass { inputs with SpotRates = rates }

    | Fail fault -> return Report.ofFault fault
  }

let makeVisit
  (atlas : AtlasClient)
  (forex : FetchExchangeRates)
  (cancel : CancellationToken)
  (request : AddVisitRequest)
  =
  backgroundTaskReport {
    // validate / transform "simple" inputs
    let! inputs = AddVisitInputs.Empty |> checkNotes request
    let! inputs = inputs |> checkStay request

    // validate / expand country information
    let! inputs = inputs |> checkPlace atlas cancel request

    // lookup exchange rates for given period
    let quotes = inputs.Country.Currencies
    let! inputs = inputs |> lookupRates forex quotes cancel request

    // construct visit
    let visit =
      Visit.assemble
        inputs.Country.Identity
        inputs.Duration
        inputs.Notes
        inputs.SpotRates

    return! Report.generalize visit
  }

let saveVisit connect cancel visit =
  backgroundTask {
    match visit with
    | Fail fault -> return Fail fault
    | Pass visit ->
      let! visitId = visit |> Store.recordVisit connect cancel
      return Report.generalize visitId
  }

let addVisit
  (logger : ILogger<Web>)
  (atlas : AtlasClient)
  (forex : FetchExchangeRates)
  (connect : ConnectToStore)
  (cancel : CancellationToken)
  (request : AddVisitRequest)
  : Task<IResult>
  =
  backgroundTask {
    let! visitId =
      request
      |> makeVisit atlas forex cancel
      |> Task.bind (saveVisit connect cancel)

    let response : IResult =
      match visitId with
      | Demoted(x : exn) ->
        // NOTE: only db calls blindly "trap" exceptions
        logger.LogError(x, "Failed to record visit!")
        TypedResults.Problem("Unable to record visit", statusCode = 500)

      | FailAs(TransportError _ as fault) ->
        // NOTE: a more robust system my retry this a few times before failing
        TypedResults.Problem(string fault, statusCode = 500)

      | FailAs(QuoteIsBase ccy) ->
        // NOTE: this is really a guard against a developer mistake (ie: should maybe be asserted instead)
        TypedResults.BadRequest(
          $"Cannot use same currency ({ccy}) for BASE and QUOTE"
        )

      | Fail fault ->
        // NOTE: could just as easily be a ValidationProblem response
        TypedResults.BadRequest(fault.Message)

      | Pass visitId ->
        let location = Uri($"/visits/{visitId}", UriKind.Relative)
        TypedResults.Created(location)

    return response
  }

let getVisit
  (logger : ILogger<Web>)
  (connect : ConnectToStore)
  (cancel : CancellationToken)
  (visitId : string)
  : Task<IResult>
  =
  backgroundTask {
    let! visit = visitId |> Store.getVisitById connect cancel

    let response : IResult =
      match visit with
      | Fail(DemotedAs(x : exn)) ->
        logger.LogError(x, "Failed to retrieve visit!")
        TypedResults.Problem("Unable to fetch visit.", statusCode = 500)

      | Fail fault -> TypedResults.Problem(fault.Message, statusCode = 500)

      | Pass None -> TypedResults.NotFound()
      | Pass(Some visit) -> TypedResults.Ok(visit)

    return response
  }

let getRecentVisits
  (logger : ILogger<Web>)
  (defaults : Defaults)
  (connect : ConnectToStore)
  (cancel : CancellationToken)
  (count : Nullable<int>)
  : Task<IResult>
  =
  backgroundTask {
    let count =
      // NOTE: Instead of clamping, validation would also be reasonable
      Int32.Clamp(
        count.GetValueOrDefault(defaults.RecentVisitsCount),
        Defaults.MinimumVisits,
        Defaults.MaximumVisits
      )

    let! visits = count |> Store.getRecentVisits connect cancel

    let reponse : IResult =
      match visits with
      | Fail(DemotedAs(x : exn)) ->
        logger.LogError(x, "Failed to retrieve visit!")
        TypedResults.Problem("Unable to fetch visit.", statusCode = 500)

      | Fail fault -> TypedResults.Problem(fault.Message, statusCode = 500)

      | Pass(visits) -> TypedResults.Ok(visits)

    return reponse
  }
