namespace Demo.Atlas

#nowarn 3535 (* because IWSAMs ... see AtlasFault<'Fault> *)

open System
open System.Collections.Generic
open System.Globalization
open System.IO
open System.Net
open System.Net.Http
open System.Reflection
open System.Text.Json
open System.Threading
open System.Threading.Tasks
open HotChocolate.Transport
open HotChocolate.Transport.Http
open MulberryLabs.FaultReport


[<AllowNullLiteral>]
type AtlasCountry() =
  member val Code = "" with get, set
  member val Name = "" with get, set
  member val Currencies : string array = [||] with get, set


type GraphLocation() =
  member val Line = 0 with get, set
  member val Column = 0 with get, set


type GraphError() =
  member val Message = "" with get, set
  member val Location = GraphLocation() with get, set
  member val Path : string array = [||] with get, set


type AtlasFault<'Fault> =
  inherit IFault
  static abstract TransportError :
    status : HttpStatusCode * reason : string -> 'Fault
  static abstract ProtocolError : details : GraphError array -> 'Fault
  static abstract UnknownCountry : countryName : string -> 'Fault


type AtlasClient(http : IHttpClientFactory) =
  static let jsonConfig = JsonSerializerOptions(JsonSerializerDefaults.Web)

  static let query =
    let assembly = Assembly.GetExecutingAssembly()
    let resource =
      "Demo.Atlas.Countries.graphql"
      |> assembly.GetManifestResourceStream
      |> nonNull
    use reader = new StreamReader(resource)
    reader.ReadToEnd()

  let httpClient = http.CreateClient(nameof AtlasClient)
  do httpClient.BaseAddress <- Uri "https://countries.trevorblades.com/"

  let graph = GraphQLHttpClient.Create(httpClient, disposeHttpClient = false)

  let prepInput value =
    value |> CultureInfo.CurrentCulture.TextInfo.ToTitleCase :> obj

  let checkForGraphErrors (json : JsonElement) =
    match json.ValueKind with
    | JsonValueKind.Undefined -> Array.empty
    | _ -> [|
        for e in json.EnumerateArray() do
          match e.Deserialize<GraphError>() with
          | Null -> ()
          | NonNull error -> error
      |]

  member _.FindCountry<'Fault when AtlasFault<'Fault>>
    (name : string, ?cancel : CancellationToken)
    : Task<Report<AtlasCountry, 'Fault>>
    =
    if String.IsNullOrWhiteSpace name then
      nullArg (nameof name)

    backgroundTask {
      let cancel = defaultArg cancel CancellationToken.None
      let variables = Dictionary [ KeyValuePair("name", prepInput name) ]
      let request = OperationRequest(query, variables = variables)

      let! response = graph.GetAsync(request, cancel)
      if response.IsSuccessStatusCode then
        let! result = response.ReadAsResultAsync(cancel)
        let errors = checkForGraphErrors result.Errors

        if 0 = Array.length errors then
          let countries = result.Data.GetProperty("countries").EnumerateArray()
          let country =
            countries
            |> Seq.tryHead
            |> Option.map _.Deserialize<AtlasCountry>(jsonConfig)

          match country with
          | Some country -> return Pass country
          | None -> return Fail('Fault.UnknownCountry name)
        else
          return Fail('Fault.ProtocolError errors)

      else
        let fault =
          'Fault.TransportError(response.StatusCode, string response.ReasonPhrase)
        return Fail fault
    }

  interface IDisposable with
    member _.Dispose() = graph.Dispose()
