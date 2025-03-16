module TravelMap.Program

open System
open System.Data
open System.Data.SQLite
open System.IO

open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Options
open MulberryLabs.FaultReport
open NodaTime

open Demo.Atlas
open Demo.ForEx
open Demo.TravelMap

open type NodaTime.DateTimeZoneProviders


let ensureTimeZone (config : IConfiguration) =
  match config.GetValue "EffectiveTimeZone" with
  | Null
  | NonNull Empty -> Tzdb.GetSystemDefault()
  | NonNull(Trimmed tzId) ->
    match Tzdb.GetZoneOrNull tzId with
    | Null -> ``panic!`` "Unable to determine time zone!"
    | NonNull zone -> zone

let ensureDatabase (config : IConfiguration) =
  let connection =
    SQLiteConnectionStringBuilder(
      Version = 3,
      JournalMode = SQLiteJournalModeEnum.Wal,
      DataSource =
        match config.GetValue("StoragePath") with
        | Null
        | NonNull Empty -> ``panic!`` "A physical database path is required!"
        | NonNull value -> value
    )

  let connect () : IDbConnection =
    (new SQLiteConnection(string connection)).OpenAndReturn()

  if not (File.Exists connection.DataSource) then
    let folder =
      match Path.GetDirectoryName connection.DataSource with
      | Null
      | NonNull Empty -> Environment.CurrentDirectory
      | NonNull(Trimmed folder) -> folder

    if not (Directory.Exists folder) then
      Directory.CreateDirectory(folder) |> ignore

    Store.install connect

  (connect : ConnectToStore)

let configureServices (builder : WebApplicationBuilder) =
  // Configuration
  builder.Services
    .AddOptions<Web.Defaults>()
    .BindConfiguration(Web.Defaults.Key)
    .ValidateDataAnnotations()
    .ValidateOnStart()
  |> ignore

  builder.Services.AddSingleton<Web.Defaults>(fun provider ->
    provider.GetRequiredService<IOptions<_>>().Value
  )
  |> ignore

  // Calendaring
  builder.Services
    .AddSingleton(ensureTimeZone builder.Configuration)
    .AddSingleton<ZonedClock>(fun provider ->
      ZonedClock(
        SystemClock.Instance,
        provider.GetRequiredService<_>(),
        CalendarSystem.Iso
      )
    )
    .AddSingleton<IClock>(fun provider ->
      provider.GetRequiredService<ZonedClock>() :> IClock
    )
  |> ignore

  // Database
  builder.Services.AddSingleton<ConnectToStore>(
    ensureDatabase builder.Configuration
  )
  |> ignore

  // Atlas
  builder.Services.AddHttpClient() |> ignore
  builder.Services.AddScoped<AtlasClient>() |> ignore

  // ForEx
  builder.Services.AddSingleton<FetchExchangeRates>(ForEx.lookupExchangeRates)
  |> ignore

  // facilitate chaining
  builder

let configureApplication (webApp : WebApplication) =
  webApp.MapPost("/visits", Func<_, _, _, _, _, _, _>(Web.addVisit)) |> ignore

  webApp.MapGet(
    "/visits",
    Func<_, _, _, _, _, _>(fun logger defaults connect cancel count ->
      // NOTE: η-reduction will cause query-string binding to fail
      Web.getRecentVisits logger defaults connect cancel count
    )
  )
  |> ignore

  webApp.MapGet(
    "/visits/{visitId}",
    Func<_, _, _, _, _>(fun logger connect cancel visitId ->
      // NOTE: η-reduction will cause URL-path binding to fail
      Web.getVisit logger connect cancel visitId
    )
  )
  |> ignore

  // facilitate chaining
  webApp


[<EntryPoint>]
let main args =
  args
  |> WebApplication.CreateBuilder
  |> configureServices
  |> _.Build()
  |> configureApplication
  |> _.Run()

  0 // SUCCESS!!
