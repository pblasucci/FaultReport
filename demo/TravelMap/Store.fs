module Demo.TravelMap.Store

open System.Data
open System.Runtime.ExceptionServices
open Dapper
open Demo.TravelMap.TravelMap
open FsToolkit.ErrorHandling
open Microsoft.VisualBasic
open MulberryLabs.FaultReport
open NodaTime.Text
open pblasucci.Ananoid


let stamp = LocalDatePattern.Iso


module private DDL =
  [<Literal>]
  let DropAll =
    """
    DROP TABLE IF EXISTS visit_rates;
    DROP TABLE IF EXISTS visits;
    DROP TABLE IF EXISTS countries;
    """

  [<Literal>]
  let CountriesTable =
    """
    CREATE TABLE IF NOT EXISTS countries(
      id    INTEGER NOT NULL PRIMARY KEY,
      code  TEXT    NOT NULL, -- ISO-2
      name  TEXT    NOT NULL,

      UNIQUE(code) -- conflicts handled by INSERT
    ) STRICT;
    """

  [<Literal>]
  let VisitsTable =
    """
    CREATE TABLE IF NOT EXISTS visits(
      id          TEXT NOT NULL PRIMARY KEY, -- nanoid
      country_id  INTEGER NOT NULL,
      stay_from   TEXT    NOT NULL, -- ISO-8601
      stay_until  TEXT    NOT NULL, -- ISO-8601
      notes       TEXT,

      FOREIGN KEY (country_id) REFERENCES countries (id),

      UNIQUE(country_id, stay_from, stay_until) ON CONFLICT ROLLBACK
    ) STRICT;
    """

  [<Literal>]
  let VisitRatesTable =
    """
    CREATE TABLE IF NOT EXISTS visit_rates(
      id        INTEGER NOT NULL PRIMARY KEY,
      visit_id  TEXT    NOT NULL, -- nanoid
      base_ccy  TEXT    NOT NULL, -- ISO-3
      quote_ccy TEXT    NOT NULL, -- ISO-3
      average   REAL    NOT NULL,

      FOREIGN KEY (visit_id) REFERENCES visit (id),

      UNIQUE(visit_id, base_ccy, quote_ccy) ON CONFLICT ROLLBACK
    ) STRICT;
    """

let install connect =
  use db : IDbConnection = connect ()
  use tx = db.BeginTransaction()

  db.Execute(DDL.DropAll, tx) |> ignore
  db.Execute(DDL.CountriesTable, tx) |> ignore
  db.Execute(DDL.VisitsTable, tx) |> ignore
  db.Execute(DDL.VisitRatesTable, tx) |> ignore

  tx.Commit()


module private DML =
  [<Literal>]
  let InsertCountry =
    """
    INSERT INTO
      countries(code, name)
      VALUES(@code, @name)
      ON CONFLICT(code) DO UPDATE SET name = excluded.name
    RETURNING id;
    """

  [<Literal>]
  let InsertVisit =
    """
    INSERT INTO
      visits(id, country_id, stay_from, stay_until, notes)
      VALUES(@id, @countryId, @stayFrom, @stayUntil, @notes)
    RETURNING id;
    """

  [<Literal>]
  let InsertVisitRate =
    """
    INSERT INTO
      visit_rates(visit_id, base_ccy, quote_ccy, average)
      VALUES(@visitId, @baseCcy, @quoteCcy, @average);
    """


exception DuplicateVisitException of place : Iso2Country * stay : DateInterval


let recordVisit connect cancel (visit : Visit) =
  backgroundTaskReport {
    use db : IDbConnection = connect ()
    use tx = db.BeginTransaction()

    try
      let insertCountry =
        CommandDefinition(
          DML.InsertCountry,
          {| code = string visit.Place.Code; name = visit.Place.Name |},
          transaction = tx,
          cancellationToken = cancel
        )
      let! countryId = db.ExecuteScalarAsync(insertCountry)

      let insertVisit =
        CommandDefinition(
          DML.InsertVisit,
          {|
            id = NanoId.NewId().ToString()
            countryId = countryId
            stayFrom = stamp.Format visit.Stay.Start
            stayUntil = stamp.Format visit.Stay.End
            notes = Option.toObj visit.Notes
          |},
          transaction = tx,
          cancellationToken = cancel
        )
      let! visitId = db.ExecuteScalarAsync<string>(insertVisit)

      let insertRate pair average =
        CommandDefinition(
          DML.InsertVisitRate,
          {|
            visitId = visitId
            baseCcy = string pair.Base
            quoteCcy = string pair.Quote
            average = average
          |},
          transaction = tx,
          cancellationToken = cancel
        )

      for KeyValue(pair, average) in visit.Rates do
        do! db.ExecuteAsync(insertRate pair average) |> Task.ignore

      tx.Commit()
      return visitId
    with x ->
      // tx.Rollback()
      let x = ExceptionDispatchInfo.Capture(x)
      return! Report.ofExn x.SourceException
  }


module private SQL =
  [<Literal>]
  let SelectVisit =
    """
    SELECT
      v.id
    , c.code
    , c.name
    , v.stay_from
    , v.stay_until
    , v.notes
    , r.base_ccy
    , r.quote_ccy
    , r.average
    FROM visits v
      JOIN countries c on v.country_id = c.id
      JOIN visit_rates r on v.id = r.visit_id
    WHERE v.id = @visitId
    ;
    """

  [<Literal>]
  let SelectRecent =
    """
    SELECT
      v.id
    , v.stay_from
    , v.stay_until
    , c.code
    FROM visits v
      JOIN countries c on v.country_id = c.id
    ORDER BY stay_from DESC, stay_until DESC
    LIMIT @count
    ;
    """

[<CLIMutable>]
type VisitRow = {
  id : string
  code : string
  name : string
  stay_from : string
  stay_until : string
  notes : string
  base_ccy : string
  quote_ccy : string
  average : float
}

let getVisitById connect cancel visitId =
  backgroundTaskReport {
    use db : IDbConnection = connect ()

    try
      let selectVisit =
        CommandDefinition(
          SQL.SelectVisit,
          {| visitId = visitId |},
          cancellationToken = cancel
        )
      let! row = db.QuerySingleOrDefaultAsync<VisitRow>(selectVisit)
      return Option.ofObj row
    with x ->
      return! Report.ofExn x
  }

[<CLIMutable>]
type RecentVisitRow = {
  id : string
  stay_from : string
  stay_until : string
  code : string
}

let getRecentVisits connect cancel count =
  backgroundTaskReport {
    use db : IDbConnection = connect ()

    try
      let recentVisits =
        CommandDefinition(
          SQL.SelectRecent,
          {| count = count |},
          cancellationToken = cancel
        )
      let! rows = db.QueryAsync<RecentVisitRow>(recentVisits)
      return Seq.toList rows
    with x ->
      return! Report.ofExn x
  }
