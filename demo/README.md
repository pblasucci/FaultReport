Travel Map
===

External library: https://countries.trevorblades.com/
+ validate country
+ get currency codes for country

External library: https://data-api.ecb.europa.eu/service/data/EXR/D.{???+???}.EUR.SP00.A?startPeriod={yyyy-MM-dd}&endPeriod={yyyy-MM-dd}&detail=dataonly&format=csvdata
+ get exchange rates for given currency codes and date range

Web App: Record places you've visited
+ on start up
  + initialized timezone (or blow up)
  + initialized new or existing database (or blow up)
+ user POSTs country name, dates of visit, and optional comment
  + validate dates
  + validate country name
  + dereference country name to currency codes
  + get averages for exchange rates
  + insert record into database
