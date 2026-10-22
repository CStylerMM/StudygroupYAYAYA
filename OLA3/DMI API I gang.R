library(httr)
library(jsonlite)
library(dplyr)

# Hjælpefunktion: endpoint + parametre -> dataframe
hent_dmi <- function(endpoint, ...) {
  res <- GET(
    paste0("https://opendataapi.dmi.dk/v2/metObs/collections", endpoint),
    query = list(...)
  )
  stop_for_status(res)                       # fejler tydeligt hvis ikke 200
  data <- fromJSON(content(res, as = "text", encoding = "UTF-8"))
  as_tibble(data$features$properties)
}

# Rå observationer: temperatur hver 10. min, Landbohøjskolen
obs <- hent_dmi(
  "metObs/collections/observation/items",
  stationId   = "06186",
  parameterId = "temp_dry",
  datetime    = "2025-01-01T00:00:00Z/2025-12-31T23:59:59Z",
  limit       = 300000
) |>
  mutate(observed = as.POSIXct(observed, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))

# Klimadata, månedsniveau
klima <- hent_dmi(
  "climateData/collections/stationValue/items",
  stationId      = "06186",
  parameterId    = "mean_temp",
  timeResolution = "month",
  datetime       = "2010-01-01T00:00:00Z/2025-12-31T23:59:59Z",
  limit          = 300000
)

# Aktive stationer
stationer <- hent_dmi("metObs/collections/station/items", status = "Active")






#vi ikan også hive api ned fra SMK
library(httr2)
res <- request("https://api.smk.dk/api/v1/art/search/") |>
  req_url_query(keys = "hammershøi", rows = 10, lang = "da") |>
  req_perform()

resp_status(res)                  # 200
resp_content_type(res)            # "application/json"
dat <- resp_body_json(res)
dat$found
sapply(dat$items, \(x) x$titles[[1]]$title)


#lad os finde de der api'er fra SMK
library(httr2)
library(tidyverse)

base_url <- "https://api.smk.dk/api/v1/art/"

# ---- Funktion: hent ét værk på object_number ----
getArtFromSMK <- function(object_number, lang = "da") {
  res <- request(base_url) |>
    req_url_query(object_number = object_number, lang = lang) |>
    req_perform()
  
  stopifnot(resp_status(res) == 200)
  
  items <- resp_body_json(res)$items
  if (length(items) == 0) stop("Intet værk fundet: tjek object_number")
  
  items[[1]]        # ét værk som nested liste
}

v <- getArtFromSMK("KKS5261")

# ---- Pluk de felter du vil bruge ----
v$titles[[1]]$title            # "Augustus og den tiburtinske sibylle"
v$artist[[1]]                  # "Antonio da Trento"
v$techniques[[1]]              # "Clairobscur-træsnit trykt med to stokke"
v$production_date[[1]]$period  # "1500-1550"
v$has_image                    # TRUE
v$image_thumbnail              # lille billede
v$image_native                 # fuld opløsning



library(httr2)
library(tidyverse)

bikesIn <- function(network_id) {
  res <- request("https://api.citybik.es/v2/networks/") |>
    req_url_path_append(network_id) |>
    req_perform()
  stopifnot(resp_status(res) == 200)
  
  stations <- resp_body_json(res)$network$stations
  
  map_dfr(stations, \(s) tibble(
    navn        = s$name,
    free_bikes  = s$free_bikes  %||% 0,
    empty_slots = s$empty_slots %||% 0
  ))
}

dublin <- bikesIn("dublinbikes")
berlin <- bikesIn("nextbike-berlin")

sum(dublin$free_bikes)   # ledige cykler i Dublin
sum(berlin$free_bikes)   # ledige cykler i Berlins nextbike








