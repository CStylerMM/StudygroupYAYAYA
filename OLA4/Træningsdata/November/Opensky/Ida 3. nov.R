# ---------- Idas script 3. nov ----------
library(dplyr)
library(rvest)
library(tidyr)
library(stringr)
library(jsonlite)
library(lubridate)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(httr)

# Source Util doc
source("Util.R")


# 1) research goal
# Find SASFlights in current states/all
baseurl <- "https://opensky-network.org/api"

# create url
endpoint="/states/all"
toturl=str_c(baseurl,endpoint)

# get my token
token=getToken()

# lav dette til en function i util - getRawFromUrl(url)
# Funktion skrevet i Util.R
source("Util.R")

test1 <- rawres(toturl) 

# Tjekke statuskode, lave det til et DF
test1$status_code
test1$content
rawtext <- httr::content(test1, as = "text")
myJason <- fromJSON(rawtext, flatten = T)
dfFlights <- myJason$states


getRawFromUrl()



# scrape variables
page <- read_html("https://openskynetwork.github.io/opensky-api/rest.html")
tabs <- page |> html_elements("table") |> html_table()
vars <- tabs[[5]]
colnames(vars)
vars$Property

# DfFlights 2
varsPath <- tabs[[18]]
df2Names <- varsPath$Property


#lav dette til en vector der kan bruges i colnames
#vars=rename(vars, Property=`3‐Ltr`)
names <- unlist(vars$Property)
names <- names[1:(length(names)-1)]
saveRDS(names,"vars.rds")
myNames <- readRDS("vars.rds")

colnames(dfFlights) <- names

# Loop 10 fly, finde icao nummer og slå tracks op
sub10fly <- dfFlights[1:10, ]
View(sub10fly)

# https://opensky-network.org/api/tracks/all?icao24=3c4b26&time=0
# https://opensky-network.org/api/tracks/all?icao24=39de4e&time=0


baseurl
endpointTrack <- "/tracks/all?"
param <- "icao24"
paramTime <- "&time=0"

totUrl <- str_c(baseurl,endpointTrack,param,"=",paramTime)

for (i in 1:nrow(sub10fly)) {
  i = 2
  icaoId <- (sub10fly[i, "icao24"]) # Hvorfor ikke $icao24 ???
  totUrl <- str_c(baseurl,endpointTrack,param,"=",icaoId,paramTime)
  test2 <- rawres(totUrl) 
  test2$status_code
  rawtext2 <- content(test2, as = "text")
  myJason2 <- fromJSON(rawtext2, flatten = T)
  dfFlights2 <- myJason2$path
  colnames(dfFlights2) <- df2Names
}

View(dfFlights2)
saveRDS(df2Names,"vars2.rds")

dfFlights2 <- as.data.frame(dfFlights2)

plot(dfFlights2$latitude, dfFlights2$longitude)
plot(dfFlights2$time, dfFlights2$baro_altitude)
plot(dfFlights2$time, dfFlights2$true_track)