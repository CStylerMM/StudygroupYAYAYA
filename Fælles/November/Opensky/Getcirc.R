# getCircl.R - samlet ud fra skaermbilleder af Wulfs RStudio (DAL24Circ)
# Funktionerne er flyttet op, saa filen kan koeres oppefra. Wulf koerer linje for linje.
# Client secret og client id ligger i util.R og er ikke med her.

library(jsonlite)
library(httr)
library(stringr)
library(leaflet)
source("Fælles/November/Opensky/Util.R")                       # getToken()

baseurl = "https://opensky-network.org/api"

endpointS = "/states/all"
endpointT = "/tracks/all"

queryS = paste0(baseurl, endpointS)
queryT = paste0(baseurl, endpointT)

# hjaelpefunktioner
myMapPlot <- function(df) {
  leaflet(df) %>%
    addTiles() %>%
    addPolylines(lat = ~lat, lng = ~lng)
}

myEDAPlots <- function(df) {
  par(mfcol = c(1, 3), ann = F)
  plot(df$lat, df$lng)
  plot(df$crs, df$time)
  plot(df$alt, df$time)
}

#### NORTH SEA ####
lamin = 52.055129
lamax = 56.196869
lomin = -6.065140
lomax = 4.305954

fullurl = paste0(baseurl, endpointS, "?lamin=", lamin, "&lomin=", lomin, "&lamax=", lamax, "&lomax=", lomax)

# one take over NS (skateboard)
token = getToken()
res <- httr::GET(fullurl, add_headers(Authorization = paste("Bearer", token)))
rescontent <- httr::content(res, as = "text")
resretval <- jsonlite::fromJSON(rescontent)
statedfpr <- as.data.frame(resretval$states)

# find all icaos and put in vector
icaov = statedfpr$V1

# find all tracks for each icao (her kun ét fly)
ictest = icaov[[6]]
token = getToken()
turl = paste0(baseurl, endpointT, "?icao24=", ictest, "&time=0")
res <- httr::GET(turl, add_headers(Authorization = paste("Bearer", token)))
res$status_code
resretval <- fromJSON(content(res, as = "text"))
statedf <- as.data.frame(resretval)
colnv = c("icao24", "callsign", "startTime", "endTime", "time", "lat", "lng", "alt", "crs", "grd")
colnames(statedf) = colnv
myEDAPlots(statedf)

# load training-data
fl = list.files(path = "./train", pattern = "*rds", full.names = T)
collist = lapply(fl, readRDS)
testdf = collist[[4]]
testdf = readRDS("circMT.rds")

# plots to explore data
myEDAPlots(testdf)
# map-plot to verify flight
myMapPlot(testdf)

# apply circmetrics
# lm
cmodel = lm(lat ~ lng + crs + alt, data = testdf)
nmodel = lm(lat ~ lng + crs + alt, data = statedf)
summary(cmodel)
summary(nmodel)
# sd
csd = sd(testdf$crs)
nsd = sd(statedf$crs)

# myOwnDetectionAlg