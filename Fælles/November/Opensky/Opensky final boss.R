library(dplyr)
library(rvest)
library(tidyr)
library(stringr)
library(jsonlite)
library(lubridate)
#husk at trykke tav ved source(") fordi så kan du se dine filer, hihi
source("Fælles/November/Opensky/Util.R")
library(ggplot2)
library(sf)
library(rnaturalearth)


# 1) research goal
# Find SASFlights in current states/all
baseurl="https://opensky-network.org/api"

# create url
endpoint="/states/all" #ændrer endpoint, til det jeg vil ha ud.
toturl=str_c(baseurl,endpoint)
toturl

# get my token
token=getToken()


# lav dette til en function i util - getRawFromUrl(url)

getRawFromUrl <- function(url) {
  token <- getToken()
  res <- GET(url, add_headers(Authorization = paste("Bearer", token)))
  stop_for_status(res)
  res
}

#rawres <- getRawFromUrl(toturl)
#rawres=GET(
#  url = toturl,
#  add_headers(Authorization = paste("Bearer", token, sep = " "))
#)

# tjek statuskode
rawres$status_code 
current=content(rawres, as="text")
currentJS=fromJSON(current)
currentDF=currentJS$states
currentDF=as.data.frame(currentDF)

# scrape variables
page=read_html("https://openskynetwork.github.io/opensky-api/rest.html")
tabs=page |> html_elements("table") |> html_table()
vars=tabs[[5]]
colnames(vars)

#lav dette til en vector der kan bruges i colnames
#vars=rename(vars, Property=`3‐Ltr`)
names=unlist(vars$Property)
names=names[1:(length(names)-1)]
saveRDS(names,"vars.rds")


colnames(currentDF)=names

# find operator from callsign
currentDF$cslet=str_extract(currentDF$callsign,"[A-Z]+")

# how many in the air now?

# find list of operators from FAA and get these operators with rvest
airurl="https://www.faa.gov/air_traffic/publications/atpubs/cnt_html/chap3_section_3.html"
page=read_html(airurl)
tabclass=".table.table-scroll"
tabs=page |> html_elements(tabclass) |> html_table()
tabsdf=bind_rows(tabs)
tabsdf=rename(tabsdf, operator=`3‐Ltr`)
saveRDS(tabsdf, "operators.rds")

# join operator with the current df
currwithop=currentDF |> left_join(tabsdf)

# 



# get the airports
airports=read.csv("/Users/vandmelonsguru/Dataanalyse/StudygroupYAYAYA/Fælles/November/Opensky/airports.csv")
colnames(airports)

# get iban countries 
ctrurl="https://www.iban.com/country-codes"
page=read_html(ctrurl)
ctrdf=page |> html_elements("table") |> html_table() |> as.data.frame()
ctrdf=rename(ctrdf, iso_country=Alpha.2.code)

# join with airports
airports = airports |> left_join(ctrdf,by = "iso_country")
unique(airports$type)

# current ts
tsnow=as.integer(Sys.time())-86400
tsthen=tsnow-1*86400
tsnow-tsthen

# find flights from Rome
airportsIT=airports |> filter(str_detect(Alpha.3.code, "ITA")) |> filter(type=="large_airport") |> filter(str_detect(name, "Rom"))
endpoint="/flights/departure"
parameters=str_c("?airport=LIRF&begin=",tsthen,"&end=",tsnow)
toturl=str_c(baseurl,endpoint,parameters)

romedep=GET(
  url = toturl,
  add_headers(Authorization = paste("Bearer", token, sep = " "))
)

romedep$status_code
romejs=content(romedep,as="parsed")
romedf=bind_rows(romejs)


# fly over den persiske golf
# one plane
icao24="4ba9c5"
icao24="706071"
endpoint="/tracks/all"
tykpar=str_c("?icao24=",icao24,"&time=0")
url=str_c(baseurl,endpoint,tykpar)
url
token=getToken()
trackTYK=GET(
  url=url,
  add_headers(Authorization = paste("Bearer", token, sep = " "))
)

trackTYK$status_code
trackTYK$status_code
tyk2=content(trackTYK,as="text")
tykdf2=as.data.frame(fromJSON(tyk2, flatten = T))
colnv=c("icao24","callsign","startTime","endTime","time","lat","lng","alt","crs","grd")
colnames(tykdf2)=colnv

# circling in high alt
summary(tykdf2$alt)
tykdfH2=tykdf2 |> filter(alt > 6000)
plot(tykdf$lat,tykdf$alt)
plot(tykdfH2$time,tykdfH2$crs)

sd(tykdfH$crs)
sd(tykdfH2$crs)


world <- ne_countries(scale = "medium", returnclass = "sf")
ggplot() +
  geom_sf(data = world, fill = "grey90", color = "white") +
  geom_point(data = tykdf,
             aes(x = lng, y = lat),
             color = "red",
             size = 1,
             alpha = 0.5) +
  coord_sf(
    xlim = range(tykdf$lng, na.rm = TRUE) + c(-1, 1),
    ylim = range(tykdf$lat, na.rm = TRUE) + c(-1, 1)
  ) +
  theme_minimal()
