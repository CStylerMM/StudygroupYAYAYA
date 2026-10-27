#### Opgave 5: Vind fra DMI ####

library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)

#Test af API'et: vindstyrke i Århus Syd 19. november 2022
baseurl <- "https://opendataapi.dmi.dk/v2/metObs/collections/observation/items?"
res <- GET(paste0(baseurl, "stationId=06074&datetime=2022-11-19T00:00:00Z/2022-11-20T00:00:00Z&parameterId=wind_speed_past1h"))
res$status_code   #200 betyder OK
resdf <- fromJSON(httr::content(res, as = "text", encoding = "UTF-8"))
resdf$numberReturned

windforceparam <- "wind_speed_past1h"
windirparam    <- "wind_dir_past1h"
sid       <- "06074"   #Århus Syd
sidanholt <- "06079"   #Anholt Havn

#Henter én parameter for én station og laver tidspunkterne om til tid
getdatafromdmi <- function(param, station, frd, tod) {
  varurl <- paste0("parameterId=", param, "&stationId=", station, "&datetime=", frd, "/", tod, "&limit=10000")
  res <- GET(paste0(baseurl, varurl))
  resdf <- fromJSON(httr::content(res, as = "text", encoding = "UTF-8"))
  obsdf <- resdf$features$properties
  windspeeddf <- obsdf[, c("observed", "value")]
  windspeeddf <- arrange(windspeeddf, observed)
  windspeeddf$observed <- as.POSIXct(windspeeddf$observed, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  return(windspeeddf)
}

#Styrke og retning for én station i en periode, samlet i én tabel
hent_vind <- function(station, fd, td) {
  styrke  <- getdatafromdmi(windforceparam, station, fd, td)
  retning <- getdatafromdmi(windirparam, station, fd, td)
  stopifnot(nrow(styrke) == nrow(retning))   #stopper, hvis antallet ikke passer
  data.frame(observed = styrke$observed, force = styrke$value, dir = retning$value)
}

#Stormen i november 2022
totalwind       <- hent_vind(sid, "2022-11-14T00:00:00Z", "2022-11-21T00:00:00Z")
totalwindanholt <- hent_vind(sidanholt, "2022-11-14T00:00:00Z", "2022-11-21T00:00:00Z")

ggplot(totalwind, aes(x = observed, y = force)) +
  geom_line(color = "red") +
  geom_line(data = totalwindanholt, aes(x = observed, y = force), color = "blue") +
  theme_classic()

#Hvilken retning kom vinden fra? dir er den retning vinden kommer FRA
#Fire sektorer: N, Ø, S, V. Vi lægger 45 grader til, så N går fra 315 til 45
retning_navne <- c("N", "Ø", "S", "V")
sektor <- function(d) {
  cut((d + 45) %% 360, breaks = c(0, 90, 180, 270, 360), labels = retning_navne, right = FALSE)
}
table(sektor(totalwind$dir))
table(sektor(totalwindanholt$dir))

#Stormen Babet, oktober 2023
babet_aarhus <- hent_vind(sid, "2023-10-19T00:00:00Z", "2023-10-23T00:00:00Z")
babet_anholt <- hent_vind(sidanholt, "2023-10-19T00:00:00Z", "2023-10-23T00:00:00Z")

table(sektor(babet_aarhus$dir))
table(sektor(babet_anholt$dir))

max(babet_aarhus$force, na.rm = TRUE)
babet_aarhus$observed[which.max(babet_aarhus$force)]
max(babet_anholt$force, na.rm = TRUE)
babet_anholt$observed[which.max(babet_anholt$force)]

#Graf med styrke og retning. Pilene peger dér hvor vinden blæser hen
babet_aarhus$station <- "Århus Syd"
babet_anholt$station <- "Anholt Havn"
begge <- rbind(babet_aarhus, babet_anholt)

#En pil for hver 3. time, for hver station for sig
pile <- begge %>%
  group_by(station) %>%
  filter(row_number() %% 3 == 1) %>%
  ungroup()

ggplot(begge, aes(x = observed, y = force, color = station)) +
  geom_line() +
  geom_text(data = pile, aes(angle = -90 - dir), label = "→", size = 9, show.legend = FALSE) +
  scale_color_manual(values = c("Anholt Havn" = "blue", "Århus Syd" = "red")) +
  labs(title = "Stormen Babet, oktober 2023",
       x = "Tid (UTC)", y = "Vindhastighed (m/s)", color = "Station") +
  theme_classic()