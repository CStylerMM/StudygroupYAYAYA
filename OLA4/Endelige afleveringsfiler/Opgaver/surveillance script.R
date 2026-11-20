#"surveillance script.R" script

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
library(leaflet)

# Source vores adgangsnøgler
source("util.R")

# Indhente live fly, over boundingboxen
base_url <- "https://opensky-network.org/api"
end_pointS <- "/states/all"

laMin <-52.055129 # Boundingbox koordinater 
laMax <- 56.196869
loMin <- -6.065140
loMax <- 4.305954

full_urlS <- paste0(base_url,end_pointS,"?lamin=",laMin,"&lomin=",loMin,"&lamax=",laMax,"&lomax=",loMax)

token <- getToken()
res_S <- httr::GET(full_urlS,add_headers(Authorization = paste("Bearer", token)))
rescontent_S <- httr::content(res_S, as="text")
resretval_S <- jsonlite::fromJSON(rescontent_S)
friske_fly <- as.data.frame(resretval_S$states)

# Step 3: Navngive kolonnerne - Wulf har trukket det ud et sted, skal findes
ff_col_names <- c("icao24","callsign","origin_country","time_position","last_contact",
                  "longitude","latitude","baro_altitude","on_ground","velocity",
                  "true_track","vertical_rate","sensors","geo_altitude","squawk",
                  "spi","position_source")

colnames(friske_fly) <- ff_col_names


# Undersøge strukturen på datasæt
str(friske_fly)

# Lave tal til numeric, så vi kan bruge dem i beregninger
friske_fly$time_position <- as.numeric(friske_fly$time_position)
friske_fly$last_contact <- as.numeric(friske_fly$last_contact)
friske_fly$longitude <- as.numeric(friske_fly$longitude)
friske_fly$latitude <- as.numeric(friske_fly$latitude)
friske_fly$baro_altitude <- as.numeric(friske_fly$baro_altitude)
friske_fly$velocity <- as.numeric(friske_fly$velocity)
friske_fly$true_track <- as.numeric(friske_fly$true_track)
friske_fly$vertical_rate <- as.numeric(friske_fly$vertical_rate)
friske_fly$geo_altitude <- as.numeric(friske_fly$geo_altitude)
friske_fly$squawk <- as.numeric(friske_fly$squawk)

# Subsette, filtrerer mindre fly fra, samt fly der ikke er i luften
fly_u_små_fly <- friske_fly[friske_fly[ , "velocity"] > 100, ]
fly_u_små_fly <- fly_u_små_fly[fly_u_små_fly[ , "on_ground"] != "TRUE", ]

# Inde i det, laver det loop
token <- getToken()

# Liste med suspekte fly
suspekte_fly <- matrix(NA, 0, 17)
colnames(suspekte_fly) <- colnames(fly_u_små_fly)
end_pointT <- "/tracks/all"

track_names <- c("icao24", "callsign", "startTime", "endTime", "time", "lat",
                 "lng"," alt", "crs", "grd")
fly <- 1
for (fly in 1:nrow(fly_u_små_fly)) {
  # Hente fly og tracks
  icao_circ_test <- fly_u_små_fly[fly,"icao24"]
  full_url_circ_test <- paste0(base_url,end_pointT,"?icao24=",icao_circ_test,"&time=0")
  res_circ_test <- httr::GET(full_url_circ_test,add_headers(Authorization = paste("Bearer", token)))
  rescontent_circ_test <- httr::content(res_circ_test, as="text")
  resretval_circ_test <- jsonlite::fromJSON(rescontent_circ_test)
  test_circJet_df <- as.data.frame(resretval_circ_test)
  colnames(test_circJet_df) <- track_names
  # Korregerer start og slutningen (lette, lande)
  test_circJet_df <- test_circJet_df[test_circJet_df[ ,8] > 3000, ]
  # Målepunkt - netto drejning og kummulativ drejning
  fly_drejning <- test_circJet_df |>
    mutate(d_crs = ((crs - lag(crs) + 180) %% 360) - 180,
           netto_drejning = cumsum(ifelse(is.na(d_crs), 0, d_crs))
    ) #undersøge specifikt hvad det gør, finde bedre navne
  max_drejning <- max(fly_drejning$netto_drejning)
  min_drejning <- min(fly_drejning$netto_drejning)
  er_suspekt <- max_drejning >= 250 | min_drejning <= -250
  if (er_suspekt == TRUE) {
    suspekte_fly <- rbind(fly_u_små_fly[fly, ], suspekte_fly)
  }
}

# konverterer det til en fil, der kan videresendes
library(readr)
write_csv2(suspekte_fly, "suspekte_fly.csv")