##### --- Opgave 3.1 – Hent fly og plot --- ####
# I tager udgangspunkt i den udleverede kode - getCircl.R fra https://github.com/cphstud/DALE25W43 
# samt rds-filerne med cirklende fly. Koden henter en liste af fly ned fra min nordsø-bounding-box 
# og putter icaonumrene i en vektor. Hent nu en ”frisk” liste af fly over Nordsøen. Lav et barplot 
# af antal fly fordelt på lande.

### --- Library brugte pakker --- ###
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
# Source util.R doc
source("util.R")

### --- Hente gamle fly --- ###
# Step 1: Hente training-data
fl=list.files(path = "./train",pattern = "*rds", full.names = T)
collist=lapply(fl,readRDS)

### --- Hente nye fly --- ###
# Step 1: httr kald - hente data fra OpenSky API
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

# Step 2: Navngive kolonnerne - Wulf har trukket det ud et sted, skal findes
ff_col_names <- c("icao24","callsign","origin_country","time_position","last_contact",
                  "longitude","latitude","baro_altitude","on_ground","velocity",
                  "true_track","vertical_rate","sensors","geo_altitude","squawk",
                  "spi","position_source")

colnames(friske_fly) <- ff_col_names

# Step 3: Gruppering af lande
landeGruppering <- friske_fly |> 
  group_by(origin_country) |>
  summarise(friske_fly = n())

colnames(landeGruppering) <- c("Land", "Antal")

# Step 4: Output - Barplot
ggplot(landeGruppering, aes(x = reorder(Land, -Antal), y = Antal)) +
  geom_col(fill = "hotpink3") +
  geom_text(aes(label = Antal), vjust = -0.5, size = 5) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(vjust = 1.5, size = 20),
        title = element_text(size = 25),
        plot.subtitle = element_text(size = 20)) +
  labs(title = "Antal fly over Nordsøen boundingbox",
       subtitle = "United Kingdom havde markant flest fly i boundingboxen over Nordsøen",
       caption = "Kilde: OpenSky Networks API & Egne beregninger",
       x = "Lande",
       y = "Antal fly") +
  scale_y_continuous(
    breaks = seq(0, 200, by = 50),
    limits = c(0, 200))


#### --- Opgave 3.2 – Start small - ét fly --- ####
# I tager udgangspunkt i ét fly og henter nu tracks for det givne fly. 
# Gør det samme for ét af de cirklende fly. Plot nu begge og kommenter.

# Step 1: Hente et gammelt fly track
gammelt_fly_circ <- collist[[3]]

# Step 2: Sample et fly, fra friske fly
icao_sample <- sample(friske_fly[ ,1], size = 1)

# Step 3: httr kald - /tracks/all, med specifikt icao nummer - et fly
end_pointT <- "/tracks/all"
full_urlT <- paste0(base_url,end_pointT,"?icao24=",icao_sample,"&time=0")

# Nyt HTTR kald, med ny URL
token <- getToken()
res_T <- httr::GET(full_urlT,add_headers(Authorization = paste("Bearer", token)))
rescontent_T <- httr::content(res_T, as="text")
resretval_T <- jsonlite::fromJSON(rescontent_T)
icao_sample_flight_track <- as.data.frame(resretval_T)

# Retter colnames
sample_track_col_names <- colnames(gammelt_fly_circ)

colnames(icao_sample_flight_track) <- sample_track_col_names

# Step 4: Plotte begge plots
# Lave plot funktion
myMapPlot <- function(df) {
  leaflet(df) %>% 
    addTiles() %>% 
    addPolylines(lat = ~lat, lng = ~lng)
}

myEDAPlots <- function(df) {
  par(mfcol=c(1,3), ann=F)
  plot(df$lat,df$lng)
  plot(df$crs,df$time)
  plot(df$alt,df$time)
}

# Prøve at plotte
myEDAPlots(icao_sample_flight_track)
myMapPlot(icao_sample_flight_track)

myEDAPlots(gammelt_fly_circ)
myMapPlot(gammelt_fly_circ)


#### --- Opgave 3.3 – Move fast - alle fly --- ####
# I looper nu igennem alle jeres friske fly-icaoer og henter deres respective tracks. 
# I skal sørge for at variablen med kursen kan bruges til at se på ændringer i kursen. 
# Udfyld standdardafvigelsen på kursen

# Step 1: Lave loop, der tager tracks fra de nye fly

ff_længde <- nrow(friske_fly)
loop_liste_FT <- rep(list(rep(list())), ff_længde)
end_pointT <- "/tracks/all"
token <- getToken()

for (fly in 1:nrow(friske_fly)) {
  icao_loop <- friske_fly[fly,"icao24"]
  full_url_loop <- paste0(base_url,end_pointT,"?icao24=",icao_loop,"&time=0")
  res_loop <- httr::GET(full_url_loop,add_headers(Authorization = paste("Bearer", token)))
  res_code <- res_loop$status_code
  if (res_code != 200) {
    failed_res <- c(icao_loop, "on_ground")
    loop_liste_FT[[fly]] <- failed_res
    next
  }
  rescontent_loop <- httr::content(res_loop, as="text")
  resretval_loop <- jsonlite::fromJSON(rescontent_loop)
  loop_liste_FT[[fly]] <- resretval_loop
}

# Sætte standardafvigelsen ind i en tabel med icao
SD_df <- matrix(NA, length(loop_liste_FT), 2)
SD_df_names <- c("icao24", "standard_afvigelse")
colnames(SD_df) <- SD_df_names

for (fly in 1:length(loop_liste_FT)) {
  loop_df <- as.data.frame(loop_liste_FT[[fly]])
  if (ncol(loop_df) != 10) {
    failed_sd <- c(loop_df[1,1], "on_ground")
    SD_df[fly, ] <- failed_sd
    next
  }
  colnames(loop_df) <- sample_track_col_names
  standard_afvigelse <- sd(loop_df$crs)
  data <- c(loop_df[1,1], standard_afvigelse)
  SD_df[fly, ] <- data
}


# Er sd rigtigt i standardafvigelse ^^^
# Noget matematik med Wulf :)) kniv

##### --- Opgave 3.4 – Aim high - jeres egen algoritme --- ####
# I skal nu designe jeres egen algoritme som kan spotte cirklende fly. Jeres resultater 
# skal samles i en dataframe a la nedenstående, hvor mine circklende fly fra rds-filerne 
# er markeret til med isOff=1.

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

# Subsette, filtrerer mindre fly fra samt fly der ikke er i luften
fly_u_små_fly <- friske_fly[friske_fly[ , "velocity"] > 100, ]
fly_u_små_fly <- fly_u_små_fly[fly_u_små_fly[ , "on_ground"] != "TRUE", ]

# Inde i det, laver det loop
token <- getToken()
library(dplyr)

fly_u_små_fly$isCirc <- NA
fly_u_små_fly$isSus <- NA

for (fly in 1:nrow(fly_u_små_fly)) {
  # Hente fly og tracks
  icao_circ_test <- fly_u_små_fly[fly,"icao24"]
  full_url_circ_test <- paste0(base_url,end_pointT,"?icao24=",icao_circ_test,"&time=0")
  res_circ_test <- httr::GET(full_url_circ_test,add_headers(Authorization = paste("Bearer", token)))
  rescontent_circ_test <- httr::content(res_circ_test, as="text")
  resretval_circ_test <- jsonlite::fromJSON(rescontent_circ_test)
  test_circJet_df <- as.data.frame(resretval_circ_test)
  colnames(test_circJet_df) <- sample_track_col_names # bedre universalt navn
  # Korregerer start og slutningen (lette, lande)
  test_circJet_df <- test_circJet_df[test_circJet_df[ , "alt"] > 3000, ]
  # Målepunkt - netto drejning og kummulativ drejning
  fly_drejning <- test_circJet_df |>
    mutate(d_crs = ((crs - lag(crs) + 180) %% 360) - 180,
           netto_drejning = cumsum(ifelse(is.na(d_crs), 0, d_crs))
    ) #undersøge specifikt hvad det gør, finde bedre navne
  max_drejning <- max(fly_drejning$netto_drejning)
  min_drejning <- min(fly_drejning$netto_drejning)
  er_cirklende <- max_drejning >= 360 | min_drejning <= -360
  er_suspekt <- max_drejning >= 250 | min_drejning <= -250
  if (er_suspekt == TRUE) {
    fly_u_små_fly[fly, "isSus"] <- TRUE
  }
  if (er_suspekt == FALSE) {
    fly_u_små_fly[fly, "isSus"] <- FALSE
  }
  if (er_cirklende == TRUE) {
    fly_u_små_fly[fly, "isCirc"] <- TRUE
    next
  }
  fly_u_små_fly[fly, "isCirc"] <- FALSE
}


#### --- Opgave 3.5 – Automate --- ####
# I skal nu beskrive hvordan I vil sætte jeres script til at kunne overvåge et bestemt 
# område og automatisk sende en liste med mistænkelige fly til sigint-20@fe.dk samt gemme 
# listen i en fil. I skal som minimum kunne køre en test på jeres maskine

# Step 1. Lave et survailance script
# Samle delelementerne fra hele opgave 3, der skal til for at en overvågning kan
# køre automatisk. Samt at samle dem
source("surveillance script.R")


# Step 2: Opsætning af e-mail
# Lave en e-mail skabelon klar med relevante input informationer
source("automated email.R")


# Step 3. E-mail trigger
# Vælge og lave en trigger der udløser en e-mail

if (nrow(suspekte_fly) >= 1) {
  smtp(warning_email)
}










