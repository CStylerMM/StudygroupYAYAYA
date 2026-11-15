#Opgave 3 - OpenSky
#Vi genbruger vores egne funktioner fra november: getToken ligger i Util.R
#Kør fra projektmappen (StudygroupYAYAYA), ellers passer stierne ikke.

library(httr)
library(jsonlite)
source("OLA4/Træningsdata/November/Opensky/Util.R")  #getToken() - secret ligger i .Renviron

baseurl <- "https://opensky-network.org/api"

#Nordsø-boksen fra den udleverede kode
lamin <- 52.055129
lamax <- 56.196869
lomin <- -6.065140
lomax <- 4.305954

####------ 3.1 Hent fly og plot ------####
token <- getToken()

url_states <- paste0(baseurl, "/states/all?lamin=", lamin, "&lomin=", lomin,
                     "&lamax=", lamax, "&lomax=", lomax)

res <- GET(url_states, add_headers(Authorization = paste("Bearer", token)))
res$status_code  #200 ellers er token/nettet galt

df_states <- as.data.frame(fromJSON(content(res, as = "text", encoding = "UTF-8"))$states)
#kolonnenavne fra OpenSky-dokumentationen (states/all)
colnames(df_states)[1:17] <- c("icao24", "callsign", "origin_country", "time_position",
                               "last_contact", "lng", "lat", "baro_altitude", "on_ground",
                               "velocity", "true_track", "vertical_rate", "sensors",
                               "geo_altitude", "squawk", "spi", "position_source")
nrow(df_states)  #vores friske liste af fly over Nordsøen

icaov <- df_states$icao24  #icao-numrene i en vektor, som opgaven siger

#barplot: antal fly fordelt på lande
library(dplyr)
library(ggplot2)
df_lande <- df_states %>%
  count(origin_country, sort = TRUE)

ggplot(df_lande, aes(x = reorder(origin_country, n), y = n)) +
  geom_col() +
  coord_flip() +  #vandrette søjler så landenavnene kan læses
  theme_classic() +
  labs(title = "Fly over Nordsøen lige nu, fordelt på land",
       x = NULL, y = "Antal fly",
       caption = "Flest fly er registreret i UK og Irland - boksen ligger jo også op ad dem. Kilde: OpenSky /states/all")
## ret caption-konklusionen til det, DIT plot faktisk viser, når du har kørt det

####------ 3.2 Start small - ét fly ------####
#track for ét fly - samme kolonner som vores trænings-rds'er
colnv <- c("icao24", "callsign", "startTime", "endTime", "time", "lat", "lng", "alt", "crs", "grd")

hentTrack <- function(icao, token) {
  url <- paste0(baseurl, "/tracks/all?icao24=", icao, "&time=0")
  res <- GET(url, add_headers(Authorization = paste("Bearer", token)))
  if (res$status_code != 200) return(NULL)  #nogle fly har ingen track, så NULL i stedet for fejl
  js <- fromJSON(content(res, as = "text", encoding = "UTF-8"))
  if (is.null(js$path) || NROW(js$path) < 2) return(NULL)
  df <- data.frame(icao24 = js$icao24,
                   callsign = ifelse(is.null(js$callsign), NA, js$callsign),
                   startTime = js$startTime, endTime = js$endTime, js$path)
  colnames(df) <- colnv
  df
}

df_frisk <- hentTrack(icaov[1], token)  #ét frisk fly - prøv icaov[2] hvis den giver NULL

#ét af de cirklende fly fra rds-filerne
fl <- list.files("OLA4/Træningsdata/November/Opensky/Training", pattern = "\\.rds$", full.names = TRUE)
df_cirkel <- readRDS(fl[1])

#plot begge - et fly der flyver ligeud vs et der cirkler
ggplot(df_frisk, aes(x = lng, y = lat)) +
  geom_path() +
  theme_classic() +
  labs(title = "Normalt fly: ruten er en nogenlunde lige streg",
       caption = paste("Flyet", df_frisk$icao24[1], "flyver fra A til B. Kilde: OpenSky /tracks/all"))

ggplot(df_cirkel, aes(x = lng, y = lat)) +
  geom_path() +
  theme_classic() +
  labs(title = "Cirklende fly: ruten snor sig rundt om sig selv",
       caption = paste("Træningsflyet", df_cirkel$icao24[1], "cirkler - det er dem, algoritmen skal fange. Kilde: rds-fil fra Wulf"))

#KOMMENTAR til rapporten: det normale fly har en rolig kurs der næsten ikke ændrer
#sig, det cirklende flys kurs kører hele vejen rundt 0-360 igen og igen.
#Det er præcis den forskel, vi måler på i 3.3 og 3.4.

####------ 3.3 Move fast - alle fly ------####
#vi looper igennem alle friske icaoer og regner standardafvigelsen på kursen.
#crs er grader 0-360, og det driller: 359 og 1 er næsten samme retning, men sd()
#synes de ligger langt fra hinanden. Vi bruger den alligevel (det er den, opgaven
#beder om) - og i 3.4 laver vi vores EGEN algoritme, der håndterer problemet.

beregn_sdcourse <- function(df) {
  df <- df[df$grd == FALSE & !is.na(df$crs), ]  #kun i luften, kun rækker med kurs
  if (nrow(df) < 5) return(NA)  #for få punkter til at sige noget
  sd(df$crs)
}

####------ 3.4 Aim high - vores egen algoritme ------####
#Vores egen idé (fra Getcirc.R): kig på samlet NETTO-drejning i stedet for sd.
#Et fly der cirkler drejer hele tiden samme vej rundt, så summen af
#kursændringerne bliver kæmpestor. Et normalt fly svinger lidt frem og
#tilbage, så det summer til næsten nul.
netto_drejning <- function(crs) {
  crs <- crs[!is.na(crs)]
  d <- diff(crs)
  d <- (d + 180) %% 360 - 180  #kursændring pakket til -180..180, så nord-springet forsvinder
  abs(sum(d))
}

GRÆNSE <- 250  #kalibreret på træningsflyene i Getcirc.R: deres laveste vindue lå på 273

#lmres: lineær regression lat ~ lng. Flyver man ligeud, ligger punkterne på en
#linje og R2 er tæt på 1. Cirkler man, er der ingen linje og R2 er lav.
beregn_lmres <- function(df) {
  df <- df[!is.na(df$lat) & !is.na(df$lng), ]
  if (nrow(df) < 5) return(NA)
  summary(lm(lat ~ lng, data = df))$r.squared
}

#én række pr. fly, præcis som opgavens dataframe: icao, sdcourse, lmres, myOwnAlg, isOff
beregn_fly <- function(df, isOff) {
  df_luft <- df[df$grd == FALSE & !is.na(df$crs), ]
  if (nrow(df_luft) < 5) return(NULL)
  data.frame(
    icao = df_luft$icao24[1],
    sdcourse = sd(df_luft$crs),
    lmres = beregn_lmres(df_luft),
    myOwnAlg = ifelse(netto_drejning(df_luft$crs) >= GRÆNSE, 1, 0),
    isOff = isOff
  )
}

#friske fly: hent track for hver icao (tager et par minutter, der er pause i)
liste_friske <- list()
for (ic in icaov) {
  Sys.sleep(1)  #skån rate limit, ellers bliver OpenSky sur
  df_track <- hentTrack(ic, token)
  if (!is.null(df_track)) liste_friske[[ic]] <- beregn_fly(df_track, isOff = 0)
  cat(ic, "\n")  #så vi kan se, den lever
}

#de cirklende træningsfly fra rds-filerne: isOff = 1, som opgaven siger
liste_cirkler <- lapply(fl, function(f) beregn_fly(readRDS(f), isOff = 1))

df_resultat <- bind_rows(liste_friske, liste_cirkler)
df_resultat  #samme opbygning som skærmbilledet i opgaven

saveRDS(df_resultat, "OLA4/Opgaverne/df_resultat_opg3.rds")

#hvor god er algoritmen? rækker = facit (isOff), kolonner = vores gæt
table(facit = df_resultat$isOff, vores_gæt = df_resultat$myOwnAlg)
## fanger den alle 8 cirkler? flagger den friske fly ved en fejl? skriv tallene i rapporten,
## også hvis de er skidt - og justér GRÆNSE hvis der er for mange falske alarmer

####------ 3.5 Automate ------####
#Opgaven: BESKRIV hvordan scriptet kan overvåge et område og automatisk sende
#listen til sigint-20@fe.dk + gemme den i en fil. Minimum: en test på egen maskine.

overvåg <- function() {
  token <- getToken()
  res <- GET(url_states, add_headers(Authorization = paste("Bearer", token)))
  df_states <- as.data.frame(fromJSON(content(res, as = "text", encoding = "UTF-8"))$states)
  icaov <- df_states$V1

  liste <- list()
  for (ic in head(icaov, 30)) {  #30 fly pr. kørsel, så vi ikke brænder API-kreditterne af
    Sys.sleep(1)
    df_track <- hentTrack(ic, token)
    if (!is.null(df_track)) liste[[ic]] <- beregn_fly(df_track, isOff = NA)
  }
  df_scan <- bind_rows(liste)
  df_mistænkte <- df_scan %>%
    filter(myOwnAlg == 1)

  #gem listen i en fil med tidspunkt i navnet
  fil <- paste0("OLA4/Opgaverne/mistænkte_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv")
  write.csv(df_mistænkte, fil, row.names = FALSE)
  cat("Gemt:", fil, "-", nrow(df_mistænkte), "mistænkelige fly\n")
  df_mistænkte
}

overvåg()  #<- det her ER testen på egen maskine, som opgaven kræver

#TIL RAPPORTEN - sådan ville vi sætte det op i drift:
#1) Scriptet gemmes som overvaag.R og køres automatisk hvert 15. minut med
#   cronR-pakken på Mac (cronR::cron_add) eller taskscheduleR på Windows.
#2) Mailen til sigint-20@fe.dk sendes med blastula-pakken: compose_email() med
#   csv-listen i kroppen og smtp_send() med en mailkonto, hvis kode ligger
#   sikkert i .Renviron - aldrig i scriptet.
#3) Hver kørsel logger til csv-filerne, så man bagefter kan se, hvornår et fly
#   første gang blev opdaget.
