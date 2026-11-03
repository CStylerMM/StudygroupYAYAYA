# OLA4 opgave 3 - OpenSky (opgave 3.1 til 3.4)
# Kør oppefra og ned. Working directory: StudygroupYAYAYA.
# Kræver:
#  - Util.R med getToken(). Secret ligger i .Renviron som OPENSKY_SECRET, aldrig i koden.
#  - rds-filerne med de cirklende fly i Fælles/November/Opensky/Training

library(jsonlite)
library(httr)
library(stringr)
library(leaflet)
source("Fælles/November/Opensky/Util.R")   # getToken()

# ---------- Opsætning (som i getCircl.R) ----------
baseurl   <- "https://opensky-network.org/api"
endpointS <- "/states/all"
endpointT <- "/tracks/all"

# Nordsøen (Wulfs bounding box)
lamin <- 52.055129
lamax <- 56.196869
lomin <- -6.065140
lomax <- 4.305954
fullurl <- paste0(baseurl, endpointS, "?lamin=", lamin, "&lomin=", lomin,
                  "&lamax=", lamax, "&lomax=", lomax)

colnv   <- c("icao24", "callsign", "startTime", "endTime", "time", "lat", "lng", "alt", "crs", "grd")
GRAENSE <- 250     # drejning (grader) over denne værdi regnes som cirkling. Foreløbigt, se kalibreringen.
maxfly  <- 40      # hvor mange friske fly der hentes track for (ca. 1 sek. pr. fly)

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

# ---------- Hjælpefunktioner ----------

# Samlet drejning i én retning, i grader. 360 = én hel cirkel.
# (sd() er dårlig til vinkler: 359 og 1 grad ligger tæt, men sd ser dem langt fra hinanden)
netto_drejning <- function(crs) {
  crs <- crs[!is.na(crs)]
  d <- diff(crs)
  d <- (d + 180) %% 360 - 180      # kursændring mellem -180 og 180, ingen spring ved nord
  abs(sum(d))
}

# Det vindue (sekunder) i et helt track, hvor drejningen er størst
bedste_vindue <- function(df, vindue = 900) {
  df <- df[df$grd == FALSE, ]
  df <- df[order(df$time), ]
  best <- c(drejning = 0, alt = NA, lat = NA, lng = NA, punkter = NA)
  for (i in seq_len(nrow(df))) {
    w <- df[df$time >= df$time[i] & df$time <= df$time[i] + vindue, ]
    if (nrow(w) >= 5) {
      s <- netto_drejning(w$crs)
      if (s > best["drejning"]) {
        best <- c(drejning = s, alt = median(w$alt, na.rm = TRUE),
                  lat = median(w$lat), lng = median(w$lng), punkter = nrow(w))
      }
    }
  }
  best
}

# Track for ét fly (den aktuelle flyvning). callsign kan være null, så vi bygger data framen selv.
hentTrack <- function(icao, token) {
  turl <- paste0(baseurl, endpointT, "?icao24=", icao, "&time=0")
  res  <- httr::GET(turl, add_headers(Authorization = paste("Bearer", token)))
  if (res$status_code != 200) return(NULL)
  js <- fromJSON(content(res, as = "text", encoding = "UTF-8"))
  if (is.null(js$path) || NROW(js$path) < 2) return(NULL)
  df <- data.frame(icao24    = js$icao24,
                   callsign  = if (is.null(js$callsign)) NA_character_ else js$callsign,
                   startTime = js$startTime,
                   endTime   = js$endTime,
                   js$path)
  colnames(df) <- colnv
  df
}

# Alle fly i boksen lige nu (states/all)
hentStates <- function(token) {
  res <- httr::GET(fullurl, add_headers(Authorization = paste("Bearer", token)))
  if (res$status_code != 200) return(NULL)
  states <- fromJSON(content(res, as = "text", encoding = "UTF-8"))$states
  if (is.null(states)) return(NULL)
  as.data.frame(states)
}

# Nøgletal for ét track: de fire kolonner opgaven vil have (sdcourse, lmres, myOwnAlg) plus drejning
metrics <- function(d, graense = GRAENSE) {
  ic <- d$icao24[1]
  d  <- d[d$grd == FALSE, ]
  m  <- tryCatch(lm(lat ~ lng + crs + alt, data = d), error = function(e) NULL)
  bv <- bedste_vindue(d)["drejning"]
  data.frame(icao     = ic,
             sdcourse = sd(d$crs, na.rm = TRUE),                  # Wulfs mål: standardafvigelse på kursen
             lmres    = if (is.null(m)) NA_real_ else sigma(m),   # residual standard error fra lm
             myOwnAlg = as.integer(bv >= graense),                # vores mål: drejning i bedste 15 minutter
             drejning = unname(bv))
}

# ---------- 3.1 Hent fly og plot ----------
token <- getToken()
fersk <- hentStates(token)                 # frisk liste af fly i Nordsø-boksen
stopifnot(!is.null(fersk))
icaov <- fersk$V1                          # icao24-numrene i en vektor
length(icaov)

# V3 = origin_country (registreringsland)
landetal <- sort(table(fersk$V3), decreasing = TRUE)
barplot(landetal, las = 2, cex.names = 0.7,
        main = "Fly over Nordsøen fordelt på land (registreringsland)", ylab = "Antal fly")

# Træningsdata: Wulfs cirklende fly
fl <- list.files("Fælles/November/Opensky/Training", pattern = r"(\.rds$)", full.names = TRUE)
collist <- lapply(fl, readRDS)
length(collist)                            # 8

# ---------- 3.2 Start small: ét fly ----------
statedf <- NULL
for (ic in icaov) {                        # første fly med et brugbart track
  statedf <- hentTrack(ic, token)
  if (!is.null(statedf) && nrow(statedf) >= 20) break
  Sys.sleep(1)
}
stopifnot(!is.null(statedf))
testdf <- collist[[4]]                     # ét af de cirklende fly

myEDAPlots(statedf)                        # lat mod lng, kurs mod tid, højde mod tid
myEDAPlots(testdf)
par(mfcol = c(1, 1))
print(myMapPlot(statedf))
print(myMapPlot(testdf))

summary(lm(lat ~ lng + crs + alt, data = testdf))     # cirklende fly
summary(lm(lat ~ lng + crs + alt, data = statedf))    # almindeligt fly

# ---------- 3.3 Move fast: alle fly ----------
tracks <- list()
for (ic in head(icaov, maxfly)) {
  Sys.sleep(1)                             # skån rate limit
  d <- tryCatch(hentTrack(ic, token), error = function(e) NULL)
  if (!is.null(d) && nrow(d) >= 10) tracks[[ic]] <- d
}
length(tracks)

# kursen (crs) og ændringerne i kursen
kursaendring <- lapply(tracks, function(d) diff(d$crs[d$grd == FALSE]))
sdcourse <- sapply(tracks, function(d) sd(d$crs, na.rm = TRUE))
head(sort(sdcourse, decreasing = TRUE), 10)

# ---------- 3.4 Aim high: egen algoritme ----------
tren_df  <- do.call(rbind, lapply(collist, function(d) cbind(metrics(d), isOff = 1)))   # Wulfs cirkelfly
fersk_df <- do.call(rbind, lapply(tracks,  function(d) cbind(metrics(d), isOff = 0)))   # friske fly

resultat <- rbind(tren_df, fersk_df)
rownames(resultat) <- NULL
# drejning er en ekstra kolonne. Fjern den, hvis tabellen skal matche opgaven præcist.
resultat <- resultat[, c("icao", "sdcourse", "lmres", "myOwnAlg", "isOff", "drejning")]
View(resultat)

# Forvekslingstabel: hvor mange af Wulfs cirkelfly fanger algoritmen, og hvor mange falske positive?
table(myOwnAlg = resultat$myOwnAlg, isOff = resultat$isOff)

saveRDS(resultat, "Fælles/November/Opensky/resultat_opgave3.rds")

