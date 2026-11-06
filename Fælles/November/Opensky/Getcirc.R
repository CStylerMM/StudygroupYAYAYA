# getCircl_ren.R - ren samlet version: Wulfs DAL24Circ + live cirkel-scanner
# Kør oppefra og ned. Secret ligger i .Renviron (OPENSKY_SECRET), aldrig i filen.

library(jsonlite)
library(httr)
library(stringr)
library(leaflet)
source("Fælles/November/Opensky/Util.R")   # getToken()

# ---------- 1) Opsætning ----------
baseurl   = "https://opensky-network.org/api"
endpointS = "/states/all"
endpointT = "/tracks/all"

# Nordsøen
lamin = 52.055129
lamax = 56.196869
lomin = -6.065140
lomax = 4.305954
fullurl = paste0(baseurl, endpointS, "?lamin=", lamin, "&lomin=", lomin,
                 "&lamax=", lamax, "&lomax=", lomax)

colnv = c("icao24", "callsign", "startTime", "endTime", "time", "lat", "lng", "alt", "crs", "grd")

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

# ---------- 2) Hjælpefunktioner ----------

# Samlet drejning i én retning, i grader.
# (sd() er dårlig til vinkler: 359 og 1 grad ligger tæt, men sd ser dem langt fra hinanden)
netto_drejning <- function(crs) {
  crs <- crs[!is.na(crs)]
  d <- diff(crs)
  d <- (d + 180) %% 360 - 180      # kursændring mellem -180 og 180, ingen spring ved nord
  abs(sum(d))
}

# Det vindue (sekunder) i et helt track, hvor drejningen er størst.
# Bruges på træningsdata for at se, hvordan rigtige cirkler ser ud.
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

# Track for ét fly (kun den aktuelle flyvning, time=0).
# callsign kan være null, og så dur as.data.frame(fromJSON(...)) ikke, så vi bygger selv.
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

# icao24 for fly i luften over minalt meter (V8 = baro_altitude)
hentIcaoer <- function(token, minalt = 0) {
  st <- hentStates(token)
  if (is.null(st)) return(NULL)
  hoejde <- suppressWarnings(as.numeric(st$V8))
  st$V1[!is.na(hoejde) & hoejde > minalt]
}

# Én scanning: de seneste 'vindue' sekunder af hvert fly, kun i luften.
# Giver samme kolonner som bedste_vindue, så kandidater kan sammenlignes med træningsfly.
scanBox <- function(vindue = 900, maxfly = 60, minalt = 0) {
  token <- getToken()
  icaov <- hentIcaoer(token, minalt)
  raekker <- lapply(head(icaov, maxfly), function(ic) {
    Sys.sleep(1)                                   # skån rate limit
    tryCatch({
      df <- hentTrack(ic, token)
      if (is.null(df)) return(NULL)
      df <- df[df$grd == FALSE & !is.na(df$alt) & df$alt > minalt &
                 df$time > max(df$time) - vindue, ]
      if (nrow(df) < 5) return(NULL)
      data.frame(icao24   = ic,
                 callsign = trimws(df$callsign[1]),
                 drejning = netto_drejning(df$crs),
                 alt      = round(median(df$alt)),
                 lat      = round(median(df$lat), 2),
                 lng      = round(median(df$lng), 2),
                 punkter  = nrow(df))
    }, error = function(e) {
      message("Droppede ", ic, ": ", conditionMessage(e))
      NULL
    })
  })
  ud <- do.call(rbind, raekker)
  if (is.null(ud)) return(NULL)
  ud$scannet <- format(Sys.time(), "%H:%M:%S")
  ud[order(-ud$drejning), ]
}

# ---------- 3) Wulfs skateboard: ét fly over Nordsøen ----------
token <- getToken()
statedfpr <- hentStates(token)
icaov <- statedfpr$V1

ictest <- icaov[[6]]                 # giver det NULL nedenfor, så prøv icaov[[7]]
statedf <- hentTrack(ictest, token)
myEDAPlots(statedf)

# ---------- 4) Træningsdata ----------
fl <- list.files("Fælles/November/Opensky/Training", pattern = r"(\.rds$)", full.names = TRUE)
collist <- lapply(fl, readRDS)
length(collist)                      # 8
testdf <- collist[[4]]

myEDAPlots(testdf)                   # plots til at udforske data
myMapPlot(testdf)                    # kort til at bekræfte flyvningen

cmodel <- lm(lat ~ lng + crs + alt, data = testdf)
nmodel <- lm(lat ~ lng + crs + alt, data = statedf)
summary(cmodel)
summary(nmodel)
csd <- sd(testdf$crs)
nsd <- sd(statedf$crs)

# Kalibrering: hvor stor er drejningen i de 15 bedste minutter af hvert træningsfly?
traening <- round(t(sapply(collist, bedste_vindue)), 1)
traening                             # drejning, alt (m), lat, lng, punkter
bedste_vindue(statedf)               # ét normalt fly til sammenligning

# ---------- 5) Live: find fly der cirkler lige nu ----------
GRAENSE <- 250     # foreløbig: laveste træningsvindue er 273. Justér efter normale fly.

test <- TRUE       # TRUE = hurtig test, FALSE = rigtig kørsel (ca. 1 time, blokerer konsollen)
if (test) {
  freq <- 5;   limit <- 1;  maxfly <- 10
} else {
  freq <- 600; limit <- 6;  maxfly <- 60
}

logliste <- list()
counter  <- 0
while (counter < limit) {
  ud <- scanBox(maxfly = maxfly)
  if (!is.null(ud)) {
    logliste[[length(logliste) + 1]] <- ud
    print(head(ud, 10))
  }
  counter <- counter + 1
  if (counter < limit) Sys.sleep(freq)
}


# ---------- 6) Resultat og kort ----------
if (length(logliste) > 0) {
  logdf <- do.call(rbind, logliste)
  saveRDS(logdf, "Fælles/November/Opensky/scanlog.rds")
  cirkler <- logdf[logdf$drejning >= GRAENSE, ]
  cirkler <- cirkler[!duplicated(cirkler$icao24), ]
  print(cirkler)
  if (nrow(cirkler) > 0) {
    print(myMapPlot(hentTrack(cirkler$icao24[1], getToken())))
  } else {
    message("Ingen fly over grænsen i denne log")
  }
} else {
  message("Ingen scanninger gav data")
}

