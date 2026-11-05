# scan_loop.R - kører cirkel-scanneren i baggrunden (RStudio Background Job)
# Gem filen i Fælles/November/Opensky/ og start den fra projektmappen StudygroupYAYAYA.
# Gemmer efter hver scanning, så intet går tabt hvis du afbryder undervejs.

library(httr)
library(jsonlite)

if (!file.exists("Fælles/November/Opensky/Util.R")) {
  stop("Forkert working directory. Start jobbet med StudygroupYAYAYA som working directory.")
}
source("Fælles/November/Opensky/Util.R")   # getToken()

# ---------- Indstillinger ----------
omraade <- "nordsoeen"   # "nordsoeen" eller "sydfrankrig" (Nice, hvor de fleste træningscirkler er)
freq    <- 600           # sekunder mellem scanninger
limit   <- 6             # antal scanninger (6 x 10 min = ca. 1 time)
maxfly  <- 60            # hvor mange fly pr. scanning (ca. 1 sek. pr. fly)
vindue  <- 900           # sekunder bagud der måles på
GRAENSE <- 250           # drejning over denne værdi gemmes som kandidat (foreløbigt gæt)
mappe   <- "Fælles/November/Opensky"

omraader <- list(
  nordsoeen   = c(lamin = 52.055129, lamax = 56.196869, lomin = -6.065140, lomax = 4.305954),
  sydfrankrig = c(lamin = 42.8,      lamax = 44.4,      lomin = 5.5,       lomax = 8.5)   # grove tal
)

# ---------- Opsætning ----------
baseurl   <- "https://opensky-network.org/api"
endpointS <- "/states/all"
endpointT <- "/tracks/all"
b <- omraader[[omraade]]
fullurl <- paste0(baseurl, endpointS, "?lamin=", b["lamin"], "&lomin=", b["lomin"],
                  "&lamax=", b["lamax"], "&lomax=", b["lomax"])
colnv <- c("icao24", "callsign", "startTime", "endTime", "time", "lat", "lng", "alt", "crs", "grd")

# ---------- Funktioner ----------
netto_drejning <- function(crs) {
  crs <- crs[!is.na(crs)]
  d <- diff(crs)
  d <- (d + 180) %% 360 - 180
  abs(sum(d))
}

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

hentIcaoer <- function(token) {
  res <- httr::GET(fullurl, add_headers(Authorization = paste("Bearer", token)))
  if (res$status_code != 200) stop("states/all gav status ", res$status_code)
  states <- fromJSON(content(res, as = "text", encoding = "UTF-8"))$states
  if (is.null(states)) return(NULL)
  st <- as.data.frame(states)
  hoejde <- suppressWarnings(as.numeric(st$V8))     # V8 = højde i meter
  st$V1[!is.na(hoejde) & hoejde > 0]
}

scanBox <- function(token) {
  icaov <- hentIcaoer(token)
  raekker <- lapply(head(icaov, maxfly), function(ic) {
    Sys.sleep(1)                                    # skån rate limit
    tryCatch({
      df <- hentTrack(ic, token)
      if (is.null(df)) return(NULL)
      df <- df[df$grd == FALSE & !is.na(df$alt) & df$time > max(df$time) - vindue, ]
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

# ---------- Loop ----------
logliste <- list()
spor     <- list()      # gemte tracks for fly over grænsen (live-spor forsvinder, når flyet lander)

for (runde in seq_len(limit)) {
  message("--- Scanning ", runde, " af ", limit, " kl. ", format(Sys.time(), "%H:%M:%S"),
          " (", omraade, ") ---")
  token <- getToken()
  ud <- tryCatch(scanBox(token), error = function(e) {
    message("Scanning fejlede: ", conditionMessage(e))
    NULL
  })
  
  if (!is.null(ud)) {
    ud$runde <- runde
    logliste[[length(logliste) + 1]] <- ud
    print(head(ud, 10))
    
    nye <- setdiff(ud$icao24[ud$drejning >= GRAENSE], names(spor))
    for (ic in nye) {
      tr <- hentTrack(ic, token)
      if (!is.null(tr)) spor[[ic]] <- tr
    }
    saveRDS(do.call(rbind, logliste), file.path(mappe, paste0("scanlog_", omraade, ".rds")))
    saveRDS(spor, file.path(mappe, paste0("spor_", omraade, ".rds")))
  }
  
  if (runde < limit) Sys.sleep(freq)
}

message("Færdig kl. ", format(Sys.time(), "%H:%M:%S"), ". ", length(spor), " fly over grænsen.")