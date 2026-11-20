# ==========================================================
# OLA 4 – Opgave 1.1: Scrape Mercedes fra Bilbasen
# Filtre: personbil, Mercedes, kontantpris, forhandler, køb (ikke leasing)
# ==========================================================

# ---------- pakker ----------
library(chromote)     # styrer en rigtig Chrome-browser (Bilbasen bygger siden med JavaScript)
library(rvest)        # læser og søger i HTML
library(dplyr)        # bind_rows, distinct, mutate osv.
library(stringr)      # tekstrensning med regulære udtryk
# fejl fanges med tryCatch, som er indbygget i R (ingen pakke nødvendig)

# ---------- URL ----------
# pricetype=Retail    = kun kontantpris (ingen engros/CVR eller biler uden afgift)
# sellertypes=dealer  = kun forhandlere (ingen private sælgere)
# fanen "Køb" er standard, så leasing er udelukket
baseurl <- "https://www.bilbasen.dk/brugt/bil/mercedes?pricetype=Retail&sellertypes=dealer"

# ---------- start browser ----------
a <- ChromoteSession$new()  # "åbner" browseren her
a$view()                    # vis browservinduet (kun til fejlfinding)
class(a)                    # skal sige "ChromoteSession" "R6"


# ==========================================================
# Funktion 1: hent EN bil (En article) -> En række
# ==========================================================
# data-e2e bruges hvor det findes (stabile navne, Bilbasen bruger dem til egne tests)
# ellers starten af klassenavnet med [class^=...], fordi hash-endelsen (__6B3kE) kan skifte

hent_bil <- function(kasse) {
  
  # linket ligger i attributten href, ikke i teksten
  link <- kasse |>
    html_element("a[data-e2e='search-result-link']") |>
    html_attr("href")
  
  # bilens id = sidste tal i linket (\\d+ = et eller flere cifre, $ = tager de digits som står i slutningen)
  carid <- str_extract(link, "\\d+$")
  
  # kun model, fx "Mercedes EQB250" – ligger rent i attributten data-e2e-value
  model <- kasse |>
    html_element("h3[data-e2e='listing-make-model']") |>
    html_attr("data-e2e-value")
  
  # variant = hele titlen minus modellen, fx "Progressive 5d"
  # fixed() = match som ren tekst, så tegn som + og ( ikke tolkes som regex
  variant <- kasse |>
    html_element("[data-e2e='search-result-make-model']") |>
    html_text2() |>
    str_remove(fixed(model)) |>
    str_trim()
  
  # pris: fjern alt der ikke er et tal, så "394.900 kr." bliver 394900, når ^ står INDE i klammerne betyder det omvendt
  #(alt der ikke er tal) (hvis det er udenfor så betyder det starten)
  pris <- kasse |>
    html_element("[data-e2e='search-result-price']") |>
    html_text2() |>
    str_remove_all("[^0-9]") |>
    as.numeric()
  
  # egenskaber: [1] = årgang, [2] = km (renses til tal i rensningsdelen), [class^='Listing_properties'] betyder 
  #“hvor class starter med ‘Listing_properties’”.
  #Her bruger vi ^ som "starter med"
  egenskaber <- kasse |>
    html_elements("[class^='Listing_properties'] div") |>
    html_text2()
  
  # alle detaljer (gear, brændstof, km/l, rækkevidde ...) samles i én tekst
  # rækkefølgen varierer mellem el- og benzinbiler, så de splittes senere
  detaljer <- kasse |>
    html_elements("[class^='ListingDetails_listItem']") |>
    html_text2() |>
    paste(collapse = " | ")
  
  # salgsteksten – forhandlerens navn/adresse/telefon står ofte her
  beskrivelse <- kasse |>
    html_element("[class^='Listing_description']") |>
    html_text2()
  
  # by/postnummer
  sted <- kasse |>
    html_element("[class^='Listing_location']") |>
    html_text2()
  
  # logoet står to gange (kort- og listevisning) – html_element tager kun det første
  # samme logo-URL = samme forhandler, så den bruges som forhandler-id indtil videre
  forhandlerlogo <- kasse |>
    html_element("img[alt='Forhandlerlogo']") |>
    html_attr("src")
  
  # én række – mangler et felt, bliver det NA, så kolonnerne aldrig forskydes
  data.frame(
    carid          = carid,
    link           = link,
    model          = model,
    variant        = variant,
    pris           = pris,
    årgang         = egenskaber[1],
    km             = egenskaber[2],
    detaljer       = detaljer,
    beskrivelse    = beskrivelse,
    sted           = sted,
    forhandlerlogo = forhandlerlogo,
    scrapedate     = Sys.Date()   # skal bruges i opgave 2
  )
}


# ==========================================================
# Funktion 2: hent ÉN side -> ca. 30 rækker
# ==========================================================
hent_side <- function(side_nr) {
  a$go_to(paste0(baseurl, "&page=", side_nr))
  
  # tilfældig pause 5-8 sek: tid til JavaScript + vi hamrer ikke på serveren
  Sys.sleep(sample(5:8, 1))
  
  # hele siden efter JavaScript er kørt -> rvest-dokument
  html <- a$Runtime$evaluate("document.documentElement.outerHTML")$result$value
  kasser <- read_html(html) |>
    html_elements("article")          # hver bil er en <article>
  
  cat("Side", side_nr, ":", length(kasser), "biler\n")   # log i konsollen
  
  # tom side = noget gik galt -> fejl med vilje, så tryCatch fanger den
  if (length(kasser) == 0) stop(paste("ingen biler på side", side_nr))
  
  lapply(kasser, hent_bil) |>         # kør hent_bil på hver af de 30 biler
    bind_rows()                       # stak de 30 rækker til én dataframe
}


# ==========================================================
# Test: én bil og én side, før vi kører det hele
# ==========================================================
test <- hent_side(1)
nrow(test)                 # skal være ca. 30
colSums(is.na(test))       # NA pr. kolonne – bør være 0 eller tæt på
head(test)


# ==========================================================
# Hvor mange sider er der?
# ==========================================================
# læser "Viser: 2.854 biler til salg" fra siden og regner antal sider ud (30 pr. side)
a$go_to(baseurl)
Sys.sleep(5)
side_tekst <- read_html(a$Runtime$evaluate("document.documentElement.outerHTML")$result$value) |>
  html_text2()

antal_biler <- side_tekst |>
  str_extract("Viser:\\s*[0-9.]+") |>
  str_remove_all("[^0-9]") |>
  as.numeric()

antal_sider <- ceiling(antal_biler / 30)   # ceiling = rund op, så sidste halve side kommer med
antal_biler
antal_sider                                # 2.854 biler -> 96 sider (d. 18/11)


# ==========================================================
# Scrape alle sider
# ==========================================================
liste_sider <- list()   # her gemmes hver side
fejlede     <- c()      # her noteres sider der fejler

for (i in 1:antal_sider) {
  # tryCatch: prøv at hente siden. Går det galt, skriv fejlen i konsollen
  # og returnér selve fejlen i stedet for at stoppe loopet
  resultat <- tryCatch(
    hent_side(i),
    error = function(e) {
      cat("Fejl på side", i, ":", conditionMessage(e), "\n")
      e
    }
  )
  
  # her bruger vi inherits til at: ja, det er en fejl → skriv sidenummeret ned
  # eller nej, det er data → gem siden
  if (inherits(resultat, "error")) {
    fejlede <- c(fejlede, i)
  } else {
    liste_sider[[i]] <- resultat
  }
  
  saveRDS(liste_sider, "bilbasen_midlertidig.rds")   # sikkerhedskopi efter hver side
}

fejlede   # sider der skal køres igen – tom = alt gik godt

# kør fejlede sider igen (springes over, hvis der ingen er)
for (i in fejlede) {
  resultat <- tryCatch(hent_side(i), error = function(e) e)   # fejl -> returnér fejlen
  if (!inherits(resultat, "error")) liste_sider[[i]] <- resultat
}


# ==========================================================
# Saml, fjern dubletter og gem
# ==========================================================
# nye annoncer skubber biler ned på næste side, så samme bil kan blive hentet to gange
df_mercedes <- bind_rows(liste_sider) |>
  distinct(carid, .keep_all = TRUE)

nrow(df_mercedes)            # sammenlign med antal_biler
colSums(is.na(df_mercedes))  # NA pr. kolonne



# ==========================================================
# Rensning af Caspers data (v2, scrapet 17/11)
# ==========================================================
# scrapes varierer fra dag til dag – analysen og opgave 2 bruger den gemte kørsel fra 17/11,
# så resultaterne kan genskabes
df_mercedes_OLA_v2 <- readRDS("df_mercedes_OLA_v2.rds")

# ---------- 1. lav kolonnerne ----------
df_mercedes_renset <- df_mercedes_OLA_v2 |>
  distinct(carid, .keep_all = TRUE) |>                               # fjerner 1 dublet
  mutate(
    pris      = as.numeric(str_remove_all(pris, "[^0-9]")),          # "334.900 kr." -> 334900 (tekst i v2)
    måned     = as.numeric(str_extract(årgang, "^\\d+")),            # tallet før / (^ = starten)
    år        = as.numeric(str_extract(årgang, "\\d{4}$")),          # fire cifre til sidst ($ = slutningen)
    km        = as.numeric(str_remove_all(km, "[^0-9]")),            # "69.000 km" -> 69000
    brændstof = str_trim(str_extract(detaljer, "[^|]+$")),           # sidste del af detaljer, fx "Diesel"
    ny_uregistreret = årgang == "-"                                  # nye biler uden 1. registrering
  )

# ---------- 2. tjek ----------
nrow(df_mercedes_renset)                                             # 2.836
colSums(is.na(df_mercedes_renset[, c("pris", "måned", "år", "km")])) # 3 NA i måned/år = nye biler
count(df_mercedes_renset, brændstof)                                 # El, Benzin, Diesel, Plug-in
sum(df_mercedes_renset$ny_uregistreret)                              # 3
summary(df_mercedes_renset$år)
summary(df_mercedes_renset$km)
summary(df_mercedes_renset$pris)

# outliers – kontrolleret manuelt på Bilbasen
df_mercedes_renset |> filter(is.na(år)) |> select(model, årgang, km, pris, link)
# -> nye biler fra 2026 uden 1. registrering, NA er korrekt

df_mercedes_renset |> arrange(pris) |> select(model, årgang, km, pris, link) |> head(5)
# -> billigste (9.800 kr.) er et mekanikertilbud, resten er gamle dieselbiler med høj km (ægte)

df_mercedes_renset |> arrange(desc(pris)) |> select(model, årgang, km, pris, link) |> head(5)
# -> AMG GT 63 / GT C, prisen matcher (ægte)

df_mercedes_renset |> arrange(desc(km)) |> select(model, årgang, km, pris, link) |> head(5)
# -> gamle E-klasser (typiske taxaer) + 1 Sprinter, lave priser (ægte)

df_mercedes_renset |> arrange(år) |> select(model, årgang, km, pris, link) |> head(5)
# -> veteranbiler (190 SL fra 1956-61), samlerpriser (ægte) – udelukkes fra sammenligningen i 1.2

# ---------- 3. fjern ----------
df_mercedes_renset <- df_mercedes_renset |>
  filter(carid != "5736777")                                         # mekanikertilbud 9.800 kr.

nrow(df_mercedes_renset)                                             # 2.835

# erhvervskøretøjer – samme filter som de tyske data (se opgave 1.2)
erhverv <- "sprinter|vito|citan|kasten|actros|arocs|atego|antos|axor|unimog|econic|kran|kipper|lkw|sattel"

df_mercedes_renset |>
  filter(str_detect(paste(model, variant), regex(erhverv, ignore_case = TRUE))) |>
  select(model, variant, pris)                                       # 14 varebilsbaserede køretøjer (Sprinter, Vito, Citan m.fl.)

df_mercedes_renset <- df_mercedes_renset |>
  filter(carid != "5736777",                                         # mekanikertilbud 9.800 kr.
         !str_detect(paste(model, variant), regex(erhverv, ignore_case = TRUE)))   # erhvervskøretøjer

nrow(df_mercedes_renset)                                             # 2.821

# ---------- 4. gem ----------
saveRDS(df_mercedes_renset, "df_mercedes_renset.rds")