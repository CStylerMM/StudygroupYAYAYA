# ==========================================================
# OLA 4 – Opgave 1.2: Hent Mercedes fra Tyskland (autoscout24)
# Bygger på Caspers funktioner – rettet, så hele prisspektret dækkes,
# og så udvalget matcher Bilbasen (brugte biler, kun forhandlere)
# ==========================================================

# ---------- pakker ----------
library(chromote)     # rigtig Chrome-browser (autoscout24 bygger siden med JavaScript)
library(rvest)        # læser og søger i HTML
library(dplyr)        # bind_rows, distinct, filter, mutate
library(stringr)      # tekstrensning
# fejl fanges med tryCatch, som er indbygget i R (ingen pakke nødvendig)

# ---------- URL ----------
# cy=D              = biler i Tyskland
# atype=C           = personbiler
# ustate=U          = kun BRUGTE (Caspers version havde N%2CU = nye + brugte)
# custtype=D        = kun forhandlere (som Bilbasen) – vi filtrerer også på sælgertype bagefter for en sikkerheds skyld
# damaged_listing   = skadede biler udelukkes
# sort=age&desc=1   = nyeste annoncer først. Neutral i forhold til pris, så vi ikke kun får
#                     de billigste i hvert interval (Caspers sort=price gav netop det problem)
baseurl <- "https://www.autoscout24.com/lst/mercedes-benz?cy=D&atype=C&ustate=U&custtype=D&damaged_listing=exclude&ocs_listing=include&powertype=kw&sort=age&desc=1"


# ==========================================================
# Funktion 1: hent ÉN tysk bil (én article) -> én række
# ==========================================================
# autoscout24 lægger næsten alt som data-attributter direkte på <article>,
# så vi kan læse rene værdier uden at rense tekst
hent_tysk_bil <- function(kasse) {
  attrs <- html_attrs(kasse)
  
  # hent en attribut – findes den ikke, bliver den NA (så kolonnerne aldrig forskydes)
  hent <- function(navn) if (navn %in% names(attrs)) attrs[[navn]] else NA
  
  titel <- kasse |> html_element("h2") |> html_text2()
  
  # bilens link står ikke i HTML'en (siden åbnes med JavaScript) – bygges ud fra guid
  # testet 19/11: /offers/<guid> åbner bilens side
  link <- paste0("https://www.autoscout24.com/offers/", hent("data-guid"))
  
  # forhandlerens side på autoscout24 – fungerer som forhandler-id
  forhandler_link <- kasse |>
    html_element("a[href*='/dealerinfo/']") |>
    html_attr("href")
  
  forhandler <- kasse |>
    html_element("[data-testid='dealer-company-name']") |>
    html_text2()
  
  forhandler_adresse <- kasse |>
    html_element("[data-testid='dealer-address']") |>
    html_text2()
  
  data.frame(
    guid               = hent("data-guid"),                  # autoscouts bil-id (svarer til carid)
    link               = link,
    make               = hent("data-make"),                  # bruges til at smide reklamer for andre mærker ud
    model              = hent("data-model"),                 # fx "e 220"
    titel              = titel,
    pris_eur           = as.numeric(hent("data-price")),     # rent tal i euro
    km                 = as.numeric(hent("data-mileage")),
    første_reg         = hent("data-first-registration"),    # fx "10-2018" eller "new"
    brændstof_kode     = hent("data-fuel-type"),             # NY: fx "d" = diesel (oversættes bagefter)
    sælgertype         = hent("data-seller-type"),           # d = forhandler, p = privat
    postnr             = hent("data-listing-zip-code"),
    forhandler         = forhandler,
    forhandler_adresse = forhandler_adresse,
    forhandler_link    = forhandler_link,                    # NY: forhandler-id
    scrapedate         = Sys.Date()
  )
}


# ==========================================================
# Funktion 2: hent ÉN side -> ca. 20 rækker
# ==========================================================
hent_tysk_side <- function(url, side_nr) {
  # Page$navigate venter ikke på alle reklamer (go_to kan hænge på autoscout24)
  a$Page$navigate(paste0(url, "&page=", side_nr))
  Sys.sleep(sample(5:8, 1))   # tid til JavaScript + vi hamrer ikke på serveren
  
  html <- a$Runtime$evaluate("document.documentElement.outerHTML")$result$value
  kasser <- read_html(html) |> html_elements("article")
  
  cat("  side", side_nr, ":", length(kasser), "biler\n")
  
  # tom side = intervallet er tømt (ikke en fejl) -> NULL, så loopet går videre
  if (length(kasser) == 0) return(NULL)
  
  lapply(kasser, hent_tysk_bil) |> bind_rows()
}


# ==========================================================
# Funktion 3: hent ÉT prisinterval (flere sider)
# ==========================================================
hent_interval <- function(fra, til, max_sider) {
  a <<- ChromoteSession$new()   # ny browser pr. interval, så Chrome ikke løber tør for hukommelse
  url <- paste0(baseurl, "&pricefrom=", fra, "&priceto=", til)
  cat("interval", fra, "-", til, "€\n")
  
  liste <- list()
  for (side in 1:max_sider) {
    # tryCatch: prøv at hente siden. Går det galt, skriv fejlen i konsollen
    # og returnér selve fejlen (ikke NULL – NULL betyder "intervallet er tømt")
    df_side <- tryCatch(
      hent_tysk_side(url, side),
      error = function(e) {
        cat("  Fejl:", conditionMessage(e), "\n")
        e
      }
    )
    
    if (inherits(df_side, "error")) {
      fejlede <<- c(fejlede, paste0(url, "&page=", side))   # <<- = gem uden for funktionen
      a <<- ChromoteSession$new()                           # browseren er måske død – start en ny
      Sys.sleep(5)
      next
    }
    
    if (is.null(df_side)) break    # tom side = ikke flere biler i intervallet
    liste[[side]] <- df_side
  }
  
  a$close()
  bind_rows(liste)
}


# ==========================================================
# Prisintervaller – SMALLE, så hele prisspektret dækkes
# ==========================================================
# casper version: 7 brede intervaller x 25 sider, sorteret efter pris
# -> kun de ~500 billigste i hvert interval (fx intet mellem 52.500 og 150.000 €)
# Nu: intervaller på 5.000 € x 5 sider (ca. 100 biler) -> jævn dækning fra 1.000 til 150.000 €+
grænser <- c(seq(1000, 150000, by = 5000), 9999999)   # sidste interval er åbent (dyre AMG'er)
df_intervaller <- data.frame(
  fra = head(grænser, -1),
  til = tail(grænser, -1)
)
nrow(df_intervaller)   # ca. 30 intervaller

SIDER_PR_INTERVAL <- 5   # 5 sider x 20 biler = ca. 100 biler pr. interval


# ==========================================================
# Test: ét interval, én side, før vi kører det hele
# ==========================================================
fejlede <- c()
test <- hent_interval(30000, 35000, max_sider = 1)
nrow(test)                  # skal være ca. 20
colSums(is.na(test))        # alle bør have 0 eller få NA (testet 19/11: 0 NA, 20 biler, kun "d")
table(test$sælgertype)      # skal kun være "d", hvis custtype=D virker
head(test)


# ==========================================================
# Kør alle intervaller
# ==========================================================
liste_tysk <- list()
fejlede    <- c()

for (i in 1:nrow(df_intervaller)) {
  liste_tysk[[i]] <- hent_interval(df_intervaller$fra[i], df_intervaller$til[i],
                                   max_sider = SIDER_PR_INTERVAL)
  saveRDS(liste_tysk, paste0("tyskland_backup_", Sys.Date(), ".rds"))   # backup efter hvert interval
}

sapply(liste_tysk, nrow)   # antal biler pr. interval – ca. 100, færre i de dyre intervaller
length(fejlede)            # sider der fejlede – 0 = alt gik godt

# kør fejlede sider igen (springes over, hvis der ingen er)
a <- ChromoteSession$new()
liste_rest <- list()
for (link in fejlede) {
  url  <- str_remove(link, "&page=\\d+$")
  side <- as.numeric(str_extract(link, "\\d+$"))
  df_side <- tryCatch(hent_tysk_side(url, side), error = function(e) e)   # fejl -> returnér fejlen
  if (!inherits(df_side, "error") && !is.null(df_side)) liste_rest[[link]] <- df_side
}
liste_tysk <- c(liste_tysk, liste_rest)


# ==========================================================
# Saml og ryd op
# ==========================================================
df_tyskland <- bind_rows(liste_tysk) |>
  distinct(guid, .keep_all = TRUE) |>     # samme bil kan dukke op to gange, når biler rykker sider
  filter(
    make == "mercedes-benz",              # smider reklamer for andre mærker ud
    sælgertype == "d",                    # kun forhandlere (som Bilbasen)
    pris_eur > 1000,                      # 1.000 € er pladsholderpriser
    første_reg != "new"                   # kun brugte biler
  )

nrow(df_tyskland)

# brændstofkoder -> samme navne som Bilbasen
# tjek koderne med count(df_tyskland, brændstof_kode) og et par stikprøver på siden,
# før du stoler på oversættelsen
count(df_tyskland, brændstof_kode)

df_tyskland <- df_tyskland |>
  mutate(
    brændstof = case_when(
      brændstof_kode == "b" ~ "Benzin",
      brændstof_kode == "d" ~ "Diesel",
      brændstof_kode == "e" ~ "El",
      brændstof_kode %in% c("2", "3") ~ "Hybrid",   # el/benzin og el/diesel (plug-in + mild hybrid)
      TRUE ~ "Andet"                                # LPG, brint osv.
    ),
    måned = as.numeric(str_extract(første_reg, "^\\d{2}")),   # "10-2018" -> 10
    år    = as.numeric(str_extract(første_reg, "\\d{4}$")),   # "10-2018" -> 2018
    
    # momsreglen ved import: under 6.000 km ELLER under 6 mdr. siden 1. reg. = "ny" -> +25 % dansk moms
    alder_mdr = (as.numeric(format(scrapedate, "%Y")) - år) * 12 +
      (as.numeric(format(scrapedate, "%m")) - måned),
    ny_moms   = km < 6000 | alder_mdr < 6
  )

count(df_tyskland, brændstof)
sum(df_tyskland$ny_moms, na.rm = TRUE)
colSums(is.na(df_tyskland))
summary(df_tyskland$pris_eur)

saveRDS(df_tyskland, "df_tyskland_v2.rds")


#outlier-tjek

df_tyskland |> arrange(desc(pris_eur)) |> select(model, titel, år, km, pris_eur, link) |> head(5)
df_tyskland |> arrange(pris_eur)       |> select(model, titel, år, km, pris_eur, link) |> head(5)
df_tyskland |> arrange(år)             |> select(model, titel, år, km, pris_eur, link) |> head(5)
df_tyskland |> arrange(desc(km))       |> select(model, titel, år, km, pris_eur, link) |> head(5)

#Vi har nogle biler der har "pladsholder-priser" da de først er tilgængelige i maj-måned 2027. 
#Det er antaget at det er pladsholder-priser da bilernes pris ikke matcher markedsprisen

# forudannoncer ("available fra 2027") med pladsholderpris på 500.000 € – kontrolleret manuelt
df_tyskland |> filter(pris_eur == 500000) |> select(model, titel, år, km, pris_eur)   # se dem først

df_tyskland <- df_tyskland |>
  filter(pris_eur != 500000)

nrow(df_tyskland)

#nu tjekker vi for varebiler

# varebiler, der er kommet med trods atype=C (personbiler)
df_tyskland |> filter(model == "unknown") |> count(titel) |> head(10)
df_tyskland |> filter(str_detect(titel, regex("sprinter|vito|citan|kasten", ignore_case = TRUE))) |> nrow()

#Der er kommet varebiler og lastbiler med, vi antager at forhandleren har sat den forkerte kategori, dem fjerner vi

# erhvervskøretøjer (varebiler + lastbiler), der er kommet med trods atype=C
erhverv <- "sprinter|vito|citan|kasten|actros|arocs|atego|antos|axor|unimog|econic|kran|kipper|lkw|sattel"

df_tyskland |>
  filter(str_detect(titel, regex(erhverv, ignore_case = TRUE))) |>
  count(model, sort = TRUE)          # se hvilke der rammes, før vi fjerner

df_tyskland <- df_tyskland |>
  filter(!str_detect(titel, regex(erhverv, ignore_case = TRUE)))   # ! = IKKE

nrow(df_tyskland)

# kontrol: alle tilbageværende modeller – står der erhvervskøretøjer, som filteret ikke fangede?
df_tyskland |> count(model, sort = TRUE) |> as_tibble() |> print(n = Inf)

# samme filter på de danske data, så begge lande behandles ens
df_mercedes_renset <- readRDS("df_mercedes_renset.rds")
df_mercedes_renset |>
  filter(str_detect(paste(model, variant), regex(erhverv, ignore_case = TRUE))) |>
  select(model, variant, pris)

df_mercedes_renset <- df_mercedes_renset |>
  filter(!str_detect(paste(model, variant), regex(erhverv, ignore_case = TRUE)))
saveRDS(df_mercedes_renset, "df_mercedes_renset.rds")

# gem det færdige tyske datasæt (EFTER alle fjernelser)
saveRDS(df_tyskland, "df_tyskland_v2.rds")

nrow(df_tyskland)



# ==========================================================
# Nu går vi til at teste hypotesen, 
# Opgave 1.2 – Er tyske biler billigere?
# Trin 1: gør de to datasæt sammenlignelige
# ==========================================================
library(dplyr)
library(stringr)

df_dk <- readRDS("df_mercedes_renset.rds")
df_de <- readRDS("df_tyskland_v2.rds")

nrow(df_dk)   # skal være 2.821
nrow(df_de)   # skal være 2.623

eur_dkk <- 7.46   # fast kurs (DKK er bundet til EUR)

# ---------- fælles afgrænsning ----------
# - kun biler fra 2010 og frem (veteranbiler har samlerpriser og egne afgiftsregler)
# - kun El, Benzin, Diesel (hybrid kan ikke sammenlignes: autoscout blander mild hybrid ind)
dk <- df_dk |>
  filter(!is.na(år), år >= 2010,
         brændstof %in% c("El", "Benzin", "Diesel")) |>
  mutate(land = "DK",
         model_kort = str_to_lower(str_remove(model, "^Mercedes ")),   # "Mercedes E220 d" -> "e220 d"
         model_kort = str_remove_all(model_kort, " "),                  # "e220 d" -> "e220d"
         pris_dkk = pris) |>
  select(land, model_kort, år, km, brændstof, pris_dkk)

de <- df_de |>
  filter(år >= 2010,
         brændstof %in% c("El", "Benzin", "Diesel")) |>
  mutate(land = "DE",
         model_kort = str_remove_all(model, " "),     # "e 220" -> "e220" (samme format som DK)
         pris_dkk = pris_eur * eur_dkk) |>             # tysk pris omregnet, UDEN afgift
  select(land, model_kort, år, km, brændstof, pris_dkk, ny_moms)

nrow(dk)
nrow(de)
count(dk, brændstof)
count(de, brændstof)

# hvor mange modeller findes i begge lande?
length(intersect(dk$model_kort, de$model_kort))


# ---------- ensret modelnavne ----------
# Bilbasen: "e220d" (diesel-d) og "eqe350+" (plus) – autoscout24: "e220", "eqe350"
dk <- dk |>
  mutate(model_kort = str_remove_all(model_kort, "\\+"),              # fjern +
         model_kort = if_else(brændstof == "Diesel",
                              str_remove(model_kort, "d$"),           # fjern diesel-d til sidst
                              model_kort))

de <- de |>
  mutate(model_kort = str_remove_all(model_kort, "\\+"))

# hvor mange modeller og tyske biler kan matches nu?
length(intersect(dk$model_kort, de$model_kort))   # antal fælles modeller
mean(de$model_kort %in% dk$model_kort)            # andel tyske biler med en dansk model at sammenligne med

# de hyppigste tyske modeller UDEN dansk match – er der flere navneforskelle?
de |> filter(!model_kort %in% dk$model_kort) |> count(model_kort, sort = TRUE) |> head(15)
dk |> count(model_kort, sort = TRUE) |> head(15)


# AMG-navne (sidste navneforskel)
de <- de |> mutate(model_kort = str_remove(model_kort, "amg$"))
dk <- dk |> mutate(model_kort = if_else(str_detect(model_kort, "^amggt"), "amggt", model_kort))

# median pr. model + årgang + brændstof i hvert land
dk_grp <- dk |>
  group_by(model_kort, år, brændstof) |>
  summarise(dk_median = median(pris_dkk), dk_antal = n(), .groups = "drop")

de_grp <- de |>
  group_by(model_kort, år, brændstof) |>
  summarise(de_median = median(pris_dkk), de_antal = n(),
            andel_ny_moms = mean(ny_moms), .groups = "drop")

# kun grupper, der findes i BEGGE lande (inner_join = fælles rækker)
sammen <- inner_join(dk_grp, de_grp, by = c("model_kort", "år", "brændstof"))

nrow(sammen)                       # antal sammenlignelige grupper
sum(sammen$dk_antal); sum(sammen$de_antal)   # antal biler, der indgår
sammen |> arrange(desc(dk_antal + de_antal)) |> head(15)


# ==========================================================
# Begrundelse for afgiftssatsen på benzin/diesel
# ==========================================================
# afgiftens andel af prisen for en benzin-/dieselbil (2026-skalaen, FDM)
# 25 % op til 76.400 kr., 85 % op til 237.400 kr., 150 % af resten
# (bundfradrag og CO2-tillæg er ikke med)
afgift_skala <- function(v) {
  0.25 * pmin(v, 76400) +
    0.85 * pmax(0, pmin(v, 237400) - 76400) +
    1.50 * pmax(0, v - 237400)
}

værdi  <- c(150000, 200000, 250000, 300000, 400000)   # bilens værdi uden afgift
afgift <- afgift_skala(værdi)
data.frame(værdi, afgift, pris = værdi + afgift,
           andel_af_pris = round(afgift / (værdi + afgift) * 100, 1))
# -> ud fra tabellen vælger vi 45 % som antagelse


# ==========================================================
# Trin B: Hvad koster den tyske bil på danske plader?
# ==========================================================
# Vi lægger afgift, moms og omkostninger oveni den tyske pris
# og sammenligner med den danske pris (begge biler på danske plader)

afgift_fossil <- 0.45     # afgift på benzin/diesel = 45 % af den danske pris (skalaen giver typisk 35-50 %)
omkostninger  <- 5000     # transport, eksportplader, syn (eget skøn)

resultat <- sammen |>
  mutate(
    # 1. afgift: regnes af den DANSKE pris (reglen for brugte importbiler)
    #    El: 0 kr. under den afgiftsfri grænse på 419.300 kr. (2026).
    #        Over grænsen er satsen 60 % af bilens værdi UDEN afgift. Bilbasen-prisen er MED afgift,
    #        så det svarer til 37,5 % af den del af prisen, der ligger over grænsen (0,6 / 1,6 = 0,375)
    #    Benzin/diesel: 45 % af den danske pris (skøn ud fra afgiftsskalaen)
    afgift = if_else(brændstof == "El",
                     0.375 * pmax(0, dk_median - 419300),   # el: 37,5 % af prisen over grænsen
                     afgift_fossil * dk_median),            # benzin/diesel: 45 % af prisen
    
    # 2. moms: "nye" tyske biler (under 6.000 km / 6 mdr.) -> dansk moms i stedet for tysk = ca. 5 % ekstra
    moms = de_median * andel_ny_moms * 0.05,
    
    # 3. den tyske bil på danske plader = tysk pris + moms + afgift + omkostninger
    de_dk_plader = de_median + moms + afgift + omkostninger,
    
    # 4. forskel: positiv = billigere at hente bilen i Tyskland
    forskel = dk_median - de_dk_plader
  )

# opsummering pr. brændstof
resultat |>
  group_by(brændstof) |>
  summarise(grupper           = n(),
            andel_billigst_de = mean(forskel > 0),
            median_forskel    = median(forskel))

# følsomhed: ret afgift_fossil øverst til 0.35 og 0.55, og kør blokken igen
# resultat: benzin billigst i DE 45 % / 37 % / 22 %, diesel 60 % / 48 % / 16 %

saveRDS(resultat, "resultat_opgave_1_2.rds")


library(ggplot2)
library(dplyr)

resultat <- readRDS("resultat_opgave_1_2.rds")

# typisk forskel pr. brændstof (3 rækker)
graf_data <- resultat |>
  group_by(brændstof) |>
  summarise(forskel = median(forskel))

# søjlediagram
ggplot(graf_data, aes(x = factor(brændstof, levels = c("El", "Diesel", "Benzin")),
                      y = forskel, fill = forskel > 0)) +
  geom_col() +
  geom_text(aes(label = paste0(format(round(forskel), big.mark = ".", decimal.mark = ","), " kr."),
                vjust = ifelse(forskel > 0, -0.5, 1.5)),
            size = 5) +                                          # tallene over søjlerne +
  geom_hline(yintercept = 0) +
  labs(title = "Elbiler er typisk billigere at hente i Tyskland - benzinbiler er ikke",
       x = NULL, y = "Besparelse ved import (kr.)") +
  scale_fill_manual(values = c("TRUE" = "darkgreen", "FALSE" = "darkred"), guide = "none") +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", suffix = " kr."),
                     breaks = seq(-50000, 50000, by = 25000),
                     limits = c(-50000, 60000)) +
  theme_minimal() +
  theme(plot.title   = element_text(size = 18, face = "bold"),   # overskriften
        axis.text    = element_text(size = 13),                  # tallene på y-aksen + teksten på x-aksen
        axis.title.y = element_text(size = 14))                  # teksten på y-aksen

ggsave("graf_opgave_1_2.png", width = 7, height = 4)