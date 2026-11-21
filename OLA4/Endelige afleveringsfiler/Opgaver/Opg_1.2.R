# ---------- Opg 1.2: Hent Mercedes fra Tyskland (autoscout24) ----------
#bygger på vores scrape fra Bilbasen, men rettet så hele prisskalaen kommer med,
#og så udvalget passer med Bilbasen (brugte biler og kun forhandlere)

# ---------- Pakker ----------
#chromote styrer en rigtig Chrome, fordi autoscout24 bygger siden med JavaScript
library(chromote)
#rvest læser html'en
library(rvest)
#dplyr til bind_rows, distinct, filter og mutate
library(dplyr)
#stringr til tekst
library(stringr)
#tryCatch er indbygget i R, så den skal ikke have en pakke

# ---------- URL ----------
#cy=D giver biler i Tyskland
#atype=C giver personbiler
#ustate=U giver kun brugte biler (vores første version havde N%2CU, som også tog nye med)
#custtype=D giver kun forhandlere som på Bilbasen. vi filtrerer også på sælgertype bagefter for en sikkerheds skyld
#damaged_listing tager skadede biler ud
#sort=age&desc=1 giver de nyeste annoncer først. det er neutralt i forhold til pris, så vi ikke kun får
#de billigste i hvert interval (vores første version med sort=price gav lige præcis det problem)
baseurl <- "https://www.autoscout24.com/lst/mercedes-benz?cy=D&atype=C&ustate=U&custtype=D&damaged_listing=exclude&ocs_listing=include&powertype=kw&sort=age&desc=1"


# ---------- Funktion 1: hent én tysk bil ----------
#autoscout24 lægger næsten alt som data-attributter direkte på <article>,
#så vi kan læse rene værdier uden at rense tekst
hent_tysk_bil <- function(kasse) {
  attrs <- html_attrs(kasse)
  
  #henter en attribut. findes den ikke, bliver den NA, så kolonnerne ikke bliver forskudt
  hent <- function(navn) if (navn %in% names(attrs)) attrs[[navn]] else NA
  
  titel <- kasse |> html_element("h2") |> html_text2()
  
  #linket står ikke i html'en, fordi siden åbnes med JavaScript. derfor bygger vi det ud fra guid
  #testet 19/11: /offers/<guid> åbner bilens side
  link <- paste0("https://www.autoscout24.com/offers/", hent("data-guid"))
  
  #forhandlerens side på autoscout24 bruger vi som forhandler-id
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
    #autoscouts id på bilen (ligesom carid)
    guid               = hent("data-guid"),
    link               = link,
    #bruges til at smide reklamer for andre mærker ud
    make               = hent("data-make"),
    #fx "e 220"
    model              = hent("data-model"),
    titel              = titel,
    #pris i euro som rent tal
    pris_eur           = as.numeric(hent("data-price")),
    km                 = as.numeric(hent("data-mileage")),
    #fx "10-2018" eller "new"
    første_reg         = hent("data-first-registration"),
    #fx "d" for diesel (oversættes bagefter)
    brændstof_kode     = hent("data-fuel-type"),
    #d er forhandler og p er privat
    sælgertype         = hent("data-seller-type"),
    postnr             = hent("data-listing-zip-code"),
    forhandler         = forhandler,
    forhandler_adresse = forhandler_adresse,
    #forhandler-id
    forhandler_link    = forhandler_link,
    scrapedate         = Sys.Date()
  )
}


# ---------- Funktion 2: hent én side (ca. 20 biler) ----------
hent_tysk_side <- function(url, side_nr) {
  #Page$navigate venter ikke på alle reklamerne. go_to kunne hænge på autoscout24
  a$Page$navigate(paste0(url, "&page=", side_nr))
  #pause, så JavaScript kan nå at loade, og vi ikke spammer siden
  Sys.sleep(sample(5:8, 1))
  
  html <- a$Runtime$evaluate("document.documentElement.outerHTML")$result$value
  kasser <- read_html(html) |> html_elements("article")
  
  cat("  side", side_nr, ":", length(kasser), "biler\n")
  
  #en tom side betyder at intervallet er tømt. det er ikke en fejl, så vi giver NULL og loopet går videre
  if (length(kasser) == 0) return(NULL)
  
  lapply(kasser, hent_tysk_bil) |> bind_rows()
}


# ---------- Funktion 3: hent ét prisinterval (flere sider) ----------
hent_interval <- function(fra, til, max_sider) {
  #ny browser pr. interval, så Chrome ikke løber tør for hukommelse
  a <<- ChromoteSession$new()
  url <- paste0(baseurl, "&pricefrom=", fra, "&priceto=", til)
  cat("interval", fra, "-", til, "€\n")
  
  liste <- list()
  for (side in 1:max_sider) {
    #tryCatch prøver at hente siden. går det galt, skriver den fejlen i konsollen
    #og giver fejlen tilbage (ikke NULL, for NULL betyder at intervallet er tømt)
    df_side <- tryCatch(
      hent_tysk_side(url, side),
      error = function(e) {
        cat("  Fejl:", conditionMessage(e), "\n")
        e
      }
    )
    
    if (inherits(df_side, "error")) {
      #<<- gemmer uden for funktionen
      fejlede <<- c(fejlede, paste0(url, "&page=", side))
      #browseren er måske gået ned, så vi starter en ny
      a <<- ChromoteSession$new()
      Sys.sleep(5)
      next
    }
    
    #tom side betyder ikke flere biler i intervallet
    if (is.null(df_side)) break
    liste[[side]] <- df_side
  }
  
  a$close()
  bind_rows(liste)
}


# ---------- Smalle prisintervaller, så hele prisskalaen kommer med ----------
#vores første version: 7 brede intervaller x 25 sider, sorteret efter pris
#det gav kun de ca. 500 billigste i hvert interval (fx intet mellem 52.500 og 150.000 €)
#nu: intervaller på 5.000 € x 5 sider (ca. 100 biler), så vi dækker jævnt fra 1.000 til over 150.000 €
#sidste interval er åbent opad (dyre AMG'er)
grænser <- c(seq(1000, 150000, by = 5000), 9999999)
df_intervaller <- data.frame(
  fra = head(grænser, -1),
  til = tail(grænser, -1)
)
#ca. 30 intervaller
nrow(df_intervaller)

#5 sider x 20 biler giver ca. 100 biler pr. interval
SIDER_PR_INTERVAL <- 5


# ---------- Test: ét interval og én side før vi kører det hele ----------
fejlede <- c()
test <- hent_interval(30000, 35000, max_sider = 1)
#skal give ca. 20
nrow(test)
#skal give 0 eller få NA (19/11: 0 NA, 20 biler, kun "d")
colSums(is.na(test))
#skal kun give "d", hvis custtype=D virker
table(test$sælgertype)
head(test)


# ---------- Kør alle intervaller ----------
liste_tysk <- list()
fejlede    <- c()

for (i in 1:nrow(df_intervaller)) {
  liste_tysk[[i]] <- hent_interval(df_intervaller$fra[i], df_intervaller$til[i],
                                   max_sider = SIDER_PR_INTERVAL)
  #backup efter hvert interval
  saveRDS(liste_tysk, paste0("tyskland_backup_", Sys.Date(), ".rds"))
}

#antal biler pr. interval. ca. 100, færre i de dyre
sapply(liste_tysk, nrow)
#sider der fejlede. 0 betyder at alt gik godt
length(fejlede)

#kører de fejlede sider igen (springes over hvis der ingen er)
a <- ChromoteSession$new()
liste_rest <- list()
for (link in fejlede) {
  url  <- str_remove(link, "&page=\\d+$")
  side <- as.numeric(str_extract(link, "\\d+$"))
  #giver fejlen tilbage i stedet for at stoppe
  df_side <- tryCatch(hent_tysk_side(url, side), error = function(e) e)
  if (!inherits(df_side, "error") && !is.null(df_side)) liste_rest[[link]] <- df_side
}
liste_tysk <- c(liste_tysk, liste_rest)


# ---------- Saml og ryd op ----------
df_tyskland <- bind_rows(liste_tysk) |>
  #samme bil kan dukke op to gange, når biler rykker sider
  distinct(guid, .keep_all = TRUE) |>
  filter(
    #smider reklamer for andre mærker ud
    make == "mercedes-benz",
    #kun forhandlere (som Bilbasen)
    sælgertype == "d",
    #under 1.000 € er pladsholderpriser
    pris_eur > 1000,
    #kun brugte biler
    første_reg != "new"
  )

nrow(df_tyskland)

#brændstofkoderne laves om til de samme navne som på Bilbasen
#tjek koderne med count(df_tyskland, brændstof_kode) og et par stikprøver på siden,
#før vi stoler på oversættelsen
count(df_tyskland, brændstof_kode)

df_tyskland <- df_tyskland |>
  mutate(
    brændstof = case_when(
      brændstof_kode == "b" ~ "Benzin",
      brændstof_kode == "d" ~ "Diesel",
      brændstof_kode == "e" ~ "El",
      #el/benzin og el/diesel (plug-in og mild hybrid)
      brændstof_kode %in% c("2", "3") ~ "Hybrid",
      #LPG, brint osv.
      TRUE ~ "Andet"
    ),
    #"10-2018" giver 10
    måned = as.numeric(str_extract(første_reg, "^\\d{2}")),
    #"10-2018" giver 2018
    år    = as.numeric(str_extract(første_reg, "\\d{4}$")),
    
    #momsreglen ved import: under 6.000 km eller under 6 mdr. siden 1. reg. tæller som "ny", og så skal der dansk moms på (25 %)
    alder_mdr = (as.numeric(format(scrapedate, "%Y")) - år) * 12 +
      (as.numeric(format(scrapedate, "%m")) - måned),
    ny_moms   = km < 6000 | alder_mdr < 6
  )

count(df_tyskland, brændstof)
sum(df_tyskland$ny_moms, na.rm = TRUE)
colSums(is.na(df_tyskland))
summary(df_tyskland$pris_eur)

saveRDS(df_tyskland, "df_tyskland_v2.rds")


# ---------- Outlier-tjek ----------

df_tyskland |> arrange(desc(pris_eur)) |> select(model, titel, år, km, pris_eur, link) |> head(5)
df_tyskland |> arrange(pris_eur)       |> select(model, titel, år, km, pris_eur, link) |> head(5)
df_tyskland |> arrange(år)             |> select(model, titel, år, km, pris_eur, link) |> head(5)
df_tyskland |> arrange(desc(km))       |> select(model, titel, år, km, pris_eur, link) |> head(5)

#Vi har nogle biler der har "pladsholder-priser" da de først er tilgængelige i maj-måned 2027.
#Det er antaget at det er pladsholder-priser da bilernes pris ikke matcher markedsprisen

#forudannoncer ("available fra 2027") med pladsholderpris på 500.000 €, tjekket på siden
#se dem først
df_tyskland |> filter(pris_eur == 500000) |> select(model, titel, år, km, pris_eur)

df_tyskland <- df_tyskland |>
  filter(pris_eur != 500000)

nrow(df_tyskland)

#nu tjekker vi for varebiler

#varebiler, der er kommet med selvom atype=C kun skulle give personbiler
df_tyskland |> filter(model == "unknown") |> count(titel) |> head(10)
df_tyskland |> filter(str_detect(titel, regex("sprinter|vito|citan|kasten", ignore_case = TRUE))) |> nrow()

#Der er kommet varebiler og lastbiler med, vi antager at forhandleren har sat den forkerte kategori, dem fjerner vi

#erhvervskøretøjer (varebiler og lastbiler), der er kommet med selvom atype=C
erhverv <- "sprinter|vito|citan|kasten|actros|arocs|atego|antos|axor|unimog|econic|kran|kipper|lkw|sattel"

df_tyskland |>
  filter(str_detect(titel, regex(erhverv, ignore_case = TRUE))) |>
  #ser hvilke der bliver ramt, før vi fjerner dem
  count(model, sort = TRUE)

df_tyskland <- df_tyskland |>
  #! betyder ikke
  filter(!str_detect(titel, regex(erhverv, ignore_case = TRUE)))

nrow(df_tyskland)

#tjek: står der stadig erhvervskøretøjer blandt de modeller der er tilbage?
df_tyskland |> count(model, sort = TRUE) |> as_tibble() |> print(n = Inf)

#samme filter på de danske data, så begge lande behandles ens
df_mercedes_renset <- readRDS("df_mercedes_renset.rds")
df_mercedes_renset |>
  filter(str_detect(paste(model, variant), regex(erhverv, ignore_case = TRUE))) |>
  select(model, variant, pris)

df_mercedes_renset <- df_mercedes_renset |>
  filter(!str_detect(paste(model, variant), regex(erhverv, ignore_case = TRUE)))
saveRDS(df_mercedes_renset, "df_mercedes_renset.rds")

#gemmer det færdige tyske datasæt, efter alt er fjernet
saveRDS(df_tyskland, "df_tyskland_v2.rds")

nrow(df_tyskland)


#Nu går vi til at teste hypotesen,
# ---------- Er tyske biler billigere? ----------
#først gør vi de to datasæt sammenlignelige
library(dplyr)
library(stringr)

df_dk <- readRDS("df_mercedes_renset.rds")
df_de <- readRDS("df_tyskland_v2.rds")

#skal give 2.821
nrow(df_dk)
#skal give 2.623
nrow(df_de)

#fast kurs, kronen er bundet til euroen
eur_dkk <- 7.46

# ---------- fælles afgrænsning ----------
#kun biler fra 2010 og frem, fordi veteranbiler har samlerpriser og egne afgiftsregler
#kun El, Benzin og Diesel. hybrid kan ikke sammenlignes, fordi autoscout blander mild hybrid ind
dk <- df_dk |>
  filter(!is.na(år), år >= 2010,
         brændstof %in% c("El", "Benzin", "Diesel")) |>
  mutate(land = "DK",
         #"Mercedes E220 d" bliver til "e220 d"
         model_kort = str_to_lower(str_remove(model, "^Mercedes ")),
         #"e220 d" bliver til "e220d"
         model_kort = str_remove_all(model_kort, " "),
         pris_dkk = pris) |>
  select(land, model_kort, år, km, brændstof, pris_dkk)

de <- df_de |>
  filter(år >= 2010,
         brændstof %in% c("El", "Benzin", "Diesel")) |>
  mutate(land = "DE",
         #"e 220" bliver til "e220" (samme format som DK)
         model_kort = str_remove_all(model, " "),
         #tysk pris i kr., uden afgift
         pris_dkk = pris_eur * eur_dkk) |>
  select(land, model_kort, år, km, brændstof, pris_dkk, ny_moms)

nrow(dk)
nrow(de)
count(dk, brændstof)
count(de, brændstof)

#hvor mange modeller findes i begge lande?
length(intersect(dk$model_kort, de$model_kort))


# ---------- ensret modelnavne ----------
#Bilbasen skriver "e220d" og "eqe350+", autoscout24 skriver "e220" og "eqe350"
dk <- dk |>
  #fjerner +
  mutate(model_kort = str_remove_all(model_kort, "\\+"),
         model_kort = if_else(brændstof == "Diesel",
                              #fjerner diesel-d til sidst
                              str_remove(model_kort, "d$"),
                              model_kort))

de <- de |>
  mutate(model_kort = str_remove_all(model_kort, "\\+"))

#hvor mange modeller og tyske biler kan matches nu?
#antal fælles modeller
length(intersect(dk$model_kort, de$model_kort))
#andel tyske biler med en dansk model at sammenligne med
mean(de$model_kort %in% dk$model_kort)

#de mest almindelige tyske modeller uden dansk match. er der flere navneforskelle?
de |> filter(!model_kort %in% dk$model_kort) |> count(model_kort, sort = TRUE) |> head(15)
dk |> count(model_kort, sort = TRUE) |> head(15)


#AMG-navne (sidste navneforskel)
de <- de |> mutate(model_kort = str_remove(model_kort, "amg$"))
dk <- dk |> mutate(model_kort = if_else(str_detect(model_kort, "^amggt"), "amggt", model_kort))

#median pr. model, årgang og brændstof i hvert land
dk_grp <- dk |>
  group_by(model_kort, år, brændstof) |>
  summarise(dk_median = median(pris_dkk), dk_antal = n(), .groups = "drop")

de_grp <- de |>
  group_by(model_kort, år, brændstof) |>
  summarise(de_median = median(pris_dkk), de_antal = n(),
            andel_ny_moms = mean(ny_moms), .groups = "drop")

#kun grupper der findes i begge lande (inner_join beholder de fælles rækker)
sammen <- inner_join(dk_grp, de_grp, by = c("model_kort", "år", "brændstof"))

#antal sammenlignelige grupper
nrow(sammen)
#antal biler, der indgår
sum(sammen$dk_antal); sum(sammen$de_antal)
sammen |> arrange(desc(dk_antal + de_antal)) |> head(15)


# ---------- Hvorfor 45 % afgift på benzin og diesel ----------
#hvor stor en del af prisen afgiften er for en benzin- eller dieselbil (2026-skalaen fra FDM)
#25 % op til 76.400 kr., 85 % op til 237.400 kr., 150 % af resten
#bundfradrag og CO2-tillæg er ikke med
afgift_skala <- function(v) {
  0.25 * pmin(v, 76400) +
    0.85 * pmax(0, pmin(v, 237400) - 76400) +
    1.50 * pmax(0, v - 237400)
}

#bilens værdi uden afgift
værdi  <- c(150000, 200000, 250000, 300000, 400000)
afgift <- afgift_skala(værdi)
data.frame(værdi, afgift, pris = værdi + afgift,
           andel_af_pris = round(afgift / (værdi + afgift) * 100, 1))
#ud fra tabellen vælger vi 45 %


# ---------- Hvad koster den tyske bil på danske plader? ----------
#vi lægger afgift, moms og omkostninger oven i den tyske pris
#og sammenligner med den danske pris (begge biler på danske plader)

#afgift på benzin og diesel er 45 % af den danske pris (skalaen giver typisk 35-50 %)
afgift_fossil <- 0.45
#transport, eksportplader og syn (vores eget skøn)
omkostninger  <- 5000

resultat <- sammen |>
  mutate(
    #1. afgift: regnes af den danske pris, det er reglen for brugte importbiler
    #el: 0 kr. under den afgiftsfri grænse på 419.300 kr. (2026)
    #over grænsen er satsen 60 % af bilens værdi uden afgift. prisen på Bilbasen er med afgift,
    #så det svarer til 37,5 % af den del af prisen, der ligger over grænsen (0,6 / 1,6 = 0,375)
    #benzin og diesel: 45 % af den danske pris (skøn ud fra afgiftsskalaen)
    afgift = if_else(brændstof == "El",
                     #el: 37,5 % af prisen over grænsen
                     0.375 * pmax(0, dk_median - 419300),
                     #benzin og diesel: 45 % af prisen
                     afgift_fossil * dk_median),
    
    #2. moms: "nye" tyske biler (under 6.000 km eller 6 mdr.) skal have dansk moms i stedet for tysk, det er ca. 5 % ekstra
    moms = de_median * andel_ny_moms * 0.05,
    
    #3. den tyske bil på danske plader er tysk pris plus moms, afgift og omkostninger
    de_dk_plader = de_median + moms + afgift + omkostninger,
    
    #4. forskel: er den positiv, er det billigere at hente bilen i Tyskland
    forskel = dk_median - de_dk_plader
  )

#opsummering pr. brændstof
resultat |>
  group_by(brændstof) |>
  summarise(grupper           = n(),
            andel_billigst_de = mean(forskel > 0),
            median_forskel    = median(forskel))

#følsomhed: sæt afgift_fossil til 0.35 og 0.55 og kør blokken igen
#resultat: benzin billigst i DE 45 % / 37 % / 22 %, diesel 60 % / 48 % / 16 %

saveRDS(resultat, "resultat_opgave_1_2.rds")


library(ggplot2)
library(dplyr)

resultat <- readRDS("resultat_opgave_1_2.rds")

#typisk forskel pr. brændstof (3 rækker)
graf_data <- resultat |>
  group_by(brændstof) |>
  summarise(forskel = median(forskel))

#søjlediagram
ggplot(graf_data, aes(x = factor(brændstof, levels = c("El", "Diesel", "Benzin")),
                      y = forskel, fill = forskel > 0)) +
  geom_col() +
  geom_text(aes(label = paste0(format(round(forskel), big.mark = ".", decimal.mark = ","), " kr."),
                vjust = ifelse(forskel > 0, -0.5, 1.5)),
            #tallene over søjlerne
            size = 5) +
  geom_hline(yintercept = 0) +
  labs(title = "Elbiler er typisk billigere at hente i Tyskland - benzinbiler er ikke",
       x = NULL, y = "Besparelse ved import (kr.)") +
  scale_fill_manual(values = c("TRUE" = "darkgreen", "FALSE" = "darkred"), guide = "none") +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", suffix = " kr."),
                     breaks = seq(-50000, 50000, by = 25000),
                     limits = c(-50000, 60000)) +
  theme_minimal() +
  #overskriften
  theme(plot.title   = element_text(size = 18, face = "bold"),
        #tallene på y-aksen og teksten på x-aksen
        axis.text    = element_text(size = 13),
        #teksten på y-aksen
        axis.title.y = element_text(size = 14))

ggsave("graf_opgave_1_2.png", width = 7, height = 4)
