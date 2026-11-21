# ---------- Opg 1.1: Scrape Mercedes fra Bilbasen ----------
#vi tager kun personbiler, Mercedes, kontantpris, forhandlere og køb (ikke leasing)

# ---------- Pakker ----------
#chromote styrer en rigtig Chrome, fordi Bilbasen bygger siden med JavaScript
library(chromote)
#rvest læser html'en
library(rvest)
#dplyr til bind_rows, distinct og mutate
library(dplyr)
#stringr til regulære udtryk
library(stringr)

# ---------- URL ----------
#pricetype=Retail giver kun kontantpris, så engros/CVR og biler uden afgift ryger ud
#sellertypes=dealer giver kun forhandlere og ingen private
#fanen "Køb" er valgt fra start, så leasing er ikke med
baseurl <- "https://www.bilbasen.dk/brugt/bil/mercedes?pricetype=Retail&sellertypes=dealer"

# ---------- Start browser ----------
#åbner browseren
a <- ChromoteSession$new()
#viser browseren, så vi kan se hvad der sker
a$view()
#skal sige "ChromoteSession" "R6", ellers kan vi ikke scrape
class(a)


# ---------- Funktion 1: hent én bil ----------
#vi bruger data-e2e hvor det findes. Bilbasen bruger dem selv til test, så de skifter ikke navn
#ellers starten af klassenavnet med [class^=...], fordi endelsen (fx __6B3kE) kan skifte

hent_bil <- function(kasse) {
  
  #linket står i href og ikke i teksten
  link <- kasse |>
    html_element("a[data-e2e='search-result-link']") |>
    html_attr("href")
  
  #carid er det sidste tal i linket. \\d+ er et eller flere cifre, og $ betyder at de skal stå til sidst
  carid <- str_extract(link, "\\d+$")
  
  #modellen, fx "Mercedes EQB250", står i data-e2e-value
  model <- kasse |>
    html_element("h3[data-e2e='listing-make-model']") |>
    html_attr("data-e2e-value")
  
  #variant er hele titlen minus modellen, fx "Progressive 5d"
  #fixed() læser model som almindelig tekst, så + og ( ikke bliver til regex
  variant <- kasse |>
    html_element("[data-e2e='search-result-make-model']") |>
    html_text2() |>
    str_remove(fixed(model)) |>
    str_trim()
  
  #pris: vi fjerner alt der ikke er et tal, så "394.900 kr." bliver 394900. ^ inde i [ ] betyder "ikke"
  #uden for [ ] betyder ^ starten af teksten
  pris <- kasse |>
    html_element("[data-e2e='search-result-price']") |>
    html_text2() |>
    str_remove_all("[^0-9]") |>
    as.numeric()
  
  #egenskaber: [1] er årgang og [2] er km. de bliver lavet til tal i rensningen
  #[class^='Listing_properties'] betyder at class starter med Listing_properties
  #her betyder ^ altså "starter med"
  egenskaber <- kasse |>
    html_elements("[class^='Listing_properties'] div") |>
    html_text2()
  
  #alle detaljer (gear, brændstof, km/l, rækkevidde) samles i én tekst
  #rækkefølgen er forskellig for el og benzin, så vi deler dem op senere
  detaljer <- kasse |>
    html_elements("[class^='ListingDetails_listItem']") |>
    html_text2() |>
    paste(collapse = " | ")
  
  #salgsteksten. her skriver forhandleren tit navn, adresse og telefon
  beskrivelse <- kasse |>
    html_element("[class^='Listing_description']") |>
    html_text2()
  
  #by og område
  sted <- kasse |>
    html_element("[class^='Listing_location']") |>
    html_text2()
  
  #logoet står to gange på siden, og html_element tager kun det første
  #samme logo betyder samme forhandler, så vi bruger det som forhandler-id
  forhandlerlogo <- kasse |>
    html_element("img[alt='Forhandlerlogo']") |>
    html_attr("src")
  
  #én række pr. bil. mangler et felt, bliver det NA, så kolonnerne ikke bliver forskudt
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
    #datoen skal bruges i opgave 2
    scrapedate     = Sys.Date()
  )
}


# ---------- Funktion 2: hent én side (ca. 30 biler) ----------
hent_side <- function(side_nr) {
  a$go_to(paste0(baseurl, "&page=", side_nr))
  
  #tilfældig pause på 5-8 sek, så JavaScript kan nå at loade, og vi ikke spammer Bilbasen
  Sys.sleep(sample(5:8, 1))
  
  #henter hele siden, når JavaScript er færdig, og giver den til rvest
  html <- a$Runtime$evaluate("document.documentElement.outerHTML")$result$value
  kasser <- read_html(html) |>
    #hver bil ligger i en <article>
    html_elements("article")
  
  #skriver i konsollen hvor langt vi er
  cat("Side", side_nr, ":", length(kasser), "biler\n")
  
  #er siden tom, er noget gået galt, så vi laver en fejl med vilje, som tryCatch fanger
  if (length(kasser) == 0) stop(paste("ingen biler på side", side_nr))
  
  #kører hent_bil på alle 30 biler
  lapply(kasser, hent_bil) |>
    #samler de 30 rækker i én data frame
    bind_rows()
}


# ---------- Test: én side før vi kører det hele ----------
test <- hent_side(1)
#skal give ca. 30
nrow(test)
#NA pr. kolonne, skal være 0 eller tæt på
colSums(is.na(test))
head(test)


# ---------- Hvor mange sider er der? ----------
#læser "Viser: 2.854 biler til salg" og regner antal sider ud, 30 biler pr. side
a$go_to(baseurl)
Sys.sleep(5)
side_tekst <- read_html(a$Runtime$evaluate("document.documentElement.outerHTML")$result$value) |>
  html_text2()

antal_biler <- side_tekst |>
  str_extract("Viser:\\s*[0-9.]+") |>
  str_remove_all("[^0-9]") |>
  as.numeric()

#ceiling runder op, så den sidste halve side kommer med
antal_sider <- ceiling(antal_biler / 30)
antal_biler
#2.854 biler giver 96 sider (18/11)
antal_sider


# ---------- Scrape alle sider ----------
#her gemmer vi hver side
liste_sider <- list()
#her skriver vi sider der fejler
fejlede     <- c()

for (i in 1:antal_sider) {
  #tryCatch prøver at hente siden. går det galt, skriver den fejlen i konsollen
  #og giver fejlen tilbage i stedet for at stoppe loopet
  resultat <- tryCatch(
    hent_side(i),
    error = function(e) {
      cat("Fejl på side", i, ":", conditionMessage(e), "\n")
      e
    }
  )
  
  #inherits tjekker om det er en fejl. er det en fejl, skriver vi sidenummeret ned
  #er det data, gemmer vi siden
  if (inherits(resultat, "error")) {
    fejlede <- c(fejlede, i)
  } else {
    liste_sider[[i]] <- resultat
  }
  
  #backup efter hver side
  saveRDS(liste_sider, "bilbasen_midlertidig.rds")
}

#sider der skal køres igen. er den tom, gik alt godt
fejlede

#kører de fejlede sider igen (springes over hvis der ingen er)
for (i in fejlede) {
  #giver fejlen tilbage i stedet for at stoppe
  resultat <- tryCatch(hent_side(i), error = function(e) e)
  if (!inherits(resultat, "error")) liste_sider[[i]] <- resultat
}


# ---------- Saml og fjern dubletter ----------
#nye annoncer skubber biler ned på næste side, så samme bil kan komme med to gange
df_mercedes <- bind_rows(liste_sider) |>
  distinct(carid, .keep_all = TRUE)

#skal passe med antal_biler
nrow(df_mercedes)
#NA pr. kolonne
colSums(is.na(df_mercedes))


# ---------- Rensning af scrapet fra 17/11 ----------
#et scrape giver lidt andre biler hver dag. derfor bruger 1.2 og opgave 2 den gemte kørsel fra 17/11,
#så vi kan få de samme tal igen
df_mercedes_OLA <- readRDS("df_mercedes_OLA_v2.rds")

#samme bil kan stå på to sider, så vi beholder kun én række pr. carid
df_mercedes_OLA <- distinct(df_mercedes_OLA, carid, .keep_all = TRUE)

# ---------- Forhandlerinfo og pris til opgave 2 ----------
#beskrivelsen er salgsteksten. nogle forhandlere skriver mail og telefon i den, og dem henter vi med regulære udtryk
#regulært udtryk for mail = alt uden mellemrum før og efter @
email <- str_extract(df_mercedes_OLA$beskrivelse, "\\S+@\\S+")

#telefon: \\b = her starter eller slutter et tal
#[2-9] = første ciffer er fra 2-9, \\d = næste ciffer er fra 0-9
#mellemrum + ? = mellemrummet må være der 0 eller 1 gang
#\\d{2} = to cifre. det gentages tre gange, så det i alt er 8 cifre
#\\b til sidst = tallet er slut, så vi ikke tager et stykke af et længere tal
telefon <- str_extract(df_mercedes_OLA$beskrivelse, "(\\+45 ?)?\\b[2-9]\\d ?\\d{2} ?\\d{2} ?\\d{2}\\b")

#fjerner +45 og mellemrum, så alle numre står som 8 cifre
telefon <- str_remove_all(telefon, "\\+45|\\s")

#NA hvor forhandleren ikke oplyser det
df_mercedes_OLA$email <- email
df_mercedes_OLA$telefon <- telefon
rm(email, telefon)

#tjek: 667 mails og 952 numre udfyldt
colSums(is.na(df_mercedes_OLA))

#henter tallet (cifre og punktummer), fjerner tusindtalspunktummer og gør det til tal
df_mercedes_OLA <- df_mercedes_OLA %>%
  mutate(pris_kr = pris %>%
           str_extract("[\\d.]+") %>%
           str_remove_all("\\.") %>%
           as.numeric())

# ---------- 1. lav kolonnerne ----------
df_mercedes_renset <- df_mercedes_OLA |>
  #fjerner 1 dublet
  distinct(carid, .keep_all = TRUE) |>
  mutate(
    #"334.900 kr." bliver til 334900
    pris      = as.numeric(str_remove_all(pris, "[^0-9]")),
    #tallet før / er måneden (^ betyder starten)
    måned     = as.numeric(str_extract(årgang, "^\\d+")),
    #fire cifre til sidst er året ($ betyder slutningen)
    år        = as.numeric(str_extract(årgang, "\\d{4}$")),
    #"69.000 km" bliver til 69000
    km        = as.numeric(str_remove_all(km, "[^0-9]")),
    #brændstof er sidste del af detaljer, fx "Diesel"
    brændstof = str_trim(str_extract(detaljer, "[^|]+$")),
    #nye biler uden 1. registrering
    ny_uregistreret = årgang == "-"
  )

# ---------- 2. tjek ----------
#skal give 2.836
nrow(df_mercedes_renset)
#3 NA i måned og år, det er de nye biler
colSums(is.na(df_mercedes_renset[, c("pris", "måned", "år", "km")]))
#El, Benzin, Diesel og Plug-in
count(df_mercedes_renset, brændstof)
#skal give 3
sum(df_mercedes_renset$ny_uregistreret)
summary(df_mercedes_renset$år)
summary(df_mercedes_renset$km)
summary(df_mercedes_renset$pris)

#outliers, som vi har tjekket på Bilbasen
df_mercedes_renset |> filter(is.na(år)) |> select(model, årgang, km, pris, link)
#nye biler fra 2026 uden 1. registrering, så NA er rigtigt

df_mercedes_renset |> arrange(pris) |> select(model, årgang, km, pris, link) |> head(5)
#den billigste (9.800 kr.) er et mekanikertilbud. resten er gamle dieselbiler med høj km, og de er ægte

df_mercedes_renset |> arrange(desc(pris)) |> select(model, årgang, km, pris, link) |> head(5)
#AMG GT 63 og GT C, prisen passer, de er ægte

df_mercedes_renset |> arrange(desc(km)) |> select(model, årgang, km, pris, link) |> head(5)
#gamle E-klasser (typisk taxaer) og 1 Sprinter, lave priser, de er ægte

df_mercedes_renset |> arrange(år) |> select(model, årgang, km, pris, link) |> head(5)
#veteranbiler (190 SL fra 1956-61) med samlerpriser. de er ægte, men kommer ikke med i 1.2

# ---------- 3. fjern ----------
df_mercedes_renset <- df_mercedes_renset |>
  #mekanikertilbuddet til 9.800 kr.
  filter(carid != "5736777")

#skal give 2.835
nrow(df_mercedes_renset)

#erhvervskøretøjer. samme filter som på de tyske data i 1.2
erhverv <- "sprinter|vito|citan|kasten|actros|arocs|atego|antos|axor|unimog|econic|kran|kipper|lkw|sattel"

df_mercedes_renset |>
  filter(str_detect(paste(model, variant), regex(erhverv, ignore_case = TRUE))) |>
  #14 varebiler (Sprinter, Vito, Citan m.fl.)
  select(model, variant, pris)

df_mercedes_renset <- df_mercedes_renset |>
  #mekanikertilbuddet til 9.800 kr.
  filter(carid != "5736777",
         #og erhvervskøretøjerne
         !str_detect(paste(model, variant), regex(erhverv, ignore_case = TRUE)))

#skal give 2.821
nrow(df_mercedes_renset)

# ---------- 4. gem ----------
saveRDS(df_mercedes_renset, "df_mercedes_renset.rds")
