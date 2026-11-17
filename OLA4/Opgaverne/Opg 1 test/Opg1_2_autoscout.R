#Opgave 1.2 - Hente biler fra Tyskland
#Samme mærke som 1.1: Mercedes. Samme fremgangsmåde som Bilbasen: chromote + rvest
#autoscout ligger ALT som data-attributter direkte på article-tagget - ingen gæt på mærkelige CSS-klassenavne. Lækkert nok.
library(chromote)
library(rvest)
library(stringr)
library(dplyr)
library(tryCatchLog)


#tester med den her med pris fra 10.000 til 50.000 €
testurl <- "https://www.autoscout24.com/lst/mercedes-benz?priceto=50000&cy=D&damaged_listing=exclude&desc=0&ocs_listing=include&powertype=kw&pricefrom=10000&sort=standard&ustate=N%2CU&page=1&atype=C&search_id=1jyhg1l1jif&source=listpage_pagination"

#cy=D er Tyskland, atype=C er personbiler, damaged_listing=exclude dropper skadede
baseurl <- "https://www.autoscout24.com/lst/mercedes-benz?cy=D&damaged_listing=exclude&ocs_listing=include&ustate=N%2CU&atype=C&powertype=kw"
a <- ChromoteSession$new()
a$view()
#a$go_to(paste0(testurl)) 
a$go_to(paste0(baseurl)) 

####------ én bil ad gangen ------####
hent_tysk_bil <- function(kasse) {
  attrs <- html_attrs(kasse)
  hent <- function(navn) ifelse(navn %in% names(attrs), attrs[[navn]], NA)  #nogle biler mangler et felt

  titel <- kasse %>%
    html_element("h2") %>%
    html_text2()

  forhandler <- kasse %>%
    html_element("[data-testid='dealer-company-name']") %>%
    html_text2()  

  forhandler_adresse <- kasse %>%
    html_element("[data-testid='dealer-address']") %>%
    html_text2()

  data.frame(
    guid = hent("data-guid"),  #autoscouts eget bil-id, vores "carid" 
    make = hent("data-make"),  #autoscout sniger boost-ANNONCER fra andre mærker ind, fx en BMW - dem fanger vi her
    titel = titel,
    model = hent("data-model"),
    pris_eur = as.numeric(hent("data-price")),  #rent tal, ingen €-parsing - tak autoscout
    km = as.numeric(hent("data-mileage")),
    første_reg = hent("data-first-registration"),  #fx "04-2012"
    sælgertype = hent("data-seller-type"),  #d = dealer, p = privat
    forhandler = forhandler,
    forhandler_adresse = forhandler_adresse,
    scrapedate = Sys.Date()
  )
}

####------ én side ad gangen ------####
hent_tysk_side <- function(url, side_nr) {
  #a$go_to(paste0(url, "&page=", side_nr))
  a$Page$navigate(paste0(url, "&page=", side_nr))  #gå bare til siden, vent ikke på at hver eneste reklame er loadet
  Sys.sleep(sample(5:8, 1))  #det er DEN her, der giver JS tid, ikke go_to

  html <- a$Runtime$evaluate("document.documentElement.outerHTML")$result$value  #den færdige side efter JS
  kasser <- read_html(html) %>%
    html_elements("article")

  cat("  side", side_nr, ":", length(kasser), "biler\n")
  if (length(kasser) == 0) return(NULL)

  lapply(kasser, hent_tysk_bil) %>%
    bind_rows()
}

####------ prisintervaller + sidedybde ------####
#Autoscout paginerer op til 200 sider pr. søgning (tjekket 12/11: side 21 virker,
#og pagineringen tilbyder op til side 200 = 4.000 biler pr. søgning).
#Vi bruger alligevel prisintervaller - ikke pga. et sideloft, men for at få biler
#fra HELE prisspektret i stedet for kun de 4.000 første i standard-sorteringen.
df_intervaller <- data.frame(
  fra = c(0, 10000, 20000, 30000, 40000, 50000, 150000),
  til = c(10000, 20000, 30000, 40000, 50000, 150000, 9999999)  #sidste er åbent: 4,6 mio børgen fra bilbasen skal med
)

MAX_SIDER <- 25  #25 sider x 20 biler x 7 intervaller = ca. 3.500 biler

#anvender her tryCatchLog som fanger fejl uden at stoppe hele loopet

fejlede <- c()  #her noterer vi de sider, der fejler, så vi kan prøve dem igen bagefter

# hent_interval <- function(fra, til) {
#   url <- paste0(baseurl, "&pricefrom=", fra, "&priceto=", til, "&sort=price&desc=0")  #sorteret efter pris, så intervallet fyldes nedefra
#   cat("interval", fra, "-", til, "€\n")
#   liste <- list()
#   for (side in 1:MAX_SIDER) {
#     df_side <- tryLog(hent_tysk_side(url, side))  #fejler siden, logges den og vi kører videre
# 
#     if (inherits(df_side, "try-error")) {
#       fejlede <<- c(fejlede, paste0(url, "&page=", side))  #<<- = gem i fejlede UDENFOR funktionen
#       next  #spring til næste side
#     }
# 
#     if (is.null(df_side)) break  #tom side = intervallet er tømt, videre til næste
#     liste[[side]] <- df_side
#   }
#   bind_rows(liste)
# }
#### overstående var gamle kør
hent_interval <- function(fra, til) {
  a <<- ChromoteSession$new()  #ny browser til hvert interval, så Chrome ikke løber tør for hukommelse
  url <- paste0(baseurl, "&pricefrom=", fra, "&priceto=", til, "&sort=price&desc=0")
  cat("interval", fra, "-", til, "€\n")
  liste <- list()
  for (side in 1:MAX_SIDER) {
    df_side <- tryLog(hent_tysk_side(url, side))
    
    if (inherits(df_side, "try-error")) {
      fejlede <<- c(fejlede, paste0(url, "&page=", side))
      a <<- ChromoteSession$new()  #browseren er måske død, så vi starter en ny og kører videre
      Sys.sleep(5)
      next
    }
    
    if (is.null(df_side)) break
    liste[[side]] <- df_side
  }
  a$close()  #luk browseren pænt når intervallet er færdigt
  bind_rows(liste)
}
####------ kør det hele ------####
#OBS: kør IKKE den her igen, den sletter liste_tysk
liste_tysk <- list()
for (i in 1:nrow(df_intervaller)) {
  liste_tysk[[i]] <- hent_interval(df_intervaller$fra[i], df_intervaller$til[i])
  saveRDS(liste_tysk, "OLA4/Opgaverne/tyskland_midlertidig.rds")  #sikkerhedskopi efter hvert interval
}

#sessionen døde 12/11 midt i kørslen - tjek hvilke intervaller der skal køres igen
sapply(liste_tysk, nrow)  #antal biler pr. interval, fuldt interval = ca. 500

fejlede <- c()
for (i in c(5, 6, 7)) {  #ret tallene til de intervaller der manglede
  liste_tysk[[i]] <- hent_interval(df_intervaller$fra[i], df_intervaller$til[i])
  saveRDS(liste_tysk, "OLA4/Opgaverne/tyskland_midlertidig.rds")
}

length(fejlede)  #sider der fejlede i genkørslen - til rapporten



####------ fortsæt fra hvor sessionen døde ------####
gamle_fejl <- fejlede  #gem listen over manglende sider, før vi nulstiller
fejlede <- c()

liste_rest <- list()
for (link in gamle_fejl) {
  url <- str_remove(link, "&page=\\d+$")  #linket uden sidetal
  side <- as.numeric(str_extract(link, "\\d+$"))  #sidetallet til sidst i linket
  df_side <- tryLog(hent_tysk_side(url, side))  #samme funktion som før, ingen ny opskrift
  
  if (inherits(df_side, "try-error")) {
    fejlede <- c(fejlede, link)  #fejlede igen - noteres
    a <- ChromoteSession$new()  #ny browser og videre
    Sys.sleep(5)
    next
  }
  liste_rest[[link]] <- df_side
}

liste_tysk <- c(liste_tysk, liste_rest)  #læg de nye biler oven i dem vi havde
saveRDS(liste_tysk, "OLA4/Opgaverne/tyskland_midlertidig.rds")
length(fejlede)  #sider der STADIG fejler

####------ ryd op ------####
df_tyskland <- bind_rows(liste_tysk) %>%
  distinct(guid, .keep_all = TRUE) %>%  #samme bil kan dukke op to gange: grænserne overlapper, og biler rykker sider mens vi scraper
  filter(make == "mercedes-benz") %>%  #smider boost-annoncerne ud (BMW'er i Mercedes-søgningen, frækt)
  filter(!is.na(pris_eur), pris_eur >= 1000)  #fjerner 1 €-pladsholdere og biler uden pris

nrow(df_tyskland)  #forventet ca. 3.500
saveRDS(df_tyskland, "OLA4/Opgaverne/df_tyskland.rds")


length(fejlede)  #hvor mange sider mangler vi
head(fejlede)

####------ Er tyske biler billigere? ------####
df_mercedes <- readRDS("OLA4/Opgaverne/df_mercedes.rds")

eur_dkk <- 7.46  #fast kurs, ca.

df_tyskland <- df_tyskland %>%
  mutate(
    pris_dkk = pris_eur * eur_dkk,  #uden dansk registreringsafgift!
    år = as.numeric(str_extract(første_reg, "\\d{4}"))
  )

df_dk <- df_mercedes %>%
  mutate(år = as.numeric(str_extract(årgang, "\\d{4}$")))

#hurtig sammenligning på medianer
median(df_dk$pris, na.rm =s TRUE)
median(df_tyskland$pris_dkk, na.rm = TRUE)

#sammenlign pr. årgang
df_dk %>%
  group_by(år) %>%
  summarise(dk_median = median(pris, na.rm = TRUE), dk_antal = n()) %>%
  inner_join(
    df_tyskland %>%
      group_by(år) %>%
      summarise(de_median = median(pris_dkk, na.rm = TRUE), de_antal = n()),
    by = "år") %>%
  arrange(desc(år))

