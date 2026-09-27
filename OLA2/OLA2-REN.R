#OLA2
library(dkstat)
library(tidyr)
library(ggplot2)   # RETTET: flyttet op, så alle pakker loades samlet
dst_get_tables()

#Opgave 1.1 – Det første skridt
#Skriv en kode, der viser hvordan du finder en tabel som kan give en liste over byer med indbyggertal vha
#DST-pakken dkstat.

# RETTET: søgning tilføjet, så det kan ses HVORDAN vi fandt BY3
dst_search(string = "byområde", field = "text")

by3_meta <- dst_meta(table = "BY3", lang = "da") #Henter tabel By3

by3_meta$variables # vi vil se variablerne navne

#Indlæs data 

byer_data <- dst_get_data(
  table = "BY3", #vi vil have tabel by3
  BYER = "*", #vi vil have alle byer
  Tid  = "2026", #vi vil for 2026
  FOLKARTAET = "Folketal",
  meta_data = by3_meta, #trække data fra by3 meta, som vi allerede har hivet ind i r.
  lang = "da"
)

#Sum data
sum(byer_data$value) #sum af folketal hele 2026. for at validerer folketal, med andre datasæt

#Trin 1: Fjern 0-værdier FØRST
byer_data_ren <- byer_data[byer_data$value != 0, ]

#Trin 2: Fjern "Uden fast bopæl" fra den RENSEDE data, da det ikke giver mening når vi skal finde byer
byer_data_ren <- byer_data_ren[!grepl("Uden fast bopæl", byer_data_ren$BYER), ]

#Trin 3: Fjern "Landdistriker" fra den Rensede data, da det ikke giver mening når vi skal finde byer
byer_data_ren <- byer_data_ren[!grepl("Landdistrikter", byer_data_ren$BYER), ]

#Tjek resultatet
nrow(byer_data_ren)
sum(byer_data_ren$value)

#5.342.281 = er vores resultat
#vi har vej hjælp af filtering, fjernet 683.322 personer/nulværdier fra datasættet - jubii
#det svarer til 11.34% af første datasæt er fjernet

#Opgave 1.2 – Kategori-variabel.
#Lav en kategorivariabel i R hvor du skal inddele byerne i følgende kategorier: "landsby","lille by",
#"almindelig by", "større by", "storby" ud fra et interval på indbyggertal som du selv definerer.

# RETTET: rensefunktion flyttet hertil, fordi bynavnene skal renses FØR vi kategoriserer
rens_by <- function(x) {
  x <- tolower(x)                      # alt til små bogstaver
  x <- gsub("\\s*\\(.*?\\)", "", x)    # fjern alt i parentes
  x <- gsub("[0-9]", "", x)            # fjern tal/id'er
  x <- gsub("æ", "ae", x)
  x <- gsub("ø", "oe", x)
  x <- gsub("å", "aa", x)
  x <- gsub("é", "e", x)
  x <- gsub("-", " ", x)               # bindestreg til mellemrum
  trimws(x)                            # fjern mellemrum i enderne
}

byer_data_ren$by <- rens_by(byer_data_ren$BYER)
head(byer_data_ren[, c("BYER", "by")])   # tjek at koder og parenteser er væk

# RETTET: byer der krydser kommunegrænser står flere gange (fx Birkerød: 4 + 27 + 21.211).
# Vi lægger delene sammen, så hver by kun optræder én gang med sit samlede folketal.
byer_samlet <- aggregate(value ~ by, data = byer_data_ren, FUN = sum)
sum(duplicated(byer_samlet$by))   # skal være 0

# RETTET: kategorier laves på de summerede byer, og grænserne er flyttet.
# DST tæller kun bebyggelser med mindst 200 indbyggere som byområde, så "< 200" ville give en tom kategori.
# Kolonnen hedder bycat, som opgaveteksten beder om.
byer_samlet$bycat <- ifelse(
  byer_samlet$value < 1000, "landsby",
  ifelse(
    byer_samlet$value < 5000, "lille by",
    ifelse(
      byer_samlet$value < 20000, "almindelig by",
      ifelse(
        byer_samlet$value < 100000, "større by", 
        "storby"
      )
    )
  )
)

table(byer_samlet$bycat)   # RETTET: tjek at alle fem kategorier har byer
#husk tilknytning til artikel, der fortæller os hvor stor vores x værdier er . landsby.



### --- Opgave 1.3 --- ###
# Indlæs filen med boliger og tilpas de to dataframes så du kan merge de to sammen via variablen ”by”
# således at du får kategorien bycat med i dit bolig-datasæt fra OLA 1.

# Indlæs boligsiden data
### --- RENSE DATA STEP --- ###

#Total renset importeret datasæt for NA
boligsiden_OLA[boligsiden_OLA == "NA"] <- NA
dfBoligSiden_ingenNA <- na.omit(boligsiden_OLA)

#Gemme subset med alle NA værdier
dfBoligSiden_NA <- boligsiden_OLA[!complete.cases(boligsiden_OLA), ]

#subSet til test
subTest <- dfBoligSiden_ingenNA

#rette forkert indtastede postnr.
dfBoligSiden_ingenNA$postnr[264] <- as.numeric(dfBoligSiden_ingenNA$by[264])
dfBoligSiden_ingenNA$postnr[1740] <- as.numeric(dfBoligSiden_ingenNA$by[1740])
dfBoligSiden_ingenNA$postnr[2031] <- as.numeric(dfBoligSiden_ingenNA$by[2031])

#sætte byen ind rigtig
dfBoligSiden_ingenNA$by[2031] <- as.character("hilleroed")
dfBoligSiden_ingenNA$by[1740] <- as.character("kibaek")
dfBoligSiden_ingenNA$by[264] <- as.character("moeldrup")

#Yderligere variable, region & alder
#lave funktion der kan give afkode region ud fra postnummer
find_region <- function(postnr) {
  retval = 0
  if(postnr >= 1000 & postnr <= 2999){
    retval = "hovedstaden"
  } else if (postnr >= 3000 & postnr <= 3699) {
    retval = "sjælland"
  } else if (postnr >= 3700 & postnr <= 3790) {
    retval = "bornholm"
  } else if (postnr >= 4000 & postnr <= 4999) {
    retval = "sydsjælland"
  } else if (postnr >= 5000 & postnr <= 6999) {
    retval = "syddanmark"
  } else if (postnr >= 7000 & postnr <= 7999) {
    retval = "midtjylland"
  } else if (postnr >= 8000 & postnr <= 8999) {
    retval = "aarhus omegn"
  } else if (postnr >= 9000 & postnr <= 9990) {
    retval = "nordjylland"
  } else {
    retval = NA   # RETTET: rigtig NA i stedet for teksten "NA"
  }
  return(retval)
}

#etabler ny kolonne med region
dfBoligSiden_ingenNA$region <- sapply(dfBoligSiden_ingenNA$postnr, find_region)

#Lave en alders beregner
alderBr <- function(opført) {
  retval = 0
  if (opført > 0) {
    retval = 2026 - opført
  } else {
    retval = NA   # RETTET: teksten "NA" gjorde hele alder-kolonnen til tekst i stedet for tal
  }
  return(retval)
}

# En den af rense steppet
dfBoligSiden_ingenNA$pris <- as.numeric(dfBoligSiden_ingenNA$pris)
dfBoligSiden_ingenNA$opført <- as.numeric(dfBoligSiden_ingenNA$opført)
dfBoligSiden_ingenNA$kvmpris <- as.numeric(dfBoligSiden_ingenNA$kvmpris)
dfBoligSiden_ingenNA$størrelse <- as.numeric(dfBoligSiden_ingenNA$størrelse)
dfBoligSiden_ingenNA$mdudg <- as.numeric(dfBoligSiden_ingenNA$mdudg)
dfBoligSiden_ingenNA$grund <- as.numeric(dfBoligSiden_ingenNA$grund)
dfBoligSiden_ingenNA$værelser <- as.numeric(dfBoligSiden_ingenNA$værelser)
dfBoligSiden_ingenNA$postnr <- as.numeric(dfBoligSiden_ingenNA$postnr)
dfBoligSiden_ingenNA$vejnr <- as.numeric(dfBoligSiden_ingenNA$vejnr)

#Kan ikke direkte overføres til numeric, fordi de har skrevet dag. Så skal først renses for dag, derefter konverteres
dfBoligSiden_ingenNA$liggetid <- gsub("[a-zA-Z]", " ", dfBoligSiden_ingenNA$liggetid)   # RETTET: A-z til A-Z
dfBoligSiden_ingenNA$liggetid <- as.numeric(dfBoligSiden_ingenNA$liggetid)

#etabler ny kolonne med alder
dfBoligSiden_ingenNA$alder <- sapply(dfBoligSiden_ingenNA$opført, alderBr)

# RETTET: bynavne i boligdata renses med samme funktion som DST-data
dfBoligSiden_ingenNA$by <- rens_by(dfBoligSiden_ingenNA$by)

# RETTET: fjern postdistrikt-bogstaver (koebenhavn k, aarhus c, odense m osv.), som DST ikke bruger
dfBoligSiden_ingenNA$by <- sub(" (c|k|v|n|s|m|sv|nv|oe|soe|noe)$", "", dfBoligSiden_ingenNA$by)


#Opgave 1.4 –Plot
#Din merge skal producere en dataframe og et plot, som minder om det du ser nedenfor - men den
#præcise udformning kommer naturligvis an på hvilken inddeling du vælger.

# Merge i nyt datasæt
# Kolonner fra boligsiden: By, Kvmpris, pris
# Kolonner fra by data: By, bycat (important at fremhæve vores definationer af bykatergoierne)

# RETTET: den første merge med alle kolonner er slettet, og der merges nu med byer_samlet
samlet_df <- merge(
  dfBoligSiden_ingenNA[, c("by", "pris", "kvmpris", "region")],
  byer_samlet[, c("by", "bycat", "value")],
  by = "by",
  all.x = TRUE
)

# RETTET: kontrol af at merge ikke har ganget boliger op
nrow(dfBoligSiden_ingenNA) == nrow(samlet_df)   # skal være TRUE

sum(is.na(samlet_df$bycat))
head(sort(table(samlet_df$by[is.na(samlet_df$bycat)]), decreasing = TRUE), 25)

samlet_df <- na.omit(samlet_df)   # RETTET: først EFTER kontrollen, ellers skjules problemer

# RETTET: plottet laves på en opsummeret tabel med gennemsnit og antal pr. kategori
plot_df <- aggregate(kvmpris ~ bycat, data = samlet_df, FUN = mean)
plot_df$antal <- aggregate(kvmpris ~ bycat, data = samlet_df, FUN = length)$kvmpris
plot_df$label <- paste0(plot_df$bycat, "\n(n = ", plot_df$antal, ")")

ggplot(plot_df, aes(x = reorder(label, kvmpris), y = kvmpris)) +
  geom_col(fill = "pink", width = 0.7) +
  geom_text(aes(label = format(round(kvmpris), big.mark = ".", decimal.mark = ",")),
            vjust = -0.5, size = 3.5) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ","),
                     expand = expansion(mult = c(0, 0.08))) +
  labs(title = "Storby har den højeste pris pr kvm i kr",
       subtitle = "Boliger til salg, byer kategoriseret efter DST's byområder 2026",
       x = "Bykategori", y = "Kr. pr. m²",
       caption = "Kilde: Boligsiden og Danmarks Statistik (BY3)") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank())


#NOTE, VI VIL KOMME TILBAGE OG SE PÅ NOGLE HVORFOR DE FORSKELLIGE KATEGORIER KOSTER DET DE GØR.


# RETTET: histogram-plottet og ?ggplot er slettet (histogram virker ikke med kategori på x-aksen)





# Opgave 2 – Forbrugertillidsindikatorer og fremtidig vækst i husholdningernes forbrugsudgift

# Opgave 2.1 – Opdatering af DI’s forbrugertillidsindikator
# Opdatér DI’s forbrugertillidsindikator med data frem til og med 2023 (2026) fra artiklen ”Forbruget
# fortsætter fremgangen i 2016” (Baum, 2016). Lav vurdering af om forbrugertillidsindikatoren fra DI
# fortsat er bedre end forbrugertillidsindikatoren fra DST. (Hint: I bliver nødt til at nærlæse bilaget for
# DI-FTI for at finde starttidspunktet for estimationen, spørgsmålene, samt tabel, der sammenligner
# FTI og DI-FTI)
#
# Vi kører analysen to gange: 2023 (opgavens krav) og 2026 (seneste data)

library(dkstat)
library(tidyverse)
library(lubridate)

# Kontakt: skift mellem 2023 og 2026. Alt andet følger med
# Modellen estimeres fra 2000K1 (Baums bilag) til 2. kvartal i det valgte år
# 3. kvartal i samme år forudsiges i opgave 2.2
version <- 2023

prognose_kvartal <- as.Date(paste0(version, "-07-01"))


# Y: Årlig realvækst i husholdningernes forbrugsudgift (NKH1)
nkh1_meta <- dst_meta(table = "NKH1", lang = "da")

forbrug_raw <- dst_get_data(
  table     = "NKH1",
  TRANSAKT  = "P.31 Husholdningernes forbrugsudgifter",
  PRISENHED = "2020-priser, kædede værdier",
  SÆSON     = "Sæsonkorrigeret",
  Tid       = "*",
  meta_data = nkh1_meta,
  lang      = "da"
) %>%
  arrange(TID)

# Kvartalsvis tidsserie. Startår og -kvartal læses fra data
forbrug_ts <- ts(forbrug_raw$value,
                 start = c(year(forbrug_raw$TID[1]), quarter(forbrug_raw$TID[1])),
                 frequency = 4)

# Årlig realvækst: kvartal ift. samme kvartal året før (lag = 4)
realvaekst_alle <- (exp(diff(log(forbrug_ts), lag = 4)) - 1) * 100

# Klip til estimationsperioden
realvaekst_ts <- window(realvaekst_alle, start = c(2000, 1), end = c(version, 2))

# Tilbage til dataframe med datoer
forbrug_y <- data.frame(
  kvartal    = seq(as.Date("2000-01-01"), by = "quarter", length.out = length(realvaekst_ts)),
  realvaekst = as.numeric(realvaekst_ts)
)

range(forbrug_y$kvartal)   # forventet: 2000-01-01 og 2023-04-01 (2026-04-01 i 2026-versionen)


# X: Forbrugerforventninger (FORV1)
# De 6 spørgsmål der indgår i enten DI-FTI eller DST's FTI
forv1_meta <- dst_meta(table = "FORV1", lang = "da")

forv1_query <- list(
  INDIKATOR = c(
    "Familiens økonomiske situation i dag, sammenlignet med for et år siden",   # F2: DI + DST
    "Familiens økonomiske  situation om et år, sammenlignet med i dag",         # F3: DST
    "Danmarks økonomiske situation i dag, sammenlignet med for et år siden",    # F4: DI + DST
    "Danmarks økonomiske situation om et år, sammenlignet med i dag",           # F5: DST
    "Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket",         # F9: DI + DST
    "Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr."           # F10: DI
  ),
  Tid = "*"
)

tillid_raw <- dst_get_data(
  table     = "FORV1",
  query     = forv1_query,
  meta_data = forv1_meta,
  lang      = "da"
)

# Én række pr. måned, én kolonne pr. spørgsmål
tillid_wide <- tillid_raw %>%
  filter(year(TID) >= 2000) %>%
  arrange(TID) %>%
  pivot_wider(names_from = INDIKATOR, values_from = value)

# Månedlig tidsserie fra januar 2000
tillid_ts <- ts(tillid_wide[, -1], start = c(2000, 1), frequency = 12)

# Måned -> kvartal: sum af 3 måneder / 3 = gennemsnit. Ufuldstændige kvartaler droppes
# X klippes ikke i slutningen, så 3. kvartal er med til opgave 2.2
tillid_q <- aggregate(tillid_ts, nfrequency = 4) / 3


# Byg de to indikatorer som simple gennemsnit (Baum, boks 1)
# DI-FTI:  F2, F4, F9, F10
# DST FTI: F2, F3, F4, F5, F9
kol     <- colnames(tillid_q)
di_kol  <- startsWith(kol, "F2 ") | startsWith(kol, "F4 ") | startsWith(kol, "F9 ") | startsWith(kol, "F10 ")
dst_kol <- startsWith(kol, "F2 ") | startsWith(kol, "F3 ") | startsWith(kol, "F4 ") | startsWith(kol, "F5 ") | startsWith(kol, "F9 ")

sum(di_kol)    # skal være 4
sum(dst_kol)   # skal være 5

tillid_x <- data.frame(
  kvartal = seq(as.Date("2000-01-01"), by = "quarter", length.out = nrow(tillid_q)),
  di_fti  = rowMeans(tillid_q[, di_kol]),
  dst_fti = rowMeans(tillid_q[, dst_kol])
)


# Saml X og Y. Y bestemmer perioden
data_21 <- tillid_x %>%
  inner_join(forbrug_y, by = "kvartal")

nrow(data_21)            # forventet: 94 (106 i 2026-versionen)
range(data_21$kvartal)


# To simple regressioner: én indikator som x i hver
lm_di  <- lm(realvaekst ~ di_fti,  data = data_21)
lm_dst <- lm(realvaekst ~ dst_fti, data = data_21)

summary(lm_di)
summary(lm_dst)


# Tabel som i Baums bilag

tabel_21 <- data.frame(
  Maal   = c("Forklaringsgrad (R²)", "Korrelation"),
  DI_FTI = c(summary(lm_di)$r.squared,  cor(data_21$di_fti,  data_21$realvaekst)),
  FTI    = c(summary(lm_dst)$r.squared, cor(data_21$dst_fti, data_21$realvaekst))
)
tabel_21


# Kontrol: samme beregning på Baums periode (2000K1-2016K2)
# Tæt på 0,54 og 0,42 = vores metode er den samme som Baums
data_baum <- data_21 %>% filter(kvartal <= as.Date("2016-04-01"))

c(DI  = summary(lm(realvaekst ~ di_fti,  data = data_baum))$r.squared,
  DST = summary(lm(realvaekst ~ dst_fti, data = data_baum))$r.squared)


# Plot: begge indikatorer mod forbruget
skala <- max(abs(data_21$di_fti)) / max(abs(data_21$realvaekst))   # så søjler og linjer kan dele akse

ggplot(data_21, aes(x = kvartal)) +
  geom_col(aes(y = realvaekst * skala, fill = "Årlig realvækst i forbruget (højre akse)"), width = 80) +
  geom_line(aes(y = di_fti,  color = "DI-FTI"),  linewidth = 1) +
  geom_line(aes(y = dst_fti, color = "DST FTI"), linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "hotpink") +
  geom_vline(xintercept = as.Date("2016-04-01"), linetype = "dashed", color = "black") +
  annotate("text", x = as.Date("2016-04-01"), y = max(data_21$di_fti), label = "2016K2 (Baum)",
           hjust = -0.05, size = 3, color = "black") +
  geom_vline(xintercept = as.Date("2023-04-01"), linetype = "dashed", color = "darkgreen") +
  annotate("text", x = as.Date("2023-04-01"), y = max(data_21$di_fti) * 0.85, label = "2023K2 (opgavens slut)",
           hjust = 1.05, size = 3, color = "darkgreen") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(name = "Nettotal", sec.axis = sec_axis(~ . / skala, name = "Pct.")) +
  scale_fill_manual(name = NULL, values = c("Årlig realvækst i forbruget (højre akse)" = "pink")) +
  scale_color_manual(name = NULL, values = c("DI-FTI" = "black", "DST FTI" = "hotpink4")) +
  labs(x = NULL,
       title = paste0("DI-FTI og DST FTI mod privatforbruget, 2000K1-", version, "K2"),
       caption = "Kilde: Danmarks Statistik (FORV1, NKH1) og egne beregninger") +
  theme_minimal() +
  theme(legend.position = "top")


# Vurdering (skrives ud fra tabel_21, kontrollen og plottet):
# - Er DI-FTI stadig bedst på både R² og korrelation?
# - Er forspringet større eller mindre end i 2016?
# - Hvad har Corona (2020-21) og inflationen (2022) gjort ved sammenhængen?


# Opgave 2.2 – Forudsigelser af forbruget
# Beregn/forudsig den årlige realvækst i husholdningernes forbrugsudgift for 3. kvartal 2023 med
# henholdsvis DI’s forbrugertillidsindikator og forbrugertillidsindikatoren fra DST.

# Tjek: har DST offentliggjort alle 3 måneders forbrugertillid for 3. kvartal?
tillid_raw %>%
  filter(year(TID) == version, quarter(TID) == 3) %>%
  distinct(TID)                                            # forventet: 3 måneder


# Forbrugertilliden for 3. kvartal
# Tom række = kvartalet er ikke komplet endnu, og så kan der ikke forudsiges
x_prognose <- tillid_x %>%
  filter(kvartal == prognose_kvartal)

x_prognose


# Forudsig med begge modeller fra 2.1
# interval = "prediction" giver et 95%-interval: hvor usikker er forudsigelsen?
pred_di  <- predict(lm_di,  newdata = x_prognose, interval = "prediction")
pred_dst <- predict(lm_dst, newdata = x_prognose, interval = "prediction")

tabel_22 <- data.frame(
  Indikator    = c("DI-FTI", "DST FTI"),
  Forudsigelse = c(pred_di[, "fit"], pred_dst[, "fit"]),
  Nedre_95     = c(pred_di[, "lwr"], pred_dst[, "lwr"]),
  Oevre_95     = c(pred_di[, "upr"], pred_dst[, "upr"])
)
tabel_22   # årlig realvækst i procent


# Facit: den faktiske vækst i 3. kvartal, hvis DST har offentliggjort den
sidste <- end(realvaekst_alle)   # seneste kvartal med forbrugstal: c(år, kvartal)

if (sidste[1] > version || (sidste[1] == version && sidste[2] >= 3)) {
  window(realvaekst_alle, start = c(version, 3), end = c(version, 3))
} else {
  "Forbruget for 3. kvartal er ikke offentliggjort endnu"
}

# Opgave 3.1 – Modellens forudsigelser
# Med udgangspunkt i jeres besvarelse i opgave 2, bedes I beregne jeres estimerede værdier for den
# kvartalsvise årlige vækstrate i husholdningernes forbrug. (hint: I skal gange jeres estimerede
# koefficienter med x-variablene fra den estimerede model).

#Nu laver vi lm()'s arbejde i hånden. Vi starter med de to modeller fra 2.1 og data_21

#X-matricen: en søjle med 1-taller (til skæringen) og en søjle med indikatoren
#1-tallet er der, fordi skæringen skal ganges med noget, og 1 gør den bare til sig selv
X_di  <- cbind(1, data_21$di_fti)
X_dst <- cbind(1, data_21$dst_fti)

#Koefficienterne fra 2.1: første tal er skæringen, andet er hældningen
b_di  <- coef(lm_di)
b_dst <- coef(lm_dst)
b_di   # kig på dem, så vi ved hvad vi ganger med

#Matrixgange med %*%: hver række bliver skæring*1 + hældning*indikator #gal et overblik
data_21$yhat_di  <- as.numeric(X_di  %*% b_di)
data_21$yhat_dst <- as.numeric(X_dst %*% b_dst)

#Kontrol: vores hjemmelavede tal skal være præcis de samme som R's egne
all.equal(data_21$yhat_di,  as.numeric(fitted(lm_di)))    # skal være TRUE
all.equal(data_21$yhat_dst, as.numeric(fitted(lm_dst)))   # skal være TRUE #jaaaa taaaaak

#Side om side: faktisk vækst og de to modellers bud
head(data_21[, c("kvartal", "realvaekst", "yhat_di", "yhat_dst")])

#Nu har vi forudsigelserne, og så kan vi i 3.2 regne ud hvor meget modellerne rammer ved siden af

# Opgave 3.2 – Residualer
# Med udgangspunkt i jeres besvarelse i opgave 3.1, bedes I beregne residualer for henholdsvis DI’s
# og DST’s forbrugertillidsindikator og plot disse i forhold til jeres forudsagte resultater fra opgave
# 3.1 for de to modeller.

#Residual = det der faktisk skete minus det modellen troede
#Positiv residual = modellen gættede for lavt. Negativ = modellen gættede for højt
data_21$res_di  <- data_21$realvaekst - data_21$yhat_di
data_21$res_dst <- data_21$realvaekst - data_21$yhat_dst

#Kontrol: vores residualer skal være de samme som R's egne #igen igen
all.equal(data_21$res_di,  as.numeric(residuals(lm_di)))    # skal være TRUE
all.equal(data_21$res_dst, as.numeric(residuals(lm_dst)))   # skal være TRUE

#Sjov lille regel: med en skæring i modellen summer residualerne altid til 0
#Modellen gætter for højt og for lavt lige meget i alt
round(sum(data_21$res_di), 10)
round(sum(data_21$res_dst), 10)

#Til plottet: begge modeller i én lang tabel, så vi kan lave to paneler side om side
res_long <- data.frame(
  kvartal = rep(data_21$kvartal, 2),
  model   = rep(c("DI-FTI", "DST FTI"), each = nrow(data_21)),
  yhat    = c(data_21$yhat_di, data_21$yhat_dst),
  res     = c(data_21$res_di,  data_21$res_dst)
)

#Vi farver coronakvartalerne, så vi kan se om det er dem der larmer
res_long$periode <- ifelse(
  res_long$kvartal >= as.Date("2020-01-01") & res_long$kvartal <= as.Date("2021-10-01"),
  "Corona 2020-21", "Øvrige kvartaler"
)

ggplot(res_long, aes(x = yhat, y = res, color = periode)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "hotpink") +
  facet_wrap(~ model) +
  scale_color_manual(name = NULL,
                     values = c("Corona 2020-21" = "hotpink4", "Øvrige kvartaler" = "black")) +
  labs(title = "Residualer mod forudsagte værdier",
       subtitle = paste0("Simple lineære regressioner, 2000K1-", version, "K2"),
       x = "Forudsagt årlig realvækst (pct.)", y = "Residual (pct.-point)",
       caption = "Kilde: Danmarks Statistik (FORV1, NKH1) og egne beregninger") +
  theme_minimal() +
  theme(legend.position = "top")

#Nu kan vi se hvor modellerne rammer ved siden af, og i 3.3 lægger vi fejlene sammen til RSS






# SIDEOPGAVE (ikke OLA2): logistisk regression fra uge 5

# Opgave 3.1 – Feature engineering, dummy variable
# (indsæt opgaveteksten ordret fra PDF'en her)

#Vi genbruger realvaekst_alle fra opgave 2. Den er regnet på hele NKH1, så 1997 er med som "året før"
start(realvaekst_alle)   # skal være før 1998

#Kontakt:  Opgaven siger 1998K1-2021K2
#Skift til den nederste linje for at tage på alt vi har (2026)
traen_slut <- c(2021, 2)
# traen_slut <- end(realvaekst_alle)

#Klip til træningsperioden
vaekst_logit <- window(realvaekst_alle, start = c(1998, 1), end = traen_slut)

logit_31 <- data.frame(
  kvartal    = seq(as.Date("1998-01-01"), by = "quarter", length.out = length(vaekst_logit)),
  realvaekst = as.numeric(vaekst_logit)
)
nrow(logit_31)   # skal være 94

#Dummy: 1 hvis væksten er 0 eller derover (OP), 0 hvis den er negativ (NED)
logit_31$op      <- ifelse(logit_31$realvaekst >= 0, 1, 0)
logit_31$retning <- ifelse(logit_31$op == 1, "OP", "NED")

#Hvor ofte går det op, og hvor ofte går det ned?
table(logit_31$retning)
round(prop.table(table(logit_31$retning)) * 100, 1)   # i procent #gal et overblik
#Lille hjælper: tæller OP og NED i en periode og regner gennemsnitlig vækst i hver gruppe
#Samme tankegang som rens_by og find_region: skriv det én gang, brug det flere gange
op_ned <- function(start, slut, navn) {
  v <- as.numeric(window(realvaekst_alle, start = start, end = slut))
  data.frame(
    Periode        = navn,
    Kvartaler      = length(v),
    OP             = sum(v >= 0),
    NED            = sum(v < 0),
    OP_pct         = round(mean(v >= 0) * 100, 1),
    Gns_vaekst_OP  = round(mean(v[v >= 0]), 2),   # hvor meget går det op, når det går op
    Gns_vaekst_NED = round(mean(v[v < 0]), 2)     # hvor meget går det ned, når det går ned
  )
}

sidste_forbrug <- end(realvaekst_alle)   # seneste kvartal med forbrugstal

tabel_logit_31 <- rbind(
  op_ned(c(1998, 1), c(2021, 2), "1998K1-2021K2 (opgaven)"),
  op_ned(c(2021, 3), sidste_forbrug, paste0("2021K3-", sidste_forbrug[1], "K", sidste_forbrug[2]))
)
tabel_logit_31   #gal et overblik

# Opgave 3.2 – Logistisk regression og forudsigelser
# (indsæt opgaveteksten ordret fra PDF'en her)

#FORV1 ligger allerede i tillid_raw fra 2.1. Men dér klippede vi ved 2000, og nu skal vi fra 1998
di_spg <- tillid_raw %>%
  filter(year(TID) >= 1998) %>%
  mutate(kvartal = floor_date(TID, "quarter"),
         kode    = sub(" .*", "", INDIKATOR)) %>%      # "F2 Familiens..." bliver til "F2"
  filter(kode %in% c("F2", "F4", "F9", "F10")) %>%     # kun DI's fire spørgsmål
  group_by(kvartal, kode) %>%
  summarise(value = mean(value), n_mdr = n(), .groups = "drop") %>%
  filter(n_mdr == 3) %>%
  select(-n_mdr) %>%
  pivot_wider(names_from = kode, values_from = value)

head(di_spg)   # én række pr. kvartal, kolonnerne F2, F4, F9, F10

#Saml med dummyen fra 3.1. HER var fejlen: det skal være logit_31, ikke data_21
logit_32 <- logit_31 %>% inner_join(di_spg, by = "kvartal")
nrow(logit_32)   # skal stadig være 94

#Logistisk regression: family = binomial er det der gør den logistisk
glm_logit <- glm(op ~ F2 + F4 + F9 + F10, data = logit_32, family = binomial)
summary(glm_logit)

#Forudsigelse for 3. kvartal 2024
x_2024 <- di_spg %>% filter(kvartal == as.Date("2024-07-01"))
x_2024

#type = "response" giver sandsynligheden (0 til 1). Uden den får vi log-odds, og dem kan ingen læse
p_op <- predict(glm_logit, newdata = x_2024, type = "response")
p_op

#Vi vælger selv grænsen. 0,5 er standard
ifelse(p_op >= 0.5, "OP", "NED")

#Facit: 3. kvartal 2024 er offentliggjort, så vi kan tjekke om modellen ramte
window(realvaekst_alle, start = c(2024, 3), end = c(2024, 3))

#Plot til 3.1: hvert kvartal farvet efter retning, fra 1998 til seneste data
plot_31 <- data.frame(
  kvartal    = seq(as.Date("1998-01-01"), by = "quarter",
                   length.out = length(window(realvaekst_alle, start = c(1998, 1)))),
  realvaekst = as.numeric(window(realvaekst_alle, start = c(1998, 1)))
)
plot_31$retning <- ifelse(plot_31$realvaekst >= 0, "OP", "NED")

#Tekstbokse med antal OP og NED, hentet direkte fra tabel_logit_31 #gal et overblik
lbl_1 <- paste0("1998K1-2021K2\n",
                "OP: ",  tabel_logit_31$OP[1],  " (", format(tabel_logit_31$OP_pct[1], decimal.mark = ","), " %)\n",
                "NED: ", tabel_logit_31$NED[1], " (", format(100 - tabel_logit_31$OP_pct[1], decimal.mark = ","), " %)")

lbl_2 <- paste0(tabel_logit_31$Periode[2], "\n",
                "OP: ",  tabel_logit_31$OP[2],  " (", format(tabel_logit_31$OP_pct[2], decimal.mark = ","), " %)\n",
                "NED: ", tabel_logit_31$NED[2], " (", format(100 - tabel_logit_31$OP_pct[2], decimal.mark = ","), " %)")

ggplot(plot_31, aes(x = kvartal, y = realvaekst, fill = retning)) +
  geom_col(width = 80) +
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = as.Date("2021-05-15"), linetype = "dashed", color = "darkgreen") +
  annotate("text", x = as.Date("2010-01-01"), y = 8.5, label = lbl_1,
           size = 3.5, lineheight = 1.1) +
  annotate("text", x = as.Date("2024-10-01"), y = 8.5, label = lbl_2,
           size = 3.5, lineheight = 1.1) +
  scale_fill_manual(name = NULL, values = c("OP" = "pink", "NED" = "hotpink4")) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(title = "Forbruget steg i 4 af 5 kvartaler til 2021, siden kun i 3 af 5",
       subtitle = "Årlig realvækst i husholdningernes forbrugsudgift pr. kvartal. Stiplet linje = 2021K2",
       x = NULL, y = "Pct.",
       caption = "Kilde: Danmarks Statistik (NKH1) og egne beregninger") +
  theme_minimal() +
  theme(legend.position = "top")

#nu laver vi lige en tabel
antal <- table(plot_31$retning)
antal
?as.numeric

#To kolonner: OP og NED med antallet af kvartaler i hver
op_ned_tabel <- data.frame(
  OP  = as.numeric(antal["OP"]),
  NED = as.numeric(antal["NED"])
)
op_ned_tabel   # OP 86, NED 28

#Nu kan de divideres med hinanden #jaaaa taaaaak
op_ned_tabel$OP / op_ned_tabel$NED   # 3,07: forbruget steg godt 3 gange for hver gang det faldt
#vi skal have svar på, hvor ofte stiger den kvartalsvise årlige vækst og hvor meget den går op og ned fra perioden 1. 
#kvartal 1998 til 2. kvartal 2021 (MEN, vi skal også have hvor meget
#den går op og ned fra 2021 til 2026) dette er opgave 3.1 det skal vi have et simpelt svar på.


#ggplot vil have data på lang form: én række pr. søjle
soejler <- data.frame(
  retning = factor(c("OP", "NED"), levels = c("OP", "NED")),   # OP til venstre
  antal   = c(op_ned_tabel$OP[1], op_ned_tabel$NED[1])         # [1] = antal-rækken
)

ggplot(soejler, aes(x = retning, y = antal, fill = retning)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = antal), vjust = -0.5, size = 5) +       # tallet oven på søjlen
  scale_fill_manual(values = c("OP" = "pink", "NED" = "hotpink4"), guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "For hver gang forbruget faldt, steg det 3,1 gange",
       subtitle = "Antal kvartaler med stigende og faldende årlig realvækst i husholdningernes forbrug",
       x = NULL, y = "Antal kvartaler",
       caption = "Anm.: n = 114 kvartaler, 1998K1-2026K2\nKilde: Danmarks Statistik (NKH1) og egne beregninger") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank())   #gal et overblik



#default, da vi lavede figur
#y er nettotal
# x alle dem der har positiv vækst og alle dem der har negativ vækst og se om der forskel mellem de to søjler.
#vi har ikke data exploret, så vi skal igenne mvores data science steps. hovedsagliht ved data exploration for at finde ud 
# hvad det betyder

#eksempel familiens økonomiske situation i dag.
#hvordan er de 3 spørgsmål fordelt på de her.

# vi skal ind og pege spørgsmålene ud ligesom vi gjorde way back med tillidsindikator.

#vi må først lave logistisk regression, når vi har data exploret.
#research goal = opgacven, har vi været igennem de her steps? data science
#DATASCIENCE er vores process.
#vi må ikke modelere noget før vi har data exploret, fordi ellers bliver vi utroværdige.
#for nemt kun med 1 spørgsmål
#v iskal lave det på tværs af de 12 spørgsmål, hvad er det faktisk der gør at forbruget går og ned
#vi skal lave et loop igennem det vi laver.
#fordi vi skal lave den samme udregning 12 gange.





#3.2
# ================================================================
# Opgave 3.2 – DATA EXPLORATION FØR vi modellerer
# (indsæt opgaveteksten ordret fra PDF'en her)
# ================================================================

#Okay makker, her er planen
#Underviseren sagde det klart: vi må IKKE modellere før vi har data exploret, ellers er vi utroværdige
#Data science-processen er: research goal -> hent data -> klargør data -> EXPLORE -> modellér -> præsentér
#Vi har været på trin 1, 2 og 3. Nu er vi på trin 4, og den logistiske regression venter pænt i kø

#Hvad vi vil finde ud af:
#Hvilke af DST's 12 spørgsmål er gode spioner for, om forbruget går OP eller NED?
#Metoden er den samme som Default-figuren fra slides: to søjler pr. spørgsmål, én for OP og én for NED
#Er søjlerne meget forskellige, er spørgsmålet en god spion. Er de ens, kan den ikke hjælpe os

#Lidt baggrund fra DST's statistikdokumentation, så vi ved hvad tallene ER:
#Hver måned spørges ca. 1.500 personer i alderen 16-74 år. Ca. 34 % svarer ikke, så ca. 1.000 svarer
#Svarene giver point: meget bedre 100, lidt bedre 50, uændret 0, lidt dårligere -50, meget dårligere -100
#Gennemsnittet af pointene hedder et NETTOTAL. Over 0 = flest positive. Under 0 = flest negative

#Sikkerhedstjek: har vi dummyen fra 3.1 og meta-data fra 2.1?
if (!exists("logit_31")) stop("Kør opgave 3.1 først, så logit_31 med OP/NED findes")
if (!exists("forv1_meta")) forv1_meta <- dst_meta(table = "FORV1", lang = "da")


# Trin 1: Hent alle 12 spørgsmål

#I opgave 2.1 hentede vi kun de 6 spørgsmål, DI og DST bruger. Nu vil vi have ALLE, ligesom Baum
#Baum testede 2^12 = 4.096 kombinationer. Vi starter mere nede på jorden med ét spørgsmål ad gangen
tillid_12_raw <- dst_get_data(
  table     = "FORV1",
  INDIKATOR = "*",          # stjerne = giv os det hele, tak
  Tid       = "*",
  meta_data = forv1_meta,
  lang      = "da"
)

#Nu rydder vi op:
#F1 er selve forbrugertillidsindikatoren, altså et gennemsnit af 5 af de andre. Den skal IKKE med,
#for så tæller vi de samme svar to gange. Vi vil have de 12 rigtige spørgsmål: F2 til F13
#Og så måned -> kvartal med gennemsnit af 3 måneder, præcis som i opgave 2
tillid_12 <- tillid_12_raw %>%
  mutate(kode    = sub(" .*", "", INDIKATOR),          # "F2 Familiens..." bliver til "F2"
         kvartal = floor_date(TID, "quarter")) %>%     # 1. feb 2010 bliver til 1. jan 2010 = 2010K1
  filter(kode %in% paste0("F", 2:13), year(TID) >= 1998) %>%
  group_by(kvartal, kode, INDIKATOR) %>%
  summarise(nettotal = mean(value), n_mdr = n(), .groups = "drop") %>%
  filter(n_mdr == 3) %>%                                # kun hele kvartaler, ingen halve
  select(-n_mdr)

unique(tillid_12$kode)   # skal være 12 koder: F2 til F13 #jaaaa taaaaak

#Nu klistrer vi OP/NED på hvert kvartal. Dummyen kommer fra 3.1 (opgavens periode 1998K1-2021K2)
#inner_join betyder: kun kvartaler der findes i BEGGE tabeller, så perioden styres af dummyen
expl <- tillid_12 %>%
  inner_join(logit_31 %>% select(kvartal, retning), by = "kvartal")

n_distinct(expl$kvartal)   # skal være 94



# Trin 2: Loopet – samme udregning 12 gange


#Vi kunne skrive den samme kode 12 gange. Men det er kedeligt, og vi laver fejl i kopi nummer 7
#Så vi laver et for-loop: "for hvert spørgsmål k, gør det her"

#Nogle spørgsmål er VENDT OM. Det står i DST's dokumentation:
#F6 og F7 handler om priser, og F8 om arbejdsløshed. Her giver "meget højere" +100 point
#Højere priser og mere ledighed er dårligt nyt, så her forventer vi at NED-søjlen er højest
#Det er ikke en fejl i data, det er bare spørgsmålet der peger den anden vej
omvendte <- c("F6", "F7", "F8")

koder      <- paste0("F", 2:13)
expl_tabel <- data.frame()   # tom tabel som loopet fylder op, én række pr. omgang

for (k in koder) {
  d <- expl %>% filter(kode == k)   # hiv ét spørgsmål ud ad gangen
  
  #Gennemsnitligt nettotal i de kvartaler hvor forbruget gik op, og hvor det gik ned
  gns_op  <- mean(d$nettotal[d$retning == "OP"])
  gns_ned <- mean(d$nettotal[d$retning == "NED"])
  
  #t-test: er forskellen mellem OP og NED større end hvad tilfældigheder kan forklare?
  #Lav p-værdi = forskellen er ægte. Høj p-værdi = det kunne lige så godt være held
  test <- t.test(nettotal ~ retning, data = d)
  
  #Klistr resultatet på som en ny række i tabellen
  expl_tabel <- rbind(expl_tabel, data.frame(
    kode        = k,
    spoergsmaal = sub("^F[0-9]+ ", "", d$INDIKATOR[1]),     # fjern koden foran teksten
    gns_OP      = round(gns_op, 1),
    gns_NED     = round(gns_ned, 1),
    forskel     = round(gns_op - gns_ned, 1),
    forventet   = ifelse(k %in% omvendte, "NED højest", "OP højest"),
    t_vaerdi    = round(abs(as.numeric(test$statistic)), 2),
    p_vaerdi    = signif(test$p.value, 3)
  ))
}

#Sortér så de bedste spioner står øverst. t-værdien er vores målestok,
#fordi den tager højde for at spørgsmålene svinger forskelligt meget #nu bliver det sindssygt
expl_tabel <- expl_tabel[order(-expl_tabel$t_vaerdi), ]
rownames(expl_tabel) <- NULL
expl_tabel   #gal et overblik

## ehm, husk: DST siger at en ændring på over 5 point er signifikant, men det gælder fra én måned til den næste
## vi sammenligner gennemsnit over mange kvartaler, og der er t-testen det rigtige værktøj


# Trin 3: 12 små figurer med to søjler hver

#Gennemsnit pr. spørgsmål og retning, klar til ggplot
plot_expl <- expl %>%
  group_by(kode, retning) %>%
  summarise(gns = mean(nettotal), .groups = "drop") %>%
  left_join(expl_tabel %>% select(kode, spoergsmaal), by = "kode")

#Paneltitel: kode + spørgsmålet brækket over flere linjer, så man kan læse det
plot_expl$panel <- paste0(plot_expl$kode, ": ", str_wrap(plot_expl$spoergsmaal, 32))

#Panelerne i samme rækkefølge som tabellen (bedste spion først), OP til venstre i hvert panel
plot_expl$panel   <- factor(plot_expl$panel,
                            levels = unique(plot_expl$panel[order(match(plot_expl$kode, expl_tabel$kode))]))
plot_expl$retning <- factor(plot_expl$retning, levels = c("OP", "NED"))

ggplot(plot_expl, aes(x = retning, y = gns, fill = retning)) +
  geom_col(width = 0.6) +
  geom_hline(yintercept = 0, color = "black") +
  facet_wrap(~ panel, ncol = 4, scales = "free_y") +      # hvert panel får sin egen y-akse
  scale_fill_manual(values = c("OP" = "pink", "NED" = "hotpink4"), guide = "none") +
  labs(title = "Hvilke spørgsmål adskiller kvartaler med vækst fra kvartaler med fald?",
       subtitle = "Gennemsnitligt nettotal pr. spørgsmål. Sorteret efter størst adskillelse (t-værdi)",
       x = NULL, y = "Gns. nettotal",
       caption = paste0("Anm.: n = ", n_distinct(expl$kvartal), " kvartaler, 1998K1-2021K2. ",
                        "Ca. 1.500 adspurgte pr. måned, 16-74 år. F6-F8 er vendt: højt nettotal = dårligt nyt.\n",
                        "DST: sammenligning over tid kun direkte mulig fra 2007 pga. ændret beregningsmetode\n",
                        "Kilde: Danmarks Statistik (FORV1, NKH1) og egne beregninger")) +
  theme_minimal() +
  theme(strip.text = element_text(size = 7, hjust = 0))

#Pas på: med free_y har hvert panel sin egen skala
#Så sammenlign spørgsmålene via t-værdien i tabellen, IKKE ved at kigge på hvor høje søjlerne er


# ----------------------------------------------------------------
# Hvad gør vi med det?
# ----------------------------------------------------------------

#Et godt spørgsmål til modellen:
#  1) stor t-værdi og lav p-værdi (under 0,05)
#  2) den højeste søjle er dér, hvor vi forventer (kolonnen "forventet")
#Hvis et spørgsmål har stor t-værdi, men den forkerte søjle er højest, skal vi forstå hvorfor, før vi bruger det

#Og nu det spændende: ligger DI's fire (F2, F4, F9, F10) i toppen?
expl_tabel$DI <- ifelse(expl_tabel$kode %in% c("F2", "F4", "F9", "F10"), "DI-FTI", "")
expl_tabel

#Ja = vores exploration bekræfter DI's valg, også når målet er OP/NED
#Nej = vi har et argument for at vælge andre spørgsmål end DI, og det er DET der gør os troværdige
## ehm, begrænsning til besvarelsen: vi starter i 1998, men DST siger sammenligning er renest fra 2007

#3 kolonner i stedet for 4 giver mere plads til paneltitlerne

#boxplotten
#Boxplot-versionen af samme idé, ligesom Default-sliden
#Boksen = de midterste 50 % af kvartalerne. Stregen i midten = medianen
#Overlapper boksene ikke, er spørgsmålet en god spion #nu bliver det sindssygt
expl_box <- expl %>%
  left_join(expl_tabel %>% select(kode, spoergsmaal), by = "kode")

expl_box$panel   <- paste0(expl_box$kode, ": ", str_wrap(expl_box$spoergsmaal, 32))
expl_box$panel   <- factor(expl_box$panel,
                           levels = unique(expl_box$panel[order(match(expl_box$kode, expl_tabel$kode))]))
expl_box$retning <- factor(expl_box$retning, levels = c("OP", "NED"))

ggplot(expl_box, aes(x = retning, y = nettotal, fill = retning)) +
  geom_boxplot(width = 0.6, outlier.size = 1) +
  facet_wrap(~ panel, ncol = 3, scales = "free_y") +
  scale_fill_manual(values = c("OP" = "pink", "NED" = "hotpink4"), guide = "none") +
  labs(title = "Hvilke spørgsmål adskiller vækst fra fald?",
       subtitle = "Nettotal pr. kvartal fordelt på OP og NED. Sorteret efter t-værdi",
       x = NULL, y = "Nettotal",
       caption = paste0("Anm.: n = ", n_distinct(expl_box$kvartal), " kvartaler, 1998K1-2021K2. ",
                        "F6-F8 er vendt: højt nettotal = dårligt nyt\n",
                        "Kilde: Danmarks Statistik (FORV1, NKH1) og egne beregninger")) +
  theme_minimal() +
  theme(strip.text = element_text(size = 7, hjust = 0))

ggsave("expl_12_boxplot.png", width = 11, height = 12, dpi = 300, bg = "white")

#vi skal til at kode meget mere lavpraktisk, vi fik hug af underviseren, der kan se ddet ai. 
#fordi chatten (dig) bare smider alt det her ind og siger det her behøver jeg ikke kigge på.
#Det er derfor vigtigt du bruger introduction to r. og btuge de første kapitler.


#det her med at lave egen kode, det bliver uff!


#nu skal vi validere.
#vi skal bruge predict
#jvilken output giver modellen?
#HVad beregner modellen?
#sandsynligheden for x og y.
#vi så det hos en tidlgiere studiekammerat, at den lå på 75. Det vil sige vi predicter er 75.
#når vi laver predict, skal vi have e nsandsynligheden, sandsynligheden omdanner vi til enten 1 eller 0 eller op eller ned.
#det gør vi ved at sætte threshhold.
#den fortæller os eks 50% - alle sandsynligheder over 50% kalder jeg for 1 og alt under 50% kalder jeg 0.
#kan også hedde
#75. går den under 75 går den ned og over 75 går den op.
#så får vi en vektor
#1. kvartal 2.kvartal 3.kvartal 4.kvartal 1.kvartal osv hen over årene.
#så hvis det er vores forudsigelse, så forudsiger jeg 1|0|2|0|1|1|0 ^
#kalder det også y m hat på
#så vi har alm y, der er ligeså lang som vores y med hat.
#nu kan vi validerer de to med hinanden.
#det gør vi ved at kigge på de to og sammennligne
#ny vektor, "rigtig" = 1 ja
#det kaldes acurracy
#hvor høj burde væres accuracy være i vores situation (over 75%) hvis vi gætter op hvert kvartal bliver det over 75%
#mit bedste gæt er at gætte historisk.


#så det tid til problemstillingen
#det svært at ramme de 3% ( i forbindelse med danskebank, hvor man kunne låne 100.000 kr. med det samme)
#hvor mange gange rammer jeg rigtig på 1?
#det handler om at finde dem der default (det koster mig penge, det koster penge for mig)
#Jeg hader jeg ikke får solgt nok, så jeg skal ramme på min efterspørgsel.
#Hvis det gik nedaf bla bla. så ligger jeg det på lager. det vigtigt for mig at vide hvornår det går op


#vi laver en ny vektor
#hvornår 
#i den her model, hver gang min model siger 1, var det så 1. True / false
#i praksis laver vi en ny model, der tager vores 4 vektore, og laver 
#så kan man derefter sige, hvor god er min model til at forudsige nuller?
#i min min model forudsiger min model
#min model består af hvad forudsiger min model og hvad er virkeligheden?
#det hedder også en confusionmatrix - det. er vores valideringsmodel
#det inde i kvadranten, vi skal kunne dividere alle kvadranter med hinanden og sige. hvad betyder det?
#vi sammenligner tal.
#lav accuracy = hmm kunne være den er god til en type mod anden type.
#defaulmodel, danske bank har sikkert lavet en default model, der siger over 50.000 kr. må de godt låne
#men de har gjort det med nogle specefikke kunder.


#næste lement er ROC-kurve
#det er alene noget der kommer fra confussionsmatrixen
#hvis virammer rigtigt på nullerne, så går kurven opad.
#det hele kommer fra y med hat og alm y.
#det danner roc kurven.

#hvis vi rammer et højt provcent, så den bedre til at forudsige.

#
#
#
#
#

#
#

#



#opgave 3.3
#hvor ofte forudsiger jeres model i 
#opgave 1.2 at den kvartalsvise årlgie realvækst i husholdningerns forbrugsugift stiger?
#hvor ofte er det så reelt tilfældet, at den kvartalsvise årlige realvækst i husholdningernes forbrugsudgift
#stiger, set i forhold til, hvad jeres model forudsiger (hvor mange tilfælde af 1 og 1 finder jeres model?)






