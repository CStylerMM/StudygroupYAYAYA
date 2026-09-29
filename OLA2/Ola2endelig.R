#OLA2
library(dkstat)
library(tidyr)
library(ggplot2)   #flyttet op, så alle pakker loades samlet
dst_get_tables()
library(readxl)

boligsiden_OLA <- read_excel("Ida Rstudio Projekter/OLA 1/boligsiden OLA.xlsx", skip = )
View(boligsiden_OLA)
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





#tid til at bruge readxl
library(readxl)
boligsiden_OLA <- read_excel("~/Dataanalyse/StudygroupYAYAYA/Ida Rstudio Projekter/OLA 1/boligsiden OLA.xlsx", skip =1)


#Opgave 2 – Forbrugertillidsindikatorer og fremtidig vækst i husholdningernes forbrugsudgift

#Opgave 2.1 – Opdatering af DI’s forbrugertillidsindikator
#Opdatér DI’s forbrugertillidsindikator med data frem til og med 2023 (2026) fra artiklen ”Forbruget
#fortsætter fremgangen i 2016” (Baum, 2016). Lav vurdering af om forbrugertillidsindikatoren fra DI
#fortsat er bedre end forbrugertillidsindikatoren fra DST. (Hint: I bliver nødt til at nærlæse bilaget for
#DI-FTI for at finde starttidspunktet for estimationen, spørgsmålene, samt tabel, der sammenligner
#FTI og DI-FTI)

#OPGAVE 2.1 – trin 1: Y (årlig realvækst i forbruget)
#før vi kan gå videre skal vi have forbruget. Vi henter ALT, så slutter det af sig selv ved seneste kvartal

#tid til at bruge dkstat
library(dkstat)

nkh1_meta <- dst_meta(table = "NKH1", lang = "da")

p_forbrug <- dst_get_data(
  table     = "NKH1",
  TRANSAKT  = "P.31 Husholdningernes forbrugsudgifter",
  PRISENHED = "2020-priser, kædede værdier",
  SÆSON     = "Sæsonkorrigeret",
  Tid       = "*",
  meta_data = nkh1_meta,
  lang      = "da"
)

p_forbrug <- p_forbrug[order(p_forbrug$TID), ]
head(p_forbrug, 1)   #skal starte 1990-01-01, ellers skal start nedenfor rettes
tail(p_forbrug, 1)   #gal et overblik: seneste kvartal vi har forbrug for

#samme metode som uge 2: ts med kvartaler og årlig vækst med lag = 4
p_forbrug_ts  <- ts(p_forbrug$value, start = c(1990, 1), frequency = 4)
realvaekst    <- (exp(diff(log(as.numeric(p_forbrug_ts)), lag = 4)) - 1) * 100
realvaekst_ts <- ts(realvaekst, start = c(1991, 1), frequency = 4)

#vi starter i 2000K1 (Baums start) og sætter INGEN slut, så den tager alt til nu
realvaekst_y <- window(realvaekst_ts, start = c(2000, 1))
end(realvaekst_y)   #seneste kvartal med forbrug


#OPGAVE 2.1 – trin 2: X (forbrugertillid, månedlig, skal omregnes til kvartaler)
#nu har vi Y, og X skal på kvartaler. DST giver os dem månedligt

forv1_meta <- dst_meta(table = "FORV1", lang = "da")

#de 6 spørgsmål der indgår i DI-FTI eller DST's FTI (F2, F3, F4, F5, F9, F10)
forv1_query <- list(
  INDIKATOR = c(
    "Familiens økonomiske situation i dag, sammenlignet med for et år siden",
    "Familiens økonomiske  situation om et år, sammenlignet med i dag",
    "Danmarks økonomiske situation i dag, sammenlignet med for et år siden",
    "Danmarks økonomiske situation om et år, sammenlignet med i dag",
    "Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket",
    "Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr."
  ),
  Tid = "*"
)

tillid_raw <- dst_get_data(
  table     = "FORV1",
  query     = forv1_query,
  meta_data = forv1_meta,
  lang      = "da"
)

#tid til at bruge tidyr, så vi får én kolonne pr. spørgsmål
library(tidyr)

tillid_raw  <- tillid_raw[order(tillid_raw$TID), ]
tillid_wide <- pivot_wider(tillid_raw, names_from = INDIKATOR, values_from = value)
tillid_wide <- tillid_wide[as.numeric(format(tillid_wide$TID, "%Y")) >= 2000, ]   #Baums start

#som på slidet: månedlig ts og aggregate til kvartaler (3 måneder / 3 = gennemsnit)
tillid_ts <- ts(as.matrix(tillid_wide[, -1]), start = c(2000, 1), frequency = 12)
tillid_q  <- aggregate(tillid_ts, nfrequency = 4) / 3

end(tillid_q)    #seneste hele kvartal med forbrugertillid
ncol(tillid_q)   #skal være 6


#OPGAVE 2.1 – trin 3: de to indikatorer, og X og Y samles
#før vi kan lave regression skal vi bygge DI-FTI og DST FTI, som bare er gennemsnit af spørgsmål

kol <- colnames(tillid_q)

#mellemrum efter koden, så "F1 " ikke fanger "F10 "
di_kol  <- startsWith(kol, "F2 ") | startsWith(kol, "F4 ") | startsWith(kol, "F9 ") | startsWith(kol, "F10 ")
dst_kol <- startsWith(kol, "F2 ") | startsWith(kol, "F3 ") | startsWith(kol, "F4 ") | startsWith(kol, "F5 ") | startsWith(kol, "F9 ")

sum(di_kol)    #skal være 4
sum(dst_kol)   #skal være 5

x_df <- data.frame(
  kvartal = round(as.numeric(time(tillid_q)), 2),   #2000.00, 2000.25 osv.
  di_fti  = rowMeans(tillid_q[, di_kol]),
  dst_fti = rowMeans(tillid_q[, dst_kol])
)

y_df <- data.frame(
  kvartal    = round(as.numeric(time(realvaekst_y)), 2),
  realvaekst = as.numeric(realvaekst_y)
)

#X og Y slutter ikke samme sted (forbrug kommer senere end tillid)
#merge beholder kun kvartaler der findes i begge, så vi får så meget som muligt uden at tælle
data_21 <- merge(x_df, y_df, by = "kvartal")

head(data_21, 3)
tail(data_21, 3)

seneste       <- tail(data_21$kvartal, 1)
seneste_tekst <- paste0(floor(seneste), "K", round((seneste - floor(seneste)) * 4) + 1)
seneste_tekst   #skal være 2026K2


#OPGAVE 2.1 – trin 4: regression og Baums tabel
#nu har vi alt, og så laver vi to simple lineære regressioner: Y = realvækst, X = én indikator ad gangen

lm_di  <- lm(realvaekst ~ di_fti,  data = data_21)
lm_dst <- lm(realvaekst ~ dst_fti, data = data_21)

summary(lm_di)    #jaaaa taaaaak
summary(lm_dst)

#funktion der laver tabellen i samme opsætning som Baums bilag, så vi kan bruge den flere gange
baum_tabel <- function(d) {
  data.frame(
    Maal   = c("Forklaringsgrad (R2)", "Korrelation"),
    DI_FTI = round(c(summary(lm(realvaekst ~ di_fti, data = d))$r.squared,
                     cor(d$di_fti, d$realvaekst)), 2),
    FTI    = round(c(summary(lm(realvaekst ~ dst_fti, data = d))$r.squared,
                     cor(d$dst_fti, d$realvaekst)), 2)
  )
}

#Baums egne tal fra bilaget (boks 1), 2000K1-2016K2
tabel_baum_orig <- data.frame(
  Maal   = c("Forklaringsgrad (R2)", "Korrelation"),
  DI_FTI = c(0.54, 0.73),
  FTI    = c(0.42, 0.65)
)

#vores tal på Baums periode og på ALLE vores data
tabel_2000_2016   <- baum_tabel(data_21[data_21$kvartal <= 2016.25, ])
tabel_2000_seneste <- baum_tabel(data_21)

nrow(data_21[data_21$kvartal <= 2016.25, ])   #skal være 66

cat("Baum (2016), 2000K1-2016K2\n");   print(tabel_baum_orig)
cat("\nVores tal, 2000K1-2016K2\n");   print(tabel_2000_2016)
cat("\nVores tal, 2000K1-", seneste_tekst, "\n", sep = "");   print(tabel_2000_seneste)

## ehm, vores niveau ligger lavere end Baums, men DI-FTI er stadig bedst. Er forspringet blevet større eller mindre?


#OPGAVE 2.1 – trin 5: plot
#før vi kan plotte skal vi vide hvad vi vil se: begge indikatorer og forbruget i samme figur
#væksten er procent og indikatorerne er nettotal, så væksten lægges på en højre akse ligesom hos Baum

#tid til at bruge ggplot2
library(ggplot2)

skala <- max(abs(data_21$di_fti)) / max(abs(data_21$realvaekst))

ggplot(data_21, aes(x = kvartal)) +
  geom_col(aes(y = realvaekst * skala, fill = "Årlig realvækst i forbruget (højre akse)"),
           width = 0.2) +
  geom_line(aes(y = di_fti,  color = "DI-FTI"),  linewidth = 1) +
  geom_line(aes(y = dst_fti, color = "DST FTI"), linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "hotpink") +
  geom_vline(xintercept = 2016.25, linetype = "dashed") +   #2016K2 = slutningen af Baums periode
  annotate("text", x = 2016.25, y = max(data_21$di_fti), label = "2016K2 (Baum)",
           hjust = 1.05, size = 3) +
  scale_x_continuous(breaks = seq(2000, 2026, by = 2)) +
  scale_y_continuous(name = "Nettotal",
                     sec.axis = sec_axis(~ . / skala, name = "Pct.")) +
  scale_fill_manual(name = NULL, values = c("Årlig realvækst i forbruget (højre akse)" = "pink")) +
  scale_color_manual(name = NULL, values = c("DI-FTI" = "black", "DST FTI" = "darkgreen")) +
  labs(x = NULL,
       title = paste0("DI-FTI og DST FTI mod privatforbruget, 2000K1-", seneste_tekst),
       caption = "Kilde: Danmarks Statistik (FORV1, NKH1) og egne beregninger") +
  theme_minimal() +
  theme(legend.position = "top")

## ehm, kig på plottet: hvad sker der i 2020 (Corona) og 2022 (inflation)? Det skal med i vurderingen
#nu kan vi skrive vurderingen ud fra tabellerne og plottet

#OPGAVE 2.1 – trin 6: tjek af Corona og krig
## Differentierer vores R2 og korrelation så meget pga. Corona og krig???
#før vi skriver det som forklaring i besvarelsen skal vi teste det, ikke gætte ud fra plottet
#vi regner R2 og korrelation på fire udsnit af data_21 og sætter dem ved siden af hinanden

#en lille funktion, så vi laver den samme udregning fire gange uden at copy-paste
test_udsnit <- function(d, navn) {
  data.frame(
    Udsnit    = navn,
    Kvartaler = nrow(d),
    R2_DI     = round(summary(lm(realvaekst ~ di_fti,  data = d))$r.squared, 2),
    R2_DST    = round(summary(lm(realvaekst ~ dst_fti, data = d))$r.squared, 2),
    Kor_DI    = round(cor(d$di_fti,  d$realvaekst), 2),
    Kor_DST   = round(cor(d$dst_fti, d$realvaekst), 2)
  )
}

#udsnittene: 2020K1 = 2020.00 og 2022K4 = 2022.75
udsnit_alle        <- data_21
udsnit_foer_corona <- data_21[data_21$kvartal <= 2019.75, ]                          #2000K1-2019K4
udsnit_uden_krise  <- data_21[data_21$kvartal < 2020 | data_21$kvartal > 2022.75, ]  #Corona og krig fjernet
udsnit_efter       <- data_21[data_21$kvartal >= 2023, ]                             #2023K1 og frem

tabel_test <- rbind(
  test_udsnit(udsnit_alle,        paste0("Alle data, 2000K1-", seneste_tekst)),
  test_udsnit(udsnit_foer_corona, "Før Corona, 2000K1-2019K4"),
  test_udsnit(udsnit_uden_krise,  "Uden 2020K1-2022K4"),
  test_udsnit(udsnit_efter,       "Kun 2023K1 og frem")
)
tabel_test   #gal et overblik

nrow(udsnit_uden_krise)   #skal være 12 kvartaler færre end alle data

## ehm, hvis R2 stiger tydeligt når Corona og krig er væk, kan vi skrive det som forklaring. Ellers er det en gætning


#Opgave 2.2 – Forudsigelser af forbruget
#Beregn/forudsig den årlige realvækst i husholdningernes forbrugsudgift for 3. kvartal 2026 med
#henholdsvis DI’s forbrugertillidsindikator og forbrugertillidsindikatoren fra DST.

#OPGAVE 2.2 – trin 1: forudsig 2026K3
#dette er en ÆGTE forudsigelse: DST har offentliggjort forbrugertillid for 2026K3, men ikke forbruget endnu
#modellerne er derfor bygget på ALT vi har (data_21, som går til 2026K2) - lm_di og lm_dst fra 2.1

#X for 2026K3 findes ikke i data_21 (Y mangler der for det kvartal), så vi henter den fra x_df
x_2026k3 <- x_df[x_df$kvartal == 2026.5, ]
x_2026k3   #gal et overblik: di_fti og dst_fti for 2026K3, ingen facit endnu

#predict() sætter X ind i hver model og giver den forudsagte realvækst
pred_di_2026  <- predict(lm_di,  newdata = x_2026k3)
pred_dst_2026 <- predict(lm_dst, newdata = x_2026k3)

pred_di_2026    #DI-FTI's forudsigelse for 2026K3
pred_dst_2026   #DST FTI's forudsigelse for 2026K3

#kontrol i hånden: skæring + hældning * X, samme metode som opgave 3.1
coef(lm_di)[1]  + coef(lm_di)[2]  * x_2026k3$di_fti    #skal matche pred_di_2026
coef(lm_dst)[1] + coef(lm_dst)[2] * x_2026k3$dst_fti   #skal matche pred_dst_2026

#usikkerheden på gættet, fordi vi ikke kan tjekke mod facit endnu
predict(lm_di,  newdata = x_2026k3, interval = "prediction")
predict(lm_dst, newdata = x_2026k3, interval = "prediction")

## ehm, dette er hovedsvaret på 2.2: en reel forudsigelse for det kommende kvartal, uden kendt facit


#OPGAVE 2.2 – trin 2 (ekstra): valider metoden på 2023K3, hvor facit findes
#opgavens tekst nævner konkret 3. kvartal 2023, og der KAN vi tjekke om metoden virker, fordi facit findes
#modellen skal her KUN kende data til 2023K2, ellers har den set facit på forhånd

data_til_2023 <- data_21[data_21$kvartal <= 2023.25, ]
nrow(data_til_2023)   #skal være 94

model_di_2023  <- lm(realvaekst ~ di_fti,  data = data_til_2023)
model_dst_2023 <- lm(realvaekst ~ dst_fti, data = data_til_2023)

x_2023k3 <- data_21[data_21$kvartal == 2023.5, ]
x_2023k3   #di_fti, dst_fti og facit (realvaekst) for 2023K3

predict_di  <- predict(model_di_2023,  newdata = x_2023k3)
predict_dst <- predict(model_dst_2023, newdata = x_2023k3)

facit <- x_2023k3$realvaekst

#tabel: forudsagt vs. facit, og hvor meget hver model ramte ved siden af
tabel_valider <- data.frame(
  Model     = c("DI-FTI", "DST FTI"),
  Forudsagt = round(c(predict_di, predict_dst), 2),
  Facit     = round(facit, 2),
  Afvigelse = round(c(predict_di, predict_dst) - facit, 2)
)
tabel_valider   #jaaaa taaaaak

## ehm, positiv afvigelse = modellen gættede for højt (mindre negativt end virkeligheden)
#dette er en efterfølgende kontrol af metoden, ikke selve hovedforudsigelsen, som er 2026K3 i trin 1
