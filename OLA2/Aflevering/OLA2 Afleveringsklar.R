#### OLA 2 – Dataanalyse, 1. semester E26 ####
#Gruppe 1: Christopher Victor Bøwig, M.N. Olliver Søndergaard, Casper Stegler Markussen, Ida Elisabeth Egebjerg Christensen

#### Før scriptet køres ####
#1. Scriptet køres fra toppen og ned, fordi senere opgaver bruger objekter fra tidligere opgaver
#2. Pakkerne dkstat, readxl, tidyr, ggplot2, restatapi og scales skal være installeret. Mangler de, køres denne linje én gang:
#install.packages(c("dkstat", "readxl", "tidyr", "ggplot2", "restatapi", "scales"))
#3. Data fra Danmarks Statistik hentes via API, så der kræves internetforbindelse
#   Tallene i rapporten er hentet d. 02.10.2026, og DST opdaterer tabellerne løbende
#4. Boligfilen "boligsiden OLA.xlsx" skal ligge i samme mappe som scriptet (mappen ses med getwd())
#   Ligger filen et andet sted, rettes filsti_bolig i opgave 1.3
#5. Scriptet skal åbnes som UTF-8, så æ, ø og å vises korrekt (RStudio: File > Reopen with Encoding > UTF-8)


#### Opgave 1.1 – Det første skridt ####
#for at finde en tabel med indbyggertal i byer søger vi i DST's tabeller
#nu tilføjer vi dkstat
library(dkstat)

df_soeg <- dst_search(string = "Befolkning", field = "text")
df_soeg[df_soeg$id == "BY3", ]  #BY3 viser befolkningen i byområder

#derefter henter vi tabellens beskrivelse og ser, hvilke variable vi kan vælge på
by3_meta <- dst_meta(table = "BY3", lang = "da")
by3_meta$variables

#nu kan vi hente alle byer for 2026
df_byer_folketal <- dst_get_data(
  table = "BY3",
  BYER = "*",
  Tid = "2026",
  FOLKARTAET = "Folketal",
  meta_data = by3_meta,
  lang = "da"
)

sum(df_byer_folketal$value)  #samlet folketal før rensning: 6025603

#rækker med 0 indbyggere, "Uden fast bopæl" og "Landdistrikter" er ikke byer, så de fjernes
df_byer_renset <- df_byer_folketal[df_byer_folketal$value != 0, ]
df_byer_renset <- df_byer_renset[!grepl("Uden fast bopæl", df_byer_renset$BYER), ]
df_byer_renset <- df_byer_renset[!grepl("Landdistrikter", df_byer_renset$BYER), ]

nrow(df_byer_renset)  #rækker tilbage: 1419 (før 1821)
sum(df_byer_renset$value)  #folketal efter rensning: 5342281
sum(df_byer_folketal$value) - sum(df_byer_renset$value)  #antal fjernede indbyggere: 683322
sum(df_byer_renset$value) / sum(df_byer_folketal$value)  #andel der er tilbage: ca. 0,887


#### Opgave 1.2 – Kategori-variabel ####
#før vi kan lave kategorien skal bynavnene renses, så hver by står én gang og kan matches med boligdata
rens_by <- function(x) {
  x <- tolower(x)
  x <- ifelse(grepl("hovedstad", x), "københavn", x)  #hovedstadsområdet samles under København
  x <- gsub("\\s*\\(.*?\\)", "", x)  #fjerner parenteser, fx "(del af flere kommuner)"
  x <- gsub("[0-9]", "", x)  #fjerner tallene foran bynavnet
  x <- gsub("æ", "ae", x)
  x <- gsub("ø", "oe", x)
  x <- gsub("å", "aa", x)
  x <- gsub("é", "e", x)
  x <- gsub("-", " ", x)
  trimws(x)
}

df_byer_renset$by <- rens_by(df_byer_renset$BYER)
head(df_byer_renset[, c("BYER", "by")])

#derefter tjekker vi byer, der står flere gange, fordi de ligger i flere kommuner
dubletter <- df_byer_renset$by[duplicated(df_byer_renset$by)]
df_byer_renset[df_byer_renset$by %in% dubletter, c("BYER", "value")]

#nu kan vi lægge delene sammen, så hver by står én gang
df_byer_bycat <- aggregate(value ~ by, data = df_byer_renset, FUN = sum)
names(df_byer_bycat) <- c("by", "folketal")
sum(duplicated(df_byer_bycat$by))  #skal være 0
sum(df_byer_bycat$folketal) == sum(df_byer_renset$value)  #skal være TRUE

#og nu kan vi lave kategorien ud fra folketal
df_byer_bycat$bycat <- ifelse(df_byer_bycat$folketal < 1000, "landsby",
                              ifelse(df_byer_bycat$folketal < 5000, "lille by",
                                     ifelse(df_byer_bycat$folketal < 20000, "almindelig by",
                                            ifelse(df_byer_bycat$folketal < 100000, "større by",
                                                   "storby"))))

table(df_byer_bycat$bycat)  #antal byer i hver kategori


#### Opgave 1.3 – Merge de to dataframes ####
#nu tilføjer vi readxl, som kan læse Excel-filen
library(readxl)

#for at kunne bruge boligdata læser vi filen ind
filsti_bolig <- "boligsiden OLA.xlsx"
df_bolig_raa <- read_excel(filsti_bolig, skip = 1)

#derefter laver vi teksten "NA" om til rigtige NA og fjerner rækker med manglende værdier
df_bolig_raa[df_bolig_raa == "NA"] <- NA
df_bolig_renset <- na.omit(df_bolig_raa)

nrow(df_bolig_raa)  #antal boliger før: 2552
nrow(df_bolig_renset)  #antal boliger efter: 2367

#tre boliger har postnummer og by byttet om, så vi tjekker rækkerne før vi retter dem
df_bolig_renset[c(264, 1740, 2031), c("postnr", "by")]

df_bolig_renset$postnr[264] <- as.numeric(df_bolig_renset$by[264])
df_bolig_renset$postnr[1740] <- as.numeric(df_bolig_renset$by[1740])
df_bolig_renset$postnr[2031] <- as.numeric(df_bolig_renset$by[2031])

df_bolig_renset$by[264] <- "moeldrup"
df_bolig_renset$by[1740] <- "kibaek"
df_bolig_renset$by[2031] <- "hilleroed"

#nu kan vi lave tal-kolonnerne om til tal
tal_kolonner <- c("pris", "opført", "kvmpris", "størrelse", "mdudg", "grund", "værelser", "postnr", "vejnr")
for (kol in tal_kolonner) {
  df_bolig_renset[[kol]] <- as.numeric(df_bolig_renset[[kol]])
}

#liggetid indeholder bogstaver ("dage"), som skal fjernes først
df_bolig_renset$liggetid <- gsub("[a-zA-Z]", " ", df_bolig_renset$liggetid)
df_bolig_renset$liggetid <- as.numeric(df_bolig_renset$liggetid)

#alder regnes ud fra opførelsesåret
df_bolig_renset$alder <- ifelse(df_bolig_renset$opført > 0, 2026 - df_bolig_renset$opført, NA)

#for at inddele boligerne i regioner bruger vi en funktion på postnummer
find_region <- function(postnr) {
  if (postnr >= 1000 & postnr <= 2999) {
    retval <- "hovedstaden"
  } else if (postnr >= 3000 & postnr <= 3699) {
    retval <- "sjælland"
  } else if (postnr >= 3700 & postnr <= 3790) {
    retval <- "bornholm"
  } else if (postnr >= 4000 & postnr <= 4999) {
    retval <- "sydsjælland"
  } else if (postnr >= 5000 & postnr <= 6999) {
    retval <- "syddanmark"
  } else if (postnr >= 7000 & postnr <= 7999) {
    retval <- "midtjylland"
  } else if (postnr >= 8000 & postnr <= 8999) {
    retval <- "aarhus omegn"
  } else if (postnr >= 9000 & postnr <= 9990) {
    retval <- "nordjylland"
  } else {
    retval <- NA
  }
  return(retval)
}

df_bolig_renset$region <- NA
for (i in 1:nrow(df_bolig_renset)) {
  df_bolig_renset$region[i] <- find_region(df_bolig_renset$postnr[i])
}

#derefter renser vi bynavnene med samme funktion som DST-data og fjerner postdistrikt-bogstaver som "c" i "aarhus c"
df_bolig_renset$by <- rens_by(df_bolig_renset$by)
df_bolig_renset$by <- sub(" (c|k|v|n|s|m|sv|nv|oe|soe|noe)$", "", df_bolig_renset$by)

#nu kan vi merge på "by": boligdata til venstre, bycat og folketal til højre
df_bolig_bycat <- merge(
  df_bolig_renset[, c("by", "pris", "kvmpris", "region")],
  df_byer_bycat[, c("by", "bycat", "folketal")],
  by = "by",
  all.x = TRUE
)

nrow(df_bolig_renset) == nrow(df_bolig_bycat)  #skal være TRUE
sum(is.na(df_bolig_bycat$bycat))  #boliger uden match i DST: 414
head(sort(table(df_bolig_bycat$by[is.na(df_bolig_bycat$bycat)]), decreasing = TRUE), 25)

#til sidst fjerner vi boliger uden bycat
df_bolig_bycat <- df_bolig_bycat[!is.na(df_bolig_bycat$bycat), ]
nrow(df_bolig_bycat)  #boliger tilbage: 1953


#### Opgave 1.4 – Plot ####
#for at lave plottet beregner vi først gennemsnitlig kvm-pris og antal boliger pr. bykategori
df_kvmpris_bycat <- aggregate(kvmpris ~ bycat, data = df_bolig_bycat, FUN = mean)
df_antal_bycat <- aggregate(kvmpris ~ bycat, data = df_bolig_bycat, FUN = length)
df_kvmpris_bycat$antal <- df_antal_bycat$kvmpris

#derefter sætter vi kategorierne i størrelsesorden
df_kvmpris_bycat$bycat <- factor(df_kvmpris_bycat$bycat,
                                 levels = c("landsby", "lille by", "almindelig by", "større by", "storby"))

#kategorierne med højest og lavest pris bruges i plottets tekst
hoejeste <- df_kvmpris_bycat$bycat[df_kvmpris_bycat$kvmpris == max(df_kvmpris_bycat$kvmpris)]
laveste <- df_kvmpris_bycat$bycat[df_kvmpris_bycat$kvmpris == min(df_kvmpris_bycat$kvmpris)]
forskel_pct <- round((max(df_kvmpris_bycat$kvmpris) / min(df_kvmpris_bycat$kvmpris) - 1) * 100)

#nu tilføjer vi ggplot2
library(ggplot2)

ggplot(df_kvmpris_bycat, aes(x = bycat, y = kvmpris)) +
  geom_bar(stat = "identity", fill = "pink", width = 0.7) +
  geom_text(aes(label = paste0(round(kvmpris), " kr.\n(n = ", antal, ")")), vjust = -0.3, size = 3.5) +
  scale_y_continuous(limits = c(0, 30000), expand = c(0, 0)) +
  labs(title = "Højeste gennemsnitspris pr. m² er i storby på 24.234 kr.",
       subtitle = paste0(hoejeste, " har den højeste pris pr. m² (", round(max(df_kvmpris_bycat$kvmpris)),
                         " kr.), ", forskel_pct, " % over laveste, ", laveste, " (", round(min(df_kvmpris_bycat$kvmpris)),
                         " kr.)"),
       x = "Bykategori",
       y = "Kr. pr. m²",
       caption = "Inddeling: landsby under 1.000, lille by under 5.000, almindelig by under 20.000, større by under 100.000, storby 100.000 og op.\nKilde: Boligsiden, Danmarks Statistik og egne beregninger") +
  theme_bw()


#### Opgave 2.1 – Opdatering af DI's forbrugertillidsindikator ####
#for at beregne den årlige realvækst henter vi husholdningernes forbrug fra NKH1
nkh1_meta <- dst_meta(table = "NKH1", lang = "da")

df_forbrug <- dst_get_data(
  table = "NKH1",
  TRANSAKT = "P.31 Husholdningernes forbrugsudgifter",
  PRISENHED = "2020-priser, kædede værdier",
  SÆSON = "Sæsonkorrigeret",
  Tid = "*",
  meta_data = nkh1_meta,
  lang = "da"
)

df_forbrug <- df_forbrug[order(df_forbrug$TID), ]
head(df_forbrug, 1)  #starter 1990K1
tail(df_forbrug, 1)  #slutter 2026K2

#derefter laver vi forbruget om til en tidsserie og regner væksten i forhold til samme kvartal året før
ts_forbrug <- ts(df_forbrug$value, start = c(1990, 1), frequency = 4)
vaekst <- (exp(diff(log(as.numeric(ts_forbrug)), lag = 4)) - 1) * 100
ts_realvaekst <- ts(vaekst, start = c(1991, 1), frequency = 4)

#nu kan vi starte i 2000K1, ligesom DI
ts_realvaekst_fra2000 <- window(ts_realvaekst, start = c(2000, 1))
end(ts_realvaekst_fra2000)  #seneste kvartal med vækst: 2026K2

#for at finde spørgsmålene bag indikatorerne henter vi forbrugertilliden fra FORV1
forv1_meta <- dst_meta(table = "FORV1", lang = "da")
forv1_meta[[3]]$INDIKATOR  #spørgsmålene F1 til F13

#derefter henter vi de seks spørgsmål, som DI-FTI og DST FTI bygger på
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

df_tillid_raa <- dst_get_data(
  table = "FORV1",
  query = forv1_query,
  meta_data = forv1_meta,
  lang = "da"
)

#nu tilføjer vi tidyr, som kan sætte hvert spørgsmål i sin egen kolonne
library(tidyr)

df_tillid_raa <- df_tillid_raa[order(df_tillid_raa$TID), ]
df_tillid_bred <- pivot_wider(df_tillid_raa, names_from = INDIKATOR, values_from = value)
df_tillid_bred <- df_tillid_bred[df_tillid_bred$TID >= as.Date("2000-01-01"), ]

#derefter laver vi månedstallene om til kvartalsgennemsnit
ts_tillid_md <- ts(as.matrix(df_tillid_bred[, -1]), start = c(2000, 1), frequency = 12)
ts_tillid_kvartal <- aggregate(ts_tillid_md, nfrequency = 4) / 3

end(ts_tillid_kvartal)  #seneste hele kvartal med forbrugertillid: 2026K3
ncol(ts_tillid_kvartal)  #skal være 6

#for at lave de to indikatorer vælger vi spørgsmålene ud fra deres kode (mellemrummet gør, at "F1 " ikke fanger "F10 ")
kol <- colnames(ts_tillid_kvartal)
di_kol <- startsWith(kol, "F2 ") | startsWith(kol, "F4 ") | startsWith(kol, "F9 ") | startsWith(kol, "F10 ")
dst_kol <- startsWith(kol, "F2 ") | startsWith(kol, "F3 ") | startsWith(kol, "F4 ") | startsWith(kol, "F5 ") | startsWith(kol, "F9 ")

sum(di_kol)  #skal være 4
sum(dst_kol)  #skal være 5

df_fti_kvartal <- data.frame(
  kvartal = round(as.numeric(time(ts_tillid_kvartal)), 2),
  di_fti = rowMeans(ts_tillid_kvartal[, di_kol]),
  dst_fti = rowMeans(ts_tillid_kvartal[, dst_kol])
)

df_vaekst_kvartal <- data.frame(
  kvartal = round(as.numeric(time(ts_realvaekst_fra2000)), 2),
  realvaekst = as.numeric(ts_realvaekst_fra2000)
)

#nu kan vi merge tillid (x) og vækst (y), så hver række er ét kvartal (2000.00 = 2000K1, 2000.25 = 2000K2 osv.)
df_fti_vaekst <- merge(df_fti_kvartal, df_vaekst_kvartal, by = "kvartal")

head(df_fti_vaekst, 3)
tail(df_fti_vaekst, 3)  #106 kvartaler, 2000K1 til 2026K2

seneste <- tail(df_fti_vaekst$kvartal, 1)
seneste_tekst <- paste0(floor(seneste), "K", round((seneste - floor(seneste)) * 4) + 1)
seneste_tekst  #2026K2

#derefter laver vi to simple lineære regressioner, én for hver indikator
lm_di <- lm(realvaekst ~ di_fti, data = df_fti_vaekst)
lm_dst <- lm(realvaekst ~ dst_fti, data = df_fti_vaekst)

summary(lm_di)
summary(lm_dst)

#for at sammenligne med DI's tabel laver vi en funktion, der regner R2 og korrelation på et udsnit af data
baum_tabel <- function(test) {
  data.frame(
    Maal = c("Forklaringsgrad (R2)", "Korrelation"),
    DI_FTI = round(c(summary(lm(realvaekst ~ di_fti, data = test))$r.squared,
                     cor(test$di_fti, test$realvaekst)), 2),
    FTI = round(c(summary(lm(realvaekst ~ dst_fti, data = test))$r.squared,
                  cor(test$dst_fti, test$realvaekst)), 2)
  )
}

#DI's egne tal for 2000K1-2016K2
df_baum_original <- data.frame(
  Maal = c("Forklaringsgrad (R2)", "Korrelation"),
  DI_FTI = c(0.54, 0.73),
  FTI = c(0.42, 0.65)
)

#nu kan vi regne vores tal på DI's periode, på perioden til og med 2023K4 og på alle data
df_r2_kor_baum_periode <- baum_tabel(df_fti_vaekst[df_fti_vaekst$kvartal <= 2016.25, ])
df_r2_kor_til2023 <- baum_tabel(df_fti_vaekst[df_fti_vaekst$kvartal <= 2023.75, ])
df_r2_kor_alle_kvartaler <- baum_tabel(df_fti_vaekst)

nrow(df_fti_vaekst[df_fti_vaekst$kvartal <= 2016.25, ])  #skal være 66
nrow(df_fti_vaekst[df_fti_vaekst$kvartal <= 2023.75, ])  #skal være 96

df_baum_original
df_r2_kor_baum_periode
df_r2_kor_til2023
df_r2_kor_alle_kvartaler

#for at se, hvad der trækker forklaringsgraden ned, regner vi R2 og korrelation på fire delperioder
r2_kor_udsnit <- function(test, navn) {
  data.frame(
    Udsnit = navn,
    Kvartaler = nrow(test),
    R2_DI = round(summary(lm(realvaekst ~ di_fti, data = test))$r.squared, 2),
    R2_DST = round(summary(lm(realvaekst ~ dst_fti, data = test))$r.squared, 2),
    Kor_DI = round(cor(test$di_fti, test$realvaekst), 2),
    Kor_DST = round(cor(test$dst_fti, test$realvaekst), 2)
  )
}

df_alle <- df_fti_vaekst
df_foer_corona <- df_fti_vaekst[df_fti_vaekst$kvartal <= 2019.75, ]
df_uden_krise <- df_fti_vaekst[df_fti_vaekst$kvartal < 2020 | df_fti_vaekst$kvartal > 2022.75, ]
df_efter_krise <- df_fti_vaekst[df_fti_vaekst$kvartal >= 2023, ]

df_r2_kor_udsnit <- rbind(
  r2_kor_udsnit(df_alle, paste0("Alle data, 2000K1-", seneste_tekst)),
  r2_kor_udsnit(df_foer_corona, "Før Corona, 2000K1-2019K4"),
  r2_kor_udsnit(df_uden_krise, "Uden 2020K1-2022K4"),
  r2_kor_udsnit(df_efter_krise, "Kun 2023K1 og frem")
)
df_r2_kor_udsnit

nrow(df_alle) - nrow(df_uden_krise)  #skal være 12 kvartaler

#til sidst laver vi plottet: søjler er årlig realvækst (højre akse), linjer er de to indikatorer (venstre akse)
skala <- 4  #højre akse er venstre akse delt med 4

ggplot(df_fti_vaekst, aes(x = kvartal)) +
  geom_bar(aes(y = realvaekst * skala), stat = "identity", fill = "hotpink") +
  geom_line(aes(y = dst_fti, color = "DST FTI"), linewidth = 0.8, linetype = "14", lineend = "round") +
  geom_line(aes(y = di_fti, color = "DI-FTI"), linewidth = 0.8) +
  geom_vline(xintercept = 2016.25, linetype = "dotted") +
  scale_x_continuous(expand = c(0, 0), breaks = seq(2000, 2026, by = 1)) +
  scale_y_continuous(name = "Nettotal", sec.axis = sec_axis(~ . / skala, name = "Pct.")) +
  scale_color_manual(values = c("DI-FTI" = "blue", "DST FTI" = "black")) +
  labs(x = "Årstal", color = NULL,
       title = "DI-FTI følger privatforbruget bedre end DST FTI",
       subtitle = paste0("Tillidsindikatorer og årlig realvækst, 2000K1-", seneste_tekst, ". Søjler: årlig realvækst i pct. Prikket linje er 2016K2"),
       caption = paste0("Forklaringsgrad (R2): DI-FTI ", round(summary(lm_di)$r.squared, 2),
                        ", DST FTI ", round(summary(lm_dst)$r.squared, 2),
                        ".\nKilde: Danmarks Statistik (FORV1, NKH1) og egne beregninger")) +
  theme_bw() +
  theme(legend.position = "top",
        panel.background = element_rect(fill = "lightgrey"),
        legend.key = element_rect(fill = "lightgrey"),
        legend.text = element_text(size = 20),
        legend.key.width = unit(2, "cm"),
        axis.title.y = element_text(angle = 0),
        axis.title.y.right = element_text(angle = 0),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 15),
        plot.caption = element_text(size = 10),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 8))


#### Opgave 2.2 – Forudsigelser af forbruget ####
#vi forudsiger 2026K3, fordi forbruget for kvartalet endnu ikke er offentliggjort
#tilliden for 2026K3 findes, men væksten gør ikke, så vi henter tilliden fra df_fti_kvartal
df_x_2026k3 <- df_fti_kvartal[df_fti_kvartal$kvartal == 2026.5, ]
df_x_2026k3

#derefter bruger vi predict(), som sætter tilliden ind i hver model
pred_di_2026 <- predict(lm_di, newdata = df_x_2026k3)
pred_dst_2026 <- predict(lm_dst, newdata = df_x_2026k3)
pred_di_2026  #-0,24
pred_dst_2026  #-0,73

#kontrol: skæring + hældning * x skal give samme tal
coef(lm_di)[1] + coef(lm_di)[2] * df_x_2026k3$di_fti
coef(lm_dst)[1] + coef(lm_dst)[2] * df_x_2026k3$dst_fti

#nu kan vi se usikkerheden med et 95 pct. prediktionsinterval
predict(lm_di, newdata = df_x_2026k3, interval = "prediction")
predict(lm_dst, newdata = df_x_2026k3, interval = "prediction")

#for at kontrollere metoden estimerer vi modellerne kun på data til 2023K2 og forudsiger 2023K3, hvor facit findes
df_til_2023 <- df_fti_vaekst[df_fti_vaekst$kvartal <= 2023.25, ]
nrow(df_til_2023)  #skal være 94

model_di_2023 <- lm(realvaekst ~ di_fti, data = df_til_2023)
model_dst_2023 <- lm(realvaekst ~ dst_fti, data = df_til_2023)

df_x_2023k3 <- df_fti_vaekst[df_fti_vaekst$kvartal == 2023.5, ]
pred_di_2023 <- predict(model_di_2023, newdata = df_x_2023k3)
pred_dst_2023 <- predict(model_dst_2023, newdata = df_x_2023k3)
facit_2023 <- df_x_2023k3$realvaekst

df_forudsagt_vs_facit <- data.frame(
  Model = c("DI-FTI", "DST FTI"),
  Forudsagt = round(c(pred_di_2023, pred_dst_2023), 2),
  Facit = round(facit_2023, 2),
  Afvigelse = round(c(pred_di_2023, pred_dst_2023) - facit_2023, 2)
)
df_forudsagt_vs_facit


#### Opgave 2.3 – Salg resten af året ####
#for at vurdere forudsigelsen ser vi først på væksten de seneste kvartaler
df_fti_vaekst[df_fti_vaekst$kvartal >= 2025.75, c("kvartal", "realvaekst")]

#derefter ser vi, om modellen de seneste kvartaler har gættet for lavt eller for højt (afvigelse = faktisk minus gæt)
seneste_kvartaler <- df_fti_vaekst$kvartal >= 2025.5
df_seneste_gaet <- data.frame(
  kvartal = df_fti_vaekst$kvartal[seneste_kvartaler],
  faktisk = round(df_fti_vaekst$realvaekst[seneste_kvartaler], 2),
  gaet_di = round(fitted(lm_di)[seneste_kvartaler], 2),
  afvigelse_di = round(resid(lm_di)[seneste_kvartaler], 2)
)
df_seneste_gaet

mean(resid(lm_di)[seneste_kvartaler])  #3,43: modellen gætter for lavt de seneste kvartaler
mean(resid(lm_di)[df_fti_vaekst$realvaekst < 0])  #-1,86: i kvartaler med faldende forbrug gætter den for højt


#### Opgave 2.4 – Prognoser fra DI og Nationalbanken ####
#DI og Nationalbanken forventer begge en vækst i privatforbruget på 1,8 pct. i 2026
di_2026 <- 1.8  #DI, prognose maj 2026
nb_2026 <- 1.8  #Nationalbanken, prognose 23.09.2026

#for at få et tal for hele 2026 bruger vi væksten i 1. og 2. kvartal og vores forudsigelse for 3. kvartal
vaekst_k1_2026 <- df_fti_vaekst$realvaekst[df_fti_vaekst$kvartal == 2026]
vaekst_k2_2026 <- df_fti_vaekst$realvaekst[df_fti_vaekst$kvartal == 2026.25]
vaekst_k3_2026 <- as.numeric(pred_di_2026)

#4. kvartal er et scenarie og ikke en forudsigelse: tilliden holder sig, så væksten er den samme som i 3. kvartal
vaekst_k4_scenarie <- vaekst_k3_2026

#årsvæksten er cirka gennemsnittet af de fire kvartalers årlige vækst
aarsvaekst_scenarie <- mean(c(vaekst_k1_2026, vaekst_k2_2026, vaekst_k3_2026, vaekst_k4_scenarie))

#nu kan vi regne ud, hvad 4. kvartal skal være for at året ender på DI's tal
vaekst_k4_krav <- 4 * di_2026 - (vaekst_k1_2026 + vaekst_k2_2026 + vaekst_k3_2026)

df_k4_scenarie_vs_krav <- data.frame(
  Kvartal = c("2026K1", "2026K2", "2026K3 (forudsigelse)", "2026K4 (scenarie)", "2026K4 (krav for at nå 1,8)"),
  Vaekst = round(c(vaekst_k1_2026, vaekst_k2_2026, vaekst_k3_2026, vaekst_k4_scenarie, vaekst_k4_krav), 2)
)
df_k4_scenarie_vs_krav

df_aarsvaekst_di_nb_model <- data.frame(
  Kilde = c("DI (maj 2026)", "Nationalbanken (23.09.2026)", "Vores model"),
  Aarsvaekst = c(di_2026, nb_2026, round(aarsvaekst_scenarie, 2))
)
df_aarsvaekst_di_nb_model

di_2026 - aarsvaekst_scenarie  #vores model ligger 0,48 procentpoint under DI og Nationalbanken


#### Opgave 3.1 – Modellens forudsigelser ####
#for at beregne de estimerede værdier henter vi skæring og hældning fra de to modeller
koef_di <- coef(lm_di)
koef_dst <- coef(lm_dst)

#derefter ganger vi indikatoren med hældningen og lægger skæringen til
df_fti_vaekst$est_di <- koef_di[1] + koef_di[2] * df_fti_vaekst$di_fti
df_fti_vaekst$est_dst <- koef_dst[1] + koef_dst[2] * df_fti_vaekst$dst_fti

#kontrol: samme tal som R's egen fitted()
all.equal(as.numeric(df_fti_vaekst$est_di), as.numeric(fitted(lm_di)))  #skal give TRUE
all.equal(as.numeric(df_fti_vaekst$est_dst), as.numeric(fitted(lm_dst)))  #skal give TRUE

round(coef(lm_di), 3)  #2,237 og 0,186
round(coef(lm_dst), 3)  #1,423 og 0,156

df_estimeret_vaekst <- data.frame(
  Kvartal = df_fti_vaekst$kvartal,
  Faktisk = round(df_fti_vaekst$realvaekst, 2),
  Est_DI = round(df_fti_vaekst$est_di, 2),
  Est_DST = round(df_fti_vaekst$est_dst, 2)
)
head(df_estimeret_vaekst)
tail(df_estimeret_vaekst)


#### Opgave 3.2 – Residualer ####
#en residual er det, der faktisk skete, minus det modellen gættede
df_fti_vaekst$res_di <- df_fti_vaekst$realvaekst - df_fti_vaekst$est_di
df_fti_vaekst$res_dst <- df_fti_vaekst$realvaekst - df_fti_vaekst$est_dst

#kontrol: samme tal som R's egne residualer, og gennemsnittet skal være 0
all.equal(as.numeric(df_fti_vaekst$res_di), as.numeric(resid(lm_di)))  #skal give TRUE
all.equal(as.numeric(df_fti_vaekst$res_dst), as.numeric(resid(lm_dst)))  #skal give TRUE
round(mean(df_fti_vaekst$res_di), 10)
round(mean(df_fti_vaekst$res_dst), 10)

df_residualer <- data.frame(
  Kvartal = df_fti_vaekst$kvartal,
  Faktisk = round(df_fti_vaekst$realvaekst, 2),
  Est_DI = round(df_fti_vaekst$est_di, 2),
  Res_DI = round(df_fti_vaekst$res_di, 2),
  Est_DST = round(df_fti_vaekst$est_dst, 2),
  Res_DST = round(df_fti_vaekst$res_dst, 2)
)
head(df_residualer)
tail(df_residualer)

#nu kan vi plotte residualerne mod modellernes gæt
ggplot(df_fti_vaekst, aes(x = est_di, y = res_di)) +
  geom_point(color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "hotpink") +
  labs(x = "Estimeret vækst (DI-FTI), pct.", y = "Residual, procentpoint",
       title = "Residualer mod estimerede værdier, DI-FTI",
       caption = paste0("Modellen afviger typisk med ", round(sd(df_fti_vaekst$res_di), 2),
                        " procentpoint.\nKilde: Danmarks Statistik (FORV1, NKH1) og egne beregninger")) +
  theme_classic()

ggplot(df_fti_vaekst, aes(x = est_dst, y = res_dst)) +
  geom_point(color = "darkgreen") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "hotpink") +
  labs(x = "Estimeret vækst (DST FTI), pct.", y = "Residual, procentpoint",
       title = "Residualer mod estimerede værdier, DST FTI",
       caption = paste0("Modellen afviger typisk med ", round(sd(df_fti_vaekst$res_dst), 2),
                        " procentpoint.\nKilde: Danmarks Statistik (FORV1, NKH1) og egne beregninger")) +
  theme_classic()

#derefter plotter vi residualerne over tid, så vi kan se, om afvigelserne ligger i stribe
ggplot(df_fti_vaekst, aes(x = kvartal)) +
  geom_point(aes(y = res_di, color = "DI-FTI")) +
  geom_point(aes(y = res_dst, color = "DST FTI")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "hotpink") +
  scale_color_manual(name = NULL, values = c("DI-FTI" = "black", "DST FTI" = "darkgreen")) +
  labs(x = NULL, y = "Residual, procentpoint",
       title = "Residualer over tid",
       caption = paste0("Modellerne gætter for lavt de seneste fire kvartaler: i snit ", round(mean(tail(df_fti_vaekst$res_di, 4)), 2),
                        " (DI-FTI) og ", round(mean(tail(df_fti_vaekst$res_dst, 4)), 2),
                        " (DST FTI) procentpoint.\nPositiv = faktisk vækst over modellens gæt.\nKilde: Danmarks Statistik (FORV1, NKH1) og egne beregninger")) +
  theme_classic() +
  theme(legend.position = "top")

#til sidst ser vi på kvartaler med afvigelser over 3 procentpoint og på spredningen
df_fti_vaekst[abs(df_fti_vaekst$res_di) > 3, c("kvartal", "realvaekst", "est_di", "res_di")]
df_fti_vaekst[abs(df_fti_vaekst$res_dst) > 3, c("kvartal", "realvaekst", "est_dst", "res_dst")]

sd(df_fti_vaekst$res_di)  #2,18
sd(df_fti_vaekst$res_dst)  #2,37
sd(df_fti_vaekst$realvaekst)  #2,75: til sammenligning svinger selve væksten


#### Opgave 3.3 – RSS og TSS ####
#RSS er alle modellens afvigelser i anden og lagt sammen
#TSS er alle udsving i væksten omkring dens gennemsnit i anden og lagt sammen
rss_di <- sum(df_fti_vaekst$res_di^2)
rss_dst <- sum(df_fti_vaekst$res_dst^2)
tss <- sum((df_fti_vaekst$realvaekst - mean(df_fti_vaekst$realvaekst))^2)

#kontrol: samme RSS som R's egne residualer
all.equal(rss_di, sum(resid(lm_di)^2))  #skal give TRUE
all.equal(rss_dst, sum(resid(lm_dst)^2))  #skal give TRUE

df_rss_tss <- data.frame(
  Maal = c("RSS (modellens afvigelser)", "TSS (udsving i væksten)"),
  DI = round(c(rss_di, tss), 1),
  DST = round(c(rss_dst, tss), 1)
)
df_rss_tss


#### Opgave 3.4 – Forklaringsgraden ####
#R2 = 1 - RSS / TSS viser, hvor stor en del af udsvingene modellen fjerner
r2_di <- 1 - rss_di / tss
r2_dst <- 1 - rss_dst / tss

#kontrol: samme tal som R's egne fra summary()
all.equal(r2_di, summary(lm_di)$r.squared)  #skal give TRUE
all.equal(r2_dst, summary(lm_dst)$r.squared)  #skal give TRUE

df_forklaringsgrad <- data.frame(
  Model = c("DI-FTI", "DST FTI"),
  RSS = round(c(rss_di, rss_dst), 1),
  TSS = round(c(tss, tss), 1),
  R2 = round(c(r2_di, r2_dst), 2)
)
df_forklaringsgrad




#### Opgave 4.1 – Illustration af forbrugertillid ####
#for at lave illustrationen henter vi DST's forbrugertillidsindikator fra FORV1 (beskrivelsen forv1_meta har vi allerede)
filter_FORV1 <- list(INDIKATOR = "Forbrugertillidsindikatoren",
                     Tid = "*")

fTillid_raw <- dst_get_data(table = "FORV1",
                            query = filter_FORV1,
                            meta_data = forv1_meta,
                            lang = "da")

#derefter afgrænser vi til januar 1996 og frem og laver månedstallene om til kvartalsgennemsnit
fTillid <- fTillid_raw[fTillid_raw$TID >= as.Date("1996-01-01"), ]
ftillid_ts <- ts(fTillid$value, start = c(1996, 1), frequency = 12)
ftillid_kvt_ts <- aggregate(ftillid_ts, nfrequency = 4) / 3

ftillid_kvt_df <- data.frame(
  Tidsinterval = paste0(floor(time(ftillid_kvt_ts)), "K", cycle(ftillid_kvt_ts)),
  Forbrugertillid = round(as.numeric(ftillid_kvt_ts), 1)
)

#for at finde, hvornår danskerne er mest og mindst optimistiske, trækker vi rækkerne med største og mindste værdi ud
stoerste_raekke <- ftillid_kvt_df[which.max(ftillid_kvt_df$Forbrugertillid), ]
mindste_raekke <- ftillid_kvt_df[which.min(ftillid_kvt_df$Forbrugertillid), ]
stoerste_raekke  #2006K1: 12,6
mindste_raekke  #2022K4: -32,1

#nu kan vi sætte de to punkter på plottet
punkter_ftillid_kvt_df <- rbind(stoerste_raekke, mindste_raekke)
punkter_ftillid_kvt_df$Tekst <- c(
  paste0("Maks: ", stoerste_raekke$Forbrugertillid, " (", stoerste_raekke$Tidsinterval, ")"),
  paste0("Min: ", mindste_raekke$Forbrugertillid, " (", mindste_raekke$Tidsinterval, ")")
)

ggplot(ftillid_kvt_df, aes(x = Tidsinterval, y = Forbrugertillid, group = 1)) +
  geom_line(colour = "hotpink4", linewidth = 1) +
  geom_point(data = punkter_ftillid_kvt_df, aes(x = Tidsinterval, y = Forbrugertillid), color = "hotpink3", size = 4) +
  geom_text(
    data = punkter_ftillid_kvt_df,
    aes(x = Tidsinterval, y = Forbrugertillid, label = Tekst),
    vjust = c(-1.2, 1.8),
    fontface = "bold",
    size = 5.5
  ) +
  scale_x_discrete(breaks = ftillid_kvt_df$Tidsinterval[seq(1, nrow(ftillid_kvt_df), by = 8)]) +
  labs(
    title = "DST's forbrugertillidsindikator over tid",
    subtitle = paste0("Kvartalsvise gennemsnit, 1996 til ", ftillid_kvt_df$Tidsinterval[nrow(ftillid_kvt_df)]),
    x = NULL,
    y = "Nettotal",
    caption = "Kilde: Danmarks Statistik & Egne beregninger"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold"),
    plot.subtitle = element_text(size = 20, margin = margin(b = 15)),
    plot.caption = element_text(size = 15, face = "italic", margin = margin(t = 20)),
    axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 10)),
    axis.text.x = element_text(angle = 60, hjust = 1, size = 12, face = "bold"),
    axis.text.y = element_text(size = 12)
  )


#### Opgave 4.2 – Gennemsnit af underspørgsmål ####
#for at beregne gennemsnittet henter vi spørgsmålet om at anskaffe større forbrugsgoder for øjeblikket
filter_forbrugsgoder <- list(INDIKATOR = "Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket",
                             Tid = "*")

forbrugsgoder_raw <- dst_get_data(table = "FORV1",
                                  query = filter_forbrugsgoder,
                                  meta_data = forv1_meta,
                                  lang = "da")

#derefter afgrænser vi til opgavens periode: 1. kvartal 2000 til og med 3. kvartal 2023 (januar 2000 til september 2023)
tid_md <- format(forbrugsgoder_raw$TID, "%Y-%m")
forbrugsgoder <- forbrugsgoder_raw[tid_md >= "2000-01" & tid_md <= "2023-09", ]
nrow(forbrugsgoder)  #skal være 285 måneder

forbrugsgoder_gns <- round(mean(forbrugsgoder$value), 2)
forbrugsgoder_gns


#### Opgave 4.3 – De 11 grupper af forbrug ####
#for at finde forbrugsgrupperne henter vi NAHC21 med dkstat
NAHC21 <- dst_meta(table = "NAHC21", lang = "da")

NAHC21$variables
NAHC21$values$PRISENHED
NAHC21$values$FORMAAAL
NAHC21$values$Tid

#de 11 forbrugsgrupper
formaal_11 <- c("Fødevarer", "Drikkevarer og tobak mv.", "Beklædning og fodtøj",
                "Boligbenyttelse", "Elektricitet, gas og andet brændsel",
                "Boligudstyr, husholdningsudstyr og vedligeholdelse heraf",
                "Medicin, lægeudgifter o.l.", "Køb af køretøjer", "Anden transport og kommunikation",
                "Fritidsudstyr, underholdning og rejser", "Andre varer og tjenester")

#teksten til boligudstyr hentes fra DST's egen liste, så stavningen matcher præcis
alle_formaal <- NAHC21$values$FORMAAAL$text
alle_formaal[grepl("Boligudstyr", alle_formaal)]  #skal give én tekst
formaal_11[6] <- alle_formaal[grepl("^Boligudstyr", alle_formaal)]

#derefter henter vi 2020 og 2023 til at beregne væksten
filter_NAHC21 <- list(PRISENHED = "2020-priser, kædede værdier",
                      Tid = c("2020", "2023"),
                      FORMAAAL = formaal_11)

forbrug_2020_2023_NAHC21 <- dst_get_data(table = "NAHC21",
                                         query = filter_NAHC21,
                                         meta_data = NAHC21,
                                         lang = "da")

#og 2022 til at finde, hvad danskerne brugte flest penge på
vaerdier_2022_NAHC21 <- dst_get_data(table = "NAHC21",
                                     PRISENHED = "2020-priser, kædede værdier",
                                     Tid = "2022",
                                     FORMAAAL = formaal_11,
                                     meta_data = NAHC21,
                                     lang = "da")

vaerdier_2022_NAHC21[which.max(vaerdier_2022_NAHC21$value), ]  #boligbenyttelse: 217.409 mio. kr.

#nu kan vi beregne væksten fra 2020 til 2023 for hver gruppe med et loop
#(forudsætter, at rækkerne står år for år med de 11 grupper i samme rækkefølge)
n <- nrow(forbrug_2020_2023_NAHC21)
procent_resultater <- rep(NA, n)

for (i in 1:(n - 11)) {
  j <- i + 11
  vaerdi_i <- forbrug_2020_2023_NAHC21$value[i]
  vaerdi_j <- forbrug_2020_2023_NAHC21$value[j]
  procent_aendring <- ((vaerdi_j - vaerdi_i) / vaerdi_i) * 100
  procent_resultater[j] <- procent_aendring
}

forbrug_2020_2023_NAHC21$Procent_aendring <- round(procent_resultater, 2)

forbrug_2020_2023_NAHC21[which.max(forbrug_2020_2023_NAHC21$Procent_aendring), ]  #andre varer og tjenester: 19,03 %


#### Opgave 4.4 – 22 simple lineære regressioner ####
#NAHC21 er en årlig tabel, så forbrugstilliden laves også om til årsgennemsnit for 2000-2023
#for at lave regressionerne henter vi først DST's forbrugertillid som årsgennemsnit
ts_ftillid_aar <- aggregate(ftillid_ts, nfrequency = 1) / 12
df_ftillid_aar_DST <- data.frame(Aar = as.numeric(floor(time(ts_ftillid_aar))),
                                 Forbrugertillid_DST = round(as.numeric(ts_ftillid_aar), 1))
df_ftillid_aar_DST <- df_ftillid_aar_DST[df_ftillid_aar_DST$Aar >= 2000 & df_ftillid_aar_DST$Aar <= 2023, ]

#derefter laver vi DI-FTI fra opgave 2.1 om til årsgennemsnit
ts_di_kvartal <- ts(df_fti_kvartal$di_fti, start = c(2000, 1), frequency = 4)
ts_di_aar <- aggregate(ts_di_kvartal, nfrequency = 1) / 4
df_ftillid_aar_DI <- data.frame(Aar = as.numeric(floor(time(ts_di_aar))),
                                Forbrugertillid_DI = round(as.numeric(ts_di_aar), 1))
df_ftillid_aar_DI <- df_ftillid_aar_DI[df_ftillid_aar_DI$Aar <= 2023, ]

#nu henter vi de 11 forbrugsgrupper fra 2000 og frem og giver dem navnene FT_CPA til FT_CPK
gruppe_navne <- c("FT_CPA", "FT_CPB", "FT_CPC", "FT_CPD", "FT_CPE", "FT_CPF", "FT_CPG", "FT_CPH", "FT_CPI", "FT_CPJ", "FT_CPK")
df_forbrugsgrupper <- NULL
for (i in 1:11) {
  df_gruppe <- dst_get_data(table = "NAHC21",
                            PRISENHED = "2020-priser, kædede værdier",
                            Tid = "*",
                            FORMAAAL = formaal_11[i],
                            meta_data = NAHC21,
                            lang = "da")
  df_gruppe <- df_gruppe[order(df_gruppe$TID), ]
  df_gruppe <- df_gruppe[as.numeric(format(df_gruppe$TID, "%Y")) >= 2000, ]
  if (i == 1) {
    df_forbrugsgrupper <- data.frame(Aar = as.numeric(format(df_gruppe$TID, "%Y")))
  }
  df_forbrugsgrupper[[gruppe_navne[i]]] <- df_gruppe$value
}

#derefter samler vi forbrugertillid og forbrug i ét datasæt, så hver række er ét år
samlet_lm_df <- merge(df_ftillid_aar_DST, df_ftillid_aar_DI, by = "Aar")
samlet_lm_df <- merge(samlet_lm_df, df_forbrugsgrupper, by = "Aar")
nrow(samlet_lm_df)  #24 år: 2000 til 2023

#nu kan vi lave de 22 regressioner og gemme hver summary i en liste
#grupperne: A fødevarer, B drikkevarer og tobak, C beklædning og fodtøj, D boligbenyttelse, E elektricitet og gas,
#F boligudstyr, G medicin, H køb af køretøjer, I anden transport og kommunikation, J fritid og rejser, K andre varer og tjenester

#DST's forbrugertillidsindikator
lm_CPA_DST <- lm(FT_CPA ~ Forbrugertillid_DST, data = samlet_lm_df)
sumr_list_lm_CPA_DST <- list(summary(lm_CPA_DST))
lm_CPB_DST <- lm(FT_CPB ~ Forbrugertillid_DST, data = samlet_lm_df)
sumr_list_lm_CPB_DST <- list(summary(lm_CPB_DST))
lm_CPC_DST <- lm(FT_CPC ~ Forbrugertillid_DST, data = samlet_lm_df)
sumr_list_lm_CPC_DST <- list(summary(lm_CPC_DST))
lm_CPD_DST <- lm(FT_CPD ~ Forbrugertillid_DST, data = samlet_lm_df)
sumr_list_lm_CPD_DST <- list(summary(lm_CPD_DST))
lm_CPE_DST <- lm(FT_CPE ~ Forbrugertillid_DST, data = samlet_lm_df)
sumr_list_lm_CPE_DST <- list(summary(lm_CPE_DST))
lm_CPF_DST <- lm(FT_CPF ~ Forbrugertillid_DST, data = samlet_lm_df)
sumr_list_lm_CPF_DST <- list(summary(lm_CPF_DST))
lm_CPG_DST <- lm(FT_CPG ~ Forbrugertillid_DST, data = samlet_lm_df)
sumr_list_lm_CPG_DST <- list(summary(lm_CPG_DST))
lm_CPH_DST <- lm(FT_CPH ~ Forbrugertillid_DST, data = samlet_lm_df)
sumr_list_lm_CPH_DST <- list(summary(lm_CPH_DST))
lm_CPI_DST <- lm(FT_CPI ~ Forbrugertillid_DST, data = samlet_lm_df)
sumr_list_lm_CPI_DST <- list(summary(lm_CPI_DST))
lm_CPJ_DST <- lm(FT_CPJ ~ Forbrugertillid_DST, data = samlet_lm_df)
sumr_list_lm_CPJ_DST <- list(summary(lm_CPJ_DST))
lm_CPK_DST <- lm(FT_CPK ~ Forbrugertillid_DST, data = samlet_lm_df)
sumr_list_lm_CPK_DST <- list(summary(lm_CPK_DST))

#DI's forbrugertillidsindikator
lm_CPA_DI <- lm(FT_CPA ~ Forbrugertillid_DI, data = samlet_lm_df)
sumr_list_lm_CPA_DI <- list(summary(lm_CPA_DI))
lm_CPB_DI <- lm(FT_CPB ~ Forbrugertillid_DI, data = samlet_lm_df)
sumr_list_lm_CPB_DI <- list(summary(lm_CPB_DI))
lm_CPC_DI <- lm(FT_CPC ~ Forbrugertillid_DI, data = samlet_lm_df)
sumr_list_lm_CPC_DI <- list(summary(lm_CPC_DI))
lm_CPD_DI <- lm(FT_CPD ~ Forbrugertillid_DI, data = samlet_lm_df)
sumr_list_lm_CPD_DI <- list(summary(lm_CPD_DI))
lm_CPE_DI <- lm(FT_CPE ~ Forbrugertillid_DI, data = samlet_lm_df)
sumr_list_lm_CPE_DI <- list(summary(lm_CPE_DI))
lm_CPF_DI <- lm(FT_CPF ~ Forbrugertillid_DI, data = samlet_lm_df)
sumr_list_lm_CPF_DI <- list(summary(lm_CPF_DI))
lm_CPG_DI <- lm(FT_CPG ~ Forbrugertillid_DI, data = samlet_lm_df)
sumr_list_lm_CPG_DI <- list(summary(lm_CPG_DI))
lm_CPH_DI <- lm(FT_CPH ~ Forbrugertillid_DI, data = samlet_lm_df)
sumr_list_lm_CPH_DI <- list(summary(lm_CPH_DI))
lm_CPI_DI <- lm(FT_CPI ~ Forbrugertillid_DI, data = samlet_lm_df)
sumr_list_lm_CPI_DI <- list(summary(lm_CPI_DI))
lm_CPJ_DI <- lm(FT_CPJ ~ Forbrugertillid_DI, data = samlet_lm_df)
sumr_list_lm_CPJ_DI <- list(summary(lm_CPJ_DI))
lm_CPK_DI <- lm(FT_CPK ~ Forbrugertillid_DI, data = samlet_lm_df)
sumr_list_lm_CPK_DI <- list(summary(lm_CPK_DI))

summary(lm_CPD_DST)  #eksempel: boligbenyttelse og DST's indikator

#til sidst plotter vi boligbenyttelse mod de to indikatorer
ggplot(samlet_lm_df, aes(y = FT_CPD, x = Forbrugertillid_DST)) +
  geom_point(color = "pink3") +
  geom_smooth(method = "lm", formula = y ~ x, color = "hotpink3") +
  theme_minimal() +
  labs(x = "DST's forbrugertillidsindikator",
       y = "Boligbenyttelse",
       title = "Lineær regression for boligbenyttelse og DST's forbrugertillidsindikator",
       caption = paste0("Forbrugertilliden forklarer ", round(summary(lm_CPD_DST)$r.squared * 100),
                        " pct. af udsvingene i boligbenyttelse.\nKilde: Danmarks Statistik & Egne beregninger"))

ggplot(samlet_lm_df, aes(y = FT_CPD, x = Forbrugertillid_DI)) +
  geom_point(color = "pink3") +
  geom_smooth(method = "lm", formula = y ~ x, color = "hotpink3") +
  theme_minimal() +
  labs(x = "DI's forbrugertillidsindikator",
       y = "Boligbenyttelse",
       title = "Lineær regression for boligbenyttelse og DI's forbrugertillidsindikator",
       caption = paste0("Forbrugertilliden forklarer ", round(summary(lm_CPD_DI)$r.squared * 100),
                        " pct. af udsvingene i boligbenyttelse.\nKilde: Danmarks Statistik & Egne beregninger"))


#### Opgave 5.1 – Kvartalsvis årlig realvækst for en række Eurolande ####
#for at hente data fra Eurostat bruger vi pakken restatapi
#nu tilføjer vi restatapi
library(restatapi)

#vi har valgt at tage data til og med 2026K2 i stedet for at stoppe i 2023
#derefter finder vi tabellen med husholdningernes forbrug, namq_10_fcs, i Eurostats tabeloversigt
alltabs <- get_eurostat_toc()
alltabs[alltabs$code == "namq_10_fcs", c("title", "code")]

#nu kan vi se tabellens filtre
husForbrugMeta <- get_eurostat_dsd("namq_10_fcs")

unique(husForbrugMeta$concept)
husForbrugMeta[husForbrugMeta$concept == "freq", ]  #kvartalsvist = Q
husForbrugMeta[husForbrugMeta$concept == "unit", ]  #vi vælger 2020 kædede værdier = CLV20_MEUR
husForbrugMeta[husForbrugMeta$concept == "s_adj", ]  #vi vælger sæson- og kalenderkorrigeret = SCA
husForbrugMeta[husForbrugMeta$concept == "na_item", ]  #vi vælger husholdningernes forbrug = P31_S14
husForbrugMeta[husForbrugMeta$concept == "geo", ]  #landekoder: DK, BE, NL, SE, AT, DE, FR, IT, ES

realvaekst_query <- list(unit = "CLV20_MEUR",
                         geo = c("DK", "BE", "NL", "SE", "AT", "DE", "FR", "IT", "ES"),
                         s_adj = "SCA",
                         na_item = "P31_S14",
                         freq = "Q")

#data hentes fra 2000, men vi tager 1999 med, så væksten kan regnes fra 2000K1
realvaekst_eurostat <- get_eurostat_data("namq_10_fcs",
                                         filters = realvaekst_query,
                                         date_filter = ">1999",
                                         verbose = TRUE)

#for at beregne realvæksten deler vi data op i ét datasæt pr. land (forudsætter, at hvert land står i tidsrækkefølge)
sub_AT <- realvaekst_eurostat[grepl("^AT", realvaekst_eurostat$geo), ]  #Østrig
sub_BE <- realvaekst_eurostat[grepl("^BE", realvaekst_eurostat$geo), ]  #Belgien
sub_DE <- realvaekst_eurostat[grepl("^DE", realvaekst_eurostat$geo), ]  #Tyskland
sub_DK <- realvaekst_eurostat[grepl("^DK", realvaekst_eurostat$geo), ]  #Danmark
sub_ES <- realvaekst_eurostat[grepl("^ES", realvaekst_eurostat$geo), ]  #Spanien
sub_FR <- realvaekst_eurostat[grepl("^FR", realvaekst_eurostat$geo), ]  #Frankrig
sub_IT <- realvaekst_eurostat[grepl("^IT", realvaekst_eurostat$geo), ]  #Italien
sub_NL <- realvaekst_eurostat[grepl("^NL", realvaekst_eurostat$geo), ]  #Holland
sub_SE <- realvaekst_eurostat[grepl("^SE", realvaekst_eurostat$geo), ]  #Sverige

#derefter regner vi den årlige realvækst (lag = 4 sammenligner med samme kvartal året før)
sub_AT_RVp <- round(data.frame((exp(diff(log(as.numeric(sub_AT$values)), lag = 4)) - 1) * 100), 3)
names(sub_AT_RVp) <- "Østrig"
sub_BE_RVp <- round(data.frame((exp(diff(log(as.numeric(sub_BE$values)), lag = 4)) - 1) * 100), 3)
names(sub_BE_RVp) <- "Belgien"
sub_DE_RVp <- round(data.frame((exp(diff(log(as.numeric(sub_DE$values)), lag = 4)) - 1) * 100), 3)
names(sub_DE_RVp) <- "Tyskland"
sub_DK_RVp <- round(data.frame((exp(diff(log(as.numeric(sub_DK$values)), lag = 4)) - 1) * 100), 3)
names(sub_DK_RVp) <- "Danmark"
sub_ES_RVp <- round(data.frame((exp(diff(log(as.numeric(sub_ES$values)), lag = 4)) - 1) * 100), 3)
names(sub_ES_RVp) <- "Spanien"
sub_FR_RVp <- round(data.frame((exp(diff(log(as.numeric(sub_FR$values)), lag = 4)) - 1) * 100), 3)
names(sub_FR_RVp) <- "Frankrig"
sub_IT_RVp <- round(data.frame((exp(diff(log(as.numeric(sub_IT$values)), lag = 4)) - 1) * 100), 3)
names(sub_IT_RVp) <- "Italien"
sub_NL_RVp <- round(data.frame((exp(diff(log(as.numeric(sub_NL$values)), lag = 4)) - 1) * 100), 3)
names(sub_NL_RVp) <- "Holland"
sub_SE_RVp <- round(data.frame((exp(diff(log(as.numeric(sub_SE$values)), lag = 4)) - 1) * 100), 3)
names(sub_SE_RVp) <- "Sverige"

#nu kan vi samle landene med en tidskolonne (de første fire kvartaler forsvinder, fordi væksten kræver et år)
sub_tid <- data.frame(Tidsperioder = sub_AT$time[-(1:4)])

samlet_RV_df_EU_stat <- cbind(sub_tid, sub_SE_RVp, sub_NL_RVp, sub_IT_RVp,
                              sub_FR_RVp, sub_ES_RVp, sub_DK_RVp, sub_DE_RVp,
                              sub_BE_RVp, sub_AT_RVp)

head(samlet_RV_df_EU_stat, 4)
tail(samlet_RV_df_EU_stat, 4)


#### Opgave 5.2 – Højeste kvartalsvise årlige realvækst ####
#for at finde landet med den højeste gennemsnitlige realvækst regner vi gennemsnittet for hvert land
gns_SE <- round(mean(samlet_RV_df_EU_stat$Sverige), 3)
gns_NL <- round(mean(samlet_RV_df_EU_stat$Holland), 3)
gns_IT <- round(mean(samlet_RV_df_EU_stat$Italien), 3)
gns_FR <- round(mean(samlet_RV_df_EU_stat$Frankrig), 3)
gns_ES <- round(mean(samlet_RV_df_EU_stat$Spanien), 3)
gns_DK <- round(mean(samlet_RV_df_EU_stat$Danmark), 3)
gns_DE <- round(mean(samlet_RV_df_EU_stat$Tyskland), 3)
gns_BE <- round(mean(samlet_RV_df_EU_stat$Belgien), 3)
gns_AT <- round(mean(samlet_RV_df_EU_stat$Østrig), 3)

gns_df <- data.frame(gns_SE, gns_NL, gns_IT, gns_FR, gns_ES, gns_DK, gns_DE, gns_BE, gns_AT)

#derefter sætter vi dem op under hinanden og giver landene navne
gnst_long <- pivot_longer(gns_df, cols = everything(), names_to = "Land", values_to = "Gennemsnit")
gnst_long$Land <- gsub("gns_", "", gnst_long$Land)

landenavne <- c(SE = "Sverige", NL = "Holland", IT = "Italien", FR = "Frankrig",
                ES = "Spanien", DK = "Danmark", DE = "Tyskland", BE = "Belgien", AT = "Østrig")
gnst_long$Land_navn <- landenavne[gnst_long$Land]

ggplot(gnst_long, aes(x = reorder(Land_navn, -Gennemsnit), y = Gennemsnit, fill = Gennemsnit)) +
  geom_col() +
  scale_fill_gradient(low = "pink", high = "hotpink3") +
  geom_text(aes(label = paste0(round(Gennemsnit, 2), "%")), vjust = -0.5) +
  guides(fill = "none") +
  labs(x = NULL,
       y = "Gennemsnit %",
       title = "Sverige har haft den største gennemsnitlige realvækst i årrækken 2000-2026",
       subtitle = "Den gennemsnitlige realvækst er beregnet ud fra husholdningernes privatforbrug",
       caption = "Kilde: EuroStat & Egne beregninger") +
  theme_minimal() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 20))


#### Opgave 5.3 – Coronakrisen som outlier ####
#for at fjerne coronakrisen sletter vi 2020K2 til og med 2023K3 (række 82 til 95) og regner gennemsnittene igen
samlet_RV_df_EU_stat_corona <- samlet_RV_df_EU_stat[-(82:95), ]

gns_SE_corona <- round(mean(samlet_RV_df_EU_stat_corona$Sverige), 3)
gns_NL_corona <- round(mean(samlet_RV_df_EU_stat_corona$Holland), 3)
gns_IT_corona <- round(mean(samlet_RV_df_EU_stat_corona$Italien), 3)
gns_FR_corona <- round(mean(samlet_RV_df_EU_stat_corona$Frankrig), 3)
gns_ES_corona <- round(mean(samlet_RV_df_EU_stat_corona$Spanien), 3)
gns_DK_corona <- round(mean(samlet_RV_df_EU_stat_corona$Danmark), 3)
gns_DE_corona <- round(mean(samlet_RV_df_EU_stat_corona$Tyskland), 3)
gns_BE_corona <- round(mean(samlet_RV_df_EU_stat_corona$Belgien), 3)
gns_AT_corona <- round(mean(samlet_RV_df_EU_stat_corona$Østrig), 3)

gns_df_corona <- data.frame(gns_SE_corona, gns_NL_corona, gns_IT_corona,
                            gns_FR_corona, gns_ES_corona, gns_DK_corona,
                            gns_DE_corona, gns_BE_corona, gns_AT_corona)

gnst_long_corona <- pivot_longer(gns_df_corona, cols = everything(), names_to = "Land", values_to = "Gennemsnit")
gnst_long_corona$Land <- gsub("gns_|_corona", "", gnst_long_corona$Land)
gnst_long_corona$Land_navn <- landenavne[gnst_long_corona$Land]

land_hoejeste_uden_corona <- gnst_long_corona$Land_navn[which.max(gnst_long_corona$Gennemsnit)]

ggplot(gnst_long_corona, aes(x = reorder(Land_navn, -Gennemsnit), y = Gennemsnit, fill = Gennemsnit)) +
  geom_col() +
  scale_fill_gradient(low = "pink", high = "hotpink3") +
  geom_text(aes(label = paste0(round(Gennemsnit, 2), "%")), vjust = -0.5) +
  guides(fill = "none") +
  labs(x = NULL,
       y = "Gennemsnit %",
       title = paste0(land_hoejeste_uden_corona, " har den største gennemsnitlige realvækst uden coronaperioden (2020K2-2023K3)"),
       subtitle = "Den gennemsnitlige realvækst er beregnet ud fra husholdningernes privatforbrug",
       caption = "Kilde: EuroStat & Egne beregninger") +
  theme_classic()

#nu kan vi sammenligne gennemsnittet med og uden coronaperioden for at se, hvor coronakrisen har haft størst effekt
aendring_i_rv <- data.frame(Lande = gnst_long$Land,
                            Samlet = gnst_long$Gennemsnit,
                            Uden_corona = gnst_long_corona$Gennemsnit)

#ændring i procent: positiv = coronaperioden trak landets gennemsnit ned, negativ = coronaperioden løftede det
aendring_i_rv$Aendring_p <- round(aendring_i_rv$Uden_corona - aendring_i_rv$Samlet, 3)
aendring_i_rv$Aendring <- round(((aendring_i_rv$Uden_corona - aendring_i_rv$Samlet) / aendring_i_rv$Samlet) * 100, 3)
aendring_i_rv

land_stoerst_effekt <- landenavne[aendring_i_rv$Lande[which.max(abs(aendring_i_rv$Aendring))]]

ggplot(aendring_i_rv, aes(x = reorder(Lande, -Aendring), y = Aendring, fill = Aendring)) +
  geom_col() +
  scale_fill_gradient(low = "pink", high = "hotpink3") +
  geom_text(aes(label = paste0(round(Aendring, 2), "%")), vjust = -0.5) +
  guides(fill = "none") +
  labs(x = NULL,
       y = "Ændring",
       title = paste0("Coronakrisen har haft størst effekt på ", land_stoerst_effekt, "s gennemsnitlige realvækst"),
       subtitle = "Ændring i gennemsnitlig realvækst, når coronaperioden fjernes, i procent af gennemsnittet for hele perioden",
       caption = "Kilde: EuroStat") +
  theme_classic()


#### Opgave 5.4 – Effekt af Corona på forbruget ####
#for at finde faldet sammenligner vi gennemsnittet i 2020K1-2023K2 (række 81 til 94) med et korrigeret gennemsnit
#perioden er lang, og opsvinget i 2022-23 udligner faldet i 2020-21, så vi bruger 2015K1-2019K4 og 2023K3 og frem som sammenligning
sub_2015_k_df <- samlet_RV_df_EU_stat[-(81:94), ]
sub_2015_k_df <- sub_2015_k_df[-(1:60), ]

sub_2015_gns_SE <- round(mean(sub_2015_k_df$Sverige), 3)
sub_2015_gns_NL <- round(mean(sub_2015_k_df$Holland), 3)
sub_2015_gns_IT <- round(mean(sub_2015_k_df$Italien), 3)
sub_2015_gns_FR <- round(mean(sub_2015_k_df$Frankrig), 3)
sub_2015_gns_ES <- round(mean(sub_2015_k_df$Spanien), 3)
sub_2015_gns_DK <- round(mean(sub_2015_k_df$Danmark), 3)
sub_2015_gns_DE <- round(mean(sub_2015_k_df$Tyskland), 3)
sub_2015_gns_BE <- round(mean(sub_2015_k_df$Belgien), 3)
sub_2015_gns_AT <- round(mean(sub_2015_k_df$Østrig), 3)

sub_2015_k_df <- data.frame(sub_2015_gns_SE, sub_2015_gns_NL, sub_2015_gns_IT,
                            sub_2015_gns_FR, sub_2015_gns_ES, sub_2015_gns_DK,
                            sub_2015_gns_DE, sub_2015_gns_BE, sub_2015_gns_AT)

sub_2015_k_df_long <- pivot_longer(sub_2015_k_df, cols = everything(),
                                   names_to = "Land", values_to = "Gennemsnit korrigeret")

#derefter regner vi gennemsnittet i coronaperioden
corona_vaekst_gns_SE <- round(mean(samlet_RV_df_EU_stat[(81:94), 2]), 3)
corona_vaekst_gns_NL <- round(mean(samlet_RV_df_EU_stat[(81:94), 3]), 3)
corona_vaekst_gns_IT <- round(mean(samlet_RV_df_EU_stat[(81:94), 4]), 3)
corona_vaekst_gns_FR <- round(mean(samlet_RV_df_EU_stat[(81:94), 5]), 3)
corona_vaekst_gns_ES <- round(mean(samlet_RV_df_EU_stat[(81:94), 6]), 3)
corona_vaekst_gns_DK <- round(mean(samlet_RV_df_EU_stat[(81:94), 7]), 3)
corona_vaekst_gns_DE <- round(mean(samlet_RV_df_EU_stat[(81:94), 8]), 3)
corona_vaekst_gns_BE <- round(mean(samlet_RV_df_EU_stat[(81:94), 9]), 3)
corona_vaekst_gns_AT <- round(mean(samlet_RV_df_EU_stat[(81:94), 10]), 3)

sub_2015_corona_df <- data.frame(corona_vaekst_gns_SE, corona_vaekst_gns_NL, corona_vaekst_gns_IT,
                                 corona_vaekst_gns_FR, corona_vaekst_gns_ES, corona_vaekst_gns_DK,
                                 corona_vaekst_gns_DE, corona_vaekst_gns_BE, corona_vaekst_gns_AT)

sub_2015_corona_df_long <- pivot_longer(sub_2015_corona_df, cols = everything(),
                                        names_to = "Land", values_to = "Gennemsnit corona")

#nu kan vi samle de to gennemsnit og beregne forskellen i procent
samlet_df_opg_5.4 <- data.frame(Land = sub_2015_k_df_long$Land,
                                Gns_korrigeret = sub_2015_k_df_long$`Gennemsnit korrigeret`,
                                Gns_corona = sub_2015_corona_df_long$`Gennemsnit corona`)

samlet_df_opg_5.4$Forskel <- round(((samlet_df_opg_5.4$Gns_corona - samlet_df_opg_5.4$Gns_korrigeret) /
                                      samlet_df_opg_5.4$Gns_korrigeret) * 100, 3)
samlet_df_opg_5.4

samlet_df_opg_5.4$Land_kort <- gsub("sub_2015_gns_", "", samlet_df_opg_5.4$Land)

ggplot(samlet_df_opg_5.4, aes(x = reorder(Land_kort, -Forskel), y = Forskel, fill = Forskel)) +
  geom_col() +
  scale_fill_gradient(low = "pink", high = "hotpink3") +
  geom_text(aes(label = paste0(round(Forskel, 2), "%"),
                vjust = ifelse(Forskel >= 0, -0.5, 1.2)),
            size = 3.5) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(title = "Forskellen i gennemsnitlig realvækst",
       subtitle = "Gennemsnit i coronaperioden (2020K1-2023K2) mod korrigeret gennemsnit",
       x = NULL,
       y = "Forskel i procent",
       caption = "Kilde: EuroStat")

#til sidst plotter vi udviklingen i realvæksten for alle landene
#nu tilføjer vi scales, som kun bruges til at sætte procenttegn på aksen i plottet
library(scales)

plot_5.4_df <- pivot_longer(samlet_RV_df_EU_stat,
                            cols = -Tidsperioder,
                            names_to = "Land",
                            values_to = "Realvaekst")

ggplot(plot_5.4_df, aes(x = Tidsperioder, y = Realvaekst, color = Land, group = Land)) +
  geom_line() +
  scale_x_discrete(breaks = function(x) x[grepl("-Q1$", x)],
                   labels = function(x) sub("-Q1$", "", x)) +
  scale_y_continuous(labels = unit_format(unit = "%")) +
  scale_color_brewer(palette = "PuRd") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  labs(x = "Tidsperiode",
       y = "Realvækst i procent",
       title = "Udvikling i realvækst i procent",
       subtitle = "Coronakrisens påvirkning på realvæksten i perioden 2020-2023 for de valgte lande",
       caption = "Kilde: EuroStat & Egne beregninger")


