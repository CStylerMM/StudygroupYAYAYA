#OLA 2, opgave 1 - Boliger og DST

#### Opgave 1.1 – Det første skridt ####
#Skriv en kode, der viser hvordan du finder en tabel som kan give en liste over byer med indbyggertal vha
#DST-pakken dkstat.

#tid til at bruge dkstat
library(dkstat)

dst_search(string = "byområde", field = "text")   #søger i DST's tabeller efter ordet "byområde", her fandt vi BY3

by3_meta <- dst_meta(table = "BY3", lang = "da")   #henter beskrivelsen af BY3
by3_meta$variables                                 #viser hvilke variable vi kan vælge på

df_byer_folketal <- dst_get_data(
  table = "BY3",               #tabellen vi fandt
  BYER = "*",                  #alle byer
  Tid = "2026",                #kun 2026
  FOLKARTAET = "Folketal",     #vi vil have folketal
  meta_data = by3_meta,        #bruger meta-dataen vi lige hentede
  lang = "da"
)

sum(df_byer_folketal$value)   #samlet folketal før vi renser, så vi kan sammenligne bagefter

#vi renser: det der ikke er en by skal ud
df_byer_renset <- df_byer_folketal[df_byer_folketal$value != 0, ]                 #fjerner rækker med 0 indbyggere
df_byer_renset <- df_byer_renset[!grepl("Uden fast bopæl", df_byer_renset$BYER), ]  #fjerner "Uden fast bopæl", det er ikke en by
df_byer_renset <- df_byer_renset[!grepl("Landdistrikter", df_byer_renset$BYER), ]   #fjerner landdistrikter, det er heller ikke en by

nrow(df_byer_renset)                                               #antal rækker tilbage
sum(df_byer_renset$value)                                          #folketal i byerne efter rensning
1 - sum(df_byer_renset$value) / sum(df_byer_folketal$value) #andel af folketallet vi har fjernet


#### Opgave 1.2 – Kategori-variabel. ####
#Lav en kategorivariabel i R hvor du skal inddele byerne i følgende kategorier: "landsby","lille by",
#"almindelig by", "større by", "storby" ud fra et interval på indbyggertal som du selv definerer.

#navnene i DST og boligdata skal være ens før vi kan merge, så vi laver en rense-funktion
rens_by <- function(x) {
  x <- tolower(x)                     #alt til små bogstaver
  x <- gsub("\\s*\\(.*?\\)", "", x)   #fjerner alt i parentes
  x <- gsub("[0-9]", "", x)           #fjerner tal
  x <- gsub("æ", "ae", x)             #æ, ø, å og é skrives om, så navnene matcher
  x <- gsub("ø", "oe", x)
  x <- gsub("å", "aa", x)
  x <- gsub("é", "e", x)
  x <- gsub("-", " ", x)              #bindestreg bliver til mellemrum
  trimws(x)                           #fjerner mellemrum i enderne
}

df_byer_renset$by <- rens_by(df_byer_renset$BYER)   #ny kolonne "by" med rensede navne
head(df_byer_renset[, c("BYER", "by")])                    #tjek at navnene ser rigtige ud

#nogle byer ligger i flere kommuner og står derfor flere gange
dubletter <- df_byer_renset$by[duplicated(df_byer_renset$by)]
df_byer_renset[df_byer_renset$by %in% dubletter, c("BYER", "value")]   #tjek: er det samme by delt over kommuner, eller to byer med samme navn?

df_byer_bycat <- aggregate(value ~ by, data = df_byer_renset, FUN = sum)   #lægger delene sammen, så hver by står én gang
names(df_byer_bycat) <- c("by", "folketal")                                       #giver kolonnen et navn der siger noget
sum(duplicated(df_byer_bycat$by))                                                 #skal være 0

#grænserne har vi selv valgt. DST tæller kun bebyggelser fra 200 indbyggere som by, så "landsby" er sat til under 1.000
df_byer_bycat$bycat <- ifelse(
  df_byer_bycat$folketal < 1000, "landsby",
  ifelse(
    df_byer_bycat$folketal < 5000, "lille by",
    ifelse(
      df_byer_bycat$folketal < 20000, "almindelig by",
      ifelse(
        df_byer_bycat$folketal < 100000, "større by",
        "storby"
      )
    )
  )
)

table(df_byer_bycat$bycat)   #antal byer i hver kategori, alle fem skal have byer

## husk at begrunde grænserne i rapporten, de skal kunne forsvares


#### Opgave 1.3 – Merge de to dataframes ####
#Indlæs filen med boliger og tilpas de to dataframes så du kan merge de to sammen via variablen ”by”
#således at du får kategorien bycat med i dit bolig-datasæt fra OLA 1.

#tid til at bruge readxl
library(readxl)

df_bolig_raa <- read_excel("Ida Rstudio Projekter/OLA 1/boligsiden OLA.xlsx", skip = 1)   #stien starter fra projektmappen

df_bolig_raa[df_bolig_raa == "NA"] <- NA      #teksten "NA" bliver til rigtige NA
df_bolig_renset <- na.omit(df_bolig_raa)      #fjerner alle rækker med mindst én NA

nrow(df_bolig_raa)       #antal boliger før
nrow(df_bolig_renset)    #antal boliger efter, forskellen skal med i rapporten

#tre boliger har postnummer og by byttet om. Tjek først at det er de rigtige rækker
df_bolig_renset[c(264, 1740, 2031), c("postnr", "by")]

df_bolig_renset$postnr[264] <- as.numeric(df_bolig_renset$by[264])     #byen stod i postnr-kolonnen
df_bolig_renset$postnr[1740] <- as.numeric(df_bolig_renset$by[1740])
df_bolig_renset$postnr[2031] <- as.numeric(df_bolig_renset$by[2031])

df_bolig_renset$by[264] <- "moeldrup"      #og den rigtige by sættes ind
df_bolig_renset$by[1740] <- "kibaek"
df_bolig_renset$by[2031] <- "hilleroed"

#alle tal-kolonner skal være tal, ellers kan vi ikke regne på dem
tal_kolonner <- c("pris", "opført", "kvmpris", "størrelse", "mdudg", "grund", "værelser", "postnr", "vejnr")
for (kol in tal_kolonner) {
  df_bolig_renset[[kol]] <- as.numeric(df_bolig_renset[[kol]])   #laver kolonnen om til tal
}

#liggetid har bogstaver i ("dage"), så de skal fjernes før det bliver til tal
df_bolig_renset$liggetid <- gsub("[a-zA-Z]", " ", df_bolig_renset$liggetid)
df_bolig_renset$liggetid <- as.numeric(df_bolig_renset$liggetid)

#alder: hvis opført er over 0 regner vi alder ud, ellers NA
df_bolig_renset$alder <- ifelse(df_bolig_renset$opført > 0, 2026 - df_bolig_renset$opført, NA)

#region ud fra postnummer. Postnr er tal nu, så sammenligningen med 1000, 3000 osv. virker rigtigt
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

df_bolig_renset$region <- sapply(df_bolig_renset$postnr, find_region)   #kører funktionen på hver bolig

#bynavnene i boligdata renses med samme funktion som DST-data
df_bolig_renset$by <- rens_by(df_bolig_renset$by)

#postdistrikt-bogstaver ("aarhus c", "koebenhavn k") bruger DST ikke, så de fjernes i enden af navnet
df_bolig_renset$by <- sub(" (c|k|v|n|s|m|sv|nv|oe|soe|noe)$", "", df_bolig_renset$by)


#### Opgave 1.4 – Plot ####
#Din merge skal producere en dataframe og et plot, som minder om det du ser nedenfor - men den
#præcise udformning kommer naturligvis an på hvilken inddeling du vælger.

#merge på "by": boligdata til venstre, bycat og folketal fra DST til højre
df_bolig_bycat <- merge(
  df_bolig_renset[, c("by", "pris", "kvmpris", "region")],
  df_byer_bycat[, c("by", "bycat", "folketal")],
  by = "by",
  all.x = TRUE      #beholder alle boliger, også dem uden match i DST
)

nrow(df_bolig_renset) == nrow(df_bolig_bycat)   #skal være TRUE, ellers har merge ganget boliger op

sum(is.na(df_bolig_bycat$bycat))   #boliger uden match i DST
head(sort(table(df_bolig_bycat$by[is.na(df_bolig_bycat$bycat)]), decreasing = TRUE), 25)   #de byer der oftest mangler match

df_bolig_bycat <- na.omit(df_bolig_bycat)   #først nu fjerner vi boliger uden bycat
nrow(df_bolig_bycat)                               #boliger tilbage til plottet

#dataframe til plottet: gennemsnitlig kvm-pris og antal boliger pr. bykategori
df_kvmpris_bycat <- aggregate(kvmpris ~ bycat, data = df_bolig_bycat, FUN = mean)
df_antal_bycat <- aggregate(kvmpris ~ bycat, data = df_bolig_bycat, FUN = length)
df_kvmpris_bycat$antal <- df_antal_bycat$kvmpris   #kategorierne står i samme rækkefølge i begge, så antal kan lægges på

#så søjlerne kommer i størrelsesorden i stedet for alfabetisk
df_kvmpris_bycat$bycat <- factor(df_kvmpris_bycat$bycat,
                                 levels = c("landsby", "lille by", "almindelig by", "større by", "storby"))

#kategorierne med højest og lavest kvm-pris, bruges i captionen
hoejeste <- df_kvmpris_bycat$bycat[which.max(df_kvmpris_bycat$kvmpris)]
laveste <- df_kvmpris_bycat$bycat[which.min(df_kvmpris_bycat$kvmpris)]

#tid til at bruge ggplot2
library(ggplot2)

ggplot(df_kvmpris_bycat, aes(x = bycat, y = kvmpris)) +
  geom_bar(stat = "identity", fill = "pink", width = 0.7) +   #søjlerne får den højde vi selv har regnet
  geom_text(aes(label = paste0(round(kvmpris), " kr.\n(n = ", antal, ")")), vjust = -0.3, size = 3.5) +   #pris og antal over søjlen
  ylim(0, max(df_kvmpris_bycat$kvmpris) * 1.15) +      #plads over søjlerne til teksten
  labs(title = "Gennemsnitlig pris pr. m² efter bykategori",
       subtitle = "Boliger til salg, byer kategoriseret efter DST's byområder 2026",
       x = "Bykategori", y = "Kr. pr. m²",
       caption = paste0("Højeste pris pr. m²: ", hoejeste, " (", round(max(df_kvmpris_bycat$kvmpris)),
                        " kr.). Laveste: ", laveste, " (", round(min(df_kvmpris_bycat$kvmpris)),
                        " kr.).\nKilde: Boligsiden og Danmarks Statistik (BY3)")) +
  theme_classic()   #rent tema: hvid baggrund, ingen gitterlinjer

## vi vender tilbage og ser på hvorfor kategorierne koster det de gør


#### Opgave 2.1 – Opdatering af DI’s forbrugertillidsindikator ####
#Opdatér DI’s forbrugertillidsindikator med data frem til og med 2023 fra artiklen ”Forbruget
#fortsætter fremgangen i 2016” (Baum, 2016). Lav vurdering af om forbrugertillidsindikatoren fra DI
#fortsat er bedre end forbrugertillidsindikatoren fra DST. (Hint: I bliver nødt til at nærlæse bilaget for
#DI-FTI for at finde starttidspunktet for estimationen, spørgsmålene, samt tabel, der sammenligner
#FTI og DI-FTI)

## Vi har valgt at bruge alle data frem til i dag (2026) i stedet for at stoppe i 2023

#Y: årlig realvækst i forbruget. Vi henter alt, så det slutter af sig selv ved seneste kvartal

#tid til at bruge dkstat
library(dkstat)

nkh1_meta <- dst_meta(table = "NKH1", lang = "da")   #beskrivelsen af NKH1

df_forbrug <- dst_get_data(
  table = "NKH1",
  TRANSAKT = "P.31 Husholdningernes forbrugsudgifter",   #husholdningernes forbrug
  PRISENHED = "2020-priser, kædede værdier",             #uden prisstigninger
  SÆSON = "Sæsonkorrigeret",
  Tid = "*",                                             #alt!!!
  meta_data = nkh1_meta,
  lang = "da"
)

df_forbrug <- df_forbrug[order(df_forbrug$TID), ]   #sorterer efter tid
head(df_forbrug, 1)   #slad os se hvornår det starter
tail(df_forbrug, 1)   #slutter det i 2026? - ja frem til 01.04.2026 lige nu.

#kvartalsdata som tidsserie, og årlig vækst (lag = 4 sammenligner med samme kvartal året før)
ts_forbrug <- ts(df_forbrug$value, start = c(1990, 1), frequency = 4)
vaekst <- (exp(diff(log(as.numeric(ts_forbrug)), lag = 4)) - 1) * 100   #formlen fra slidesene
ts_realvaekst <- ts(vaekst, start = c(1991, 1), frequency = 4)          #første vækst er 1991K1, fordi vi mister et år

#vi starter i 2000K1, fordi Baum starter dér
ts_realvaekst_fra2000 <- window(ts_realvaekst, start = c(2000, 1)) #bruger window til at udvælge et tidspunkt
end(ts_realvaekst_fra2000)   #seneste kvartal med vækst


#X: forbrugertillid. DST giver den månedligt, så den skal omregnes til kvartaler, så det matcher yyyyyyyyyy
forv1_meta <- dst_meta(table = "FORV1", lang = "da")

#de seks spørgsmål der indgår i DI-FTI eller DST FTI (F2, F3, F4, F5, F9, F10)
unique(forv1_meta)

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

#tid til at bruge tidyr
library(tidyr)

df_tillid_raa <- df_tillid_raa[order(df_tillid_raa$TID), ] #rækkerne sorteres efter tid
df_tillid_bred <- pivot_wider(df_tillid_raa, names_from = INDIKATOR, values_from = value)   #ét spørgsmål pr. kolonne
df_tillid_bred <- df_tillid_bred[df_tillid_bred$TID >= as.Date("2000-01-01"), ] #vi starter fra år 2000 ligesom Mr. Baum

#månedlig tidsserie, og så aggregeres der til kvartaler (3 måneder lagt sammen og delt med 3 = gennemsnit)
ts_tillid_md <- ts(as.matrix(df_tillid_bred[, -1]), start = c(2000, 1), frequency = 12) #tidsserie m måned
ts_tillid_kvartal <- aggregate(ts_tillid_md, nfrequency = 4) / 3 #tidsserie m kvartal 

end(ts_tillid_kvartal)    #seneste hele kvartal med forbrugertillid
ncol(ts_tillid_kvartal)   #skal være 6, fordi vi har 6 spørgsmål 


#de to indikatorer er bare gennemsnit af spørgsmål
kol <- colnames(ts_tillid_kvartal)
kol

#mellemrum efter koden, så "F1 " ikke fanger "F10 " - med andre ord. vi siger di er 4 spørgsmål og dst er 5 spørgsmål
di_kol <- startsWith(kol, "F2 ") | startsWith(kol, "F4 ") | startsWith(kol, "F9 ") | startsWith(kol, "F10 ")   #Baums fire spørgsmål
dst_kol <- startsWith(kol, "F2 ") | startsWith(kol, "F3 ") | startsWith(kol, "F4 ") | startsWith(kol, "F5 ") | startsWith(kol, "F9 ")   #DST's fem spørgsmål

sum(di_kol)    #skal være 4
sum(dst_kol)   #skal være 5

df_fti_kvartal <- data.frame(
  kvartal = round(as.numeric(time(ts_tillid_kvartal)), 2),   #2000.00, 2000.25, 2000.50 osv.
  di_fti = rowMeans(ts_tillid_kvartal[, di_kol]),
  dst_fti = rowMeans(ts_tillid_kvartal[, dst_kol])
) #Yay nu har vi kolonner med DI og DST

df_vaekst_kvartal <- data.frame(
  kvartal = round(as.numeric(time(ts_realvaekst_fra2000)), 2),
  realvaekst = as.numeric(ts_realvaekst_fra2000)
) #dobbelt yay, det her er vores realvækst yay

#merge: vi sætter tillid (X) og vækst (Y) sammen i én df, så hver række er ét kvartal
#kvartal er et tal: 2000.00 = 2000K1, 2000.25 = 2000K2, 2000.50 = 2000K3, 2000.75 = 2000K4, 2001.00 = 2001K1
df_fti_vaekst <- merge(df_fti_kvartal, df_vaekst_kvartal, by = "kvartal")

head(df_fti_vaekst, 3)
tail(df_fti_vaekst, 3) #yay vi kan se vi har 106 kvartaller 

seneste <- tail(df_fti_vaekst$kvartal, 1) #tager det sidste kvartal
seneste_tekst <- paste0(floor(seneste), "K", round((seneste - floor(seneste)) * 4) + 1)
seneste_tekst   #seneste kvartal vi har både tillid og forbrug for


#to simple lineære regressioner: Y = realvækst, X = én indikator ad gangen
lm_di <- lm(realvaekst ~ di_fti, data = df_fti_vaekst)
lm_dst <- lm(realvaekst ~ dst_fti, data = df_fti_vaekst)

summary(lm_di)
#Coefficients:
#             Estimate Std. Error t value Pr(>|t|)
#(Intercept)   2.23679    0.24095   9.283 2.68e-15 ***
#di_fti        0.18606    0.02362   7.879 3.39e-12 ***
#Residual standard error: 2.19 on 104 degrees of freedom
#Multiple R-squared:  0.3738

##Estimate = koefficenten (altså når DI FTI stiger med 1 stiger væksten i gennemsnit med 0.186

##Std. Error = Standard afvigelse for koefficenten, hvis vi brugte nye kvartaller og lavede det her igen.
##ville svinge med 0.02362

##t-value = ja tværdi som kigger på støj og signal. det egentlig blot est/std. error. signal på 7,9 viser godt signal, 
##hvis vi følger tommelfingerreglen om at det skal være over 2. før der stærkt signal.

##Pr(>|t|)= p værdi. hvor vi skal opstille H0, nulhypotese. Vi skal være mr. negativ og sige "der er ikke en 
##sammenhæng mellem vækst og forbrugertillid" er dette tæt på nul, har vi afvist hypotesen.
##Da vores p værdi er 3.39e-12 *** Betyder det at det er MEGET tæt på nul.

##Så det super. den hypotese er afvist. (man kan heller ikke have en t værdi der er dårlig (under 2.) 
##og en lav p-værdi. de to tal siger det samme. Men fungerer som en både at forklare begge ting.

##R2 0,37: DI-FTI forklarer 37 pct. af udsvingene i væksten
##residual standard error 2,19: modellens gæt ligger typisk cirka 2 procentpoint fra den faktiske vækst


summary(lm_dst)
#Coefficients:
#             Estimate Std. Error t value Pr(>|t|)
#(Intercept)   1.42304    0.23186   6.137 1.55e-08 ***
#dst_fti       0.15574    0.02588   6.018 2.68e-08 ***
#Residual standard error: 2.383 on 104 degrees of freedom
#Multiple R-squared:  0.2583

##R2 0,2583 mod R2 0,3738 for DI-FTI: DST FTI forklarer mindre, win for baum

#funktion der laver Baums tabel (R2 og korrelation) på et udsnit af data
#test = det stykke data vi giver funktionen, fx alle kvartaler eller kun til 2016K2
baum_tabel <- function(test) {
  data.frame(
    Mål = c("Forklaringsgrad (R2)", "Korrelation"),            #rækkerne: det vi måler
    DI_FTI = round(c(summary(lm(realvaekst ~ di_fti, data = test))$r.squared,   #R2: regression med DI-FTI som X
                     cor(test$di_fti, test$realvaekst)), 2),                     #korrelation mellem DI-FTI og væksten
    FTI = round(c(summary(lm(realvaekst ~ dst_fti, data = test))$r.squared,     #samme med DST FTI
                  cor(test$dst_fti, test$realvaekst)), 2)
  )
}

#vi rekreare Baum's Di FTI fra 2000 til 2016. starter med at samle R2 og korrelation
df_baum_original <- data.frame(
  Mål = c("Forklaringsgrad (R2)", "Korrelation"),
  DI_FTI = c(0.54, 0.73),
  FTI = c(0.42, 0.65)
)

#vores tal på Baums periode og på alle vores data
#perioden skal være mindre eller lig med 2016.25 (2016K2). Starten er 2000, for det er der df_fti_vaekst starter
df_r2_kor_baum_periode <- baum_tabel(df_fti_vaekst[df_fti_vaekst$kvartal <= 2016.25, ])   #kun kvartalerne fra 2000 til og med 2016K2
df_r2_kor_alle_kvartaler <- baum_tabel(df_fti_vaekst)                                      #alle kvartaler vi har, 2000K1 til 2026K2

#nrow tæller rækkerne i udsnittet, altså hvor mange kvartaler der er med fra 2000 til 2016K2
#skal være 66, for Baum kører 2000K1 til 2016K2 og det er 66 kvartaler
nrow(df_fti_vaekst[df_fti_vaekst$kvartal <= 2016.25, ])

df_baum_original           #Baums egne tal
df_r2_kor_baum_periode     #vores tal på hans periode
df_r2_kor_alle_kvartaler   #vores tal på alle kvartaler


#som vi kan se. så er mønsteret det samme fra baum originalen og vores tal på hans periode

#hvis vi tager alle kvartaller er mønsteret der. men vi kan se DST holder en højere korrelation (Estimate)
## er forspringet til DI-FTI blevet større eller mindre siden Baum? Sammenlign forskellen i R2 mellem de tre tabeller


#Så skal der plottes!

#væksten er procent og indikatorerne er nettotal, så væksten får sin egen akse til højre

#tid til at bruge ggplot2
#plot: Baums figur efterlignet, men strakt fra hans 2000-2016 til 2026
#søjler = årlig realvækst i forbruget (højre akse). Linjerne = DI-FTI og DST FTI (venstre akse)
#den stiplede lodrette linje er 2016K2, hvor Baum slutter. Alt til højre for den er det nye, vi har lagt til
#væksten er procent og indikatorerne er nettotal, så væksten ganges med skala og får sin egen akse til højre

library(ggplot2)

#Baums figur efterlignet: søjler = forbrugets vækst, linjer = tillidsindikatorerne
#tid til at bruge ggplot2
library(ggplot2)

skala <- 4   #højre akse (procent) er venstre akse (nettotal) delt med 4

ggplot(df_fti_vaekst, aes(x = kvartal)) +
  geom_bar(aes(y = realvaekst * skala), stat = "identity", fill = "hotpink") +   #søjler = årlig realvækst
  geom_line(aes(y = dst_fti, color = "DST FTI"), linewidth = 0.8, linetype = "14", lineend = "round") +
  geom_line(aes(y = di_fti, color = "DI-FTI"), linewidth = 0.8) +
  geom_vline(xintercept = 2016.25, linetype = "dotted") +   #2016K2, hvor Baum slutter
  scale_x_continuous(expand = c(0, 0), breaks = seq(2000, 2026, by = 1)) +   #hvert år
  scale_y_continuous(name = "Nettotal", sec.axis = sec_axis(~ . / skala, name = "Pct.")) +   #højre akse = venstre delt med skala
  scale_color_manual(values = c("DI-FTI" = "blue", "DST FTI" = "black")) +
  labs(x = "Årstal", color = NULL,
       title = "DI-FTI følger privatforbruget bedre end DST FTI",
       subtitle = paste0("Tillidsindikatorer og årlig realvækst, 2000K1-", seneste_tekst, ". Søjler: årlig realvækst i pct. Prikket linje er 2016K2"),
       caption = paste0("Forklaringsgrad (R2): DI-FTI ", round(summary(lm_di)$r.squared, 2),
                        ", DST FTI ", round(summary(lm_dst)$r.squared, 2),
                        ".\nKilde: Danmarks Statistik (FORV1, NKH1) og egne beregninger")) +
  #Lets make it a nice, med vinkler og størrelsse
  theme_bw() +
  theme(legend.position = "top",
        panel.background = element_rect(fill = "lightgrey"),   #mørk baggrund inde i figuren
        legend.key = element_rect(fill = "lightgrey"),   #mørk baggrund bag stregerne i forklaringen
        legend.text = element_text(size = 20),   #DI-FTI og DST FTI
        legend.key.width = unit(2, "cm"),   #længere streger i forklaringen
        axis.title.y = element_text(angle = 0),   #"Nettotal" vandret
        axis.title.y.right = element_text(angle = 0),   #"Pct." vandret
        plot.title = element_text(size = 20),   #titlen
        plot.subtitle = element_text(size = 15),   #undertitlen
        plot.caption = element_text(size = 10),   #teksten nederst
        axis.title = element_text(size = 15),   #"Nettotal", "Pct." og "Årstal"
        axis.text = element_text(size = 8))   #tallene på akserne
#Corona og krig: er det dem der gør R2 lavere end hos Baum? Vi tester det og gætter ikke
#funktion der regner R2 og korrelation på ét udsnit
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

#fire udsnit. 2020K1 = 2020.00 og 2022K4 = 2022.75
df_alle <- df_fti_vaekst
df_foer_corona <- df_fti_vaekst[df_fti_vaekst$kvartal <= 2019.75, ]                                  #2000K1-2019K4
df_uden_krise <- df_fti_vaekst[df_fti_vaekst$kvartal < 2020 | df_fti_vaekst$kvartal > 2022.75, ]     #uden 2020K1-2022K4
df_efter_krise <- df_fti_vaekst[df_fti_vaekst$kvartal >= 2023, ]                                     #2023K1 og frem

df_r2_kor_udsnit <- rbind(
  r2_kor_udsnit(df_alle, paste0("Alle data, 2000K1-", seneste_tekst)),
  r2_kor_udsnit(df_foer_corona, "Før Corona, 2000K1-2019K4"),
  r2_kor_udsnit(df_uden_krise, "Uden 2020K1-2022K4"),
  r2_kor_udsnit(df_efter_krise, "Kun 2023K1 og frem")
)
df_r2_kor_udsnit

nrow(df_alle) - nrow(df_uden_krise)   #skal være 12 kvartaler (2020K1-2022K4)

## hvis R2 stiger tydeligt uden Corona og krig kan vi skrive det som forklaring, ellers er det en gætning


#### Opgave 2.2 – Forudsigelser af forbruget ####
#Beregn/forudsig den årlige realvækst i husholdningernes forbrugsudgift for 3. kvartal 2023 med
#henholdsvis DI’s forbrugertillidsindikator og forbrugertillidsindikatoren fra DST.

## gruppen har valgt 3. kvartal 2026 som hovedforudsigelse, fordi den er ægte: forbruget er ikke offentliggjort endnu
## 3. kvartal 2023 bruger vi bagefter som kontrol af metoden, fordi facit findes dér

#2026K3 (= 2026.5) findes ikke i df_fti_vaekst, fordi vækst mangler. Tilliden findes i df_fti_kvartal
df_x_2026k3 <- df_fti_kvartal[df_fti_kvartal$kvartal == 2026.5, ]
df_x_2026k3   #di_fti og dst_fti for 2026K3

#predict() sætter X ind i hver model og giver den forudsagte vækst
pred_di_2026 <- predict(lm_di, newdata = df_x_2026k3)
pred_dst_2026 <- predict(lm_dst, newdata = df_x_2026k3)

pred_di_2026    #DI-FTI's forudsigelse for 2026K3
pred_dst_2026   #DST FTI's forudsigelse for 2026K3

#kontrol i hånden: skæring + hældning * X, skal give samme tal
coef(lm_di)[1] + coef(lm_di)[2] * df_x_2026k3$di_fti
coef(lm_dst)[1] + coef(lm_dst)[2] * df_x_2026k3$dst_fti

#usikkerheden på forudsigelsen, 95 pct. interval
predict(lm_di, newdata = df_x_2026k3, interval = "prediction")
predict(lm_dst, newdata = df_x_2026k3, interval = "prediction")


#kontrol af metoden på 2023K3, hvor facit findes
#modellen må kun kende data til 2023K2, ellers har den set facit på forhånd
df_til_2023 <- df_fti_vaekst[df_fti_vaekst$kvartal <= 2023.25, ]
nrow(df_til_2023)   #skal være 94 kvartaler

model_di_2023 <- lm(realvaekst ~ di_fti, data = df_til_2023)
model_dst_2023 <- lm(realvaekst ~ dst_fti, data = df_til_2023)

df_x_2023k3 <- df_fti_vaekst[df_fti_vaekst$kvartal == 2023.5, ]   #her har vi også facit (realvaekst)
df_x_2023k3

pred_di_2023 <- predict(model_di_2023, newdata = df_x_2023k3)
pred_dst_2023 <- predict(model_dst_2023, newdata = df_x_2023k3)
facit_2023 <- df_x_2023k3$realvaekst

df_forudsagt_vs_facit <- data.frame(
  Model = c("DI-FTI", "DST FTI"),
  Forudsagt = round(c(pred_di_2023, pred_dst_2023), 2),
  Facit = round(facit_2023, 2),
  Afvigelse = round(c(pred_di_2023, pred_dst_2023) - facit_2023, 2)   #forudsagt minus facit
)
df_forudsagt_vs_facit

## positiv afvigelse = modellen gættede for højt. Det er kun ét kvartal, så det er ikke en generel test


#### Opgave 2.3 – Salg resten af året ####
#Med afsæt i jeres forudsigelse fra opgave 2.2, ville I så være bekymrede for virksomhedernes salg
#til hr. og fru Danmark resten af året. Giv en uddybende forklaring på jeres svar.

#hvor stor har væksten været for nylig? Referencen for om forudsigelsen er meget eller lidt
df_fti_vaekst[df_fti_vaekst$kvartal >= 2025.75, c("kvartal", "realvaekst")]

#gættede modellen for lavt eller for højt de seneste kvartaler? (fejl = faktisk minus gæt)
seneste_kvartaler <- df_fti_vaekst$kvartal >= 2025.5   #2025K3 og frem
df_seneste_gaet <- data.frame(
  kvartal = df_fti_vaekst$kvartal[seneste_kvartaler],
  faktisk = round(df_fti_vaekst$realvaekst[seneste_kvartaler], 2),
  gaet_di = round(fitted(lm_di)[seneste_kvartaler], 2),
  fejl_di = round(resid(lm_di)[seneste_kvartaler], 2)
)
df_seneste_gaet

mean(resid(lm_di)[seneste_kvartaler])   #positiv = modellen gætter for lavt de seneste kvartaler

#og i kvartaler hvor forbruget faldt: gætter modellen for højt eller lavt?
mean(resid(lm_di)[df_fti_vaekst$realvaekst < 0])   #negativ = gættede for højt, altså undervurderede faldet

## svaret skal bruge både punktforudsigelsen, det brede interval fra 2.2 og fejlene fra de seneste kvartaler


#### Opgave 2.4 – Prognoser fra DI og Nationalbanken ####
#Hvor stor realvækst i privatforbruget forventer DI og Nationalbanken i deres seneste prognoser?
#Sammenhold deres tal med jeres svar i opgave 2.3.

## tjek begge tal og datoer i kilderne før aflevering, og om DI har en nyere prognose end maj
di_2026 <- 1.8   #DI, prognose maj 2026
nb_2026 <- 1.8   #Nationalbanken, prognose sep. 2026

#kvartalernes årlige vækst i 2026. 2026K1 = 2026.00 og 2026K2 = 2026.25
vaekst_k1_2026 <- df_fti_vaekst$realvaekst[df_fti_vaekst$kvartal == 2026]
vaekst_k2_2026 <- df_fti_vaekst$realvaekst[df_fti_vaekst$kvartal == 2026.25]
vaekst_k3_2026 <- as.numeric(pred_di_2026)   #vores forudsigelse fra 2.2 (DI-FTI)

#K4 kender vi ikke, så vi antager at væksten er gennemsnittet af de tre første
vaekst_k4_antaget <- mean(c(vaekst_k1_2026, vaekst_k2_2026, vaekst_k3_2026))

#årsvæksten er cirka gennemsnittet af de fire kvartalers årlige vækst
aarsvaekst_antaget <- mean(c(vaekst_k1_2026, vaekst_k2_2026, vaekst_k3_2026, vaekst_k4_antaget))

#hvad skal K4 være, for at året ender på DI's tal? Fire kvartaler gange DI's tal minus de tre vi har
vaekst_k4_krav <- 4 * di_2026 - (vaekst_k1_2026 + vaekst_k2_2026 + vaekst_k3_2026)

df_k4_antaget_vs_krav <- data.frame(
  Kvartal = c("2026K1", "2026K2", "2026K3 (forudsigelse)", "2026K4 (antaget)", "2026K4 (krav for at nå DI's tal)"),
  Vaekst = round(c(vaekst_k1_2026, vaekst_k2_2026, vaekst_k3_2026, vaekst_k4_antaget, vaekst_k4_krav), 2)
)
df_k4_antaget_vs_krav

df_aarsvaekst_di_nb_model <- data.frame(
  Kilde = c("DI (maj 2026)", "Nationalbanken (sep. 2026)", "Vores model"),
  Aarsvaekst = c(di_2026, nb_2026, round(aarsvaekst_antaget, 2))
)
df_aarsvaekst_di_nb_model

di_2026 - aarsvaekst_antaget   #hvor mange procentpoint ligger vores model under DI?

## i rapporten: årsvæksten er en tilnærmelse (gennemsnit af kvartalernes vækst), 
#og K4 er en antagelse, ikke en forudsigelse

#### Opgave 3.1 – Modellens forudsigelser ####
#Med udgangspunkt i jeres besvarelse i opgave 2, bedes I beregne jeres estimerede værdier for den
#kvartalsvise årlige vækstrate i husholdningernes forbrug. (hint: I skal gange jeres estimerede
#koefficienter med x-variablene fra den estimerede model).

koef_di <- coef(lm_di)     #skæring og hældning fra DI-modellen
koef_dst <- coef(lm_dst)   #skæring og hældning fra DST-modellen

#skæring + hældning * x giver den estimerede vækst for hvert kvartal
df_fti_vaekst$est_di <- koef_di[1] + koef_di[2] * df_fti_vaekst$di_fti
df_fti_vaekst$est_dst <- koef_dst[1] + koef_dst[2] * df_fti_vaekst$dst_fti

#kontrol: samme tal som R selv får?
all.equal(as.numeric(df_fti_vaekst$est_di), as.numeric(fitted(lm_di)))     #skal give TRUE
all.equal(as.numeric(df_fti_vaekst$est_dst), as.numeric(fitted(lm_dst)))   #skal give TRUE

round(coef(lm_di), 3)    #tal til rapporten
round(coef(lm_dst), 3)

df_estimeret_vaekst <- data.frame(
  Kvartal = df_fti_vaekst$kvartal,
  Faktisk = round(df_fti_vaekst$realvaekst, 2),
  Est_DI = round(df_fti_vaekst$est_di, 2),
  Est_DST = round(df_fti_vaekst$est_dst, 2)
)
head(df_estimeret_vaekst)
tail(df_estimeret_vaekst)


#### Opgave 3.2 – Residualer ####
#Med udgangspunkt i jeres besvarelse i opgave 3.1, bedes I beregne residualer for henholdsvis DI’s
#og DST’s forbrugertillidsindikator og plot disse i forhold til jeres forudsagte resultater fra opgave
#3.1 for de to modeller.

#residual = det der faktisk skete minus det modellen gættede
df_fti_vaekst$res_di <- df_fti_vaekst$realvaekst - df_fti_vaekst$est_di
df_fti_vaekst$res_dst <- df_fti_vaekst$realvaekst - df_fti_vaekst$est_dst

#kontrol: samme tal som R selv får?
all.equal(as.numeric(df_fti_vaekst$res_di), as.numeric(resid(lm_di)))     #skal give TRUE
all.equal(as.numeric(df_fti_vaekst$res_dst), as.numeric(resid(lm_dst)))   #skal give TRUE

round(mean(df_fti_vaekst$res_di), 10)    #residualerne skal i snit være 0 (rundet, ellers kan R vise fx -9e-17, det er afrundingsstøj)
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

#to små hjælpefunktioner til captions
komma <- function(x) format(round(x, 2), decimal.mark = ",")                      #tal med komma
kvartal_tekst <- function(k) paste0(floor(k), "K", round((k - floor(k)) * 4) + 1)   #2021.25 bliver til 2021K2

#kvartalet med den største fejl i hver model
stoerste_di <- which.max(abs(df_fti_vaekst$res_di))
stoerste_dst <- which.max(abs(df_fti_vaekst$res_dst))

#tid til at bruge ggplot2
library(ggplot2)

#residualer mod modellens gæt. En god model giver punkter spredt tilfældigt omkring nul
ggplot(df_fti_vaekst, aes(x = est_di, y = res_di)) +
  geom_point(color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "hotpink") +   #nul = modellen ramte rigtigt
  labs(x = "Estimeret vækst (DI-FTI), pct.", y = "Residual, procentpoint",
       title = "Residualer mod estimerede værdier, DI-FTI",
       caption = paste0("Residualernes standardafvigelse er ", komma(sd(df_fti_vaekst$res_di)),
                        " procentpoint. Største fejl: ", kvartal_tekst(df_fti_vaekst$kvartal[stoerste_di]),
                        " (", komma(df_fti_vaekst$res_di[stoerste_di]), ").\nKilde: Danmarks Statistik (FORV1, NKH1) og egne beregninger")) +
  theme_classic()

ggplot(df_fti_vaekst, aes(x = est_dst, y = res_dst)) +
  geom_point(color = "darkgreen") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "hotpink") +
  labs(x = "Estimeret vækst (DST FTI), pct.", y = "Residual, procentpoint",
       title = "Residualer mod estimerede værdier, DST FTI",
       caption = paste0("Residualernes standardafvigelse er ", komma(sd(df_fti_vaekst$res_dst)),
                        " procentpoint. Største fejl: ", kvartal_tekst(df_fti_vaekst$kvartal[stoerste_dst]),
                        " (", komma(df_fti_vaekst$res_dst[stoerste_dst]), ").\nKilde: Danmarks Statistik (FORV1, NKH1) og egne beregninger")) +
  theme_classic()

#ekstra: residualerne over tid. Hvis fejlene ligger i stribe over eller under nul, er de ikke tilfældige
ggplot(df_fti_vaekst, aes(x = kvartal)) +
  geom_point(aes(y = res_di, color = "DI-FTI")) +
  geom_point(aes(y = res_dst, color = "DST FTI")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "hotpink") +
  scale_color_manual(name = NULL, values = c("DI-FTI" = "black", "DST FTI" = "darkgreen")) +
  labs(x = NULL, y = "Residual, procentpoint",
       title = "Residualer over tid",
       caption = paste0("Gennemsnitlig fejl de seneste fire kvartaler: ", komma(mean(tail(df_fti_vaekst$res_di, 4))),
                        " (DI-FTI) og ", komma(mean(tail(df_fti_vaekst$res_dst, 4))),
                        " (DST FTI).\nPositiv = modellen gætter for lavt.\nKilde: Danmarks Statistik (FORV1, NKH1) og egne beregninger")) +   #linjeskift, så captionen ikke klippes af
  theme_classic() +
  theme(legend.position = "top")

#kvartaler med fejl over 3 procentpoint
df_fti_vaekst[abs(df_fti_vaekst$res_di) > 3, c("kvartal", "realvaekst", "est_di", "res_di")]
df_fti_vaekst[abs(df_fti_vaekst$res_dst) > 3, c("kvartal", "realvaekst", "est_dst", "res_dst")]

sd(df_fti_vaekst$res_di)         #hvor meget bomber modellen typisk?
sd(df_fti_vaekst$res_dst)
sd(df_fti_vaekst$realvaekst)     #til sammenligning: hvor meget svinger selve væksten?


#### Opgave 3.3 – RSS og TSS ####
#Med udgangspunkt i jeres besvarelse i opgave 3.1, bedes I beregne residualer for henholdsvis DI’s
#og DST’s forbrugertillidsindikator og plot disse i forhold til jeres forudsagte resultater fra opgave
#3.1 for de to modeller.

## opgaveteksten her er den samme som i 3.2 (formentlig en copy-paste-fejl), så vi svarer på det overskriften siger: RSS og TSS

#RSS = alle modellens fejl (residualer) i anden og lagt sammen
#TSS = alle udsving i væksten omkring dens eget gennemsnit, i anden og lagt sammen
rss_di <- sum(df_fti_vaekst$res_di^2)
rss_dst <- sum(df_fti_vaekst$res_dst^2)
tss <- sum((df_fti_vaekst$realvaekst - mean(df_fti_vaekst$realvaekst))^2)

#kontrol: samme tal som R selv får?
all.equal(rss_di, sum(resid(lm_di)^2))     #skal give TRUE
all.equal(rss_dst, sum(resid(lm_dst)^2))   #skal give TRUE

df_rss_tss <- data.frame(
  Maal = c("RSS (modellens fejl)", "TSS (udsving i væksten)"),
  DI = round(c(rss_di, tss), 1),
  DST = round(c(rss_dst, tss), 1)
)
df_rss_tss


#### Opgave 3.4 – Forklaringsgraden ####
#Opstil ligningen for forklaringsgraden og brug denne til at beregne forklaringsgraden for jeres
#model i opgave 2.

#R2 = 1 - RSS/TSS: hvor stor en del af udsvingene modellen fjerner
r2_di <- 1 - rss_di / tss
r2_dst <- 1 - rss_dst / tss

#kontrol: samme tal som R selv får?
all.equal(r2_di, summary(lm_di)$r.squared)     #skal give TRUE
all.equal(r2_dst, summary(lm_dst)$r.squared)   #skal give TRUE

df_forklaringsgrad <- data.frame(
  Model = c("DI-FTI", "DST FTI"),
  RSS = round(c(rss_di, rss_dst), 1),
  TSS = round(c(tss, tss), 1),
  R2 = round(c(r2_di, r2_dst), 2)
)
df_forklaringsgrad



### --- Opgave 4 --- ###

### Opgave 4.1 - Illustration af forbrugertillid
# Hent data for forbrugertillidsundersøgelsen fra januar 1996 til i dag og omregn jeres data til
# kvartaler. Lav en grafisk illustration af jeres omregnede data for DST’s forbrugertillidsindikator og
# kommentér på, hvornår de danske forbrugere er mest og mindst optimistiske.

#Hente forbrugertillidsundersøgelsen ned via API

library(dkstat)

# Hente meta data
FORV1 <- dst_meta(table = "FORV1", lang = "da")

# Explore variable
FORV1$variables # Kategorierne/Variablerne vi skal filtrere i
FORV1$values$INDIKATOR # Deres værdier
FORV1$values$Tid # Deres værdier

# Laver query liste
filter_FORV1 <- list( INDIKATOR = "Forbrugertillidsindikatoren",
                      Tid = "*")

# Hente data ned og putte den i dataframe
fTillid_raw <- dst_get_data(table = "FORV1",
                            query = filter_FORV1,
                            meta_data = FORV1,
                            lang = "da")

# Lave subset fra 1996
fTillid <- fTillid_raw[fTillid_raw$TID >= as.Date("1996-01-01"), ]

# Kun tallene (value-kolonnen) skal ind i tidsserien, ikke hele data framen
ftillid_ts <- ts(fTillid$value, start = c(1996, 1), frequency = 12)

# Omregn til kvartaler (gennemsnit af 3 måneder)
ftillid_kvt_ts <- aggregate(ftillid_ts, nfrequency = 4)/3

# Lave til data frame
ftillid_kvt_df <- data.frame(
  Tidsinterval = paste0(floor(time(ftillid_kvt_ts)), "K", cycle(ftillid_kvt_ts)),
  Forbrugertillid = round(as.numeric(ftillid_kvt_ts), 1))

# Plotte det
library(ggplot2)
ggplot(ftillid_kvt_df, aes(x = Tidsinterval, y = Forbrugertillid, group = 1)) +
  geom_line(colour = "hotpink4") +
  scale_x_discrete(breaks = ftillid_kvt_df$Tidsinterval[seq(1, nrow(ftillid_kvt_df), by = 8)]) +
  labs(title = "DST's forbrugertillidsindikator",
       subtitle = "Kvartalsvise gennemsnit, 1996 til i dag",
       x = NULL, y = "Nettotal", caption = "Kilde: Danmarks Statistik & Egne beregninger") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


# Finde den største og mindste forbrugertillid
største_række <- ftillid_kvt_df[which.max(ftillid_kvt_df$Forbrugertillid), ]
største_række

mindste_række <- ftillid_kvt_df[which.min(ftillid_kvt_df$Forbrugertillid), ]
mindste_række

# "Forbedret" chat graf
# Sætte punkter på med størst om mindst tillid ^^^
punkter_ftillid_kvt_df <- data.frame(
  Tidsinterval = c("2006K1", "2022K4"),
  Forbrugertillid = c(12.6, -32.1),
  Tekst = c("Maks: 12.6 (2006K1)", "Min: -32.1 (2022K4)")
)

# Plotte det :))
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
    subtitle = "Kvartalsvise gennemsnit, 1996 til 2026 2. kvartal",
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


### Opgave 4.2 – Gennemsnit af underspørgsmål
# Beregn gennemsnittet for underspørgsmålet ”Set i lyset af den økonomiske situation, mener du, at
# det for øjeblikket er fordelagtigt at anskaffe større forbrugsgoder som fjernsyn, vaskemaskine eller
# lignende, eller er det bedre at vente?” for perioden 1. kvartal 2000 til og med 3. kvartal 2023.
# Vurdér jeres resultat set i forhold til spørgsmålet og svarmulighederne. (Hint: giver resultatet
# analytisk mening?)

filter_forbrugsgoder <- list( INDIKATOR = "Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket",
                              Tid = "*")

forbrugsgoder_raw <- dst_get_data(table = "FORV1",
                                  query = filter_forbrugsgoder,
                                  meta_data = FORV1,
                                  lang = "da")

forbrugsgoder <- forbrugsgoder_raw[-(1:303), ]

forbrugsgoder_gns <- round(mean(forbrugsgoder$value), 2)
forbrugsgoder_gns



### Opgave 4.3 - De 11 grupper af forbrug
# Hent data for de 11 grupper af forbrug blandt husholdningerne. Hvad brugte danskerne flest penge
# på i 2022? Hvilken gruppe af forbruget steg mest fra 2020 til 2023? (hint: I kan ikke lægge
# kvartalerne sammen, når I har kædede værdier)

# Hive NAHC21 ned med API

library(dkstat)
NAHC21 <- dst_meta(table = "NAHC21", lang = "da")

# Explore variable
NAHC21$variables # Kategorierne/Variablerne vi skal filtrere i
NAHC21$values$PRISENHED # Deres værdier
NAHC21$values$FORMAAAL # Deres værdier
NAHC21$values$Tid # Deres værdier

# Laver query liste
filter_NAHC21 <- list( PRISENHED = "2020-priser, kædede værdier",
                       Tid = c("2020", "2023"),
                       FORMAAAL = c("Fødevarer", "Drikkevarer og tobak mv.", "Beklædning og fodtøj",
                                    "Boligbenyttelse", "Elektricitet, gas og andet brændsel", 
                                    "Boligudstyr, husholdningsudstyr og vedligholdelse heraf",
                                    "Medicin, lægeudgifter o.l.",  "Køb af køretøjer", "Anden transport og kommunikation",
                                    "Fritidsudstyr, underholdning og rejser", "Andre varer og tjenester"))

# Hente 2020-2023 data ned og putte den i dataframe
forbrug_2020_2023_NAHC21 <- dst_get_data(table = "NAHC21",
                                         query = filter_NAHC21,
                                         meta_data = NAHC21,
                                         lang = "da")

# Hente 2022 data ned og putte den i dataframe
værdier_2022_NAHC21 <- dst_get_data(table = "NAHC21",
                                    PRISENHED = "2020-priser, kædede værdier",
                                    Tid = "2022",
                                    FORMAAAL = c("Fødevarer", "Drikkevarer og tobak mv.", "Beklædning og fodtøj",
                                                 "Boligbenyttelse", "Elektricitet, gas og andet brændsel", 
                                                 "Boligudstyr, husholdningsudstyr og vedligholdelse heraf",
                                                 "Medicin, lægeudgifter o.l.",  "Køb af køretøjer", "Anden transport og kommunikation",
                                                 "Fritidsudstyr, underholdning og rejser", "Andre varer og tjenester"),
                                    meta_data = NAHC21,
                                    lang = "da")

# Hive rækken med størst forbrug ud
værdier_2022_NAHC21[which.max(værdier_2022_NAHC21$value), ]
# Boligbenyttelse er der brugt mest på med 217.409.000.000 kr.


# Forloop til beregning af vækst fra 2020 til 2023

n <- nrow(forbrug_2020_2023_NAHC21)

# Tom "kurv" til at samle resultaterne i, på forhånd fyldt med NA
procent_resultater <- rep(NA, n)

for (i in 1:(n - 11)) {
  j <- i + 11
  
  værdi_i <- forbrug_2020_2023_NAHC21$value[i]
  værdi_j <- forbrug_2020_2023_NAHC21$value[j]
  
  procent_ændring <- ((værdi_j - værdi_i) / værdi_i) * 100
  
  procent_resultater[j] <- procent_ændring   # gem resultatet på PLADS j i "kurven"
}

forbrug_2020_2023_NAHC21$Procent_ændring <- round((procent_resultater), 2)

# Trække rækken med størst ændring i procent ud
forbrug_2020_2023_NAHC21[which.max(forbrug_2020_2023_NAHC21$Procent_ændring), ]

# Andre vare og tjenester er steget mest med 19.03%



### Opgave 4.4 - De 11 grupper af forbrug
# Lav 22 simple lineære regressioner mellem hver af de 11 grupper i forbruget (y-variable) og
# henholdsvis forbrugertillidsindikatoren fra DST og DI. I skal gemme summary i 22 lister. I skal
# lave jeres regressioner fra 1. kvartal 2000 til og med 2. kvartal 2023.

# Hente samlet forbrugertillidsindekator - DST
fTillid_sub_lm <- ftillid_kvt_df[-(1:16), ]

# Hente samlet forbrugertillidsindekator - DI
library(dkstat)
library(tidyr)
library(ggplot2)


#TRIN 2: X (forbrugertillid, månedlig -> kvartaler)
meta_DI <- dst_meta(table = "FORV1", lang = "da")

#kun de 4 spørgsmål der indgår i DI-FTI: F2, F4, F9, F10
DI_FTI_query <- list(
  INDIKATOR = c(
    "Familiens økonomiske situation i dag, sammenlignet med for et år siden",
    "Danmarks økonomiske situation i dag, sammenlignet med for et år siden",
    "Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket",
    "Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr."
  ),
  Tid = "*"
)

DI_tillid_raw <- dst_get_data(
  table     = "FORV1",
  query     = DI_FTI_query,
  meta_data = meta_DI,
  lang      = "da"
)

DI_tillid_raw  <- DI_tillid_raw[order(DI_tillid_raw$TID), ]
DI_tillid_wide <- pivot_wider(DI_tillid_raw, names_from = INDIKATOR, values_from = value)
DI_tillid_wide <- DI_tillid_wide[as.numeric(format(DI_tillid_wide$TID, "%Y")) >= 2000, ]

#månedlig ts og aggregate til kvartaler (3 måneder / 3 = gennemsnit)
DI_tillid_ts <- ts(as.matrix(DI_tillid_wide[, -1]), start = c(2000, 1), frequency = 12)
DI_tillid_q  <- aggregate(DI_tillid_ts, nfrequency = 4) / 3

end(DI_tillid_q)    #seneste hele kvartal med forbrugertillid
ncol(DI_tillid_q)   #skal være 4


#TRIN 3: byg DI-FTI (gennemsnit af de 4 spørgsmål) og saml X og Y
DI_tillid_df <- data.frame(
  kvartal = round(as.numeric(time(DI_tillid_q)), 2),   #2000.00, 2000.25 osv.
  di_fti  = rowMeans(DI_tillid_q)
)


# Subsette de 11 forbrugsgrupper

sub_CPA <- dst_get_data(table = "NAHC21",
                        PRISENHED = "2020-priser, kædede værdier",
                        Tid = "*",
                        FORMAAAL = "Fødevarer",
                        meta_data = NAHC21,
                        lang = "da")
sub_CPA <- sub_CPA[-(1:34), ]
sub_CPA <- data.frame(sub_CPA[ , -(1:3)])
colnames(sub_CPA) <- "FT_CPA"

sub_CPB <- dst_get_data(table = "NAHC21",
                        PRISENHED = "2020-priser, kædede værdier",
                        Tid = "*",
                        FORMAAAL = "Drikkevarer og tobak mv.",
                        meta_data = NAHC21,
                        lang = "da")
sub_CPB <- sub_CPB[-(1:34), ]
sub_CPB <- data.frame(sub_CPB[ , -(1:3)])
colnames(sub_CPB) <- "FT_CPB"

sub_CPC <- dst_get_data(table = "NAHC21",
                        PRISENHED = "2020-priser, kædede værdier",
                        Tid = "*",
                        FORMAAAL = "Beklædning og fodtøj",
                        meta_data = NAHC21,
                        lang = "da")
sub_CPC <- sub_CPC[-(1:34), ]
sub_CPC <- data.frame(sub_CPC[ , -(1:3)])
colnames(sub_CPC) <- "FT_CPC"

sub_CPD <- dst_get_data(table = "NAHC21",
                        PRISENHED = "2020-priser, kædede værdier",
                        Tid = "*",
                        FORMAAAL = "Boligbenyttelse",
                        meta_data = NAHC21,
                        lang = "da")
sub_CPD <- sub_CPD[-(1:34), ]
sub_CPD <- data.frame(sub_CPD[ , -(1:3)])
colnames(sub_CPD) <- "FT_CPD"

sub_CPE <- dst_get_data(table = "NAHC21",
                        PRISENHED = "2020-priser, kædede værdier",
                        Tid = "*",
                        FORMAAAL = "Elektricitet, gas og andet brændsel",
                        meta_data = NAHC21,
                        lang = "da")
sub_CPE <- sub_CPE[-(1:34), ]
sub_CPE <- data.frame(sub_CPE[ , -(1:3)])
colnames(sub_CPE) <- "FT_CPE"

sub_CPF <- dst_get_data(table = "NAHC21",
                        PRISENHED = "2020-priser, kædede værdier",
                        Tid = "*",
                        FORMAAAL = "Boligudstyr, husholdningsudstyr og vedligholdelse heraf",
                        meta_data = NAHC21,
                        lang = "da")
sub_CPF <- sub_CPF[-(1:34), ]
sub_CPF <- data.frame(sub_CPF[ , -(1:3)])
colnames(sub_CPF) <- "FT_CPF"

sub_CPG <- dst_get_data(table = "NAHC21",
                        PRISENHED = "2020-priser, kædede værdier",
                        Tid = "*",
                        FORMAAAL = "Medicin, lægeudgifter o.l.",
                        meta_data = NAHC21,
                        lang = "da")
sub_CPG <- sub_CPG[-(1:34), ]
sub_CPG <- data.frame(sub_CPG[ , -(1:3)])
colnames(sub_CPG) <- "FT_CPG"

sub_CPH <- dst_get_data(table = "NAHC21",
                        PRISENHED = "2020-priser, kædede værdier",
                        Tid = "*",
                        FORMAAAL = "Køb af køretøjer",
                        meta_data = NAHC21,
                        lang = "da")
sub_CPH <- sub_CPH[-(1:34), ]
sub_CPH <- data.frame(sub_CPH[ , -(1:3)])
colnames(sub_CPH) <- "FT_CPH"

sub_CPI <- dst_get_data(table = "NAHC21",
                        PRISENHED = "2020-priser, kædede værdier",
                        Tid = "*",
                        FORMAAAL = "Anden transport og kommunikation",
                        meta_data = NAHC21,
                        lang = "da")
sub_CPI <- sub_CPI[-(1:34), ]
sub_CPI <- data.frame(sub_CPI[ , -(1:3)])
colnames(sub_CPI) <- "FT_CPI"

sub_CPJ <- dst_get_data(table = "NAHC21",
                        PRISENHED = "2020-priser, kædede værdier",
                        Tid = "*",
                        FORMAAAL = "Fritidsudstyr, underholdning og rejser",
                        meta_data = NAHC21,
                        lang = "da")
sub_CPJ <- sub_CPJ[-(1:34), ]
sub_CPJ <- data.frame(sub_CPJ[ , -(1:3)])
colnames(sub_CPJ) <- "FT_CPJ"

sub_CPK <- dst_get_data(table = "NAHC21",
                        PRISENHED = "2020-priser, kædede værdier",
                        Tid = "*",
                        FORMAAAL = "Andre varer og tjenester",
                        meta_data = NAHC21,
                        lang = "da")
sub_CPK <- sub_CPK[-(1:34), ]
sub_CPK <- data.frame(sub_CPK[ , -(1:3)])
colnames(sub_CPK) <- "FT_CPK"

# Lave 22 lineære regressioner

# Lave forbrugertillidsindikatoren (DST) til årligt og ikke kvartalvis
ftillid_aar_ts <- aggregate(ftillid_ts, nfrequency = 1)/12 # Bruge prædefineret tidsserie

# Lave til data frame på årsdata
ftillid_aar_df <- data.frame(
  Tidsinterval = floor(time(ftillid_aar_ts)),
  Forbrugertillid = round(as.numeric(ftillid_aar_ts), 1))

ftillid_aar_df_DST <- ftillid_aar_df[-(1:4), ]
ftillid_aar_df_DST <- ftillid_aar_df_DST[-(25:26), ]

# Lave forbrugertillidsindikatoren (DI) til årligt og ikke kvartalvis
# Lave en time series
fTillid_ts_DI <- ts(DI_tillid_df$kvartal, start = c(2000, 1), frequency = 4)

# Omregn til år
fTillid_aar_ts_DI <- aggregate(fTillid_ts_DI, nfrequency = 1)/4

# Lave til data frame
ftillid_aar_df_DI <- data.frame(
  Tidsinterval = paste0(floor(time(fTillid_aar_ts_DI)), "K", cycle(fTillid_aar_ts_DI)),
  Forbrugertillid = round(as.numeric(fTillid_aar_ts_DI), 1))

# Slette de sidste to rækker
ftillid_aar_df_DI <- ftillid_aar_df_DI[-(25:26), ]
ftillid_aar_df_DI <- data.frame(ftillid_aar_df_DI[ , -(1)])

# cbind til samlet datasæt med forbrugsgrupper
samlet_lm_df <- cbind(ftillid_aar_df_DST, ftillid_aar_df_DI, sub_CPA, sub_CPB, sub_CPC,
                      sub_CPD, sub_CPE, sub_CPF, sub_CPG, sub_CPH, sub_CPI,
                      sub_CPJ, sub_CPK)

colnames(samlet_lm_df)[2:3] <- c("Forbrugertillid_DST", "Forbrugertillid_DI")

# Lineære regressioner på DST og forbrugsgrupper
# Gruppe A: (Fødevare)
lm_CPA_DST <- lm(FT_CPA ~ Forbrugertillid_DST, data = samlet_lm_df)
summary(lm_CPA_DST)
sumr_list_lm_CPA_DST <- list(summary(lm_CPA_DST))

# Gruppe B: (Drikkevarer og tobak mv.)
lm_CPB_DST <- lm(FT_CPB ~ Forbrugertillid_DST, data = samlet_lm_df)
summary(lm_CPB_DST)
sumr_list_lm_CPB_DST <- list(summary(lm_CPB_DST))

# Gruppe C: (Beklædning og fodtøj)
lm_CPC_DST <- lm(FT_CPC ~ Forbrugertillid_DST, data = samlet_lm_df)
summary(lm_CPC_DST)
sumr_list_lm_CPC_DST <- list(summary(lm_CPC_DST))

# Gruppe D: (Boligudnyttelse)
lm_CPD_DST <- lm(FT_CPD ~ Forbrugertillid_DST, data = samlet_lm_df)
summary(lm_CPD_DST)
sumr_list_lm_CPD_DST <- list(summary(lm_CPD_DST))

# Gruppe E: (Elektricitet, gas og andet brændsel)
lm_CPE_DST <- lm(FT_CPE ~ Forbrugertillid_DST, data = samlet_lm_df)
summary(lm_CPE_DST)
sumr_list_lm_CPE_DST <- list(summary(lm_CPE_DST))

# Gruppe F: (Boligudstyr, husholdningsudstyr og vedligeholdelse heraf)
lm_CPF_DST <- lm(FT_CPF ~ Forbrugertillid_DST, data = samlet_lm_df)
summary(lm_CPF_DST)
sumr_list_lm_CPF_DST <- list(summary(lm_CPF_DST))

# Gruppe G: (Medicin, lægeudgifter o.l.)
lm_CPG_DST <- lm(FT_CPG ~ Forbrugertillid_DST, data = samlet_lm_df)
summary(lm_CPG_DST)
sumr_list_lm_CPG_DST <- list(summary(lm_CPG_DST))

# Gruppe H: (Køb af køretøjer)
lm_CPH_DST <- lm(FT_CPH ~ Forbrugertillid_DST, data = samlet_lm_df)
summary(lm_CPH_DST)
sumr_list_lm_CPH_DST <- list(summary(lm_CPH_DST))

# Gruppe I: (Anden transport og kommunikation)
lm_CPI_DST <- lm(FT_CPI ~ Forbrugertillid_DST, data = samlet_lm_df)
summary(lm_CPI_DST)
sumr_list_lm_CPI_DST <- list(summary(lm_CPI_DST))

# Gruppe J: (Fritidsudstyr, underholdning og rejser)
lm_CPJ_DST <- lm(FT_CPJ ~ Forbrugertillid_DST, data = samlet_lm_df)
summary(lm_CPJ_DST)
sumr_list_lm_CPJ_DST <- list(summary(lm_CPJ_DST))

# Gruppe K: (Andre vare og tjenester)
lm_CPK_DST <- lm(FT_CPK ~ Forbrugertillid_DST, data = samlet_lm_df)
summary(lm_CPK_DST)
sumr_list_lm_CPK_DST <- list(summary(lm_CPK_DST))

# Lineære regressioner på DI og forbrugsgrupper
# Gruppe A: (Fødevare)
lm_CPA_DI <- lm(FT_CPA ~ Forbrugertillid_DI, data = samlet_lm_df)
summary(lm_CPA_DI)
sumr_list_lm_CPA_DI <- list(summary(lm_CPA_DI))

# Gruppe B: (Drikkevarer og tobak mv.)
lm_CPB_DI <- lm(FT_CPB ~ Forbrugertillid_DI, data = samlet_lm_df)
summary(lm_CPB_DI)
sumr_list_lm_CPB_DI <- list(summary(lm_CPB_DI))

# Gruppe C: (Beklædning og fodtøj)
lm_CPC_DI <- lm(FT_CPC ~ Forbrugertillid_DI, data = samlet_lm_df)
summary(lm_CPC_DI)
sumr_list_lm_CPC_DI <- list(summary(lm_CPC_DI))

# Gruppe D: (Boligudnyttelse)
lm_CPD_DI <- lm(FT_CPD ~ Forbrugertillid_DI, data = samlet_lm_df)
summary(lm_CPD_DI)
sumr_list_lm_CPD_DI <- list(summary(lm_CPD_DI))

# Gruppe E: (Elektricitet, gas og andet brændsel)
lm_CPE_DI <- lm(FT_CPE ~ Forbrugertillid_DI, data = samlet_lm_df)
summary(lm_CPE_DI)
sumr_list_lm_CPE_DI <- list(summary(lm_CPE_DI))

# Gruppe F: (Boligudstyr, husholdningsudstyr og vedligeholdelse heraf)
lm_CPF_DI <- lm(FT_CPF ~ Forbrugertillid_DI, data = samlet_lm_df)
summary(lm_CPF_DI)
sumr_list_lm_CPF_DI <- list(summary(lm_CPF_DI))

# Gruppe G: (Medicin, lægeudgifter o.l.)
lm_CPG_DI <- lm(FT_CPG ~ Forbrugertillid_DI, data = samlet_lm_df)
summary(lm_CPG_DI)
sumr_list_lm_CPG_DI <- list(summary(lm_CPG_DI))

# Gruppe H: (Køb af køretøjer)
lm_CPH_DI <- lm(FT_CPH ~ Forbrugertillid_DI, data = samlet_lm_df)
summary(lm_CPH_DI)
sumr_list_lm_CPH_DI <- list(summary(lm_CPH_DI))

# Gruppe I: (Anden transport og kommunikation)
lm_CPI_DI <- lm(FT_CPI ~ Forbrugertillid_DI, data = samlet_lm_df)
summary(lm_CPI_DI)
sumr_list_lm_CPI_DI <- list(summary(lm_CPI_DI))

# Gruppe J: (Fritidsudstyr, underholdning og rejser)
lm_CPJ_DI <- lm(FT_CPJ ~ Forbrugertillid_DI, data = samlet_lm_df)
summary(lm_CPJ_DI)
sumr_list_lm_CPJ_DI <- list(summary(lm_CPJ_DI))

# Gruppe K: (Andre vare og tjenester)
lm_CPK_DI <- lm(FT_CPK ~ Forbrugertillid_DI, data = samlet_lm_df)
summary(lm_CPK_DI)
sumr_list_lm_CPK_DI <- list(summary(lm_CPK_DI))

# Plotte linære regressioner på 

ggplot(samlet_lm_df, aes(y = FT_CPD, x = Forbrugertillid_DST)) +
  geom_point(color = "pink3") +
  geom_smooth(method = "lm", color = "hotpink3")+
  theme_minimal() +
  labs(x = "DST Forbrugertillidsindekator",
       y = " Forbrugsgruppen: Boligudbyttelse",
       title = " Linære regression for Boligydnyttelse & DST Forbrugertillidsindekator",
       caption = "Kilde: Danmarks Statistik & Egne beregninger")

ggplot(samlet_lm_df, aes(y = FT_CPD, x = Forbrugertillid_DI)) +
  geom_point(color = "pink3") +
  geom_smooth(method = "lm", color = "hotpink3")+
  theme_minimal() +
  labs(x = "DST Forbrugertillidsindekator",
       y = " Forbrugsgruppen: Boligudbyttelse",
       title = " Linære regression for Boligydnyttelse & DI Forbrugertillidsindekator",
       caption = "Kilde: Danmarks Statistik & Egne beregninger")


### --- Opgave 5 --- ###

### Opgave 5.1 – Kvartalsvis årlig realvækst for en række Eurolande
# Beregn den kvartalsvise årlige realvækst for husholdningernes forbrugsudgift 
# for Danmark,Belgien, Holland, Sverige, Østrig, Tyskland, Frankrig, Italien og 
# Spanien i perioden 1. kvartal 2000 til og med 2. kvartal 2023. I skal hente 
# data vha. API’et fra Eurostat.

# Vi har valgt at tage med til de seneste data i 2026Q2 :))

library(eurostat)
install.packages("restatapi")
library(restatapi)
library(stringr)
library(dplyr)

# total content
alltabs <- get_eurostat_toc()

#filtrer efter emne
realvæstTabs= alltabs |> filter(str_detect(title, "House|house")) |>
  filter(str_detect(title, "house"))

# henter meta data, efter at have kigget på gastabs og fundet koden
# Vi er gået med Final consumption expenditure of households and non-profit 
# institutions serving households - quarterly data

# Note, undersøg hvis muligt hvad denne indebærer

husForbrugMeta =get_eurostat_dsd("namq_10_fcs")

# Undersøge datasæt med henblik på at finde relevante punkter til filtrering
unique(husForbrugMeta$concept)

# Undersøge værdierne i hvert "filter"
husForbrugMeta |> filter(concept=="freq") # 1 værdi, kvartalvist = Q
husForbrugMeta |> filter(concept=="unit") # 19 værdier, priser - vi vælger 2020 kædet værdier = CLV20_MEUR
husForbrugMeta |> filter(concept=="s_adj") # 4 værdier, sæsonkorrigering - vi vælger sæson og kalender korrigeret = SCA 
husForbrugMeta |> filter(concept=="na_item") # 6 værdier, forbrugsgrupper - vi vælger samlet forbrug = P31_S14
husForbrugMeta |> filter(concept=="geo") # 41 værdier, lande koder = DK, BE, NL, SE, AT, DE, FR, IT, ES

# Lave en query med vores filter
realVæskt_query <- list(unit = "CLV20_MEUR", # Vælger kædet værdier
                        geo = c("DK", "BE", "NL", "SE", "AT", "DE", "FR", "IT", "ES"), # Landekoderne
                        s_adj = "SCA", # Vælger at tage dem sæsonkorregerede
                        na_item = "P31_S14",
                        freq = "Q") # Kvartalmæssige værdier

# Hente færdigt datasæt
realVækst_euroStat <- get_eurostat_data("namq_10_fcs", # Det rå datasæt
                                        filters = realVæskt_query, # Vores filter liste
                                        date_filter = ">1999", # For at kunne beregne realvæksten for 2000 også
                                        verbose = T) # Få statusbeskeder på importeringsprocessen

# Beregne realvækst for alle landene

# Hvordan gør vi?
# Subsetter vi så vi laver et dataframe til hverland og beregner realvækst på dem?
# Så ligner det fremgangsmetoden som på den danske
# Kan cbindes til sidst i et samlet data når vi har beregnet realvæksten :))
# Andet?

# Subsette alle landende for at berenge realvækst enkelvist
sub_AT <- realVækst_euroStat[grepl("^AT", realVækst_euroStat$geo), ] # Østrig

sub_BE <- realVækst_euroStat[grepl("^BE", realVækst_euroStat$geo), ] # Belgien

sub_DE <- realVækst_euroStat[grepl("^DE", realVækst_euroStat$geo), ] # Tyskland

sub_DK <- realVækst_euroStat[grepl("^DK", realVækst_euroStat$geo), ] # Danmark :))

sub_ES <- realVækst_euroStat[grepl("^ES", realVækst_euroStat$geo), ] # Spanien

sub_FR <- realVækst_euroStat[grepl("^FR", realVækst_euroStat$geo), ] # Frankrig

sub_IT <- realVækst_euroStat[grepl("^IT", realVækst_euroStat$geo), ] # Italien

sub_NL <- realVækst_euroStat[grepl("^NL", realVækst_euroStat$geo), ] # Holland

sub_SE <- realVækst_euroStat[grepl("^SE", realVækst_euroStat$geo), ] # Sverige

# Lave REALVÆKST
sub_AT_RVp <- (diff(log(as.numeric(sub_AT$values)), lag = 4)) * 100
sub_AT_RVp <- (exp(diff(log(as.numeric(sub_AT$values)), lag = 4)) - 1) * 100
sub_AT_RVp <- round(data.frame(sub_AT_RVp), 3)
names(sub_AT_RVp) <- "Østrig"

sub_BE_RVp <- (diff(log(as.numeric(sub_BE$values)), lag = 4)) * 100
sub_BE_RVp <- (exp(diff(log(as.numeric(sub_BE$values)), lag = 4)) - 1) * 100
sub_BE_RVp <- round(data.frame(sub_BE_RVp), 3)
names(sub_BE_RVp) <- "Belgien"

sub_DE_RVp <- (diff(log(as.numeric(sub_DE$values)), lag = 4)) * 100
sub_DE_RVp <- (exp(diff(log(as.numeric(sub_DE$values)), lag = 4)) - 1) * 100
sub_DE_RVp <- round(data.frame(sub_DE_RVp), 3)
names(sub_DE_RVp) <- "Tyskland"

sub_DK_RVp <- (diff(log(as.numeric(sub_DK$values)), lag = 4)) * 100
sub_DK_RVp <- (exp(diff(log(as.numeric(sub_DK$values)), lag = 4)) - 1) * 100
sub_DK_RVp <- round(data.frame(sub_DK_RVp), 3)
names(sub_DK_RVp) <- "Danmark"

sub_ES_RVp <- (diff(log(as.numeric(sub_ES$values)), lag = 4)) * 100
sub_ES_RVp <- (exp(diff(log(as.numeric(sub_ES$values)), lag = 4)) - 1) * 100
sub_ES_RVp <- round(data.frame(sub_ES_RVp), 3)
names(sub_ES_RVp) <- "Spanien"

sub_FR_RVp <- (diff(log(as.numeric(sub_FR$values)), lag = 4)) * 100
sub_FR_RVp <- (exp(diff(log(as.numeric(sub_FR$values)), lag = 4)) - 1) * 100
sub_FR_RVp <- round(data.frame(sub_FR_RVp), 3)
names(sub_FR_RVp) <- "Frankrig"

sub_IT_RVp <- (diff(log(as.numeric(sub_IT$values)), lag = 4)) * 100
sub_IT_RVp <- (exp(diff(log(as.numeric(sub_IT$values)), lag = 4)) - 1) * 100
sub_IT_RVp <- round(data.frame(sub_IT_RVp), 3)
names(sub_IT_RVp) <- "Italien"

sub_NL_RVp <- (diff(log(as.numeric(sub_NL$values)), lag = 4)) * 100
sub_NL_RVp <- (exp(diff(log(as.numeric(sub_NL$values)), lag = 4)) - 1) * 100
sub_NL_RVp <- round(data.frame(sub_NL_RVp), 3)
names(sub_NL_RVp) <- "Holland"

sub_SE_RVp <- (diff(log(as.numeric(sub_SE$values)), lag = 4)) * 100
sub_SE_RVp <- (exp(diff(log(as.numeric(sub_SE$values)), lag = 4)) - 1) * 100
sub_SE_RVp <- round(data.frame(sub_SE_RVp), 3)
names(sub_SE_RVp) <- "Sverige"

# Tidsramme
sub_tid <- data.frame(sub_AT$time)
sub_tid <- sub_tid[-(1:4), ]
sub_tid <- data.frame(sub_tid)
names(sub_tid) <- "Tidsperioder"

# Merge
samlet_RV_df_EU_stat <- cbind(sub_tid, sub_SE_RVp, sub_NL_RVp, sub_IT_RVp,
                              sub_FR_RVp, sub_ES_RVp, sub_DK_RVp, sub_DE_RVp,
                              sub_BE_RVp, sub_AT_RVp)




### Opgave 5.2 – Højeste kvartalsvise årlige realvækst
# Hvilket af de landene har gennemsnitligt haft den højeste kvartalsvise årlige realvækst i
# husholdningernes forbrugsudgift i perioden 1. kvartal 2000 til 2. kvartal 2023.

gns_SE <- round(mean(samlet_RV_df_EU_stat$Sverige), 3)
gns_NL <- round(mean(samlet_RV_df_EU_stat$Holland), 3)
gns_IT <- round(mean(samlet_RV_df_EU_stat$Italien), 3)
gns_FR <- round(mean(samlet_RV_df_EU_stat$Frankrig), 3)
gns_ES <- round(mean(samlet_RV_df_EU_stat$Spanien), 3)
gns_DK <- round(mean(samlet_RV_df_EU_stat$Danmark), 3)
gns_DE <- round(mean(samlet_RV_df_EU_stat$Tyskland), 3)
gns_BE <- round(mean(samlet_RV_df_EU_stat$Belgien), 3)
gns_AT <- round(mean(samlet_RV_df_EU_stat$Østrig), 3)

# Samle dem i dataframe?
gns_df <- data.frame(gns_SE, gns_NL, gns_IT, gns_FR, gns_ES, gns_DK,
                     gns_DE, gns_BE, gns_AT)

# Sætte dem i størrelsesmæssig orden på en smart måde?

library(tidyr)
library(ggplot2)

gnst_long <- pivot_longer(
  gns_df,
  cols = everything(),
  names_to = "Land",
  values_to = "Gennemsnit"
)

gnst_long$Land <- gsub("gns_", "", gnst_long$Land)

landenavne <- c(
  SE = "Sverige", NL = "Holland", IT = "Italien", FR = "Frankrig",
  ES = "Spanien", DK = "Danmark", DE = "Tyskland", BE = "Belgien", AT = "Østrig"
)

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
  theme(
    axis.text  = element_text(size = 14),
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 12),
    plot.title = element_text(size = 20))


### Opgave 5.3 – Coronakrisen som outlier
# Fjerne Coronakrisen fra jeres data og find igen den gennemsnitligt kvartalsvise realvækst i
# husholdningernes forbrugsudgift i perioden 1. kvartal 2000 til 2. kvartal 2023. I hvilket af landene
# har Coronakrisen haft en største effekt på den gennemsnitligt kvartalsvise realvækst.

# Hvor længe varede coronakrisen???

samlet_RV_df_EU_stat_corona <- samlet_RV_df_EU_stat[-(82:95), ]

# Lave gennemsnit på corona
gns_SE_corona <- round(mean(samlet_RV_df_EU_stat_corona$Sverige), 3)
gns_NL_corona <- round(mean(samlet_RV_df_EU_stat_corona$Holland), 3)
gns_IT_corona <- round(mean(samlet_RV_df_EU_stat_corona$Italien), 3)
gns_FR_corona <- round(mean(samlet_RV_df_EU_stat_corona$Frankrig), 3)
gns_ES_corona <- round(mean(samlet_RV_df_EU_stat_corona$Spanien), 3)
gns_DK_corona <- round(mean(samlet_RV_df_EU_stat_corona$Danmark), 3)
gns_DE_corona <- round(mean(samlet_RV_df_EU_stat_corona$Tyskland), 3)
gns_BE_corona <- round(mean(samlet_RV_df_EU_stat_corona$Belgien), 3)
gns_AT_corona <- round(mean(samlet_RV_df_EU_stat_corona$Østrig), 3)

# Samle dem i dataframe?
gns_df_corona <- data.frame(gns_SE_corona, gns_NL_corona, gns_IT_corona, 
                            gns_FR_corona, gns_ES_corona, gns_DK_corona, 
                            gns_DE_corona, gns_BE_corona, gns_AT_corona)

# Gør dem mere overskuelige???
gnst_long_corona <- pivot_longer(
  gns_df_corona,
  cols = everything(),
  names_to = "Land",
  values_to = "Gennemsnit"
)


# Ændre så værdierne i land er landekoder
gnst_long_corona$Land <- gsub("gns_", "", gnst_long$Land)

landenavne <- c(
  SE = "Sverige", NL = "Holland", IT = "Italien", FR = "Frankrig",
  ES = "Spanien", DK = "Danmark", DE = "Tyskland", BE = "Belgien", AT = "Østrig"
)

gnst_long_corona$Land_navn <- landenavne[gnst_long$Land]

ggplot(gnst_long_corona, aes(x = reorder(Land_navn, -Gennemsnit), y = Gennemsnit, fill = Gennemsnit)) +
  geom_col() +
  scale_fill_gradient(low = "pink", high = "hotpink3") +
  geom_text(aes(label = paste0(round(Gennemsnit, 2), "%")), vjust = -0.5) +
  guides(fill = "none") +
  labs(x = NULL, 
       y = "Gennemsnit", 
       title = "Belgien, Spanien & Østrig har haft den største gennemsnitlige 
realvækst i årrækken 2000-2026 modregnet Coronakrisen 
(2020Q2 - 2023Q3)",
       subtitle = "Den gennemsnitlige realvækst er beregnet ud fra husholdningernes 
privatforbrug, samt NGO´er der betjener husholdningerne",
       caption = "Kilde: EuroStat") +
  theme_classic()

# Hvilken realvækst har haft den største ændring?
corona_gns <- data.frame(gnst_long_corona$Gennemsnit)
names(corona_gns) <- "Corona"

samlet_gns <- data.frame(gnst_long$Gennemsnit)
names(samlet_gns) <- "Samlet"

lande_gns <- data.frame(gnst_long$Land)
names(lande_gns) <- "Lande"

ændring_i_rv <- cbind(lande_gns, samlet_gns, corona_gns)
View(ændring_i_rv)

# Lave et for loop der viser ændring
ændring_i_rv$Ændring_p <- round((ændring_i_rv$Corona - ændring_i_rv$Samlet), 3)
ændring_i_rv$Ændring <- round(((ændring_i_rv$Corona - ændring_i_rv$Samlet)/ændring_i_rv$Samlet)*100, 3)

ggplot(ændring_i_rv, aes(x = reorder(Lande, -Ændring), y = Ændring, fill = Ændring)) +
  geom_col() +
  scale_fill_gradient(low = "pink", high = "hotpink3") +
  geom_text(aes(label = paste0(round(Ændring, 2), "%")), vjust = -0.5) +
  guides(fill = "none") +
  labs(x = NULL, 
       y = "Ændring", 
       title = "Coronakrisen har haft størst effekt på Belgien & Hollands 
gennemsnitlige realvækst",
       subtitle = "Grafen viser den største forskel i procent, for den gennemsnitlige 
samlet realvækst modregnet coronaperioden i de forskellige lande i 
årrækken 2000-2026",
       caption = "Kilde: EuroStat") +
  theme_classic()

# Put a pin in it



### Opgave 5.4 – Effekt af Corona på forbruget
# I hvilket europæiske land faldt den gennemsnitligt kvartalsvise realvækst i 
# husholdningernes forbrugsudgift, i perioden 1. kvartal 2020 til 2. kvartal 2023, mest?

# Vi har data explored og perioden på 3,5 år er for lang :))
# Opsvinget der kom i 2022 og 2023 udligner crash i 2020 og 2021

# Korrigeret vækst
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

# Samle i dataframe
sub_2015_k_df <- data.frame(sub_2015_gns_SE, sub_2015_gns_NL, sub_2015_gns_IT,
                            sub_2015_gns_FR, sub_2015_gns_ES, sub_2015_gns_DK,
                            sub_2015_gns_DE, sub_2015_gns_BE, sub_2015_gns_AT)

# Gør dem mere overskuelige?
sub_2015_k_df_long <- pivot_longer(
  sub_2015_k_df,
  cols = everything(),
  names_to = "Land",
  values_to = "Gennemsnit korrigeret"
)

# Corona vækst
corona_vækst_gns_SE <- round(mean(samlet_RV_df_EU_stat[(81:94), 2]), 3) 
corona_vækst_gns_NL <- round(mean(samlet_RV_df_EU_stat[(81:94), 3]), 3)
corona_vækst_gns_IT <- round(mean(samlet_RV_df_EU_stat[(81:94), 4]), 3)
corona_vækst_gns_FR <- round(mean(samlet_RV_df_EU_stat[(81:94), 5]), 3)
corona_vækst_gns_ES <- round(mean(samlet_RV_df_EU_stat[(81:94), 6]), 3)
corona_vækst_gns_DK <- round(mean(samlet_RV_df_EU_stat[(81:94), 7]), 3)
corona_vækst_gns_DE <- round(mean(samlet_RV_df_EU_stat[(81:94), 8]), 3)
corona_vækst_gns_BE <- round(mean(samlet_RV_df_EU_stat[(81:94), 9]), 3)
corona_vækst_gns_AT <- round(mean(samlet_RV_df_EU_stat[(81:94), 10]), 3)

#Samle i dataframe
sub_2015_corona_df <- data.frame(corona_vækst_gns_SE, corona_vækst_gns_NL, corona_vækst_gns_IT,
                                 corona_vækst_gns_FR, corona_vækst_gns_ES, corona_vækst_gns_DK,
                                 corona_vækst_gns_DE, corona_vækst_gns_BE, corona_vækst_gns_AT)

# Gør dem mere overskuelige?
sub_2015_corona_df_long <- pivot_longer(
  sub_2015_corona_df,
  cols = everything(),
  names_to = "Land",
  values_to = "Gennemsnit corona"
)

sub_2015_corona_df_long <- data_frame(sub_2015_corona_df_long[ , -1])

# Samlet df for 5.4
samlet_df_opg_5.4 <- cbind(sub_2015_k_df_long, sub_2015_corona_df_long)

# Beregne forskellen
samlet_df_opg_5.4$Forskel <- round((((samlet_df_opg_5.4$`Gennemsnit corona`
                                      - samlet_df_opg_5.4$`Gennemsnit korrigeret`)/
                                       samlet_df_opg_5.4$`Gennemsnit korrigeret`)*100), 3)

# Visualisering af faldet
sub_2015_plot_data <- samlet_df_opg_5.4 %>%
  mutate(
    Land_kort = gsub("sub_2015_gns_", "", Land)
  )

# Lav plottet
ggplot(sub_2015_plot_data, aes(x = reorder(Land_kort, -Forskel), y = Forskel, fill = Forskel)) +
  geom_col() +
  scale_fill_gradient(low = "pink", high = "hotpink3") +
  geom_text(
    aes(
      label = paste0(round(Forskel, 2), "%"),
      vjust = ifelse(Forskel >= 0, -0.5, 1.2)
    ),
    size = 3.5
  ) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(
    title = "Forskellen i gennemsnitlig realvækst",
    subtitle = "Gennemsnit korrigeret vs. Gennemsnit corona",
    x = NULL,
    y = "Forskel i procentpoint",
    caption = "Kilde: EuroStat"
  )


# Visualisering af coronakrisens påvirkning
library(ggplot2)
library(tidyr)
library(scales)

# Omstrukturer data så det er plot´able
plot_5.4_df <- pivot_longer(
  data = samlet_RV_df_EU_stat,
  cols = -Tidsperioder, 
  names_to = "Land", 
  values_to = "Realvaekst"
)

# Lave et ggplot
ggplot(plot_5.4_df, aes(x = Tidsperioder, y = Realvaekst, color = Land, group = Land)) +
  geom_line() +
  scale_x_discrete(
    breaks = function(x) x[grepl("-Q1$", x)],
    labels = function(x) sub("-Q1$", "", x)
  ) +
  scale_y_continuous(labels = unit_format(unit = "%")) +
  scale_color_brewer(palette = "PuRd") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  labs(x = "Tidsperiode",
       y = "Realvækst i procent",
       title = "Udvikling i Realvækst i procent",
       subtitle = "Visualisering af coronakrisens påvirkning på realvæksen i perioden 2020-2023 for de valgte EU lande",
       caption = "Kilde: EuroStat & Egne beregninger")

# Den danske gennemsnitlige realvækst under coronaperioden faldt mest sammenlignet 
# med et korrigeret gennemsnit der er renset for coronaårene.
# Den danske realvækst var i gennemsnit 76,7% lavere i perioden.