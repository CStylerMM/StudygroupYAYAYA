#OLA2
library(dkstat)
library(tidyr)
dst_get_tables()
#Opgave 1.1 – Det første skridt
#Skriv en kode, der viser hvordan du finder en tabel som kan give en liste over byer med indbyggertal vha
#DST-pakken dkstat.

library(dkstat)
by3_meta <- dst_meta(table = "BY3", lang = "da")

by3_meta$variables

#Indlæs data 

byer_data <- dst_get_data(
  table = "BY3",
  BYER = "*",
  Tid  = "2026",
  FOLKARTAET = "Folketal",
  meta_data = by3_meta,
  lang = "da"
)

#Sum data
sum(byer_data_ren$value)

#Fjern 
byer_data_test <- byer_data_ren [!grepl("Uden fast bopæl", byer_data_ren$byer),]
byer_data_ren <- byer_data[byer_data$value != 0, ]

byer_data_ren <- byer_data[byer_data$value != 0, ]
byer_data_ren

#Trin 1: Fjern 0-værdier FØRST
byer_data_ren <- byer_data[byer_data$value != 0, ]

#Trin 2: Fjern "Uden fast bopæl" fra den RENSEDE data
byer_data_ren <- byer_data_ren[!grepl("Uden fast bopæl", byer_data_ren$BYER), ]

#Trin 3: Fjern "Landdistriker" fra den Rensede data
byer_data_ren <- byer_data_ren[!grepl("Landdistrikter", byer_data_ren$BYER), ]

#Tjek resultatet
nrow(byer_data_ren)
sum(byer_data_ren$value, na.rm = TRUE)



#Opgave 1.2 – Kategori-variabel.
#Lav en kategorivariabel i R hvor du skal inddele byerne i følgende kategorier: "landsby","lille by"
#, "almindelig
#by", "større by", "storby" ud fra et interval på indbyggertal som du selv definerer.

byer_data_ren$bykategori <- ifelse(
  byer_data_ren$value < 200, "landsby",
  ifelse(
    byer_data_ren$value < 1000, "lille by",
    ifelse(
      byer_data_ren$value < 5000, "almindelig by",
      ifelse(
        byer_data_ren$value < 20000, "større by", 
        "storby"
      )
    )
  )
)
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
    retval = "NA"
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
    retval = "NA"
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
dfBoligSiden_ingenNA$liggetid <- gsub("[a-zA-z]", " ", dfBoligSiden_ingenNA$liggetid)
dfBoligSiden_ingenNA$liggetid <- as.numeric(dfBoligSiden_ingenNA$liggetid)

#etabler ny kolonne med alder
dfBoligSiden_ingenNA$alder <- sapply(dfBoligSiden_ingenNA$opført, alderBr)

