install.packages("devtools")
devtools::install_github("rOpenGov/dkstat")

library(dkstat)

alltable=dkstat::dst_get_tables()

# hent via API
# hente metadata mhp filtrering
uheld_meta <- dst_meta(table = "UHELDK1", lang = "da")

# konstruer liste med filtre
uheld_meta_filters <- list(
  OMRÅDE = "*",
  INDBLAND = c("Almindelig personbil","Lastbil over 3.500 kg.","Cykel","Fodgænger"),
  ALDER = "*",
  KØN = c("Mænd","Kvinder"),
  Tid = "*"
) 

# hente data via filtre 
uheldsdata <- dst_get_data(table = "UHELDK1", query = uheld_meta_filters, lang = "da")

### --- Forbrugertillidsindekator exploreation --- ###

# Start med at hente data
hentFTdata <- dst_meta("FORV1")

# Filterer
hentFTdata$variables
hentFTdata$values$INDIKATOR
hentFTdata$values$INDIKATOR$text
hentFTdata$values$Tid
hentFTdata$values$Tid$text


### --- Wulf --- ###
library(dkstat)

alltable=dkstat::dst_get_tables()

dkstat::
  regmeta <- dst_meta("INDKFPP3")
regmeta$variables
regmeta$values$
regmeta$values$ENHED
regmeta$values$KOEN
regmeta$values$INDKINTB
regmeta$values$Tid

myquery <- list(
  REGION = "*",
  ENHED = "Personer i gruppen (antal)",
  KOEN = "Mænd og kvinder i alt",
  INDKINTB = c("Under 100.000 kr.","200.000 - 299.999 kr.","1.000.000 - 1.999.999 kr."),
  Tid = "2024"
)
dfIndk <- dst_get_data("INDKFPP3", query = myquery)










