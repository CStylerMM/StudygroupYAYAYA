### --- OLA 1 --- ###

install.packages("readr")
library(readr)

#importeret datasæt fra excel, givet den et andet navn
dfBS <- boligsiden_OLA

#Sortere data, slette første row der kun viser NA
dfBoligSiden <- dfBS [-1,]



### --- Opgave 1.1 Find Data --- ###

# Finde rækken
dfBoligSiden[dfBoligSiden$vej == "tousvej",]
dfBoligSiden[dfBoligSiden$vej == "egevej" & dfBoligSiden$vejnr == 20,]


#Finde række nr.
which(dfBoligSiden$vej == "tousvej")
which(dfBoligSiden$vej == "egevej" & dfBoligSiden$vejnr == 20)



### --- Opgave 1.2 Find Data --- ###

#Sample 2 rækker
dfBoligSiden[sample(nrow(dfBoligSiden), size = 2), ]














