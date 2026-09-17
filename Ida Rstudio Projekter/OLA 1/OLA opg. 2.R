
### --- Opgave 2 --- ###

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

#character værdier med tal konverteres til numeric i stedet
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



### --- Opgave 2.1 --- ###

#opsummere rækkernes indholder i datasættet
summary(dfBoligSiden_ingenNA)

# størrelsen på datasættet samt hvillen værdi rækkerne indeholder (chr, num osv.)
str(dfBoligSiden_ingenNA)

#noget fra timen, dont really know
testRow <- dfBoligSiden_ingenNA[23,]
View(testRow)

#Giver en bedre opsumering på datasættet, som i meget bedre - lettere at forstå
library(skimr)
skim(dfBoligSiden_ingenNA)

#Lave sæt af outliers
summary(dfBoligSiden_ingenNA$pris) #Outlier på alt over 15 mil.
OLsubSetPris <- dfBoligSiden_ingenNA[(dfBoligSiden_ingenNA$pris > 15000000),]

summary(dfBoligSiden_ingenNA$alder) #Outlier på alt over 150 år gammelt
OLsubSetAlder <- dfBoligSiden_ingenNA[(dfBoligSiden_ingenNA$alder > 200),]

summary(dfBoligSiden_ingenNA$grund) #Outlier på alt over 1200 kvm grund
OLsubSetGrund <- dfBoligSiden_ingenNA[(dfBoligSiden_ingenNA$grund > 2000),]

summary(dfBoligSiden_ingenNA$kvmpris) #Outlier på kvmpris på over 70.000
OLsubSetKvmpris <- dfBoligSiden_ingenNA[(dfBoligSiden_ingenNA$kvmpris > 70000),]

summary(dfBoligSiden_ingenNA$størrelse) #Outlier på boligareal på over 350
OLsubSetStørrelse <- dfBoligSiden_ingenNA[(dfBoligSiden_ingenNA$størrelse > 350),]

summary(dfBoligSiden_ingenNA$mdudg) #Outlier på ejerafgift på over 10.000
OLsubSetMdudg <- dfBoligSiden_ingenNA[(dfBoligSiden_ingenNA$mdudg > 10000),]

summary(dfBoligSiden_ingenNA$værelser) #Outlier på boliger med mere end 11 værelser
OLsubSetVærelser <- dfBoligSiden_ingenNA[(dfBoligSiden_ingenNA$værelser > 11),]

#Lave subset til datasæt uden outliers

dfBoligSiden_ingenNA_uOL <- dfBoligSiden_ingenNA # Hvis den skal "resettes"

dfBoligSiden_ingenNA_uOL <- dfBoligSiden_ingenNA_uOL[(dfBoligSiden_ingenNA_uOL$pris <= 10000000),]
dfBoligSiden_ingenNA_uOL <- dfBoligSiden_ingenNA_uOL[(dfBoligSiden_ingenNA_uOL$alder <= 150),]
dfBoligSiden_ingenNA_uOL <- dfBoligSiden_ingenNA_uOL[(dfBoligSiden_ingenNA_uOL$grund <= 2000),]
dfBoligSiden_ingenNA_uOL <- dfBoligSiden_ingenNA_uOL[(dfBoligSiden_ingenNA_uOL$kvmpris <= 70000),]
dfBoligSiden_ingenNA_uOL <- dfBoligSiden_ingenNA_uOL[(dfBoligSiden_ingenNA_uOL$størrelse <= 350),]
dfBoligSiden_ingenNA_uOL <- dfBoligSiden_ingenNA_uOL[(dfBoligSiden_ingenNA_uOL$mdudg <= 10000),]
dfBoligSiden_ingenNA_uOL <- dfBoligSiden_ingenNA_uOL[(dfBoligSiden_ingenNA_uOL$værelser <= 11),]

#Lave linære regressioner (til beskrivende statistik)
library(ggplot2) 

#Sammenhæng mellem pris og alder
ggplot(dfBoligSiden_ingenNA_uOL, aes(alder, pris)) + 
  geom_point(alpha = 0.25, color = "lightpink") +
  geom_smooth(method = "lm", color = "deeppink") +
  theme_minimal() +
  labs(title = "Sammenhæng mellem Pris & Alder",
       x = "Boligens Alder",
       y = "Boligens Pris")

#Sammenhæng mellem grund og pris
ggplot(dfBoligSiden_ingenNA_uOL, aes(grund, pris)) + 
  geom_point(alpha = 0.25, color = "lightpink") + 
  geom_smooth(method = "lm", color = "deeppink") + 
  theme_minimal() + 
  labs(title = "Sammenhæng mellem Grund & Pris",
       x = "Grundens Størrelse",
       y = "Pris på ejendom")

#Sammenhæng mellem ejerudgift og pris
ggplot(dfBoligSiden_ingenNA_uOL, aes(mdudg, pris)) +
  geom_point(alpha = 0.25, color = "lightpink") + 
  geom_smooth(method = "lm", color = "deeppink") + 
  theme_minimal() +
  labs(title = "Sammenhæng mellem Pris og Ejerudgift",
       x = "Ejerudgift",
       y = "Boligens Pris")

#Sammenhæng mellem postnummer og ejerafgift
ggplot(dfBoligSiden_ingenNA_uOL, aes(postnr, mdudg)) +
  geom_point(alpha = 0.25, color = "lightpink") + 
  geom_smooth(method = "lm", color = "deeppink") + 
  theme_minimal() +
  labs(title = "Sammenhæng mellem Postnummer og Ejerudgift",
       x = "Postnummer",
       y = "Ejerudgift")

#Sammenhæng mellem postnummer og pris
ggplot(dfBoligSiden_ingenNA_uOL, aes(postnr, pris)) +
  geom_point(alpha = 0.25, color = "bisque2") + 
  geom_smooth(method = "lm", color = "tan4") + 
  theme_minimal() +
  labs(title = "Sammenhæng mellem Postnummer og Pris",
       x = "Postnummer",
       y = "Pris")

#Oversigt over gennemsnitlig kvm pris fordelt på regioner
ggplot(dfBoligSiden_ingenNA_uOL, aes(region, kvmpris)) +
  stat_summary(fun = mean, geom = "bar", fill = "lightpink") + 
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(title = "Gns. kvm. pris fordelt på Regioner",
       x = "Region",
       y = "Kvm. Pris")

#Samme^^^, men bruger reorder til visuelt at sorterer efter størrelse (med datasæt ikke renset for outliers)
ggplot(dfBoligSiden_ingenNA, aes(reorder(region, kvmpris, FUN = mean), kvmpris)) +
  stat_summary(fun = mean, geom = "bar", fill = "lightpink") + 
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(title = "Gns. kvm. pris fordelt på Regioner",
       x = "Region",
       y = "Kvm. Pris")



















### --- Mere rod --- ###

# sub set med outliers test
subTest <- dfBoligSiden_ingenNA
subTest2 <- subTest
skim(subTest)

#Outlier sæt
OLsubSetPris <- subTest[(subTest$pris > 15000000),]
OLsubSetAlder <- subTest[(subTest$alder > 150),]
OLsubSetGrund <- subTest[(subTest$grund > 1200),]

#final subSet eksl. outliers - kun dem der er relevante
subTest2 <- subTest[(subTest$pris <= 15000000),] 
subTest2 <- subTest2[(subTest2$alder <= 150),]
subTest2 <- subTest2[(subTest2$grund <= 1200),]

#Linær regression på grundareal og pris
ggplot(subTest2, aes(grund, pris))+
  geom_point(alpha = 0.2, color = "steelblue") + 
  geom_smooth(method = "lm", color = "dark red") + 
  labs(title = "Sammenhæng mellem grund & pris",
       x = "Grundareal",
       y = "Pris på bolig")

#Linær regression på pris og alder
ggplot(subTest2, aes(alder, pris)) +
  geom_point(alpha = 0.40, color = "steelblue") +
  geom_smooth(method = "lm", color = "dark red") +
  labs(title = "Sammenhæng mellem alder & pris",
       x = "Alder på bolig",
       y = "Pris på bolig")


#Linær regression på pris og alder
ggplot(subTest2, aes(postnr, størrelse)) +
  geom_point(alpha = 0.40, color = "steelblue") +
  geom_smooth(method = "lm", color = "dark red")



#Linær regression på pris og alder


#ggplots
ggplot(dfBoligSiden_ingenNA, aes( ,pris))+
  geom_boxplot()

boxplot(dfBoligSiden_ingenNA$pris)
summary(dfBoligSiden_ingenNA$pris)

#outlier på pris - subTest
OLsubSetPris <- dfBoligSiden_ingenNA[(dfBoligSiden_ingenNA$pris > 27000000),]


ggplot(subTest, aes( ,pris))+
  geom_boxplot()
summary(dfBoligSiden_ingenNA$pris)


ggplot(subTest, aes(, pris))+
  geom_boxplot()


### --- ROD --- ###

library(ggplot2)

#lave en graf over region kontra kvm pris

#box plots
ggplot(dfBoligSiden_ingenNA, aes(alder, pris)) +
  geom_boxplot()



class(dfBoligSiden_ingenNA$pris)
class(dfBoligSiden_ingenNA$region)
class(dfBoligSiden_ingenNA$alder)



dfBoligSiden_ingenNA$pris <- as.numeric(dfBoligSiden_ingenNA$pris)
dfBoligSiden_ingenNA$vejnr <- as.numeric(dfBoligSiden_ingenNA$vejnr)
dfBoligSiden_ingenNA$postnr <- as.numeric(dfBoligSiden_ingenNA$postnr)
dfBoligSiden_ingenNA$region <- as.character(dfBoligSiden_ingenNA$region)

ggplot(dfBoligSiden_ingenNA, aes(postnr, pris)) +
  geom_point(position = "jitter")+
  geom_smooth(method = "lm")


#Kan ikke direkte overføres til numeric, fordi de har skrevet dag. Så skal først renses for dag, derefter konverteres
dfBoligSiden_ingenNA$liggetid <- as.numeric(dfBoligSiden_ingenNA$liggetid)

subTest$liggetid <- gsub("[a-zA-Z]", "", subTest$liggetid)
subTest$liggetid <- as.numeric(subTest$liggetid)
class(subTest$liggetid)

?gsub





