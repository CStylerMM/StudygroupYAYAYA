### --- Upser... --- ###

# Her havde vi misforstået opgaven, men lavet noget sygt godt :))

# Replikerer 9-1 sekvensen 4 gange for hvert tal
rep(seq(9, 1), times = 4)
KlasseTal <- rep(seq(9, 1), times = 4)
# Replikere a-b sekvensen 9 gange i alt
rep(letters[1:4], each = 9)
KlasseBogstav <- rep(letters[1:4], each = 9)
# Tjekke lægnden
length(KlasseBogstav)
length(KlasseTal)
# Integrerer sekvenserne med hinanden
Klasser <- paste(KlasseTal, KlasseBogstav, sep = ")")

#Lave en data frame med 3 kolonner, og indsætte vores kædet lister i relevant kolonne
df <- data.frame("Klasser" = Klasser, "Uger" = " ", "Score" = " ")

### --- Mere rod :)) --- ###
#rense data fra postnummer (NA)
dfBoligSiden_ren_postnr <- dfBoligSiden[dfBoligSiden$postnr != "NA", ]

postnr <- as.numeric(dfBoligSiden_ren_postnr$postnr)
postnr

#lave funktion der kan regne region ud fra postnr :))
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

#lave opført om til numeric i stedet for character
dfBoligSiden_ingenNA$opført <- as.numeric(dfBoligSiden_ingenNA$opført)
dfBoligSiden_ingenNA$opført

#etabler ny kolonne med alder
dfBoligSiden_ingenNA$alder <- sapply(dfBoligSiden_ingenNA$opført, alderBr)


dfBoligSiden$Region <- function(postnr) {
  retval = 0
  if(postnr >= 1000 & postnr <= 2999){
    retval = "hovedstaden"
  } else if (postnr >= 3000 & postnr <= 3699) {
    retval = "sjælland"
  } else if (postnr >= 3700 & postnr <= 3790){
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


myAgeCat <- function(age) {
  retval = 0
  if(age < 5) {
    retval = "I"
  } else if (age >= 5 & age < 12) {
    retval = "C"
  } else {
    retval = "A"
  }
  return(retval)
}
myAgeCat(2)


dfBoligSiden$region <- function(postnr) {
  retval= "NA"
  # logik der inddeler i kategorier
  if (postnr >= 0 & postnr <= 2999) {
    retval="Region Hovedstaden"
  } else if (postnr >= 3000 & age <= 3699) {
    retval="Nordsjælland"
  } else if (postnr >= 4000 & age <= 4999){
    retval="Region Sjælland"
  } else if (postnr >= 5000 & age <= 6999){
    retval="Region Syddanmark"
  } else if (postnr >= 7000 & age <= 7999){
    retval="Midt- og Vestjylland"
  } else if (postnr >= 9000 & age <= 9990){
    retval="Region Nordjylland"
  } else {retval = "NA"}
  
  return(retval)
}





