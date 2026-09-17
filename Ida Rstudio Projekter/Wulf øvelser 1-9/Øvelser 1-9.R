### --- Liste øvelse --- ###

v1 <- runif(5,25,40)
v2 <- runif(2,40,60)
v3 <- runif(6,25,40)
v4 <- runif(3,40,50)
v5 <- runif(6,50,60)
v6 <- runif(4,45,52)
v7 <- runif(7,43,58)
v8 <- runif(6,42,53)


deltagere <- list("deltager"="kurt")
deltagere <- list("deltager"=
                    list("id_32"=list("navn"="Kurt","alder"=45,
                                      "løb"=list("DHL"=v3,"ERM"=v4)),
                         "id_33"=list("navn"="Valde","alder"=12,
                                      "løb"=list("DHL"=v1,"ERM"=v2)),
                         "id_34"=list("navn"="Maja","alder"=23,
                                      "løb"=list("DHL"=v5,"ERM"=v6)),
                         "id_35"=list("navn"="Nanna","alder"=31,
                                      "løb"=list("DHL"=v7,"ERM"=v8))))


### --- Estonia øvelse --- ###
dfestonia=read.csv("https://raw.githubusercontent.com/cphstud/RIntroData/refs/heads/master/estonia-passenger-list.csv")

saveRDS(dfestonia,"estonia.rds")

# Øvelse i at lave if funktioner
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

# Øvelse i at lave flere alderskategorier i vores if function
                
### --- WULF TING --- ###

# ny kolonne
dfestonia$AgeCat="A"

# subset alle voksne
newdf=dfestonia$Age > 18
kids=dfestonia$Age <= 18
newdf
newAdults=dfestonia[newdf,]
newKids=dfestonia[kids,]

# compare dk and sweeden
(unique(dfestonia$Country))
unique(dfestonia$Category)
table(dfestonia$Category)/sum(table(dfestonia$Category))
table(dfestonia$Survived)/sum(table(dfestonia$Survived))
table(dfestonia$Survived)
ctrv=dfestonia$Country %in% c("Denmark","Sweden")
dfdkse=dfestonia[ctrv,]

# 
myAgeCat <- function(age) {
  retval=0
  # logik der inddeler i kategorier
  if (age < 5) {
    retval="I"
  } else if (age >= 5 & age < 12) {
    retval="C"
  } else {
    retval="A"
  } 
  return(retval)
}

myAgeCat2 <- function(age) {
  retval=0
  # logik der inddeler i kategorier
  if (age < 5) {
    retval="I"
  } else if (age >= 5 & age < 12) {
    retval="C"
  } else if (age >= 50) {
    retval = "O"
  } else {
    retval="A"
  } 
  return(retval)
}

myAgeCat2(51)

dfestonia$AgeCat=sapply(dfestonia$Age, FUN=myAgeCat)

table(dfestonia$AgeCat)
                  