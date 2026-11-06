#Her er r. script med api ting.
## Henter tre værker fra SMK's API og gør dem klar til hjemmesiden

library(httr)      #tid til at lave GET-requests
library(jsonlite)  #tid til at læse JSON

objekt_numre <- c("KMS8162", "KMS4947", "KMS_____")   #moderne, ?, ? - skriv det sidste nummer ind selv
perioder <- c("efter 1900", "?", "?")                 #skriv selv, hvilken periode hvert værk er fra

dir.create("img", showWarnings = FALSE)   #laver mappen img, hvis den ikke findes

## Trin 1: kig på ét svar først
url <- paste0("https://api.smk.dk/api/v1/art/?object_number=", objekt_numre[1], "&lang=da")   #kaldet til første værk
svar <- GET(url)                       #henter svaret fra SMK
status_code(svar)                      #200 betyder, at det gik godt
liste <- fromJSON(content(svar, "text", encoding = "UTF-8"), simplifyVector = FALSE)   #JSON bliver til en liste
str(liste, max.level = 4)              #find selv ud af, hvor felterne ligger, før du kører trin 2

## Trin 2: hent alle tre og træk metadata ud
titler <- c()          #tomme vektorer, som loopet fylder op
kunstnere <- c()
år <- c()
køn <- c()
thumbnails <- c()
highres <- c()

for (nr in objekt_numre) {
  url <- paste0("https://api.smk.dk/api/v1/art/?object_number=", nr, "&lang=da")   #bygger URL for hvert værk
  svar <- GET(url)
  stopifnot(status_code(svar) == 200)   #stopper, hvis noget gik galt
  liste <- fromJSON(content(svar, "text", encoding = "UTF-8"), simplifyVector = FALSE)
  værk <- liste$items[[1]]              #selve værket
  
  ## GÆT på feltnavne, ret dem efter str() i trin 1
  titler <- c(titler, værk$titles[[1]]$title)                      #titel
  kunstnere <- c(kunstnere, værk$production[[1]]$creator)          #kunstner
  køn <- c(køn, værk$production[[1]]$creator_gender)               #køn
  år <- c(år, værk$production_date[[1]]$period)                    #produktionsår
  thumbnails <- c(thumbnails, værk$image_thumbnail)                #link til thumbnail
  highres <- c(highres, værk$image_native)                         #link til highres
  
  Sys.sleep(1)   #lille pause, så vi er flinke ved serveren
}

## Trin 3: saml i et data frame
df_værker <- data.frame(
  objekt_nr = objekt_numre,
  periode = perioder,
  titel = titler,
  kunstner = kunstnere,
  år = år,
  køn = køn,
  thumbnail = thumbnails,
  highres = highres
)
df_værker   #tjek at alle tre rækker er der, og at ingen kolonner er tomme

## Trin 4: hent billederne ned i mappen img
for (i in 1:nrow(df_værker)) {
  download.file(df_værker$thumbnail[i], paste0("img/", df_værker$objekt_nr[i], "_thumb.jpg"), mode = "wb")   #thumbnail
  download.file(df_værker$highres[i], paste0("img/", df_værker$objekt_nr[i], "_highres.jpg"), mode = "wb")   #highres
}