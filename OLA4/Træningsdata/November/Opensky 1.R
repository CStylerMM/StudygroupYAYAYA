#OpenskyR
#Starter med baseurl
library(httr)
library(jsonlite)
baseurl <- "https://opensky-network.org/api"

endpoints <- "/states/all"
query <-""
un <- "casperstegler@gmail.com-api-client"
pw <-  "REDACTED"

totalurl <- paste0(baseurl,endpoints)
res <- httr::GET(totalurl,authenticate(un,pw))
rescontent <- httr::content(res, as."text")
str(rescontent)
resretval <- jsonlite::fromJSON(rescontent)
statedf <- as.data.frame(resretval$states)

# get rows where string matches substing dataframe r. (søg i google lol)

library(stringr)
retcount <- str_detect(statedf$V2, "SAS")
sum(retcount) #hvor mange sas er der true false

sasdf <- statedf[retcount,]
#et dataframe med de fly der flyver sas
sum(retcount)
#ind i powerpoint.

