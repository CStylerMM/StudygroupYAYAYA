library(stringr)
library(readr)


# opvarmning  
#find v.h.a et regulært udtryk årstallet 2020 i flg. sætning og gem det i en variabel
testtext <- c("I 2020 boede Kurt Verner i nr. 34.")
# Husk at str_match_all returnerer en liste med en matrix

pattern <- "<dit mønster>"
result <- str_match_all(testtext,pattern)
year <- "1234"

# indlæs filen edclinks.html
allLinks <- read_lines("edclinks.html")

# indsæt første linje som kommentar
# <her er første linje>

# Du skal nu vha et regulært udtryk konstruere et link til dette sagsnummer 
# linket skal kunne bruges til at åbne sagen i en browser dvs linket skal være en url.

firstline <- allLinks[1,]
pattern <- "<dit mønster>"
result <- str_match_all(firstline,pattern)
target <- "123123"
url <- paste0("baseurl",target)
print(url)


# Til sidst skal du loope igennem alle links
# For hvert link konstruerer du en url og lægger den i en liste
urllist <- list()
for (i in 1:3) {
  urllist[[i]] <- url
}

#Ekstraopgave
#Lav en data-frame hvor du gemmer link og sagsnummmer i hver sin kolonne

pattern <- "<dit mønster>"
coln <- c("link","sagsnr")
df <- data.frame()
for (i in 1:3) {
  sagsnr <- 123123123
  url <- paste0(base,target)
  tmpv <- c(url,sagsnr)
  df <- rbind(df,tmpv)
} 
