library(readxl)
library(tidyr)
library(ggplot2)
write

mpg
diamonds
ggplot(diamonds, aes(x=cut,fill=clarity))+
  geom_bar(position = "dodge")
#Hedder barplot

ggplot(mpg,aes(x=displ, y=hwy))+
  geom_point(position="jitter")

#retrieve data
dfbilbase <- read.csv("https://raw.githubusercontent.com/cphstud/RIntroData/refs/heads/master/bilbasen.csv")
#data prep
summary(dfbilbase)
structure(dfbilbase)
#Introducer umbrako nøglen (gsub)
#search and replace

#samplerow
testrow <- dfbilbase[23,]
#pakke skim lole

library(skimr)
#pas på med postnumre
skim(dfbilbase)
#extrack na på mpg
mpglv <- is.na(dfbilbase$mpg)
sum(mpglv)
mpglv
mpgNA <- dfbilbase[mpglv,]
saveRDS(mpgNA,"underligeBiler.rds")
write.csv(mpgNA,"underligeBiler.csv")
dfbilbaseSub <- dfbilbase[!mpglv,]

#outlier
#kontinuerte variabler

ggplot(dfbilbaseSub, aes(price))+
  geom_boxplot()
boxplot(dfbilbaseSub$price)
boxplot(dfbilbaseSub2$price)
summary(dfbilbaseSub$price)

#Subsette
dfbilbaseSub2 <- dfbilbaseSub[(dfbilbaseSub$price<250000),]
dfbilbaseSub3 <- dfbilbaseSub2[(dfbilbaseSub2$year<2000),]
dfbilbasePriceOutlier<- dfbilbaseSub[(dfbilbaseSub$price>=250000),]
dfbilbaseAgeOutlier<- dfbilbaseSub2[(dfbilbaseSub2$year<=2000),]

#ggplot er vejen frem hvis ikke du skal arbejde i excel
ggplot(dfbilbasePriceOutlier,aes(price))+
  geom_histogram(binwidth=5)
hist(dfbilbasePriceOutlier$price, breaks=23)

#gist hans kode.
#Miles pr gallon

## dat aengineering
# cont: alder
summary(dfbilbaseSub2$year)
boxplot(dfbilbaseSub2$year)
dfbilbaseSub3$age=2026-(dfbilbaseSub3$year)


