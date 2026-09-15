library(skimr)
library(readxl)

# retrieve data
dfbilbase <- read.csv("https://raw.githubusercontent.com/cphstud/RIntroData/refs/heads/master/bilbasen.csv")

# data prep
summary(dfbilbase)
str(dfbilbase)


# samplerow
testrow <- dfbilbase[23,]
skim(dfbilbase)

# extract NA på mpg
mpglv <- is.na(dfbilbase$mpg)
sum(mpglv)
mpglv
mpgNA <- dfbilbase[mpglv,]
saveRDS(mpgNA,"underligeBiler.rds")
write.csv(mpgNA,"underligeBiler.csv")

dfbilbaseSub <- dfbilbase[!mpglv,]

# outliers
# kontinuerte variabler
ggplot(dfbilbaseSub, aes(price))+
  geom_boxplot()
boxplot(dfbilbaseSub$price)
boxplot(dfbilbaseSub2$price)
boxplot(dfbilbaseSub3$mpg)

summary(dfbilbaseSub$price)
summary(dfbilbaseSub3$mpg)

##kategori variabler
#regioner -frekvensplot
dfregion=aggregate(dfbilbaseSub3$price~dfbilbaseSub3$region, FUN = length)
dfregionPrice=aggregate(dfbilbaseSub3$price~dfbilbaseSub3$region, FUN = mean)
                        
                        
# subsette
dfbilbaseSub2 <- dfbilbaseSub[(dfbilbaseSub$price < 250000),]
dfbilbaseSub3 <- dfbilbaseSub2[(dfbilbaseSub2$year > 2000 ),]

#outlier
dfbilbasePriceOutlier <- dfbilbaseSub[(dfbilbaseSub$price >= 250000),]
dfbilbaseAgeOutlier <- dfbilbaseSub[(dfbilbaseSub2$year <= 2000),]
dfbilbaseMileOutlier <- dfbilbaseSub3[(dfbilbaseSub3$milage <30),]
dfbilbaseSub4 <- dfbilbaseSub3[(dfbilbaseSub3$milage >= 30),]

summary(dfbilbaseSub4$mpg)
mpg_graense <-  39 
nrow(dfbilbaseSub5) + nrow(dfbilbaseMpgOutlier) == nrow(dfbilbaseSub4)
  
dfbilbaseSub5      <- dfbilbaseSub4[dfbilbaseSub4$mpg >  mpg_graense, ]
dfbilbaseMpgOutlier <- dfbilbaseSub4[dfbilbaseSub4$mpg <= mpg_graense, ]


library(ggplot2)
#For price
ggplot(dfbilbasePriceOutlier, aes(price))+
  geom_histogram()
hist(dfbilbasePriceOutlier$price,breaks = 23)
#for milage
ggplot(dfbilbaseSub4, aes(mpg))+
  geom_histogram()
hist(dfbilbaseSub4$milage,breaks=20)

## data engineering 
# cont: alder
summary(dfbilbaseSub2$year)
boxplot(dfbilbaseSub2$year)
dfbilbaseSub3$age=2023-(dfbilbaseSub3$year)

#Mileage
boxplot(dfbilbaseMileOutlier$milage)
install.packages(vi)