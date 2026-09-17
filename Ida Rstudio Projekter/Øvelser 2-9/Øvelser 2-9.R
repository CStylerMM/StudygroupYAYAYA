### --- Wulf øvelser 2/9--- ###

#retrieve data
dfBilBase <- read.csv("https://raw.githubusercontent.com/cphstud/RIntroData/refs/heads/master/bilbasen.csv")

#data prep
summary(dfBilBase)
str(dfBilBase)

#samlpe row
testRow <- dfBilBase[23,]
View(testRow)

#skimR
install.packages("skimr")
library(skimr)

skim(dfBilBase)

#extract NA på mpg
mpg_lv <- is.na(dfBilBase$mpg)
sum(mpg_lv)
mpg_lv
mpgNA <- dfBilBase[mpg_lv,]
View(mpgNA)

dfBilBaseSub <- dfBilBase[!mpg_lv,]
View(dfBilBaseSub)

mean(dfBilBaseSub[,2])

# Gemme data frames
saveRDS(mpgNA,"underligeBiler.rds")
write.csv(mpgNA,"underligeBiler.csv")
write_xlsx(mpgNA,"underligeBiler.xlsx")

# - Hvis man vil gemme i excelfiler
install.packages("readxl")
library(readxl)
install.packages("writexl")
library(writexl)

# outliners
# Kontinuerlige variabler
ggplot(dfBilBasePriceOutliner,aes(price))+
  geom_boxplot()

ggplot(dfBilBaseSub,aes(price))+
  geom_boxplot()
boxplot(dfBilBaseSub$price)
summary(dfBilBaseSub$price)

boxplot(dfBilBaseSub$milage)
summary(dfBilBaseSub$milage)

boxplot(dfBilBaseSub$mpg)
summary(dfBilBaseSub$mpg)


#subsette 
dfBilBaseSub2 <- dfBilBaseSub[(dfBilBaseSub$price < 250000),]
dfBilBaseSub3 <- dfBilBaseSub[(dfBilBaseSub$year < 2000),]
dfBilBaseSub4 <- dfBilBaseSub3[(dfBilBaseSub$milage > 300000),]
dfBilBaseSub5 <- dfBilBaseSub4[(dfBilBaseSub$mpg < 12.5),]

dfBilBasePriceOutlier <- dfBilBaseSub[(dfBilBaseSub$price >=250000),]
dfBilBaseAgeOutlier <- dfBilBaseSub2[(dfBilBaseSub2$year >= 2000),]
dfBilBaseMilageOutlier <- dfBilBaseSub3[(dfBilBaseSub3$milage <= 300000),]
dfBilBaseMpgOutlier <- dfBilBaseSub4[(dfBilBaseSub4$mpg >= 12.5),]

dfBilBasePriceOutliner <- dfBilBaseSub[(dfBilBaseSub$price >=250000),]
dfBilBaseAgeOutliner <- dfBilBasePriceOutliner[(dfBilBasePriceOutliner$year <= 2000),]
dfBilBaseMilageOutlier <- dfBilBaseAgeOutliner[(dfBilBaseAgeOutliner$milage <= 300000),]
dfBilBaseMpgOutlier <- dfBilBaseMilageOutlier [(dfBilBaseMilageOutlier$mpg >=12.5),]

ggplot(dfBilBaseMpgOutlier, aes(price, milage))+
  geom_point()+
  geom_smooth(method="lm")

View(dfBilBaseMilageOutlier)
View(dfBilBaseSub5)

#intro til ggplots
install.packages("ggplot2")
library("ggplot2")

ggplot(dfBilBasePriceOutliner,aes(milage, price))+ 
  geom_point()

hist(dfBilBasePriceOutliner$price,breaks = 23)

ggplot(dfBilBaseMilageOutlier,aes(price))+ 
  geom_histogram()

hist(dfBilBaseMilageOutlier$price)


# Data engineering
# cont: alder
summary(dfBilBaseSub2$year)
boxplot(dfBilBaseSub2$year)
dfBilBaseSub3$age = 2023 - (dfBilBaseSub3$year)
View(dfBilBaseSub3)

### --- Wulf Gist --- ###









