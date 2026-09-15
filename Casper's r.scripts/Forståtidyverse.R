library(tidyverse)
install.packages("nycflights13")
library(nycflights13)
?flights
view(flights)
flights
#vi skal filterer
#Vi filtrere det mindre
#filter er subset hos tidyverse
filter(flights,month==1,day==1)
#det enkelte = betyder noget andet, mens == betyder sammenlign

jan1 <- filter(flights,month==1,day==1)


#nu vil jeg have alle fly der er afgået fra jan til december
#måden man siger eller/ og er | = pipe
filter(flights,month==11|month==12|month==6)

#kan også sige %in% i stedet for pipe, fordi man kan pipe 
#derudaf indtil man ikke kan mere. en lettere måde er med %in%
filter(flights,month %in% c(11,12,6))
#c er at vi putter det i en vektor
#en vektor er det her "alder" som er øverst i kolonnen eller i det her tilfælde
#måned! og så giver vi det nogle observertioner eller værdier

#arrange() arrangere et datasæt, så udvælger vi hvad vi vil sortere
#her er det dag, så vi får dag og måned fra den første værdi (1)
#desc= descend - starter med de højeste værdier
arrange(flights, desc(day))
view(flights)

#så det tid til Select()
#vi vælger data i datasæt
select(flights,month,day)
#i stedet vil jeg smide variablerne ud
#så bruger vi minus
#vi kan også sige vi vil have smidt en variable ud hen til en anden variable

select(flights,-month,-day)
#f.eks vil vi gerne fjerne dage hen til "time_hour"
select(flights,-(day:time_hour))

#mutate - vi muterer datasættet
#vi tilføjer variable til vores datasæt
#først laver vi en liste over variable vi vil definere.
mutate(flights,
       gain=dep_delay-arr_delay,
       )
view(mutate(flights,
       gain=dep_delay-arr_delay,
       hours =air_time/60,
       gain_per_hour=gain/hours))
#og sådan laver du dine egne udregninger ud fra ting i datasættet GG!!!!
#så det tid til summarise()
#vi reducerer et meget stort datasæt til en række!
#vi vil gerne have gennemsnittet af departure delays. husk vi definere delay
summarise(flights, delay=mean(dep_delay))
#det fejler, men vi skal sige den skal fjerne NA værdier
#Det gøres sådan her
summarise(flights, delay=mean(dep_delay, na.rm=T))
#NA = NOT AVAIBLE
is.na(NA)
is.na(5)
#læg mærke til vi kigger efter de her NA'er
#men hey vi kan sgu da det samme med filter
filter(flights,is.na(dep_delay))
# ℹ 8,245 more rows

#så det tid til group_by()
#lad os gruppere ud fra måned
flights_by_month <- group_by(flights,month)
#bruger jeg samme summarise og ændrer flights til det nye datasæt flights_by_month
#og selvfølgelig kan vi gøre det med flere datasæt
summarise(flights_by_month, delay=mean(dep_delay,na.rm=T))

#hvordan blander vi vores funktioner sammen?
#kombi funktion group_by lagt ind i summarise. der var summarise input og output er
#output
summarise(group_by(filter(flights, carrier =="DL"), month, day), delay = mean(dep_delay,na.rm =T)) 
#Shit det langt var
#lad os kombi for at putte kassen i kassen og lave en ny kasse.
#vi har en input og en output
#lad os lave det om til en variable.
#vi laver derfor om på om
#starter med filter
#vi sætter det i et datasæt
flights_df <- filter(flights, carrier =="DL")
flights_df_groups <- group_by(flights_df, month, day)
summarise(flights_df_groups,delay = mean(dep_delay,na.rm=T))
#er der en måde at gøre det lettere? - ja da. 

#så det tid til at pipe igås!
#.    %>%
#bruges sådan her:
flights%>%
  filter(carrier =="DL") %>% 
  group_by(month, day) %>%
  summarise(delay = mean(dep_delay,na.rm=T))
  
#Her er så opgaven:
## Exercise 1.1:
# load data from url
url= "https://raw.githubusercontent.com/cphstud/RIntroData/refs/heads/master/bilbasen.csv"
dfbilbase=(url)

## Exercise 1.2:
# Create numeric variable age
dfbilbase$age=dfbilbase$year

## Exercise 1.3:
# remove observations where mpg is NA in two steps creating the logical vector
# nrow(dfbclean) should return 2791
dfbcleanlv <- T 
dfbclean <- dfbcleanlv 
nrow(dfbclean)

## Exercise 1.4:
# create two subsets form dfbclean. One with column milage and maketype
# and on with column price and maketype. Both from row 43 to 50
dfcarSubP=dfbclean
dfcarSubM=dfbclean

## NOW PRICE

## Exercise 1.5
# compute sumtotal of the prices with + and a for-loop
totalsum=0
for (i in (1:10)) {
  totalsum=totalsum
}
dfcarSubP$sum=totalsum

## Exercise 1.6:
# compute mean using vectorized approach and add as a column 'mean'
dfcarSubP$mean=10


## Exercise 1.7:
# compute the distance from mean for each car using a for-loop
# and save in a new column called "dist"
dfcarSubP$dist=0
for (i in (1:10)) {
  dfcarSubP[i,'dist']=10
}

## Exercise 1.8:
# do the same in a vectorized manner
# and save in the same column "dist"
dfcarSubP$dist=10

## Exercise 1.9:
# compute the relative distance to the mean
# and save in a new column called "reldist"
dfcarSubP$reldist=10

## Exercise 1.10:
# compute the squareddistance from mean for each car in a vectorized manner
# and save in a new column called "sqdist"
dfcarSubP$sqdist=10

## Exercise 1.11:
# compute the sum of sqdist and divide it by number of rows
# and save in a new column called "var"
dfcarSubP$var2=10

## Exercise 1.12:
# compute the squareroot of var
# and save in a new column called "sd"
dfcarSubP$sd2=10

## Exercise 1.13:
# compute the standard-deviation of the price using and R-built-in-function
# save in a new column called "sdR"and compare with sd
dfcarSubP$sdR=10

## Exercise 1.14:
# creat a lineplot with pricedist on the y-axis and the car-index on the x-axis
# and a straight horistontal line with the mean of the price as intersect
options(scipen = 999)
par(mfrow=c(1,1))
plot(dfcarSubP$dist, type="l")
intersect=12
abline(a=intersect, b=0, )

## Exercise 1.15:
# creat a lineplot with milage on the y-axis and the car-index on the x-axis
# and a straight line with the mean of the milage
plot(type="l")
intersect=10
abline(a=intersect, b=0)


### NOW PREPARE MILAGE
## Exercise 1.16:
# create the mean, distance and relative distance on the second dataframe
# using the mean-function 
dfcarSubM$mean=mean(12)
dfcarSubM$dist=10
dfcarSubM$reldist=10

### HOW to overlay the two plot?
# same relative y-values!
## Exercise 1.17:
# create a lineplot with relative pricedistance on the y-axis and the car-index on the x-axis
# create a second lineplot with relative milagedistance on the y-axis and the car-index on the x-axis
# and a line through 0 with slope 0 (horisontal line)
par(mfrow=c(1,1))
plot(dfcarSubM$reldist, type="l", ylab="Price")
lines(dfcarSubP$reldist, type="l", xlab = "Milage", col="red")
abline(a=0, b=0, )

## Exercise 1.18:
# suppose they are negatively correlated can you find a good bargain 
# and a bad one using the graph?

## Exercise 1.19:
### now merge the two together using row.names
row.names(dfcarSubP)
colnames(dfcarSubP)
colnames(dfcarSubM)

# first choose only milage and dist
dfcarSubMforMege=dfcarSubM
# renmame dist to distm 
colnames(dfcarSubMforMege)="kurt"

# merge into the dataframe with price using row.names
dfcarSub2=merge()
# choose only price,milage,maketype and the two distances
dfcarSub2A=dfcarSub2[,c()]
# renmame dist to distp 
names(dfcarSub2A)[6]='distp'

# now check the formula for covariance
## Exercise 1.20:
# compute the covariance in a vectorized way
dfcarSub2A$mycov=123123

# compute the covariance using R's cov-function
dfcarSub2A$cov=cov()

# now check the formula for correlation
## Exercise 1.21:
# and compute the correlation in a vectorized way using the cov
# and R's sd-function
dfcarSub2A$mycor=dfcarSub2A$mycov/33

# can you explain the reason why it's between -1 and 1?

## Exercise 2.1:
# create a subset of dfbclean with only numeric variables
# you must go through the following steps:
# create a list to collect index of numeric columns
collist=list()

# use a for-loop to walk through each column
# for each index you must do:
#   combine sapply and is.numeric to get logical vector
#   sum logical vector
#   if sum is equal t nrow of original dataframe add index to list

for (i in (1:10)) {
  collist=append(collist,i)
}
dfnum=dfbclean[,unlist(collist)]

## Exercise 2.2:
# use the car-id-column to merge the maketype into  the dfbnum
dfMT=dfbclean[,c('maketype','car_id')]
dfnumMT=merge()









