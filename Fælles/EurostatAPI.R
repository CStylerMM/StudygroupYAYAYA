#tid til at bruge eurostat
library(eurostat)

#hent kun Finland, BNP-tallet og den rigtige enhed, ellers får vi hele lortet
dd <- get_eurostat("nama_10_gdp",
                   filters = list(geo = "DE",
                                  na_item = "B1GQ",
                                  unit = "CLV_I10"))
#______ hergra er det wulf kode




library(eurostat)
library(restatapi)
library(stringr)
library(dplyr)
# total content
alltabs <- get_eurostat_toc()

#filtrer efter emne
migrantTabs= alltabs |> filter(str_detect(title, "Leave|leave")) |>
  filter(str_detect(title, "leave"))
view(syrierTabs)
unique(migrantTabs$title)
# henter meta data, efter at have kigget på gastabs ig fundet koden

Migrantmeta =get_eurostat_dsd("migr_eiord")

#undersøg meta data med henblij på filtering
unique(Migrantmeta$concept)


#[1] "freq"     "siec"     "nrg_cons" "unit"     "tax"      "currency" "geo"    

#undersøg hver af dem for værdier
Migrantmeta |> filter(concept=="freq")

#   concept   code                    name
#<char> <char>                  <char>
# 1:    freq      S Half-yearly, semesterly
#næsteee
Migrantmeta |> filter(concept=="siec")
Migrantmeta |> filter(concept=="unit") #vi kan se der 3 værdier. dette ville gange alt med 3
#det vil vi ikke så nu vælger vi det kune 1 euro vi skal have med
Migrantmeta |> filter(concept=="geo")

#man kan også putte filter ind i data hentningen med enter

#meeen vi laver vores egen querey eller filter og derefter henter vi det ned


myquery = list(
  geo=c("DK", "DE", "IT", "ES"))

Migrantdata=get_eurostat_data("migr_eiord",
                          filters = myquery,
                          date_filter = ">2000",
                          verbose = T
)

#tid til at se hvad der gemmer sig i hvert concept
#Syriens kode i citizen (søg på navnet, ikke koden)
Migrantmeta |> filter(concept=="citizen") |> filter(str_detect(name, "Syria"))

#age og sex: jeg leder efter totalen, så vi ikke får tallene delt op
Migrantmeta |> filter(concept=="age")
Migrantmeta |> filter(concept=="sex")

#unit: er det antal personer, eller noget andet?
Migrantmeta |> filter(concept=="unit")

#freq: årlig eller kvartal?
Migrantmeta |> filter(concept=="freq")

#nu har vi syrernes kode (SY), så nu bygger vi vores egen query
myquery = list(
  citizen = "SY",
  geo = c("DK", "DE", "ES", "IT")
)

#henter fra den rigtige tabel denne gang: migr_eiord, altså ordren om at forlade landet
syrerUD = get_eurostat_data("migr_eiord",
                               filters = myquery,
                               date_filter = ">2007",
                               verbose = TRUE)

#gal et overblik, så vi ser hvordan det ser ud
head(syrerOrdre)
unique(syrerOrdre$age)
unique(syrerOrdre$sex)
unique(syrerOrdre$unit)

#og så det done
#lad os tælle sammen hvert land

#sta
library(ggplot2)

ggplot(syrerUD)=aes(x=time, y=value) + geom_line()





unique(gasData) #kigger på første 5 og sidste 5
unique(gasData$nrg_cons)

#undersøger dem og foretag valg
#fjerne kolonner

gasData= gasData |> select(-currency)
gasData= gasData |> select(-siec)
gasData= gasData |> filter(unit=="KWH")

library(tidyverse)
#installer først
library(eurostat)
search <- search_eurostat("population", type = "table")
elderly_pop <- get_eurostat("tps00028",filter = list(geo = "AT")) 
#"tps bla bla er selve Proportion of population aged 65 and over og list bruger vi til at sige
#kun østrig eller AT

#nu vil vi have en figur der viser populationen over de 65 og så med en x og y værdi
#kigger du i elderly_pop kan du se vi har nogle kolonner. som f.eks time og values

#dem tager vi ud med select og laver et ggplot med.
elderly_pop %>% select(time, values) %>% ggplot(aes(x=time, y=values))+
  geom_line()
#nu laver vi noget med eustat kilde omkring unge møder (teen moms)
library(restatapi)
library(dplyr)

#retrieval - external
allTabs <- get_eurostat_toc()

#search for variables in title
colnames(allTabs)
sel = allTabs %>% select(c(title,code)) %>% filter(str_detect(title, "birth")) %>%
  filter(str_detect(title,"mother"))
#vi bruger så demo_r_fagec3 efter at kigge på sel
#meta data på
youngmomsmeta <- get_eurostat_dsd("demo_r_fagec3")
unique(youngmomsmeta$concept)         #Hvad er der?
#"freq" "age"  "unit" "geo" 
youngmomsmeta %>% filter(concept=="age") %>% select(code) %>% unique()


#data fra eurostat
#vi laver en liste
myfilter=list(
  age=c("Y10-14","Y15-19"),
  geo=c("^DE.", "DK.", "ES.", "RO.", "FI.", "HU."))


clean_restatapi_cache()
youngmomsDF = get_eurostat_data("demo_r_fagec3",
                                filters = myfilter,
                                date_filter= "2000":"2026"
)
#Prepare date for Germany
youngmomsDFDE = youngmomsDF[grepl("DE.*", youngmomsDF$geo),]
youngmomsDFDE$nut0 = grepl("DE$", youngmomsDFDE$geo)
youngmomsDFDE$nut1 = grepl("DE[0-9]{1}$", youngmomsDFDE$geo)
youngmomsDFDE$nut2 = grepl("DE[0-9]{2}$", youngmomsDFDE$geo)
youngmomsDFDE$nut3 = grepl("DE[0-9]{3}$", youngmomsDFDE$geo)
#youngmomsDFDE %>% filter(nut1==T %>% count()






#fattigmandsbackup er at gemme koden i en rds.
gasData=readRDS("gas1.rds")
# search
birth <- search_eurostat("birth")

# get table raw
allbirths <- get_eurostat_data("demo_fordagec")

# get metadata

# filtrering via eurostat
dd2 <- get_eurostat("demo_fordagec",
                    filters = list(
                      age = c("TOTAL","Y10-14","Y15-19","Y20-24","Y25-29","Y30-34","Y35-39","Y40-44","Y45-49","Y_GE50"),
                      ord_brth = c("TOTAL","1")
                    )
)
dffs2<- get_eurostat("demo_fordagec",
                     filters = list(
                       age = c("Y10-14","Y15-19","Y20-24","Y25-29","Y30-34","Y35-39","Y40-44","Y45-49","Y_GE50"),
                       ord_brth = c("1")
                     )
)

eurostat::clean_eurostat_cache()

#total numbers of births

#df_birth<-get_eurostat_data("demo_fmonth", stringsAsFactors = F)
#df_b <- df_birth[nchar(df_birth$geo)==2,]
#df_byear <- aggregate(df_b$values, by=list(year=df_b$time,geo=df_b$geo), FUN = sum)
#library(dplyr)
#library(tidyr)
#subctr
#df_p=df_byear %>% filter(geo =="DK")
#attributes(df_p)
#df_p=ungroup(df_p)
#ggplot(df_p, aes(x=year, y=x))+geom_point()
#

dd3 <- get_eurostat("demo_fordagec",
                    filters = list(
                      ord_brth = c("TOTAL")
                    )
)


#subdd3 <- dd3[dd3$time > '2007-01-01',]
#subdd3 <- dffs2[dffs2$time > '2007-01-01',]
#subdd3 <- subdd3 %>% filter(nchar(geo)==2)
#subt <- subdd3[is.na(subdd3$values),]
#dd <- get_eurostat("nama_10_gdp",
#   filters = list(
#     geo = "FI",
#     na_item = "B1GQ",
#     unit = "CLV_I10"
#   )
#)

# slanket min df
library(ggplot2)
library(dplyr)
ddsub2 <- dffs2[-c(1,2,4)]
ddsub2 <- ddsub2 %>% filter(nchar(geo)==2)
ddsub <- ddsub2
#ddsubdk <- ddsub[ddsub$geo=="DK",]
#ddsubdk2 <- ddsub2[ddsub2$geo=="DK",]
#ddsubde <- ddsub[ddsub$geo=="DE",]
#ddsubdedk <- ddsub[ddsub$geo %in% c("DE","DK"),]

#ddctr <- aggregate(ddsub$values, by = list(geo=ddsub$geo, time=ddsub$time, age=ddsub$age), FUN = sum)
#library(dplyr)
#ddctr <- aggregate(ddsub$values, by = list(geo=ddsub$geo, time=ddsub$time), FUN = sum)
#ddctrL <- ddctr %>% filter(ddctr$time == '1980-01-01')
#ddctrL <- ddsubdedk %>% filter(ddsubdedk$time == '1980-01-01')
#ddctrL <- ddsubdedk %>% filter(ddsubdedk$time == '1980-01-01')
#ddc <- ddc[order(ddc$`ddctr$time`),]
#ddc <- ddctr[order(ddctr$geo),]
#a=max(ddctr$time)
# Problemer med Tyskland

sss=ddctr %>% filter(geo=="DE") %>% mutate(r=min(time))
sss[1,'time']
a = as.numeric(format(a,"%Y"))
b = a - 13
totctr = unique(ddsub$geo)

# Liste af udvalge lande
subctr = c("SE","DK","FI","ES","RO","EL")
#subage = c("Y10-14","Y15-19","Y40-44")
subage = c("Y15-19")

#relativ til de samlede antal fødsler
#df_byear_ctr <- df_byear %>% filter(geo %in% subctr) 
#df_byear_ctr$year2 <- sapply(df_byear_ctr$year, function(x) (paste0(x,"-01-01")))
#df_byear_ctr$year3 <- as.Date(df_byear_ctr$year2)
#df_byear_ctr_sub <- df_byear_ctr %>% filter(year3 > '2007-01-01' & year3 < '2022-01-01')
#df_byear_ctr_sub <- df_byear_ctr_sub[,-c(1,4)]
#colnames(df_byear_ctr_sub) = c("geo","total","time")
#ggplot(df_byear_ctr_sub, aes(x=time,y=total,color=geo))+geom_point()+geom_line()+
#  ggtitle("Samlet antal fødsler")+
#  scale_y_continuous(breaks = seq(0,2000000,by=100000))
#
#relativ til det første år
ddmysel <- ddsub %>% filter( geo %in% subctr & age %in% subage & time > '2007-01-01')
#ddfv <- ddmysel %>% filter( time == min(time) ) %>% arrange(age)

#
#ddxx <- dd2 %>% filter(geo == "DK")
#ddxx <- ddxx[!is.na(ddxx$values),]
#ddxx2 <- ddxx %>% filter(age %in% subage)

#bvf = ddsub[ddsub$time=='1997-01-01' & ddsub$geo %in% subctr & ddsub$age %in% subage,]
#bvf97 = bvf[,-3]
ddplotsub <- ddsub %>% filter( geo %in% subctr & age %in% subage & time > '1996-01-01')  
bvmerge = ddplotsub %>%  left_join(bvf97, by=c("age","geo"))
colnames(bvmerge)=c("age","geo","time","values","startval")
bvmerge$prcchg = (bvmerge$values-bvmerge$startval)/bvmerge$startval




ggplot(ddplotsub, aes(x=time, y=values, color=geo))+geom_line()+geom_point()+
  facet_wrap(~age, scales="free")+theme(axis.text.x = element_text(angle = 90))+ggtitle("Spain lags")

library(ggplot2)
bvmerge %>% filter(age=='Y15-19') %>% 
  ggplot(aes(x=time, y=prcchg, color=geo))+geom_line()+geom_point()+
  facet_wrap(~age)

colnames()

