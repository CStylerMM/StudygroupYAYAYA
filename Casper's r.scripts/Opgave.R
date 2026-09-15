setwd('/Users/vandmelonsguru/Dataanalyse/DATA/FørsteData')
library(tidyverse)
library(readxl)
library(lubridate)
raw<-read_excel("/Users/vandmelonsguru/Dataanalyse/DATA/FørsteData2", 
                sheet="FORV1", skip=2)
list.files("/Users/vandmelonsguru/Dataanalyse/DATA/FørsteData")
sti <- file.path("/Users/vandmelonsguru/Dataanalyse/DATA/FørsteData", "ForbrugerData2.xlsx")
raw <- read_excel(sti, sheet = "FORV1", skip = 2)
names(raw)[1]<-"indikator1"
data_long<-raw%>%
  pivot_longer(-indikator1, names_to = "maaned_raw", values_to = "vaerdi") %>%
  mutate(date=ym(gsub("M","-",maaned_raw)))%>%
  filter(date>=as.Date("2015-01-01"), date<=as.Date(("2026-08-01")))
ggplot(data_long, aes(x=date, y=vaerdi))+
  geom_line(color="steelblue", linewidth = 1)+
  geom_hline(yintercept = 0, linetype ="dashed", color= "grey50")+
  labs(title="Forbrugertillidsindikatoren, januar 2015 til august 2026",
       x=NULL, y="NETTOTAL") +
  theme_minimal()

names(raw)
sti <- "/Users/vandmelonsguru/Dataanalyse/DATA/FørsteData/Forbrugerforventninger alt samlet.xlsx"
raw <- read_excel(sti, sheet = "FORV1", skip = 2)
names(raw)[1] <- "indikator"
dim(raw)
unique(raw$indikator)
data_long <- raw %>%
  pivot_longer(-indikator, names_to = "maaned_raw", values_to = "vaerdi",
               values_transform = list(vaerdi=as.character)) %>%
  mutate(dato = ym(gsub("M", "-", maaned_raw)))
opgave1 <- data_long %>%
  filter(indikator == "Forbrugertillidsindikatoren",
         dato >= as.Date("2015-01-01"),
         dato <= as.Date("2026-08-01"))

