# Vi har scrapet autoscout. Vi får derfor følgende datasæt ud.
df_tyskebiler_OLA <- readRDS("~/Dataanalyse/StudygroupYAYAYA/OLA4/Opgaverne/Scrapes/df_tyskland.rds")

# ---------- Opg 1.2: Tidy ----------
library(dplyr)
library(stringr)

#tjek først hvad vi har
nrow(df_tyskebiler_OLA)
str(df_tyskebiler_OLA)
colSums(is.na(df_tyskebiler_OLA))

#Bilbasen-sættet er kun forhandlere (d), så vi sammenligner ens med ens
table(df_tyskebiler_OLA$sælgertype)

df_tyskebiler_OLA <- df_tyskebiler_OLA %>%
  filter(sælgertype == "d")

#scrapen tog også nye biler med. 0 km er nye, og km > 0 smider samtidig dem uden km ud
df_tyskebiler_OLA <- df_tyskebiler_OLA %>%
  filter(km > 0)

#første_reg er fx "07-2009", vi tager de fire sidste cifre som årgang
df_tyskebiler_OLA <- df_tyskebiler_OLA %>%
  mutate(år = as.numeric(str_extract(første_reg, "\\d{4}$")))

#kronen er bundet til euroen, så vi bruger en fast kurs
eur_dkk <- 7.46

#prisen er uden dansk afgift. pris_kr hedder det samme som i dit danske sæt
df_tyskebiler_OLA <- df_tyskebiler_OLA %>%
  mutate(pris_kr = pris_eur * eur_dkk)

#tjek: priser og årgange skal ligge fornuftigt
summary(df_tyskebiler_OLA$pris_kr)
summary(df_tyskebiler_OLA$år)
nrow(df_tyskebiler_OLA)
