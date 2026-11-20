# ---------- Opg 1.1: Tidy ----------

library(dplyr)
library(stringr)

#læser den rå scrape
df_mercedes_OLA <- readRDS("~/Dataanalyse/StudygroupYAYAYA/OLA4/Opgaverne/df_mercedes_OLA_v2.rds")

#samme bil kan stå på to sider, så vi beholder kun én række pr. carid
df_mercedes_OLA <- distinct(df_mercedes_OLA, carid, .keep_all = TRUE)

#skal give 2836
nrow(df_mercedes_OLA)


# ---------- Forhandlerinfo ----------

#beskrivelsen er salgsteksten. Nogle forhandlere skriver mail og telefon i den, og dem henter vi med regulære udtryk
email <- str_extract(df_mercedes_OLA$beskrivelse, "\\S+@\\S+")
#regulært udtryk for mail= alt uden mellemrum før og efter @

#telefon: \\b = her starter eller slutter et tal (kan også være ord hihihihih)
#[2-9] = første ciffer er fra 2-9, \\d = næste ciffer er fra 0-9
#mellemrum + ? = mellemrummet må være der 0 eller 1 gang
#\\d{2} = to cifre. Det gentages tre gange, så det i alt er 8 cifre

#\\b til sidst = tallet er slut, så vi ikke tager et stykke af et længere tal
telefon <- str_extract(df_mercedes_OLA$beskrivelse, "(\\+45 ?)?\\b[2-9]\\d ?\\d{2} ?\\d{2} ?\\d{2}\\b")

#fjerner +45 og mellemrum, så alle numre står som 8 cifre
telefon <- str_remove_all(telefon, "\\+45|\\s")

#NA hvor forhandleren ikke oplyser det
df_mercedes_OLA$email <- email
df_mercedes_OLA$telefon <- telefon
rm(email, telefon)

#tjek: omkring 659 mails og 952 numre udfyldt
colSums(is.na(df_mercedes_OLA))

#hvor mail og telefon mangler, bruger vi forhandlerlogoet som id. Et logo ses med:
#browseURL(df_mercedes_OLA$forhandler_logo[1])


# ---------- Pris ----------

#henter tallet (cifre og punktummer), fjerner tusindtalspunktummer og gør det til tal
df_mercedes_OLA <- df_mercedes_OLA %>%
  mutate(pris_kr = pris %>%
           str_extract("[\\d.]+") %>%
           str_remove_all("\\.") %>%
           as.numeric())

#tjek: ingen NA, og priserne skal ligge fornuftigt
sum(is.na(df_mercedes_OLA$pris_kr))
summary(df_mercedes_OLA$pris_kr)
