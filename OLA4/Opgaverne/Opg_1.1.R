df_mercedes_OLA <- readRDS("~/Dataanalyse/StudygroupYAYAYA/OLA4/Opgaverne/df_mercedes_OLA_v2.rds")

#samme bil kan stå på to sider, så vi beholder kun én række pr. carid
df_mercedes_OLA <- distinct(df_mercedes_OLA, carid, .keep_all = TRUE)

nrow(df_mercedes_OLA)
#It is done
#succes!

df_mercedes_OLA$beskrivelse[12]

# ---------- Der undersøges ang forhandler info i beskrivelsen ----------

#hvor mange beskrivelser indeholder Kontakt os, uanset store og små bogstaver
sum(str_detect(df_mercedes_OLA$beskrivelse, regex("Køb hos", ignore_case = TRUE)), na.rm = TRUE)
sum(str_detect(df_mercedes_OLA$beskrivelse, regex("Kontakt os", ignore_case = TRUE)), na.rm = TRUE)
sum(str_detect(df_mercedes_OLA$beskrivelse, regex("Ring til os", ignore_case = TRUE)), na.rm = TRUE)
#hvor mange biler har slet ingen beskrivelse
sum(is.na(df_mercedes_OLA$beskrivelse))

#er teksten afkortet af Bilbasen? Så ligger max på et fast tal
summary(nchar(df_mercedes_OLA$beskrivelse))

#hvor mange har en mail i teksten, uanset hvordan de indleder
email <- str_extract(df_mercedes_OLA$beskrivelse, "[[:alnum:]._-]+@[[:alnum:].-]+\\.[a-z]+")
sum(!is.na(email))

nrow(distinct(df_mercedes_OLA, forhandler_logo, sted))


#hvor mange biler har enten logo eller mail i teksten
sum(!is.na(df_mercedes_OLA$forhandler_logo) | !is.na(email))

#henter det sidste Kontakt os og alt efter det
forhandler_tekst <- str_match(df_mercedes_OLA$beskrivelse, "(?s).*(Kontakt os.*)")[, 2]

#10 tilfældige, så vi ser variationen
set.seed(1)
sample(na.omit(forhandler_tekst), 10)

#8 cifre, med eller uden mellemrum, og evt. +45 foran
telefon <- str_extract(df_mercedes_OLA$beskrivelse, "(\\+45 ?)?\\d{2} ?\\d{2} ?\\d{2} ?\\d{2}")

#hvor mange biler har et nummer
sum(!is.na(telefon))

#10 tilfældige, så vi tjekker, at det ikke er priser eller andet pjat
set.seed(1)
sample(na.omit(telefon), 10)

#første ciffer 2-9, og \\b sikrer at tallet ikke er et stykke af et længere tal
telefon <- str_extract(df_mercedes_OLA$beskrivelse, "(\\+45 ?)?\\b[2-9]\\d ?\\d{2} ?\\d{2} ?\\d{2}\\b")

sum(!is.na(telefon))

i <- which(telefon == "53430902")[1]
str_extract(df_mercedes_OLA$beskrivelse[i], ".{40}53430902.{20}")

#fjerner +45 og mellemrum, så alle numre står som 8 cifre
telefon <- str_remove_all(telefon, "\\+45|\\s")

#sætter dem ind som kolonner. NA hvor forhandleren ikke oplyser det
df_mercedes_OLA$telefon <- telefon
df_mercedes_OLA$email <- email

#tjek
colSums(is.na(df_mercedes_OLA))

#henter tallet (cifre og punktummer), fjerner tusindtalspunktummer og gør det til tal
df_mercedes_OLA <- df_mercedes_OLA %>%
  mutate(pris_kr = pris %>%
           str_extract("[\\d.]+") %>%
           str_remove_all("\\.") %>%
           as.numeric())

#tjek: ingen NA, og priserne skal ligge fornuftigt
sum(is.na(df_mercedes_OLA$pris_kr))
summary(df_mercedes_OLA$pris_kr)
