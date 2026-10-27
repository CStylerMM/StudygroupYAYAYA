#casper hiver opgave 3 ud og finder ud af hvordan vi forklarer det.
#Hvad skal jeg bruge tho

library(dkstat)
#Forbrugertilliden (FORV1) kommer månedligt, så den skal laves om til kvartaler
forv_meta <- dst_meta(table = "FORV1", lang = "da")

#Vi henter alle indikatorer og alle måneder
FTIALLEINDI <- dst_get_data(
  table = "FORV1",
  query = list(INDIKATOR = "*", Tid = "*"),
  meta_data = forv_meta,
  lang = "da"
)
#1) Julehandel pr. år: oktober for de otte julegrupper i NKHC021
forbrug_meta <- dst_meta(table = "NKHC021", lang = "da")

forbrug_faste <- dst_get_data(
  table = "NKHC021",
  query = list(FORMAAAL = "*",
               PRISENHED = "2020-priser, kædede værdier",
               SÆSON = "Ikke sæsonkorrigeret",
               Tid = "*"),
  meta_data = forbrug_meta,
  lang = "da"
)

#Fra lang til bred: én kolonne pr. forbrugsgruppe
df_forbrug_bred <- data.frame(TID = forbrug_faste$TID[forbrug_faste$FORMAAAL == forbrug_faste$FORMAAAL[1]])
for (f in unique(forbrug_faste$FORMAAAL)) {
  df_forbrug_bred[[f]] <- forbrug_faste$value[forbrug_faste$FORMAAAL == f]
}

#Fra 1999, så vi har et år før 2000 og kan regne vækst for 2000
df_forbrug_bred <- df_forbrug_bred[df_forbrug_bred$TID >= as.Date("1999-01-01"), ]
df_K4 <- df_forbrug_bred[format(df_forbrug_bred$TID, "%m") == "10", ]

julegoder <- names(df_forbrug_bred)[substr(names(df_forbrug_bred), 1, 3) %in%
                                      c("CPA", "CPB", "CPC", "CPF", "CPJ", "CPK", "CPM", "CPO")]

#De otte grupper lægges sammen til ét tal pr. år
df_K4$julehandel <- rowSums(df_K4[, julegoder])

#Vækst i forhold til året før
df_K4$vækst   <- c(NA, diff(log(df_K4$julehandel)) * 100)
df_K4$retning <- ifelse(df_K4$vækst >= 0, "OP", "NED")
df_K4$År      <- as.numeric(format(df_K4$TID, "%Y"))

#2) Forbrugertillid. Kun K3 (juli til september) bruges
forv_F1 <- dst_get_data(
  table = "FORV1",
  query = list(INDIKATOR = "Forbrugertillidsindikatoren", Tid = "*"),
  meta_data = forv_meta,
  lang = "da"
)

forv_F1 <- forv_F1[forv_F1$TID >= as.Date("2000-01-01"), ]
forv_F1$År      <- as.numeric(format(forv_F1$TID, "%Y"))
forv_F1$kvartal <- (as.numeric(format(forv_F1$TID, "%m")) - 1) %/% 3 + 1

#Gennemsnit af de tre måneder i hvert kvartal
df_forv_kvartal <- aggregate(value ~ År + kvartal, data = forv_F1, FUN = mean)
df_forv_K3      <- df_forv_kvartal[df_forv_kvartal$kvartal == 3, ]

tillid_ny <- df_forv_K3$value[df_forv_K3$År == 2026]

#3) Model: retning forklaret af tilliden i K3
df_model <- merge(df_K4[, c("År", "julehandel", "vækst", "retning")],
                  df_forv_K3[, c("År", "value")],
                  by = "År")
names(df_model)[names(df_model) == "value"] <- "tillid"
df_model <- df_model[df_model$År >= 2000, ]

glm_julehandel <- glm(as.factor(retning) ~ tillid, data = df_model, family = "binomial")
summary(glm_julehandel)

lm_julehandel <- lm(vækst ~ tillid, data = df_model)   #samme X, men Y er væksten i %
summary(lm_julehandel)
vækst_2026 <- predict(lm_julehandel, newdata = data.frame(tillid = tillid_ny))   #bud på væksten i 2026
vækst_2026
#4) Validering: tag ét år ud ad gangen, træn på resten, og gæt året vi tog ud
df_model$p_uden_aaret <- NA
for (i in 1:nrow(df_model)) {
  glm_uden_i <- glm(as.factor(retning) ~ tillid, data = df_model[-i, ], family = "binomial")
  df_model$p_uden_aaret[i] <- predict(glm_uden_i, newdata = df_model[i, ], type = "response")
}

#Over 0,5 gætter vi OP
forudsagt <- ifelse(df_model$p_uden_aaret > 0.5, "OP", "NED")

konfusion <- table(forudsagt = factor(forudsagt, levels = c("OP", "NED")),
                   faktisk   = factor(df_model$retning, levels = c("OP", "NED")))
konfusion

TP <- konfusion["OP", "OP"]
TN <- konfusion["NED", "NED"]
FP <- konfusion["OP", "NED"]
FN <- konfusion["NED", "OP"]

(TP + TN) / sum(konfusion)                      #andel rigtige gæt
TP / (TP + FN)                                  #sensitivitet
TN / (TN + FP)                                  #specificitet
max(table(df_model$retning)) / nrow(df_model)   #baseline

#5) Gættet for 2026
p_ny_aar <- predict(glm_julehandel, newdata = data.frame(tillid = tillid_ny), type = "response")
ifelse(p_ny_aar > 0.5, "OP", "NED")

#6) Tal til aflevering
table(df_model$retning)
mean(df_model$retning == "OP")
df_model$År[df_model$retning == "NED"]
summary(glm_julehandel)$coefficients
round(p_ny_aar * 100)
df_model[df_model$År == 2025, c("retning", "p_uden_aaret")]
df_model[df_model$År %in% c(2005, 2020, 2022, 2025), c("År", "tillid", "retning")]
tillid_ny
mean(df_model$tillid)

## Kør linjen og skriv de nye tal ind i rapporten

#4) Validering: tag ét år ud ad gangen, træn på resten, og gæt året vi tog ud
df_model$p_uden_aaret <- NA
for (i in 1:nrow(df_model)) {
  glm_uden_i <- glm(as.factor(retning) ~ tillid, data = df_model[-i, ], family = "binomial")
  df_model$p_uden_aaret[i] <- predict(glm_uden_i, newdata = df_model[i, ], type = "response")
}

#Over 0,5 gætter vi OP
forudsagt <- ifelse(df_model$p_uden_aaret > 0.5, "OP", "NED")

konfusion <- table(forudsagt = factor(forudsagt, levels = c("OP", "NED")),
                   faktisk   = factor(df_model$retning, levels = c("OP", "NED")))
konfusion

TP <- konfusion["OP", "OP"]
TN <- konfusion["NED", "NED"]
FP <- konfusion["OP", "NED"]
FN <- konfusion["NED", "OP"]

(TP + TN) / sum(konfusion)    #andel rigtige gæt
TP / (TP + FN)                #sensitivitet
TN / (TN + FP)                #specificite t
max(table(df_model$retning)) / nrow(df_model)   #baseline: altid gæt på det hyppigste

#5) Gættet for 2026
p_ny_aar <- predict(glm_julehandel, newdata = data.frame(tillid = tillid_ny), type = "response")
p_ny_aar   #sandsynlighed for at julehandlen i 2026 er større end i 2025
ifelse(p_ny_aar > 0.5, "OP", "NED")

table(df_model$retning)
mean(df_model$retning == "OP")
df_model$År[df_model$retning == "NED"]
addmargins(konfusion)


#Opgave 3: holder vores 8 julegrupper? Vi tjekker om K4 stikker af de sidste 5 år

library(dkstat)   #tid til at bruge dkstat

nkhc021_meta <- dst_meta(table = "NKHC021", lang = "da")   #henter metadata én gang
nkhc021_meta$variables   #tjek navnet på formål-variablen, ret FORMAAL nedenfor hvis den hedder noget andet
formål <- nkhc021_meta$values[[1]]   #listen du lige printede, med id og tekst
names(nkhc021_meta$values)   #første navn er formål-variablen
julegrupper <- formål$text[formål$id %in% c("CPA", "CPB", "CPC", "CPF", "CPJ", "CPK", "CPM", "CPO")]   #vælger vores 8 ud fra id

julegrupper   #tjek at du får de 8 rigtige navne
julegrupper <- c(
  "Fødevarer mv.",
  "Drikkevarer og tobak mv.",
  "Beklædning og fodtøj",
  "Boligudstyr, husholdningstjenester mv.",
  "Information og kommunikation",
  "Fritid, sport og kultur",
  "Restauranter og hoteller",
  "Andre varer og tjenester"
)   #vores 8 grupper, stavet præcis som DST staver dem

forbrug_raw <- dst_get_data(
  table = "NKHC021",
  query = list(
    FORMAAAL  = julegrupper,   #DST staver den med tre A'er, ikke en fejl fra os
    PRISENHED = "2020-priser, kædede værdier",   #faste priser, så inflation ikke snyder
    SÆSON     = "Ikke sæsonkorrigeret",   #vigtigt! ellers er julen glattet væk
    Tid       = "*"
  ),
  meta_data = nkhc021_meta,
  lang = "da"
)

str(forbrug_raw$FORMAAAL)   #tjek at vi har 8 grupper og kvartaler

library(tidyverse)   #tid til at bruge tidyverse

df_julehandel_kvartal <- forbrug_raw %>%
  rename(kvartal = TID, mio_kr = value) %>%
  mutate(kvartal = as.Date(kvartal, tz = "Europe/Copenhagen")) %>%   #dansk tid, ellers ryger vi en dag tilbage
  group_by(kvartal) %>%
  summarise(julehandel = sum(mio_kr)) %>%   #lægger de 8 grupper sammen pr. kvartal
  filter(kvartal >= as.Date("2021-01-01"), kvartal <= as.Date("2025-12-01")) %>%   #de sidste 5 hele år
  mutate(K4 = if_else(quarter(kvartal) == 4, "K4", "K1-K3"))   #markerer K4

df_julehandel_kvartal   #tjek: 20 rækker, 4 pr. år

df_julehandel_kvartal <- df_julehandel_kvartal %>%
  mutate(kvartal_tekst = paste0(year(kvartal), " K", quarter(kvartal)))   #fx "2021 K4" i stedet for en dato

ggplot(df_julehandel_kvartal, aes(x = julehandel / 1000, y = (kvartal_tekst), fill = K4)) +   #fct_rev sætter 2021 øverst
  geom_col() +
  scale_fill_manual(values = c("K1-K3" = "grey40", "K4" = "#E6007E")) +   #pink K4 som på sliden
  labs(
    title = "K4 er højest i 4 ud af 5 år",
    x = "Mia. kr. (2020-priser)", y = NULL, fill = NULL,
    caption = "Julen kan ses i de 8 grupper, men 2022 er undtagelsen, hvor K2 slår K4.\nKilde: Danmarks Statistik, NKHC021 (ikke sæsonkorrigeret)"
  ) +
  theme_classic()
#her er fti sat sammen med julehandel hihi
ggplot(df_model, aes(x = År)) +
  geom_col(aes(y = vækst * scale_factor, fill = retning)) +   #søjler: vækst i julehandlen
  geom_line(aes(y = tillid), linewidth = 1) +   #linje: forbrugertillid i K3
  geom_hline(yintercept = 0, color = "grey40") +
  scale_x_continuous(
    breaks = 2000:2025,   #alle årstal på aksen
    expand = expansion(add = 0.5)   #kun en halv søjle luft i siderne, så grafen starter ved aksen
  ) +
  scale_y_continuous(
    name = "Forbrugertillid, gns. juli-september",
    sec.axis = sec_axis(~ . / scale_factor, name = "Vækst i julehandlen, %", breaks = seq(-8, 8, 2))   #flere tal på højre akse
  ) +
  scale_fill_manual(values = c("OP" = "#6AA84F", "NED" = "#E06666")) +
  labs(
    title = "Forbrugertillid og julehandel, 2000-2025",
      subtitle = "Sort linje = forbrugertillid (venstre akse) · søjler = vækst i julehandlen (højre akse)",
    x = NULL, fill = "Julehandel",
    caption = "Når tilliden styrtdykker, som i 2008 og 2022, falder julehandlen. Men i 2025 steg den trods lav tillid.\nKilde: Danmarks Statistik, FORV1 og NKHC021"
  ) +
  theme_minimal() +   #som classic, men med gitter
  theme(
    panel.grid.minor = element_blank(),   #fjerner de små ekstra gitterlinjer
    axis.text.x = element_text(angle = 90, vjust = 0.5)   #drejer årstallene, så alle 26 kan være der
  )

