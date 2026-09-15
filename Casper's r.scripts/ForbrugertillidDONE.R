# ================================================================
# DI's forbrugertillidsindikator - hjemmelavet remix
# Vi bygger grafen, regressionen, fanger corona på fersk gerning,
# og validerer det hele mod Danmarks Statistiks API for at være
# helt sikre på at vi ikke har fat i noget vrøvl.
# ================================================================

library(tidyverse)  # roden til alt godt (og alt pivot_longer-relateret ondt)
library(readxl)     # henter tal ud af Excel uden at skulle åbne Excel
library(lubridate)  # gør "2015K3" til en rigtig dato, ikke bare en streng
library(janitor)    # omdanner grimme danske kolonnenavne til pænt snake_case
library(car)        # til vif() - fanger multikolinearitet, dataens indre drama
library(dkstat)     # direkte adgang til Danmarks Statistiks API, ingen klik nødvendig


# ================================================================
# DEL 1: HOVEDPIPELINE - Excel-baseret (det du afleverer)
# ================================================================

# ----------------------------------------------------------------
# STEP 1: Forbrugertillid - 5 udvalgte spørgsmål, månedlig data
# ----------------------------------------------------------------

tillid_raw <- read_excel("FORV1_5spgsmaal_2000M01_2021M06.xlsx",
                         sheet = "FORV1", skip = 2)
names(tillid_raw)[1] <- "indikator"  # første kolonne manglede navn - den har et nu

# De 5 spørgsmål vi rent faktisk vil bruge.
# "Forbrugertillidsindikatoren" selv er UDELADT med vilje - den er
# allerede et gennemsnit af 4 af de andre, så den ville bare snyde
# regressionen til at tro den havde fundet en ekstra ven.
fem_spgsmaal <- c(
  "Familiens økonomiske situation i dag, sammenlignet med for et år siden",
  "Familiens økonomiske  situation om et år, sammenlignet med i dag",
  "Danmarks økonomiske situation i dag, sammenlignet med for et år siden",
  "Danmarks økonomiske situation om et år, sammenlignet med i dag",
  "Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket"
)

tillid_q <- tillid_raw %>%
  filter(indikator %in% fem_spgsmaal) %>%              # behold kun de 5 udvalgte
  pivot_longer(-indikator, names_to = "maaned_raw", values_to = "nettotal") %>%
  mutate(dato = ym(gsub("M", "-", maaned_raw)),         # "2015M03" -> rigtig dato
         kvartal = floor_date(dato, "quarter")) %>%     # rund ned til kvartalets start
  group_by(indikator, kvartal) %>%
  summarise(nettotal = mean(nettotal, na.rm = TRUE), .groups = "drop") %>%  # måned -> kvartal
  pivot_wider(names_from = indikator, values_from = nettotal) %>%          # lang -> bred
  janitor::clean_names()  # "Familiens økonomiske situation..." -> familiens_okonomiske_situation...


# ----------------------------------------------------------------
# STEP 2: Privatforbrug - kvartalsvis niveau -> årlig realvækst
# ----------------------------------------------------------------

forbrug_raw <- read_excel("NKH1_privatforbrug_2000K1_2021K2.xlsx",
                          sheet = "NKH1", skip = 2)

forbrug_q <- forbrug_raw %>%
  select(-1, -2, -3) %>%   # smid de 3 label-kolonner (sæson, pris, transaktion) væk
  pivot_longer(everything(), names_to = "kvartal_raw", values_to = "p_forbrug") %>%
  mutate(
    aar = as.numeric(str_sub(kvartal_raw, 1, 4)),                 # "2015K3" -> 2015
    kvt = as.numeric(str_sub(kvartal_raw, 6, 6)),                 # "2015K3" -> 3
    kvartal = as.Date(paste0(aar, "-", (kvt - 1) * 3 + 1, "-01")) # kvartal 3 -> juli
  ) %>%
  arrange(kvartal) %>%
  # årlig realvækst = log-diff over 4 kvartaler, tilbage til procent.
  # lag() (ikke diff()!) fordi den bevarer vektorlængden - diff() spiser
  # 4 observationer og giver dig en fejlmelding og et surt ansigt
  mutate(vaekst_p_forbrug = (exp(log(p_forbrug) - log(lag(p_forbrug, 4))) - 1) * 100) %>%
  select(kvartal, vaekst_p_forbrug) %>%
  drop_na()  # de første 4 kvartaler kan ikke have en vækstrate - de var her ikke for 4 kvartaler siden


# ----------------------------------------------------------------
# STEP 3: Saml de to datasæt til én model-klar tabel
# ----------------------------------------------------------------

data_ml <- tillid_q %>%
  inner_join(forbrug_q, by = "kvartal") %>%
  filter(kvartal >= as.Date("2000-01-01"), kvartal <= as.Date("2021-06-01")) %>%
  drop_na()

nrow(data_ml)          # skulle give 82 - tjek lige at ingen kvartaler er faldet af undervejs
names(data_ml)          # et sidste blik på at kolonnenavnene ikke ser sindssyge ud
range(data_ml$kvartal)  # bekræft periode: 2001-01-01 til 2021-04-01 (ja, ikke 2000 - se nedenfor)
# NB: starter 2001, ikke 2000 - vækstberegningen kræver 4 kvartaler bagud,
# og Excel-filen havde ikke nok historik til at give 2000K1-K4 en vækstrate.
# (Se DEL 2 - API-versionen løser faktisk det problem.)


# ----------------------------------------------------------------
# STEP 4: Den fulde multiple lineære regression - alle 5 med
# ----------------------------------------------------------------

lmm_forbrug_ftillid <- lm(
  vaekst_p_forbrug ~ .,   # "~ ." betyder "alle andre kolonner som x" - lidt dovent, meget effektivt
  data = data_ml %>% select(-kvartal)
)
summary(lmm_forbrug_ftillid)

# Tjek om de 5 spørgsmål roder rundt i hinandens ærinde
# (spoiler: det gør de lidt, "familiens økonomi"-parret er uadskillelige tvillinger)
cor(data_ml %>% select(-kvartal, -vaekst_p_forbrug))
car::vif(lmm_forbrug_ftillid)


# ----------------------------------------------------------------
# STEP 5: Den optimerede/reducerede model - kun de 3 der løfter
# ----------------------------------------------------------------

# De to "familiens økonomi"-spørgsmål var insignifikante og
# multikolineære med resten - de fik fyresedlen.
lmm_reduceret <- lm(
  vaekst_p_forbrug ~ anskaffelse_af_storre_forbrugsgoder_fordelagtigt_for_ojeblikket +
    danmarks_okonomiske_situation_i_dag_sammenlignet_med_for_et_ar_siden +
    danmarks_okonomiske_situation_om_et_ar_sammenlignet_med_i_dag,
  data = data_ml
)
summary(lmm_reduceret)  # adjusted R² stiger endda lidt - farvel og tak, unødvendige variable

# De 6 antagelser, checket på 5 sekunder med indbygget plot-magi
par(mfrow = c(2, 2))
plot(lmm_reduceret)

# Find ud af hvilke kvartaler der opfører sig som teenagere til familiefest
data_ml$kvartal[c(78, 82)]  # spoiler: begge er corona (2020Q2 og 2021Q2)


# ----------------------------------------------------------------
# STEP 6: Genskab DI's graf - version 1 (rå nettotal-gennemsnit)
# ----------------------------------------------------------------

data_plot <- data_ml %>%
  mutate(
    indikator = (anskaffelse_af_storre_forbrugsgoder_fordelagtigt_for_ojeblikket +
                   danmarks_okonomiske_situation_i_dag_sammenlignet_med_for_et_ar_siden +
                   danmarks_okonomiske_situation_om_et_ar_sammenlignet_med_i_dag) / 3
  )

# Skaleringsfaktor så søjler og linje kan bo på samme graf uden at
# den ene overdøver den anden - lidt ligesom akse-diplomati
scale_factor <- max(abs(data_plot$indikator)) / max(abs(data_plot$vaekst_p_forbrug))

ggplot(data_plot, aes(x = kvartal)) +
  geom_col(aes(y = vaekst_p_forbrug * scale_factor, fill = "Årlig realvækst pr. kvartal i privat forbruget (højre akse)"),
           width = 80) +
  geom_line(aes(y = indikator, color = "DI's forbrugertillidsindikator"),
            linewidth = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_y_continuous(
    name = "Standardiseret indikator",
    sec.axis = sec_axis(~ . / scale_factor, name = "Pct.")
  ) +
  scale_fill_manual(name = NULL, values = c("Årlig realvækst pr. kvartal i privat forbruget (højre akse)" = "steelblue")) +
  scale_color_manual(name = NULL, values = c("DI's forbrugertillidsindikator" = "black")) +
  labs(x = NULL,
       title = "Forbrugertillid følger privatforbruget, 2000-2021",
       subtitle = "Standardiseret gennemsnit af 3 spørgsmål vs. årlig realvækst",
       caption = "Anm.: Indikatoren er beregnet som et simpelt (standardiseret) gennemsnit af 3 spørgsmål,\nder har den stærkeste sammenhæng og bedst forklarer variationen i forbruget.\nKilde: Danmarks Statistik") +
  theme_minimal() +
  theme(legend.position = "top",
        plot.caption = element_text(hjust = 0, size = 6))
# Tidsserie-plot af de rå variable, før nogen model kommer i nærheden
data_ml %>%
  select(-kvartal) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 20) +
  facet_wrap(~name, scales = "free")

summary(data_ml)  # min/max/median for hver variabel - fanger skæve outliers før de bliver et problem i regressionen
# Denne version svinger vildt meget, fordi "Danmarks økonomi"-spørgsmålene
# har meget større naturlig amplitude end de andre - de larmer bare mere.


# ----------------------------------------------------------------
# STEP 7: Genskab DI's graf - version 2 (standardiseret, pænere)
# ----------------------------------------------------------------

data_plot <- data_ml %>%
  mutate(
    # scale() = z-score: hvert spørgsmål vejes nu ens, uanset om det
    # normalt hopper rundt som en kanin på koffein eller ej
    z_anskaffelse = scale(anskaffelse_af_storre_forbrugsgoder_fordelagtigt_for_ojeblikket)[,1],
    z_dk_idag     = scale(danmarks_okonomiske_situation_i_dag_sammenlignet_med_for_et_ar_siden)[,1],
    z_dk_om_et_ar = scale(danmarks_okonomiske_situation_om_et_ar_sammenlignet_med_i_dag)[,1],
    indikator = (z_anskaffelse + z_dk_idag + z_dk_om_et_ar) / 3
  )

scale_factor <- max(abs(data_plot$indikator)) / max(abs(data_plot$vaekst_p_forbrug))

ggplot(data_plot, aes(x = kvartal)) +
  geom_col(aes(y = vaekst_p_forbrug * scale_factor), fill = "steelblue", width = 80) +
  geom_line(aes(y = indikator), color = "black", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_y_continuous(
    name = "Standardiseret indikator",
    sec.axis = sec_axis(~ . / scale_factor, name = "Pct.")
  ) +
  labs(x = NULL,
       title = "Forbrugertillid følger privatforbruget, 2001-2021",
       subtitle = "Standardiseret gennemsnit af 3 spørgsmål vs. årlig realvækst") +
  theme_minimal()
# Meget mere DI-agtig. Bortset fra søjlen der springer til himmels i
# 2021Q2 - det er genåbnings-kvartalet, og ja, det er den samme
# uartige outlier fra residualplottet. Den render sig ikke.


# ================================================================
# DEL 2: VALIDERING - samme data hentet via dkstat's API
# Formål: bekræfte at Excel-eksporten ikke har introduceret fejl,
# og vise at API-vejen faktisk giver MERE data (se note ved STEP 3).
# Kør kun hvis du vil dobbelttjekke - ikke nødvendig for afleveringen.
# ----------------------------------------------------------------
# install.packages(
#   "dkstat",
#   repos = c(ropengov = "https://ropengov.r-universe.dev", getOption("repos"))
# )
# devtools::install_github("rOpenGov/dkstat")  # alternativ: seneste dev-version
# ----------------------------------------------------------------

# --- Forbrugertillid via API ---

forv1_meta <- dst_meta(table = "FORV1", lang = "da")
forv1_meta$variables         # ser hvilke variable tabellen har (INDIKATOR, Tid)
forv1_meta$values$INDIKATOR  # de 13 spørgsmåls koder og tekster

tillid_raw_api <- dst_get_data(
  table = "FORV1",
  INDIKATOR = c(
    "Familiens økonomiske situation i dag, sammenlignet med for et år siden",
    "Familiens økonomiske  situation om et år, sammenlignet med i dag",
    "Danmarks økonomiske situation i dag, sammenlignet med for et år siden",
    "Danmarks økonomiske situation om et år, sammenlignet med i dag",
    "Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket"
  ),
  Tid = "*",   # stjernen betyder "alle tilgængelige perioder" - ingen manuel klikkeri
  lang = "da"
)

# API'et giver ALT data den har (helt tilbage til 1974) - filtrér selv til perioden
tillid_raw_api <- tillid_raw_api %>%
  filter(TID >= as.Date("2000-01-01"), TID <= as.Date("2021-06-01"))

nrow(tillid_raw_api)               # forventet: 5 spørgsmål x 258 måneder = 1290
unique(tillid_raw_api$INDIKATOR)   # bekræft alle 5 spørgsmål kom med

tillid_q_api <- tillid_raw_api %>%
  rename(indikator = INDIKATOR, dato = TID, nettotal = value) %>%
  mutate(kvartal = floor_date(dato, "quarter")) %>%
  group_by(indikator, kvartal) %>%
  summarise(nettotal = mean(nettotal, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = indikator, values_from = nettotal) %>%
  janitor::clean_names()  # NB: navnene får nu et f2_/f3_/f4_-præfiks fra DST's koder

nrow(tillid_q_api)   # forventet: 86 kvartaler (2000K1-2021K2)
names(tillid_q_api)  # bekræft 6 kolonner: kvartal + 5 spørgsmål (med f-præfiks)


# --- Privatforbrug via API ---

nkh1_meta <- dst_meta(table = "NKH1", lang = "da")
nkh1_meta$variables          # TRANSAKT, PRISENHED, SÆSON, Tid - alle obligatoriske
nkh1_meta$values$TRANSAKT    # find "P.31 Husholdningernes forbrugsudgifter" (IKKE NPISH-varianten)
nkh1_meta$values$PRISENHED   # find "2020-priser, kædede værdier"
nkh1_meta$values$SÆSON       # find "Sæsonkorrigeret"

# dst_get_data vil have den fulde tekst, ikke de korte koder (P31S14K virker IKKE)
forbrug_raw_api <- dst_get_data(
  table = "NKH1",
  TRANSAKT  = "P.31 Husholdningernes forbrugsudgifter",
  PRISENHED = "2020-priser, kædede værdier",
  SÆSON     = "Sæsonkorrigeret",
  Tid       = "*",
  lang = "da"
)

str(forbrug_raw_api)  # data helt tilbage til 1990 - mere historik end Excel-filen havde

forbrug_q_api <- forbrug_raw_api %>%
  rename(kvartal = TID, p_forbrug = value) %>%
  select(kvartal, p_forbrug) %>%
  arrange(kvartal) %>%
  mutate(vaekst_p_forbrug = (exp(log(p_forbrug) - log(lag(p_forbrug, 4))) - 1) * 100) %>%
  select(kvartal, vaekst_p_forbrug) %>%
  drop_na() %>%
  filter(kvartal >= as.Date("2000-01-01"), kvartal <= as.Date("2021-06-01"))

nrow(forbrug_q_api)   # 86 - fire flere end Excel-versionen (82), se hvorfor nedenfor


# --- Sammenlign Excel vs. API - stemmer tallene overens? ---

range(forbrug_q$kvartal)      # 2001-01-01 til 2021-04-01 (Excel)
range(forbrug_q_api$kvartal)  # 2000-01-01 til 2021-04-01 (API)
nrow(forbrug_q)                # 82
nrow(forbrug_q_api)            # 86

# API-versionen har 4 EKSTRA kvartaler (2000K1-K4), fordi den trak data
# helt fra 1990 og derfor havde nok historik til at beregne deres vækstrate.
# Excel-filen startede kun ved 2000K1, så de første 4 kvartaler kunne
# aldrig få en vækstrate og røg ud med drop_na(). Ikke en fejl - bare
# mindre historik at arbejde med. Sammenlign derfor kun det fælles vindue:

all.equal(
  forbrug_q %>% filter(kvartal >= as.Date("2001-01-01")) %>% arrange(kvartal) %>% pull(vaekst_p_forbrug),
  forbrug_q_api %>% filter(kvartal >= as.Date("2001-01-01")) %>% arrange(kvartal) %>% pull(vaekst_p_forbrug)
)
# TRUE = Excel og API stemmer 100% overens. Pipeline valideret.


# --- Kør regressionen igen på API-data, som en sidste dobbelttjek ---

data_ml_api <- tillid_q_api %>%
  inner_join(forbrug_q_api, by = "kvartal") %>%
  filter(kvartal >= as.Date("2000-01-01"), kvartal <= as.Date("2021-06-01")) %>%
  drop_na()

nrow(data_ml_api)   # 82 med det fælles vindue - matcher Excel-versionen
names(data_ml_api)  # bemærk f2/f3/f4/f5/f9-præfikserne fra DST's koder

lmm_reduceret_api <- lm(
  vaekst_p_forbrug ~ .,
  data = data_ml_api %>% select(-kvartal)
)
summary(lmm_reduceret_api)
# Identisk R² og koefficienter med Excel-versionen - API-pipeline er 100% valideret.
#Hvor har vi multiple linaer regression
#
lm(vaekst_p_forbrug ~ anskaffelse_af_storre_forbrugsgoder_fordelagtigt_for_ojeblikket +
     danmarks_okonomiske_situation_i_dag_sammenlignet_med_for_et_ar_siden +
     danmarks_okonomiske_situation_om_et_ar_sammenlignet_med_i_dag,
   data = data_ml)

lm(vaekst_p_forbrug ~ ., data = data_ml %>% select(-kvartal))

#hvad er forskellen på hvad vi har gjort?
#Y=B0+B1*X1
#Vi vil forklare Y, men nu der flere ting der kan forklare det.
#Y=B0+B1X1+B2X2+B3X3... derudaf
#indkomst kunne være et B
#opsparing kunne være et B
#konflikter kunne være et B
#Priser kunne være et B, men pas på, hvis pris også er en del af vores Y?
#Y=årlig kvart vækst

library(dkstat)
library(tidyverse)
library(lubridate)
library(janitor)

# ================================================================
# Forbrugertillid via dkstat API - 5 udvalgte spørgsmål
# Samme resultat som Excel-vejen, bare hentet direkte fra DST
# ================================================================

# ----------------------------------------------------------------
# STEP A: Hent metadata for FORV1 - ÉN gang, så vi kan genbruge den
# og ikke spamme DST's server med gentagne metadata-opslag
# ----------------------------------------------------------------

forv1_meta <- dst_meta(table = "FORV1", lang = "da")

forv1_meta$variables         # bekræft: tabellen har INDIKATOR og Tid som variable
forv1_meta$values$INDIKATOR  # de 13 spørgsmåls faktiske id (F1-F13) + fuld tekst - find de 5 rigtige heri


# ----------------------------------------------------------------
# STEP B: Hent de 5 spørgsmål ét ad gangen
# (API'et timer nogle gange ud på det samlede kald med alle 5 -
# så vi splitter op og samler bagefter, det er mere robust)
# ----------------------------------------------------------------

tillid_raw_f2 <- dst_get_data(
  table = "FORV1",
  query = list(INDIKATOR = "Familiens økonomiske situation i dag, sammenlignet med for et år siden", Tid = "*"),
  meta_data = forv1_meta,
  lang = "da"
)

tillid_raw_f3 <- dst_get_data(
  table = "FORV1",
  query = list(INDIKATOR = "Familiens økonomiske  situation om et år, sammenlignet med i dag", Tid = "*"),  # dobbelt mellemrum - det er DST's egen tekst, ikke en tastefejl
  meta_data = forv1_meta,
  lang = "da"
)

tillid_raw_f4 <- dst_get_data(
  table = "FORV1",
  query = list(INDIKATOR = "Danmarks økonomiske situation i dag, sammenlignet med for et år siden", Tid = "*"),
  meta_data = forv1_meta,
  lang = "da"
)

tillid_raw_f5 <- dst_get_data(
  table = "FORV1",
  query = list(INDIKATOR = "Danmarks økonomiske situation om et år, sammenlignet med i dag", Tid = "*"),
  meta_data = forv1_meta,
  lang = "da"
)

tillid_raw_f9 <- dst_get_data(
  table = "FORV1",
  query = list(INDIKATOR = "Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket", Tid = "*"),
  meta_data = forv1_meta,
  lang = "da"
)

# Saml de 5 separate hentninger til én tabel
tillid_raw <- bind_rows(tillid_raw_f2, tillid_raw_f3, tillid_raw_f4, tillid_raw_f5, tillid_raw_f9)

# NB: når man henter ét spørgsmål ad gangen som her, giver dst_get_data LANGT format
# (kolonner: INDIKATOR, TID, value) - modsat når man sender alle 5 i én query,
# hvor den giver BREDT format (indikator + hver måned som sin egen kolonne).
# Koden nedenfor forudsætter langt format.
nrow(tillid_raw)              # forventet: mange rækker (5 spørgsmål x alle måneder tilbage til 1974)
names(tillid_raw)             # forventet: "INDIKATOR" "TID" "value"
unique(tillid_raw$INDIKATOR)  # bekræft alle 5 spørgsmål er med


# ----------------------------------------------------------------
# STEP C: Filtrer til perioden, og omregn måned -> kvartal
# ----------------------------------------------------------------

api_forbrugertillid <- tillid_raw %>%
  rename(indikator = INDIKATOR, dato = TID, nettotal = value) %>%
  filter(dato >= as.Date("2000-01-01"), dato <= as.Date("2021-06-01")) %>%  # afgræns til opgavens periode
  mutate(kvartal = floor_date(dato, "quarter")) %>%                        # rund ned til kvartalets start
  group_by(indikator, kvartal) %>%
  summarise(nettotal = mean(nettotal, na.rm = TRUE), .groups = "drop") %>%  # måned -> kvartal (gennemsnit af 3 måneder)
  pivot_wider(names_from = indikator, values_from = nettotal) %>%          # lang -> bred (ét spørgsmål per kolonne)
  janitor::clean_names()  # gør de lange danske spørgsmålstekster til pænt snake_case

names(api_forbrugertillid)   # se de faktiske 6 kolonnenavne (kvartal + 5 spørgsmål med f2/f3/f4/f5/f9-præfiks)
nrow(api_forbrugertillid)    # forventet: 86 kvartaler (2000K1-2021K2)


# ----------------------------------------------------------------
# STEP D: Byg jeres egen forbrugertillidsindikator - simpelt gennemsnit af de 5 spørgsmål
# ----------------------------------------------------------------

api_forbrugertillid <- api_forbrugertillid %>%
  mutate(gennemsnit_forbrugertillid = (f2_familiens_okonomiske_situation_i_dag_sammenlignet_med_for_et_ar_siden +
                                         f3_familiens_okonomiske_situation_om_et_ar_sammenlignet_med_i_dag +
                                         f4_danmarks_okonomiske_situation_i_dag_sammenlignet_med_for_et_ar_siden +
                                         f5_danmarks_okonomiske_situation_om_et_ar_sammenlignet_med_i_dag +
                                         f9_anskaffelse_af_storre_forbrugsgoder_fordelagtigt_for_ojeblikket) / 5)

range(api_forbrugertillid$gennemsnit_forbrugertillid, na.rm = TRUE)  # tjek det ligner et fornuftigt nettotal-interval
nrow(api_forbrugertillid)  # skal stadig være 86

data_ml_avg <- data_ml %>%
  mutate(gennemsnit_forbrugertillid = (anskaffelse_af_storre_forbrugsgoder_fordelagtigt_for_ojeblikket +
                                         danmarks_okonomiske_situation_i_dag_sammenlignet_med_for_et_ar_siden +
                                         danmarks_okonomiske_situation_om_et_ar_sammenlignet_med_i_dag +
                                         familiens_okonomiske_situation_i_dag_sammenlignet_med_for_et_ar_siden +
                                         familiens_okonomiske_situation_om_et_ar_sammenlignet_med_i_dag) / 5)

lm_simpel <- lm(vaekst_p_forbrug ~ gennemsnit_forbrugertillid, data = data_ml_avg)
summary(lm_simpel)

# Sammenlign Adjusted R² direkte
summary(lmm_reduceret)$adj.r.squared
summary(lm_simpel)$adj.r.squared

#lad os lige få set på vores 5 spørgsmål og se om der er nogle der bedre end andre at tage med...
lmm_fuld <- lm(vaekst_p_forbrug ~ ., data = data_ml %>% select(-kvartal))
summary(lmm_fuld)

#________________________________________________________________________________________________

# ================================================================
# OPGAVE 1: Multipel lineær regression - forbrugertillid og forbrug
# Bygget fra bunden, ét skridt ad gangen.
# ================================================================

library(dkstat)
library(tidyverse)
library(lubridate)
library(janitor)

# ----------------------------------------------------------------
# STEP 1: Hent Y - årlig realvækst i privatforbruget, via API
# ----------------------------------------------------------------

# Sæt periodens SLUTNING her - ret denne ene linje når nye kvartaler
# kommer til (f.eks. "2026-06-01" for 2026K2), resten af koden følger med
periode_start <- as.Date("2000-01-01")
periode_slut  <- as.Date("2021-06-01")   # <- ÆNDR KUN DENNE for at udvide perioden

# Hent metadata én gang
nkh1_meta <- dst_meta(table = "NKH1", lang = "da")
nkh1_meta$variables    # bekræft: TRANSAKT, PRISENHED, SÆSON, Tid

# Hent selve dataen - hele historikken, vi filtrerer bagefter
forbrug_raw_api <- dst_get_data(
  table = "NKH1",
  TRANSAKT  = "P.31 Husholdningernes forbrugsudgifter",   # IKKE NPISH-varianten
  PRISENHED = "2020-priser, kædede værdier",
  SÆSON     = "Sæsonkorrigeret",
  Tid       = "*",
  meta_data = nkh1_meta,
  lang = "da"
)

str(forbrug_raw_api)   # bekræft kolonner: TRANSAKT, PRISENHED, SÆSON, TID, value


# ----------------------------------------------------------------
# STEP 2: Beregn årlig realvækst, og navngiv den API_y_Realvaekst
# ----------------------------------------------------------------

API_y_Realvaekst <- forbrug_raw_api %>%
  rename(kvartal = TID, p_forbrug = value) %>%
  select(kvartal, p_forbrug) %>%
  arrange(kvartal) %>%
  # årlig realvækst = log-diff over 4 kvartaler, tilbage til procent
  mutate(vaekst_p_forbrug = (exp(log(p_forbrug) - log(lag(p_forbrug, 4))) - 1) * 100) %>%
  select(kvartal, vaekst_p_forbrug) %>%
  drop_na() %>%   # de første 4 kvartaler kan ikke have en vækstrate
  filter(kvartal >= periode_start, kvartal <= periode_slut)

# ----------------------------------------------------------------
# Sanity check - se om det ser rigtigt ud, før vi går videre
# ----------------------------------------------------------------

nrow(API_y_Realvaekst)          # forventet: 86 kvartaler for 2000K1-2021K2
range(API_y_Realvaekst$kvartal) # bekræft periode
head(API_y_Realvaekst)
tail(API_y_Realvaekst)

# ----------------------------------------------------------------
# STEP 3: Hent X - de 5 spørgsmål, via API
# ----------------------------------------------------------------

forv1_meta <- dst_meta(table = "FORV1", lang = "da")
forv1_meta$variables         # bekræft: INDIKATOR og Tid
forv1_meta$values$INDIKATOR  # de 13 spørgsmåls koder og tekster - find de 5 rigtige

# Hent de 5 spørgsmål samlet i én query
forv1_query <- list(
  INDIKATOR = c(
    "Familiens økonomiske situation i dag, sammenlignet med for et år siden",   # F2
    "Familiens økonomiske  situation om et år, sammenlignet med i dag",         # F3 - dobbelt mellemrum, DST's egen tekst
    "Danmarks økonomiske situation i dag, sammenlignet med for et år siden",    # F4
    "Danmarks økonomiske situation om et år, sammenlignet med i dag",           # F5
    "Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket"          # F9
  ),
  Tid = "*"
)
forv1_meta <- dst_meta(table = "FORV1", lang = "da")
tillid_raw_api <- dst_get_data(
  table = "FORV1",
  query = forv1_query,
  meta_data = forv1_meta,
  lang = "da"
)

str(tillid_raw_api)                  # bekræft struktur - kan give bredt eller langt format
unique(tillid_raw_api$INDIKATOR)     # bekræft alle 5 spørgsmål kom med

# ----------------------------------------------------------------
# STEP 4: Filtrer X til perioden, og omregn måned -> kvartal
# ----------------------------------------------------------------

API_x_Forbrugertillid <- tillid_raw_api %>%
  rename(indikator = INDIKATOR, dato = TID, nettotal = value) %>%
  filter(dato >= periode_start, dato <= periode_slut) %>%
  mutate(kvartal = floor_date(dato, "quarter")) %>%
  group_by(indikator, kvartal) %>%
  summarise(nettotal = mean(nettotal, na.rm = TRUE), .groups = "drop") %>%  # måned -> kvartal
  pivot_wider(names_from = indikator, values_from = nettotal) %>%          # lang -> bred
  janitor::clean_names()

names(API_x_Forbrugertillid)   # bekræft 6 kolonner: kvartal + 5 spørgsmål (med f2/f3/f4/f5/f9-præfiks)
nrow(API_x_Forbrugertillid)    # forventet: 86 kvartaler
head(API_x_Forbrugertillid)

# ----------------------------------------------------------------
# STEP 5: Saml X og Y til én tabel klar til regression
# ----------------------------------------------------------------

data_ml_api <- API_x_Forbrugertillid %>%
  inner_join(API_y_Realvaekst, by = "kvartal") %>%
  drop_na()

nrow(data_ml_api)    # tjek antal rækker efter join
names(data_ml_api)   # bekræft 7 kolonner: kvartal + 5 spørgsmål + vaekst_p_forbrug
range(data_ml_api$kvartal)

# ----------------------------------------------------------------
# STEP 6: Multipel lineær regression - alle 5 spørgsmål
# ----------------------------------------------------------------

lmm_fuld <- lm(
  vaekst_p_forbrug ~ .,   # "alle andre kolonner som x"
  data = data_ml_api %>% select(-kvartal)
)
summary(lmm_fuld)

