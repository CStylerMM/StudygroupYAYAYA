library(dkstat)
library(tidyverse)
library(lubridate)
library(janitor)
library(car)
install.packages("broom")
library(broom)
# ================================================================
# OPGAVE 1: Multipel lineær regression - forbrugertillid og forbrug
# Bygget fra bunden, ét skridt ad gangen, ren API-vej.
# ================================================================

# Sæt periodens SLUTNING her - ret denne ene linje når nye kvartaler
# kommer til (f.eks. "2026-06-01" for 2026K2), resten af koden følger med
periode_start <- as.Date("2000-01-01")
periode_slut  <- as.Date("2026-06-01")


# ----------------------------------------------------------------
# STEP 1: Hent Y - årlig realvækst i privatforbruget, via API
# ----------------------------------------------------------------

nkh1_meta <- dst_meta(table = "NKH1", lang = "da")
nkh1_meta$variables

forbrug_raw_api <- dst_get_data(
  table = "NKH1",
  TRANSAKT  = "P.31 Husholdningernes forbrugsudgifter",
  PRISENHED = "2020-priser, kædede værdier",
  SÆSON     = "Sæsonkorrigeret",
  Tid       = "*",
  meta_data = nkh1_meta,
  lang = "da"
)

str(forbrug_raw_api)


# ----------------------------------------------------------------
# STEP 2: Beregn årlig realvækst -> API_y_Realvaekst
# ----------------------------------------------------------------

API_y_Realvaekst <- forbrug_raw_api %>%
  rename(kvartal = TID, p_forbrug = value) %>%
  mutate(kvartal = as.Date(kvartal)) %>%   # RETTET: TID er POSIXct, gør den til ren Date
  select(kvartal, p_forbrug) %>%
  arrange(kvartal) %>%
  mutate(vaekst_p_forbrug = (exp(log(p_forbrug) - log(lag(p_forbrug, 4))) - 1) * 100) %>%
  select(kvartal, vaekst_p_forbrug) %>%
  drop_na() %>%
  filter(kvartal >= periode_start, kvartal <= periode_slut)

nrow(API_y_Realvaekst)
range(API_y_Realvaekst$kvartal)


# ----------------------------------------------------------------
# STEP 3: Hent X - de 5 spørgsmål, via API
# ----------------------------------------------------------------

forv1_meta <- dst_meta(table = "FORV1", lang = "da")
forv1_meta$variables
forv1_meta$values$INDIKATOR
forv1_query <- list(
  INDIKATOR = c(
    "Familiens økonomiske situation i dag, sammenlignet med for et år siden",
    "Familiens økonomiske  situation om et år, sammenlignet med i dag",
    "Danmarks økonomiske situation i dag, sammenlignet med for et år siden",
    "Danmarks økonomiske situation om et år, sammenlignet med i dag",
    "Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket"
  ),
  Tid = "*"
)

tillid_raw_api <- dst_get_data(
  table = "FORV1",
  query = forv1_query,
  meta_data = forv1_meta,
  lang = "da"
)

str(tillid_raw_api)
unique(tillid_raw_api$INDIKATOR)


# ----------------------------------------------------------------
# STEP 4: Filtrer X til perioden, og omregn måned -> kvartal
# ----------------------------------------------------------------

API_x_Forbrugertillid <- tillid_raw_api %>%
  rename(indikator = INDIKATOR, dato = TID, nettotal = value) %>%
  filter(dato >= periode_start, dato <= periode_slut) %>%
  mutate(kvartal = as.Date(floor_date(dato, "quarter"))) %>%   # RETTET: as.Date() tilføjet
  group_by(indikator, kvartal) %>%
  summarise(nettotal = mean(nettotal, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = indikator, values_from = nettotal) %>%
  janitor::clean_names()

names(API_x_Forbrugertillid)
nrow(API_x_Forbrugertillid)


# ----------------------------------------------------------------
# STEP 5: Saml X og Y
# ----------------------------------------------------------------

data_ml_api <- API_x_Forbrugertillid %>%
  inner_join(API_y_Realvaekst, by = "kvartal") %>%
  drop_na()

nrow(data_ml_api)
names(data_ml_api)
#eller
names(API_x_Forbrugertillid)
range(data_ml_api$kvartal)


# ----------------------------------------------------------------
# STEP 6: Multipel lineær regression - alle 5 spørgsmål
# ----------------------------------------------------------------

lmm_fuld <- lm(
  vaekst_p_forbrug ~ .,
  data = data_ml_api %>% select(-kvartal)
)
summary(lmm_fuld)

# ----------------------------------------------------------------
# STEP 7: Multikollinearitet - er de insignifikante variable
# insignifikante fordi de forklarer hinanden?
# ----------------------------------------------------------------

cor(data_ml_api %>% select(-kvartal, -vaekst_p_forbrug))
car::vif(lmm_fuld)


# ----------------------------------------------------------------
# STEP 8: Koefficientplot - visualisering til "hvad viser koefficienterne"
# ----------------------------------------------------------------

tidy(lmm_fuld) %>%
  filter(term != "(Intercept)") %>%
  mutate(term = fct_reorder(term, p.value, .desc = TRUE)) %>%
  ggplot(aes(x = p.value, y = term)) +
  geom_col(fill = "steelblue") +
  geom_vline(xintercept = 0.05, linetype = "dashed", color = "red") +
  labs(x = "P-værdi", y = NULL,
       title = "P-værdi er et win for F4 Danmarks øko sit i dag sammenlignet med for et år siden",
       subtitle = "Rød streg = 0,05-grænsen. Under linjen = signifikant.") +
  theme_minimal(base_size = 12)

#
tidy(lmm_fuld) %>%
  filter(term != "(Intercept)") %>%
  mutate(term = fct_reorder(term, abs(statistic))) %>%
  ggplot(aes(x = statistic, y = term)) +
  geom_col(fill = "steelblue") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey30") +
  labs(x = "T-værdi", y = NULL,
       title = "T-Værdi er et win for F4 Danmarks øko sit i dag sammenlignet med for et år siden",
       subtitle = "Længere væk fra 0 (i begge retninger) = mere signifikant") +
  theme_minimal(base_size = 12)

# ----------------------------------------------------------------
# STEP 9: Hent F10 - den sidste variabel til DI-FTI
# ("Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.")
# ----------------------------------------------------------------

tillid_raw_f10 <- dst_get_data(
  table = "FORV1",
  query = list(INDIKATOR = "Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.", Tid = "*"),
  meta_data = forv1_meta,
  lang = "da"
)

unique(tillid_raw_f10$INDIKATOR)   # bekræft teksten matcher - kig efter "F10" i starten


# ----------------------------------------------------------------
# STEP 10: Filtrer F10 til perioden, omregn måned -> kvartal
# ----------------------------------------------------------------

API_x_F10 <- tillid_raw_f10 %>%
  rename(indikator = INDIKATOR, dato = TID, nettotal = value) %>%
  filter(dato >= periode_start, dato <= periode_slut) %>%
  mutate(kvartal = as.Date(floor_date(dato, "quarter"))) %>%   # RETTET: as.Date() tilføjet
  group_by(kvartal) %>%
  summarise(nettotal = mean(nettotal, na.rm = TRUE), .groups = "drop") %>%
  rename(f10_anskaffelse_naeste_12_mdr = nettotal)

names(API_x_F10)
nrow(API_x_F10)   # forventet: 86


# ----------------------------------------------------------------
# STEP 11: Byg DI-FTI - simpelt gennemsnit af F2, F4, F9, F10
# ----------------------------------------------------------------

data_ml_di_fti <- data_ml_api %>%
  select(kvartal,
         f2_familiens_okonomiske_situation_i_dag_sammenlignet_med_for_et_ar_siden,
         f4_danmarks_okonomiske_situation_i_dag_sammenlignet_med_for_et_ar_siden,
         f9_anskaffelse_af_storre_forbrugsgoder_fordelagtigt_for_ojeblikket,
         vaekst_p_forbrug) %>%
  inner_join(API_x_F10, by = "kvartal") %>%
  mutate(
    di_fti = (f2_familiens_okonomiske_situation_i_dag_sammenlignet_med_for_et_ar_siden +
                f4_danmarks_okonomiske_situation_i_dag_sammenlignet_med_for_et_ar_siden +
                f9_anskaffelse_af_storre_forbrugsgoder_fordelagtigt_for_ojeblikket +
                f10_anskaffelse_naeste_12_mdr) / 4
  )

nrow(data_ml_di_fti)
range(data_ml_di_fti$di_fti, na.rm = TRUE)


# ----------------------------------------------------------------
# STEP 12: Simpel regression - DI-FTI mod realvækst
# Sammenlign R² med Baums egne tal (0,54 for 2000-2016K2)
# ----------------------------------------------------------------

lm_di_fti <- lm(vaekst_p_forbrug ~ di_fti, data = data_ml_di_fti)
summary(lm_di_fti)

cor(data_ml_di_fti$di_fti, data_ml_di_fti$vaekst_p_forbrug)   # sammenlign med Baums korrelation 0,73

forv1_meta$values$INDIKATOR


#Nu laver vi lige en graf for spørgsmål 9 for at se hvor nederen danskerne har det med at anskaffe sig ting
# Simpel regression: kun F9 som x
lm_f9 <- lm(vaekst_p_forbrug ~ f9_anskaffelse_af_storre_forbrugsgoder_fordelagtigt_for_ojeblikket,
            data = data_ml_api)
summary(lm_f9)

# Visualiseret som scatterplot med regressionslinje
# Genkør Y-beregningen med den nye periode
API_y_Realvaekst <- forbrug_raw_api %>%
  rename(kvartal = TID, p_forbrug = value) %>%
  mutate(kvartal = as.Date(kvartal)) %>%   # RETTET: as.Date() tilføjet
  select(kvartal, p_forbrug) %>%
  arrange(kvartal) %>%
  mutate(vaekst_p_forbrug = (exp(log(p_forbrug) - log(lag(p_forbrug, 4))) - 1) * 100) %>%
  select(kvartal, vaekst_p_forbrug) %>%
  drop_na() %>%
  filter(kvartal >= periode_start, kvartal <= periode_slut)

nrow(API_y_Realvaekst)
range(API_y_Realvaekst$kvartal)   # bekræft den nu går til 2026-04-01

# Genkør X-filtreringen med den nye periode
API_x_Forbrugertillid <- tillid_raw_api %>%
  rename(indikator = INDIKATOR, dato = TID, nettotal = value) %>%
  filter(dato >= periode_start, dato <= periode_slut) %>%
  mutate(kvartal = as.Date(floor_date(dato, "quarter"))) %>%   # RETTET: as.Date() tilføjet
  group_by(indikator, kvartal) %>%
  summarise(nettotal = mean(nettotal, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = indikator, values_from = nettotal) %>%
  janitor::clean_names()

nrow(API_x_Forbrugertillid)   # forventet: ca. 106 kvartaler nu (2000K1-2026K2)

# Saml og kør regressionen igen
data_ml_api <- API_x_Forbrugertillid %>%
  inner_join(API_y_Realvaekst, by = "kvartal") %>%
  drop_na()

nrow(data_ml_api)
range(data_ml_api$kvartal)

lmm_fuld <- lm(vaekst_p_forbrug ~ ., data = data_ml_api %>% select(-kvartal))
summary(lmm_fuld)

ggplot(data_ml_api, aes(x = kvartal, y = f9_anskaffelse_af_storre_forbrugsgoder_fordelagtigt_for_ojeblikket)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = as.Date("2021-06-01"), linetype = "dashed", color = "red", linewidth = 0.8) +
  annotate("text", x = as.Date("2021-06-01"), y = max(data_ml_api$f9_anskaffelse_af_storre_forbrugsgoder_fordelagtigt_for_ojeblikket, na.rm = TRUE),
           label = "2021K2 (opgavens oprindelige slut)", hjust = -0.05, vjust = 1, size = 3, color = "red") +
  labs(
    x = NULL, y = "Nettotal",
    title = "F9: Anskaffelse af forbrugsgoder, 2000K1-2026K2",
    subtitle = "Rød streg markerer 2021K2 - opgavens oprindelige periode"
  ) +
  theme_minimal(base_size = 12)


#lad os lige cook nu
# ----------------------------------------------------------------
ggplot(data_ml_api, aes(x = kvartal, y = f5_danmarks_okonomiske_situation_om_et_ar_sammenlignet_med_i_dag)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = as.Date("2021-06-01"), linetype = "dashed", color = "red", linewidth = 0.8) +
  labs(
    x = NULL, y = "Nettotal",
    title = "F5: danmarks økonomiske sit om 1 år sam med i dag, 2000K1-2026K2",
    subtitle = "Rød streg markerer 2021K2 - opgavens oprindelige periode"
  ) +
  theme_minimal(base_size = 12)

# ----------------------------------------------------------------
# Model 1: DST's fulde FTI - alle 5 spørgsmål
# ----------------------------------------------------------------

lmm_dst <- lm(
  vaekst_p_forbrug ~ f2_familiens_okonomiske_situation_i_dag_sammenlignet_med_for_et_ar_siden +
    f3_familiens_okonomiske_situation_om_et_ar_sammenlignet_med_i_dag +
    f4_danmarks_okonomiske_situation_i_dag_sammenlignet_med_for_et_ar_siden +
    f5_danmarks_okonomiske_situation_om_et_ar_sammenlignet_med_i_dag +
    f9_anskaffelse_af_storre_forbrugsgoder_fordelagtigt_for_ojeblikket,
  data = data_ml_api
)
summary(lmm_dst)


# ----------------------------------------------------------------
# Model 2: Jeres optimerede model - kun F4, F5, F9
# ----------------------------------------------------------------

lmm_optimeret <- lm(
  vaekst_p_forbrug ~ f4_danmarks_okonomiske_situation_i_dag_sammenlignet_med_for_et_ar_siden +
    f5_danmarks_okonomiske_situation_om_et_ar_sammenlignet_med_i_dag +
    f9_anskaffelse_af_storre_forbrugsgoder_fordelagtigt_for_ojeblikket,
  data = data_ml_api
)
summary(lmm_optimeret)


# ----------------------------------------------------------------
# Sammenlign de to direkte
# ----------------------------------------------------------------

summary(lmm_dst)$adj.r.squared
summary(lmm_optimeret)$adj.r.squared


# ----------------------------------------------------------------
# GRAF 1: DST's fulde model (alle 5 spørgsmål) - simpelt gennemsnit
# ----------------------------------------------------------------

data_plot_dst <- data_ml_api %>%
  mutate(
    indikator_dst = (f2_familiens_okonomiske_situation_i_dag_sammenlignet_med_for_et_ar_siden +
                       f3_familiens_okonomiske_situation_om_et_ar_sammenlignet_med_i_dag +
                       f4_danmarks_okonomiske_situation_i_dag_sammenlignet_med_for_et_ar_siden +
                       f5_danmarks_okonomiske_situation_om_et_ar_sammenlignet_med_i_dag +
                       f9_anskaffelse_af_storre_forbrugsgoder_fordelagtigt_for_ojeblikket) / 5
  )

scale_factor_dst <- max(abs(data_plot_dst$indikator_dst)) / max(abs(data_plot_dst$vaekst_p_forbrug))

ggplot(data_plot_dst, aes(x = kvartal)) +
  geom_col(aes(y = vaekst_p_forbrug * scale_factor_dst, fill = "Årlig realvækst pr. kvartal i privat forbruget (højre akse)"),
           width = 80) +
  geom_line(aes(y = indikator_dst, color = "DST's forbrugertillidsindikator (FTI)"), linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_y_continuous(
    name = "Nettotal",
    sec.axis = sec_axis(~ . / scale_factor_dst, name = "Pct.")
  ) +
  scale_fill_manual(name = NULL, values = c("Årlig realvækst pr. kvartal i privat forbruget (højre akse)" = "steelblue")) +
  scale_color_manual(name = NULL, values = c("DST's forbrugertillidsindikator (FTI)" = "black")) +
  labs(x = NULL,
       title = "DST's forbrugertillidsindikator følger privatforbruget",
       subtitle = "Simpelt gennemsnit af alle 5 spørgsmål i forbrugerforventningsundersøgelsen") +
  theme_minimal() +
  theme(legend.position = "top")


# ----------------------------------------------------------------
# GRAF 2: DI/Baums model (F2, F4, F9, F10) - simpelt gennemsnit
# Kræver F10 hentet fra tidligere i samtalen (API_x_F10)
# ----------------------------------------------------------------

data_plot_baum <- data_ml_api %>%
  select(kvartal, vaekst_p_forbrug,
         f2_familiens_okonomiske_situation_i_dag_sammenlignet_med_for_et_ar_siden,
         f4_danmarks_okonomiske_situation_i_dag_sammenlignet_med_for_et_ar_siden,
         f9_anskaffelse_af_storre_forbrugsgoder_fordelagtigt_for_ojeblikket) %>%
  inner_join(API_x_F10, by = "kvartal") %>%
  mutate(
    indikator_baum = (f2_familiens_okonomiske_situation_i_dag_sammenlignet_med_for_et_ar_siden +
                        f4_danmarks_okonomiske_situation_i_dag_sammenlignet_med_for_et_ar_siden +
                        f9_anskaffelse_af_storre_forbrugsgoder_fordelagtigt_for_ojeblikket +
                        f10_anskaffelse_naeste_12_mdr) / 4
  )

scale_factor_baum <- max(abs(data_plot_baum$indikator_baum)) / max(abs(data_plot_baum$vaekst_p_forbrug))

# RETTET: brugte data_plot_dst/scale_factor_dst/indikator_dst her, men det er GRAF 2 -
# den skal bruge Baum-versionerne, ellers tegner den samme graf som GRAF 1 to gange
ggplot(data_plot_baum, aes(x = kvartal)) +
  geom_col(aes(y = vaekst_p_forbrug * scale_factor_baum, fill = "Årlig realvækst pr. kvartal i privat forbruget (højre akse)"),
           width = 80, alpha = 1) +
  geom_line(aes(y = indikator_baum, color = "DI's forbrugertillidsindikator (DI-FTI)"), linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  
  # Skel 1: Baums oprindelige artikel stoppede her (2016K2)
  geom_vline(xintercept = as.Date("2016-06-01"), linetype = "dashed", color = "darkgreen", linewidth = 0.8) +
  annotate("text", x = as.Date("2016-06-01"), y = max(data_plot_baum$vaekst_p_forbrug * scale_factor_baum, na.rm = TRUE),
           label = "2016K2 (Baums artikel)", hjust = -0.05, vjust = 1, size = 3, color = "darkgreen") +
  
  # Skel 2: opgavens oprindelige periode stoppede her (2021K2)
  geom_vline(xintercept = as.Date("2021-06-01"), linetype = "dashed", color = "red", linewidth = 0.8) +
  annotate("text", x = as.Date("2021-06-01"), y = max(data_plot_baum$vaekst_p_forbrug * scale_factor_baum, na.rm = TRUE) * 0.85,
           label = "2021K2 (opgavens slut)", hjust = -0.05, vjust = 1, size = 3, color = "red") +
  
  scale_y_continuous(
    name = "Nettotal",
    sec.axis = sec_axis(~ . / scale_factor_baum, name = "Pct.")
  ) +
  scale_fill_manual(name = NULL, values = c("Årlig realvækst pr. kvartal i privat forbruget (højre akse)" = "steelblue")) +
  scale_color_manual(name = NULL, values = c("DI's forbrugertillidsindikator (DI-FTI)" = "black")) +
  labs(x = NULL,
       title = "DI's forbrugertillidsindikator følger privatforbruget, 2000K1-2026K2",
       subtitle = "Simpelt gennemsnit af 4 udvalgte spørgsmål (Baum, 2016). Grøn = Baums artikel, rød = opgavens periode") +
  theme_minimal() +
  theme(legend.position = "top")

