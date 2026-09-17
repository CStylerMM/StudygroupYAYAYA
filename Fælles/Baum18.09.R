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
periode_slut  <- as.Date("2021-06-01")


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
  mutate(kvartal = floor_date(dato, "quarter")) %>%
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

tidy(lmm_fuld, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(x = estimate, y = term)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  labs(x = "Koefficient (95% konfidensinterval)", y = NULL,
       title = "Koefficienter i den fulde model - alle 5 spørgsmål") +
  theme_minimal()
