### --- Opgave 4 --- ###

### Opgave 4.1 - Illustration af forbrugertillid
# Hent data for forbrugertillidsundersøgelsen fra januar 1996 til i dag og omregn jeres data til
# kvartaler. Lav en grafisk illustration af jeres omregnede data for DST’s forbrugertillidsindikator og
# kommentér på, hvornår de danske forbrugere er mest og mindst optimistiske.

#Hente forbrugertillidsundersøgelsen ned via API

library(dkstat)

# Hente meta data
FORV1 <- dst_meta(table = "FORV1", lang = "da")

# Explore variable
FORV1$variables # Kategorierne/Variablerne vi skal filtrere i
FORV1$values$INDIKATOR # Deres værdier
FORV1$values$Tid # Deres værdier

# Laver query liste
filter_FORV1 <- list( INDIKATOR = "Forbrugertillidsindikatoren",
                      Tid = "*")

# Hente data ned og putte den i dataframe
fTillid_raw <- dst_get_data(table = "FORV1",
                            query = filter_FORV1,
                            meta_data = FORV1,
                            lang = "da")

# Lave subset fra 1996
fTillid <- fTillid_raw[fTillid_raw$TID >= as.Date("1996-01-01"), ]

# Kun tallene (value-kolonnen) skal ind i tidsserien, ikke hele data framen
ftillid_ts <- ts(fTillid$value, start = c(1996, 1), frequency = 12)

# Omregn til kvartaler (gennemsnit af 3 måneder)
ftillid_kvt_ts <- aggregate(ftillid_ts, nfrequency = 4)/3

# Lave til data frame
ftillid_kvt_df <- data.frame(
  Tidsinterval = paste0(floor(time(ftillid_kvt_ts)), "K", cycle(ftillid_kvt_ts)),
  Forbrugertillid = round(as.numeric(ftillid_kvt_ts), 1))

# Plotte det
library(ggplot2)
ggplot(ftillid_kvt_df, aes(x = Tidsinterval, y = Forbrugertillid, group = 1)) +
  geom_line(colour = "hotpink4") +
  scale_x_discrete(breaks = ftillid_kvt_df$Tidsinterval[seq(1, nrow(ftillid_kvt_df), by = 8)]) +
  labs(title = "DST's forbrugertillidsindikator",
       subtitle = "Kvartalsvise gennemsnit, 1996 til i dag",
       x = NULL, y = "Nettotal", caption = "Kilde: Danmarks Statistik & Egne beregninger") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


# Finde den største og mindste forbrugertillid
største_række <- ftillid_kvt_df[which.max(ftillid_kvt_df$Forbrugertillid), ]
største_række

mindste_række <- ftillid_kvt_df[which.min(ftillid_kvt_df$Forbrugertillid), ]
mindste_række

# "Forbedret" chat graf
# Sætte punkter på med størst om mindst tillid ^^^
punkter_ftillid_kvt_df <- data.frame(
  Tidsinterval = c("2006K1", "2022K4"),
  Forbrugertillid = c(12.6, -32.1),
  Tekst = c("Maks: 12.6 (2006K1)", "Min: -32.1 (2022K4)")
)

# Plotte det :))
ggplot(ftillid_kvt_df, aes(x = Tidsinterval, y = Forbrugertillid, group = 1)) +
  geom_line(colour = "hotpink4", linewidth = 1) +
  geom_point(data = punkter_ftillid_kvt_df, aes(x = Tidsinterval, y = Forbrugertillid), color = "hotpink3", size = 4) +
  geom_text(
    data = punkter_ftillid_kvt_df, 
    aes(x = Tidsinterval, y = Forbrugertillid, label = Tekst),
    vjust = c(-1.2, 1.8), 
    fontface = "bold",
    size = 5.5
  ) +
  scale_x_discrete(breaks = ftillid_kvt_df$Tidsinterval[seq(1, nrow(ftillid_kvt_df), by = 8)]) +
  labs(
    title = "DST's forbrugertillidsindikator over tid",
    subtitle = "Kvartalsvise gennemsnit, 1996 til 2026 2. kvartal",
    x = NULL, 
    y = "Nettotal", 
    caption = "Kilde: Danmarks Statistik & Egne beregninger"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold"),
    plot.subtitle = element_text(size = 20, margin = margin(b = 15)),
    plot.caption = element_text(size = 15, face = "italic", margin = margin(t = 20)),
    axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 10)),
    axis.text.x = element_text(angle = 60, hjust = 1, size = 12, face = "bold"),
    axis.text.y = element_text(size = 12)
  )


### Opgave 4.2 – Gennemsnit af underspørgsmål
# Beregn gennemsnittet for underspørgsmålet ”Set i lyset af den økonomiske situation, mener du, at
# det for øjeblikket er fordelagtigt at anskaffe større forbrugsgoder som fjernsyn, vaskemaskine eller
# lignende, eller er det bedre at vente?” for perioden 1. kvartal 2000 til og med 3. kvartal 2023.
# Vurdér jeres resultat set i forhold til spørgsmålet og svarmulighederne. (Hint: giver resultatet
# analytisk mening?)

filter_forbrugsgoder <- list( INDIKATOR = "Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket",
                      Tid = "*")

forbrugsgoder_raw <- dst_get_data(table = "FORV1",
                            query = filter_forbrugsgoder,
                            meta_data = FORV1,
                            lang = "da")

forbrugsgoder <- forbrugsgoder_raw[-(1:303), ]

forbrugsgoder_gns <- round(mean(forbrugsgoder$value), 2)
forbrugsgoder_gns



### Opgave 4.3 - De 11 grupper af forbrug
# Hent data for de 11 grupper af forbrug blandt husholdningerne. Hvad brugte danskerne flest penge
# på i 2022? Hvilken gruppe af forbruget steg mest fra 2020 til 2023? (hint: I kan ikke lægge
# kvartalerne sammen, når I har kædede værdier)

# Hive NAHC21 ned med API

library(dkstat)
NAHC21 <- dst_meta(table = "NAHC21", lang = "da")

# Explore variable
NAHC21$variables # Kategorierne/Variablerne vi skal filtrere i
NAHC21$values$PRISENHED # Deres værdier
NAHC21$values$FORMAAAL # Deres værdier
NAHC21$values$Tid # Deres værdier

# Laver query liste
filter_NAHC21 <- list( PRISENHED = "2020-priser, kædede værdier",
                      Tid = c("2020", "2023"),
                      FORMAAAL = c("Fødevarer", "Drikkevarer og tobak mv.", "Beklædning og fodtøj",
                      "Boligbenyttelse", "Elektricitet, gas og andet brændsel", 
                      "Boligudstyr, husholdningsudstyr og vedligholdelse heraf",
                       "Medicin, lægeudgifter o.l.",  "Køb af køretøjer", "Anden transport og kommunikation",
                      "Fritidsudstyr, underholdning og rejser", "Andre varer og tjenester"))

# Hente 2020-2023 data ned og putte den i dataframe
forbrug_2020_2023_NAHC21 <- dst_get_data(table = "NAHC21",
                            query = filter_NAHC21,
                            meta_data = NAHC21,
                            lang = "da")

# Hente 2022 data ned og putte den i dataframe
værdier_2022_NAHC21 <- dst_get_data(table = "NAHC21",
                                   PRISENHED = "2020-priser, kædede værdier",
                                   Tid = "2022",
                                   FORMAAAL = c("Fødevarer", "Drikkevarer og tobak mv.", "Beklædning og fodtøj",
                                               "Boligbenyttelse", "Elektricitet, gas og andet brændsel", 
                                               "Boligudstyr, husholdningsudstyr og vedligholdelse heraf",
                                               "Medicin, lægeudgifter o.l.",  "Køb af køretøjer", "Anden transport og kommunikation",
                                               "Fritidsudstyr, underholdning og rejser", "Andre varer og tjenester"),
                                   meta_data = NAHC21,
                                   lang = "da")

# Hive rækken med størst forbrug ud
værdier_2022_NAHC21[which.max(værdier_2022_NAHC21$value), ]
# Boligbenyttelse er der brugt mest på med 217.409.000.000 kr.


# Forloop til beregning af vækst fra 2020 til 2023
 
n <- nrow(forbrug_2020_2023_NAHC21)

# Tom "kurv" til at samle resultaterne i, på forhånd fyldt med NA
procent_resultater <- rep(NA, n)

for (i in 1:(n - 11)) {
  j <- i + 11
  
  værdi_i <- forbrug_2020_2023_NAHC21$value[i]
  værdi_j <- forbrug_2020_2023_NAHC21$value[j]
  
  procent_ændring <- ((værdi_j - værdi_i) / værdi_i) * 100
  
  procent_resultater[j] <- procent_ændring   # gem resultatet på PLADS j i "kurven"
}

forbrug_2020_2023_NAHC21$Procent_ændring <- round((procent_resultater), 2)

# Trække rækken med størst ændring i procent ud
forbrug_2020_2023_NAHC21[which.max(forbrug_2020_2023_NAHC21$Procent_ændring), ]

# Andre vare og tjenester er steget mest med 19.03%



### Opgave 4.4 - De 11 grupper af forbrug
# Lav 22 simple lineære regressioner mellem hver af de 11 grupper i forbruget (y-variable) og
# henholdsvis forbrugertillidsindikatoren fra DST og DI. I skal gemme summary i 22 lister. I skal
# lave jeres regressioner fra 1. kvartal 2000 til og med 2. kvartal 2023.

# Hente samlet forbrugertillidsindekator - DST
fTillid_sub_lm <- ftillid_kvt_df[-(1:16), ]

# Hente samlet forbrugertillidsindekator - DI
library(dkstat)
library(tidyr)
library(ggplot2)


#TRIN 2: X (forbrugertillid, månedlig -> kvartaler)
meta_DI <- dst_meta(table = "FORV1", lang = "da")

#kun de 4 spørgsmål der indgår i DI-FTI: F2, F4, F9, F10
DI_FTI_query <- list(
  INDIKATOR = c(
    "Familiens økonomiske situation i dag, sammenlignet med for et år siden",
    "Danmarks økonomiske situation i dag, sammenlignet med for et år siden",
    "Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket",
    "Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr."
  ),
  Tid = "*"
)

DI_tillid_raw <- dst_get_data(
  table     = "FORV1",
  query     = DI_FTI_query,
  meta_data = meta_DI,
  lang      = "da"
)

DI_tillid_raw  <- DI_tillid_raw[order(DI_tillid_raw$TID), ]
DI_tillid_wide <- pivot_wider(DI_tillid_raw, names_from = INDIKATOR, values_from = value)
DI_tillid_wide <- DI_tillid_wide[as.numeric(format(DI_tillid_wide$TID, "%Y")) >= 2000, ]

#månedlig ts og aggregate til kvartaler (3 måneder / 3 = gennemsnit)
DI_tillid_ts <- ts(as.matrix(DI_tillid_wide[, -1]), start = c(2000, 1), frequency = 12)
DI_tillid_q  <- aggregate(DI_tillid_ts, nfrequency = 4) / 3

end(DI_tillid_q)    #seneste hele kvartal med forbrugertillid
ncol(DI_tillid_q)   #skal være 4


#TRIN 3: byg DI-FTI (gennemsnit af de 4 spørgsmål) og saml X og Y
DI_tillid_df <- data.frame(
  kvartal = round(as.numeric(time(DI_tillid_q)), 2),   #2000.00, 2000.25 osv.
  di_fti  = rowMeans(DI_tillid_q)
)


# Subsette de 11 forbrugsgrupper

sub_CPA <- dst_get_data(table = "NAHC21",
                        PRISENHED = "2020-priser, kædede værdier",
                        Tid = "*",
                        FORMAAAL = "Fødevarer",
                        meta_data = NAHC21,
                        lang = "da")
sub_CPA <- sub_CPA[-(1:34), ]
sub_CPA <- data.frame(sub_CPA[ , -(1:3)])
colnames(sub_CPA) <- "FT_CPA"

sub_CPB <- dst_get_data(table = "NAHC21",
                        PRISENHED = "2020-priser, kædede værdier",
                        Tid = "*",
                        FORMAAAL = "Drikkevarer og tobak mv.",
                        meta_data = NAHC21,
                        lang = "da")
sub_CPB <- sub_CPB[-(1:34), ]
sub_CPB <- data.frame(sub_CPB[ , -(1:3)])
colnames(sub_CPB) <- "FT_CPB"

sub_CPC <- dst_get_data(table = "NAHC21",
                        PRISENHED = "2020-priser, kædede værdier",
                        Tid = "*",
                        FORMAAAL = "Beklædning og fodtøj",
                        meta_data = NAHC21,
                        lang = "da")
sub_CPC <- sub_CPC[-(1:34), ]
sub_CPC <- data.frame(sub_CPC[ , -(1:3)])
colnames(sub_CPC) <- "FT_CPC"

sub_CPD <- dst_get_data(table = "NAHC21",
                        PRISENHED = "2020-priser, kædede værdier",
                        Tid = "*",
                        FORMAAAL = "Boligbenyttelse",
                        meta_data = NAHC21,
                        lang = "da")
sub_CPD <- sub_CPD[-(1:34), ]
sub_CPD <- data.frame(sub_CPD[ , -(1:3)])
colnames(sub_CPD) <- "FT_CPD"

sub_CPE <- dst_get_data(table = "NAHC21",
                        PRISENHED = "2020-priser, kædede værdier",
                        Tid = "*",
                        FORMAAAL = "Elektricitet, gas og andet brændsel",
                        meta_data = NAHC21,
                        lang = "da")
sub_CPE <- sub_CPE[-(1:34), ]
sub_CPE <- data.frame(sub_CPE[ , -(1:3)])
colnames(sub_CPE) <- "FT_CPE"

sub_CPF <- dst_get_data(table = "NAHC21",
                    PRISENHED = "2020-priser, kædede værdier",
                    Tid = "*",
                    FORMAAAL = "Boligudstyr, husholdningsudstyr og vedligholdelse heraf",
                    meta_data = NAHC21,
                    lang = "da")
sub_CPF <- sub_CPF[-(1:34), ]
sub_CPF <- data.frame(sub_CPF[ , -(1:3)])
colnames(sub_CPF) <- "FT_CPF"

sub_CPG <- dst_get_data(table = "NAHC21",
                        PRISENHED = "2020-priser, kædede værdier",
                        Tid = "*",
                        FORMAAAL = "Medicin, lægeudgifter o.l.",
                        meta_data = NAHC21,
                        lang = "da")
sub_CPG <- sub_CPG[-(1:34), ]
sub_CPG <- data.frame(sub_CPG[ , -(1:3)])
colnames(sub_CPG) <- "FT_CPG"

sub_CPH <- dst_get_data(table = "NAHC21",
                    PRISENHED = "2020-priser, kædede værdier",
                    Tid = "*",
                    FORMAAAL = "Køb af køretøjer",
                    meta_data = NAHC21,
                    lang = "da")
sub_CPH <- sub_CPH[-(1:34), ]
sub_CPH <- data.frame(sub_CPH[ , -(1:3)])
colnames(sub_CPH) <- "FT_CPH"

sub_CPI <- dst_get_data(table = "NAHC21",
                        PRISENHED = "2020-priser, kædede værdier",
                        Tid = "*",
                        FORMAAAL = "Anden transport og kommunikation",
                        meta_data = NAHC21,
                        lang = "da")
sub_CPI <- sub_CPI[-(1:34), ]
sub_CPI <- data.frame(sub_CPI[ , -(1:3)])
colnames(sub_CPI) <- "FT_CPI"

sub_CPJ <- dst_get_data(table = "NAHC21",
                       PRISENHED = "2020-priser, kædede værdier",
                       Tid = "*",
                       FORMAAAL = "Fritidsudstyr, underholdning og rejser",
                       meta_data = NAHC21,
                       lang = "da")
sub_CPJ <- sub_CPJ[-(1:34), ]
sub_CPJ <- data.frame(sub_CPJ[ , -(1:3)])
colnames(sub_CPJ) <- "FT_CPJ"

sub_CPK <- dst_get_data(table = "NAHC21",
                        PRISENHED = "2020-priser, kædede værdier",
                        Tid = "*",
                        FORMAAAL = "Andre varer og tjenester",
                        meta_data = NAHC21,
                        lang = "da")
sub_CPK <- sub_CPK[-(1:34), ]
sub_CPK <- data.frame(sub_CPK[ , -(1:3)])
colnames(sub_CPK) <- "FT_CPK"

# Lave 22 lineære regressioner

# Lave forbrugertillidsindikatoren (DST) til årligt og ikke kvartalvis
ftillid_aar_ts <- aggregate(ftillid_ts, nfrequency = 1)/12 # Bruge prædefineret tidsserie

# Lave til data frame på årsdata
ftillid_aar_df <- data.frame(
  Tidsinterval = floor(time(ftillid_aar_ts)),
  Forbrugertillid = round(as.numeric(ftillid_aar_ts), 1))

ftillid_aar_df_DST <- ftillid_aar_df[-(1:4), ]
ftillid_aar_df_DST <- ftillid_aar_df_DST[-(25:26), ]

# Lave forbrugertillidsindikatoren (DI) til årligt og ikke kvartalvis
# Lave en time series
fTillid_ts_DI <- ts(DI_tillid_df$kvartal, start = c(2000, 1), frequency = 4)

# Omregn til år
fTillid_aar_ts_DI <- aggregate(fTillid_ts_DI, nfrequency = 1)/4

# Lave til data frame
ftillid_aar_df_DI <- data.frame(
  Tidsinterval = paste0(floor(time(fTillid_aar_ts_DI)), "K", cycle(fTillid_aar_ts_DI)),
  Forbrugertillid = round(as.numeric(fTillid_aar_ts_DI), 1))

# Slette de sidste to rækker
ftillid_aar_df_DI <- ftillid_aar_df_DI[-(25:26), ]
ftillid_aar_df_DI <- data.frame(ftillid_aar_df_DI[ , -(1)])

# cbind til samlet datasæt med forbrugsgrupper
samlet_lm_df <- cbind(ftillid_aar_df_DST, ftillid_aar_df_DI, sub_CPA, sub_CPB, sub_CPC,
                      sub_CPD, sub_CPE, sub_CPF, sub_CPG, sub_CPH, sub_CPI,
                      sub_CPJ, sub_CPK)

colnames(samlet_lm_df)[2:3] <- c("Forbrugertillid_DST", "Forbrugertillid_DI")

# Lineære regressioner på DST og forbrugsgrupper
# Gruppe A: (Fødevare)
lm_CPA_DST <- lm(FT_CPA ~ Forbrugertillid_DST, data = samlet_lm_df)
summary(lm_CPA_DST)
sumr_list_lm_CPA_DST <- list(summary(lm_CPA_DST))

# Gruppe B: (Drikkevarer og tobak mv.)
lm_CPB_DST <- lm(FT_CPB ~ Forbrugertillid_DST, data = samlet_lm_df)
summary(lm_CPB_DST)
sumr_list_lm_CPB_DST <- list(summary(lm_CPB_DST))

# Gruppe C: (Beklædning og fodtøj)
lm_CPC_DST <- lm(FT_CPC ~ Forbrugertillid_DST, data = samlet_lm_df)
summary(lm_CPC_DST)
sumr_list_lm_CPC_DST <- list(summary(lm_CPC_DST))

# Gruppe D: (Boligudnyttelse)
lm_CPD_DST <- lm(FT_CPD ~ Forbrugertillid_DST, data = samlet_lm_df)
summary(lm_CPD_DST)
sumr_list_lm_CPD_DST <- list(summary(lm_CPD_DST))

# Gruppe E: (Elektricitet, gas og andet brændsel)
lm_CPE_DST <- lm(FT_CPE ~ Forbrugertillid_DST, data = samlet_lm_df)
summary(lm_CPE_DST)
sumr_list_lm_CPE_DST <- list(summary(lm_CPE_DST))

# Gruppe F: (Boligudstyr, husholdningsudstyr og vedligeholdelse heraf)
lm_CPF_DST <- lm(FT_CPF ~ Forbrugertillid_DST, data = samlet_lm_df)
summary(lm_CPF_DST)
sumr_list_lm_CPF_DST <- list(summary(lm_CPF_DST))

# Gruppe G: (Medicin, lægeudgifter o.l.)
lm_CPG_DST <- lm(FT_CPG ~ Forbrugertillid_DST, data = samlet_lm_df)
summary(lm_CPG_DST)
sumr_list_lm_CPG_DST <- list(summary(lm_CPG_DST))

# Gruppe H: (Køb af køretøjer)
lm_CPH_DST <- lm(FT_CPH ~ Forbrugertillid_DST, data = samlet_lm_df)
summary(lm_CPH_DST)
sumr_list_lm_CPH_DST <- list(summary(lm_CPH_DST))

# Gruppe I: (Anden transport og kommunikation)
lm_CPI_DST <- lm(FT_CPI ~ Forbrugertillid_DST, data = samlet_lm_df)
summary(lm_CPI_DST)
sumr_list_lm_CPI_DST <- list(summary(lm_CPI_DST))

# Gruppe J: (Fritidsudstyr, underholdning og rejser)
lm_CPJ_DST <- lm(FT_CPJ ~ Forbrugertillid_DST, data = samlet_lm_df)
summary(lm_CPJ_DST)
sumr_list_lm_CPJ_DST <- list(summary(lm_CPJ_DST))

# Gruppe K: (Andre vare og tjenester)
lm_CPK_DST <- lm(FT_CPK ~ Forbrugertillid_DST, data = samlet_lm_df)
summary(lm_CPK_DST)
sumr_list_lm_CPK_DST <- list(summary(lm_CPK_DST))

# Lineære regressioner på DI og forbrugsgrupper
# Gruppe A: (Fødevare)
lm_CPA_DI <- lm(FT_CPA ~ Forbrugertillid_DI, data = samlet_lm_df)
summary(lm_CPA_DI)
sumr_list_lm_CPA_DI <- list(summary(lm_CPA_DI))

# Gruppe B: (Drikkevarer og tobak mv.)
lm_CPB_DI <- lm(FT_CPB ~ Forbrugertillid_DI, data = samlet_lm_df)
summary(lm_CPB_DI)
sumr_list_lm_CPB_DI <- list(summary(lm_CPB_DI))

# Gruppe C: (Beklædning og fodtøj)
lm_CPC_DI <- lm(FT_CPC ~ Forbrugertillid_DI, data = samlet_lm_df)
summary(lm_CPC_DI)
sumr_list_lm_CPC_DI <- list(summary(lm_CPC_DI))

# Gruppe D: (Boligudnyttelse)
lm_CPD_DI <- lm(FT_CPD ~ Forbrugertillid_DI, data = samlet_lm_df)
summary(lm_CPD_DI)
sumr_list_lm_CPD_DI <- list(summary(lm_CPD_DI))

# Gruppe E: (Elektricitet, gas og andet brændsel)
lm_CPE_DI <- lm(FT_CPE ~ Forbrugertillid_DI, data = samlet_lm_df)
summary(lm_CPE_DI)
sumr_list_lm_CPE_DI <- list(summary(lm_CPE_DI))

# Gruppe F: (Boligudstyr, husholdningsudstyr og vedligeholdelse heraf)
lm_CPF_DI <- lm(FT_CPF ~ Forbrugertillid_DI, data = samlet_lm_df)
summary(lm_CPF_DI)
sumr_list_lm_CPF_DI <- list(summary(lm_CPF_DI))

# Gruppe G: (Medicin, lægeudgifter o.l.)
lm_CPG_DI <- lm(FT_CPG ~ Forbrugertillid_DI, data = samlet_lm_df)
summary(lm_CPG_DI)
sumr_list_lm_CPG_DI <- list(summary(lm_CPG_DI))

# Gruppe H: (Køb af køretøjer)
lm_CPH_DI <- lm(FT_CPH ~ Forbrugertillid_DI, data = samlet_lm_df)
summary(lm_CPH_DI)
sumr_list_lm_CPH_DI <- list(summary(lm_CPH_DI))

# Gruppe I: (Anden transport og kommunikation)
lm_CPI_DI <- lm(FT_CPI ~ Forbrugertillid_DI, data = samlet_lm_df)
summary(lm_CPI_DI)
sumr_list_lm_CPI_DI <- list(summary(lm_CPI_DI))

# Gruppe J: (Fritidsudstyr, underholdning og rejser)
lm_CPJ_DI <- lm(FT_CPJ ~ Forbrugertillid_DI, data = samlet_lm_df)
summary(lm_CPJ_DI)
sumr_list_lm_CPJ_DI <- list(summary(lm_CPJ_DI))

# Gruppe K: (Andre vare og tjenester)
lm_CPK_DI <- lm(FT_CPK ~ Forbrugertillid_DI, data = samlet_lm_df)
summary(lm_CPK_DI)
sumr_list_lm_CPK_DI <- list(summary(lm_CPK_DI))

# Plotte linære regressioner på 

ggplot(samlet_lm_df, aes(y = FT_CPD, x = Forbrugertillid_DST)) +
  geom_point(color = "pink3") +
  geom_smooth(method = "lm", color = "hotpink3")+
  theme_minimal() +
  labs(x = "DST Forbrugertillidsindekator",
       y = " Forbrugsgruppen: Boligudbyttelse",
       title = " Linære regression for Boligydnyttelse & DST Forbrugertillidsindekator",
       caption = "Kilde: Danmarks Statistik & Egne beregninger")

ggplot(samlet_lm_df, aes(y = FT_CPD, x = Forbrugertillid_DI)) +
  geom_point(color = "pink3") +
  geom_smooth(method = "lm", color = "hotpink3")+
  theme_minimal() +
  labs(x = "DST Forbrugertillidsindekator",
       y = " Forbrugsgruppen: Boligudbyttelse",
       title = " Linære regression for Boligydnyttelse & DI Forbrugertillidsindekator",
       caption = "Kilde: Danmarks Statistik & Egne beregninger")
