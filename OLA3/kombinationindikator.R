#tid til at lave vores egen mikroindikator
#er mikroindikatoren bedre end dst og DI?
#brug data fra 1. kvartal til 3.kvartal 2026 (nyeste tid)
library(dkstat)
#lad os hente det ned med dkstat
#X: forbrugertillid. DST giver den månedligt, så den skal omregnes til kvartaler, så det matcher yyyyyyyyyy
voresfti1 <- dst_meta(table = "FORV1", lang = "da")

#de 8 spørgsmål der indgår i vores mikro er  (F2,F3,F9,F10,F11,F12,F13,)
unique(voresfti1)
voresfti1$values$INDIKATOR


voresfti_query <- list(
  INDIKATOR = c(
    "Familiens økonomiske situation i dag, sammenlignet med for et år siden",
    "Familiens økonomiske  situation om et år, sammenlignet med i dag",
    "Priser i dag, sammenlignet med for et år siden",
    "Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket",
    "Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.",
    "Anser det som fornuftigt at spare op i den nuværende økonomiske situation",
    "Regner med at kunne spare op i de kommende 12 måneder",
    "Familiens økonomiske situation lige nu: kan spare/penge slår til/ bruger mere end man tjener"
  ),
  Tid = "*"
)

voresFTI_mikro<- dst_get_data(
  table = "FORV1",
  query = voresfti_query,
  meta_data = voresfti1,
  lang = "da"
)
unique(voresFTI_mikro$INDIKATOR)


### --- BEDSTE INDIKATOR QUEST --- ###
#Vi laver en der samler alle 11 spørgsmål

FTIALLEINDI_query <- list(
  INDIKATOR = "*",
  Tid = "*"
)

FTIALLEINDI<- dst_get_data(
  table = "FORV1",
  query = FTIALLEINDI_query,
  meta_data = voresfti1,
  lang = "da"
)

#Lad os lige rense FTI ALLE INDI
#først lad os se hvad der sker
unique(FTIALLEINDI$INDIKATOR)

#fjerner F11 fra INDIKATOR
FTIALLEINDI_11_spg <- FTIALLEINDI[!grepl("^F11|^F1 ", FTIALLEINDI$INDIKATOR), ]
#beholder kun tid fra 1999 og frem
FTIALLEINDI_11_spg <- FTIALLEINDI_11_spg[FTIALLEINDI_11_spg$TID >= as.Date("2000-01-01"), ]


sum(is.na(FTIALLEINDI_11_spg$value))  #antal NA i value (1460 i alle. nu det 0 jibiii

#skal laves til kvartaler

#tager kun F2 og sætter månederne i rækkefølge
df_spm <- FTIALLEINDI_11_spg[grepl("^F2 ", FTIALLEINDI_11_spg$INDIKATOR), ]
df_spm <- df_spm[order(df_spm$TID), ]

#3 rækker pr. kolonne, så hver kolonne er et kvartal (jan-feb-mar osv.)
matrix(df_spm$value, nrow = 3)[, 1:2]  #kig på de to første kvartaler

#gennemsnit af hver kolonne = ét tal pr. kvartal
spm_kvartal <- colMeans(matrix(df_spm$value, nrow = 3))
head(spm_kvartal)

#sorterede måneder, bruges til at lave kvartalsnavne
måneder <- sort(unique(FTIALLEINDI_11_spg$TID))
format(måneder[1], "%m")  #skal være "01"
length(måneder) %% 3      #skal være 0, ellers er sidste kvartal ikke fuldt

første_måned <- måneder[seq(1, length(måneder), by = 3)]  #første måned i hvert kvartal
df_kvartal <- data.frame(kvartal = paste0(format(første_måned, "%Y"), "K",
                                          (as.numeric(format(første_måned, "%m")) - 1) %/% 3 + 1))

spørgsmål <- unique(FTIALLEINDI_11_spg$INDIKATOR)

for (i in spørgsmål) {
  df_spm <- FTIALLEINDI_11_spg[FTIALLEINDI_11_spg$INDIKATOR == i, ]
  df_spm <- df_spm[order(df_spm$TID), ]
  df_kvartal[[i]] <- colMeans(matrix(df_spm$value, nrow = 3))  #ny kolonne pr. spørgsmål
}

dim(df_kvartal)  #kolonner skal være 12: kvartal + 11 spørgsmål
head(df_kvartal)

#de 11 spørgsmål i første kvartal som en almindelig talrække
første_kvartal <- unlist(df_kvartal[1, 2:ncol(df_kvartal)])

kombi_par <- combn(første_kvartal, 2)  #alle par af spørgsmål
dim(kombi_par)  #2 rækker, og så et antal kolonner, én pr. par

choose(11, 2)  #antal par, skal være det samme som antal kolonner

indikator_par <- colMeans(kombi_par)  #gennemsnit pr. par = indikatorværdi for 1. kvartal
head(indikator_par)

sum(choose(11, 1:11))

antal_spm <- ncol(df_kvartal) - 1  #11 spørgsmål
alle_kombi <- list()  #ét element pr. kombinationsstørrelse

for (j in 1:antal_spm) {
  kombi_matrix <- matrix(NA, nrow = nrow(df_kvartal), ncol = choose(antal_spm, j))  #tom matrix til størrelse j
  for (i in 1:nrow(df_kvartal)) {
    kvartal_tal <- unlist(df_kvartal[i, 2:ncol(df_kvartal)])  #spørgsmålene i kvartal i
    kombi_matrix[i, ] <- colMeans(combn(kvartal_tal, j))  #gennemsnit af hver kombination
  }
  alle_kombi[[j]] <- kombi_matrix
}

sapply(alle_kombi, ncol)  #11 55 165 330 462 462 330 165 55 11 1
sum(sapply(alle_kombi, ncol))  #skal give 2047 og det gør det

### --- Indikator projekt - alle indikatore --- ###

indikator_liste <- rep(list(rep(list())), 11)
z_length <- length(df_kvartal[ , 2:ncol(df_kvartal)])

for (z_length in 1:z_length) {
  l <- length(colMeans(matrix(as.numeric(combn(df_kvartal[i,2:ncol(df_kvartal)], z_length)), nrow = z_length)))
  df_combi_d <- matrix(0, nrow = nrow(df_kvartal), ncol = l)
  for (i in 1:nrow(df_kvartal)) {
    df_combi_d[i, ] <- colMeans(matrix(as.numeric(combn(df_kvartal[i,2:ncol(df_kvartal)], z_length)), nrow = z_length))
    indikator_liste[[z_length]] <- df_combi_d
  }
}

samlet_indikator_df_combi <- data.frame(indikator_liste)

# Opdateret loop med navngivet kolonner,
indikator_liste <- rep(list(rep(list())), 11)
z_length <- length(df_kvartal[ , 2:ncol(df_kvartal)])
col_names <- c("F2", "F3", "F4", "F5", "F9", "F6", "F7", "F8", "F10", "F12", "F13" )

for (z_length in 1:z_length) {
  l <- length(colMeans(matrix(as.numeric(combn(df_kvartal[i,2:ncol(df_kvartal)], z_length)), nrow = z_length)))
  df_combi_d <- matrix(0, nrow = nrow(df_kvartal), ncol = l)
  names <- combn(col_names, z_length)
  names <- combn(col_names, z_length, paste, collapse = ",")
  colnames(df_combi_d) <- names
  for (i in 1:nrow(df_kvartal)) {
    df_combi_d[i, ] <- colMeans(matrix(as.numeric(combn(df_kvartal[i,2:ncol(df_kvartal)], z_length)), nrow = z_length))
    indikator_liste[[z_length]] <- df_combi_d
  }
}

samlet_indikator_df_combi <- data.frame(indikator_liste)



### --- Lave LM på dem, OLA3? --- ###

# Nu skal vi jo bruge vores y :))

nkh1_meta <- dst_meta(table = "NKH1", lang = "da")

df_forbrug <- dst_get_data(
  table = "NKH1",
  TRANSAKT = "P.31 Husholdningernes forbrugsudgifter",
  PRISENHED = "2020-priser, kædede værdier",
  SÆSON = "Sæsonkorrigeret",
  Tid = "*",
  meta_data = nkh1_meta,
  lang = "da"
)

df_forbrug <- df_forbrug[order(df_forbrug$TID), ]
head(df_forbrug, 1)  #starter 1990K1
tail(df_forbrug, 1)  #slutter 2026K2

#derefter laver vi forbruget om til en tidsserie og regner væksten i forhold til samme kvartal året før
ts_forbrug <- ts(df_forbrug$value, start = c(1990, 1), frequency = 4)
vaekst <- (exp(diff(log(as.numeric(ts_forbrug)), lag = 4)) - 1) * 100
ts_realvaekst <- ts(vaekst, start = c(1991, 1), frequency = 4)

#nu kan vi starte i 2000K1, ligesom DI
ts_realvaekst_fra2000 <- window(ts_realvaekst, start = c(2000, 1))
end(ts_realvaekst_fra2000)  #seneste kvartal med vækst: 2026K2

df_vaekst_kvartal <- data.frame(
  kvartal = round(as.numeric(time(ts_realvaekst_fra2000)), 2),
  realvaekst = as.numeric(ts_realvaekst_fra2000)
)

df_kvartal_clean <- samlet_indikator_df_combi[-107, ]

samlet_indikator_forbrug <- cbind(df_vaekst_kvartal, df_kvartal_clean)


# Loope lm :))
#Lav LM loop opg 1.

lm_indikator_liste <- rep(list(rep(list())), 2047)

for (i in 3:ncol(samlet_indikator_forbrug)) {
  lm_indikator <- lm(samlet_indikator_forbrug$realvaekst ~ samlet_indikator_forbrug[[i]])
  lm_indikator_liste[[i-2]] <- summary(lm_indikator)
}

#vi finder R2 "adj.r.squared"
names(lm_indikator_liste[[1]])
adj_r2 <- sapply(lm_indikator_liste, function(x) x$adj.r.squared)
rss <- sapply(lm_indikator_liste, function(x) sum(x$residuals^2))

# Dataframe med kombinationer, r^2 og RSS
df_r2 <- data.frame(
  kombination = names(samlet_indikator_forbrug)[3:ncol(samlet_indikator_forbrug)],
  adj_r2 = adj_r2,
  RSS = rss
)

# Top 10 😎
nrow(df_r2)
head(df_r2)

top10 <- head(df_r2[order(df_r2$adj_r2, decreasing = TRUE), ], 10)
top10


### --- Tjekke for stabilitet --- ###
# Overvejelser 
# Vende fortegn om, i udvalgte spørgsmål f6, f7, f8, for at få en fælles retning
# for hvad der er godt og hvad der er skidt

df_kvartal$`F6 Priser i dag, sammenlignet med for et år siden` <- df_kvartal$`F6 Priser i dag, sammenlignet med for et år siden` * -1
df_kvartal$`F8 Arbejdsløsheden om et år, sammenlignet med i dag` <- df_kvartal$`F8 Arbejdsløsheden om et år, sammenlignet med i dag` * -1

# Tjekke antagelse af korrelation er rigtigt
val_cor_tillid <- df_kvartal[-107, ]

cor(val_cor_tillid$`F2 Familiens økonomiske situation i dag, sammenlignet med for et år siden`, samlet_indikator_forbrug$realvaekst)
# 0.4822062
cor(val_cor_tillid$`F3 Familiens økonomiske  situation om et år, sammenlignet med i dag`, samlet_indikator_forbrug$realvaekst)
# 0.2966892
cor(val_cor_tillid$`F4 Danmarks økonomiske situation i dag, sammenlignet med for et år siden`, samlet_indikator_forbrug$realvaekst)
# 0.5792219
cor(val_cor_tillid$`F5 Danmarks økonomiske situation om et år, sammenlignet med i dag`, samlet_indikator_forbrug$realvaekst)
# 0.1426621
cor(val_cor_tillid$`F6 Priser i dag, sammenlignet med for et år siden`, samlet_indikator_forbrug$realvaekst)
# 0.2989761
cor(val_cor_tillid$`F7 Priser om et år, sammenlignet med i dag`, samlet_indikator_forbrug$realvaekst)
# 0.3489288 (original, vi overvejede at vende den om) 
cor(val_cor_tillid$`F8 Arbejdsløsheden om et år, sammenlignet med i dag`, samlet_indikator_forbrug$realvaekst)
# 0.5027595
cor(val_cor_tillid$`F9 Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`, samlet_indikator_forbrug$realvaekst)
# 0.4813302
cor(val_cor_tillid$`F10 Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.`, samlet_indikator_forbrug$realvaekst)
# 0.6615258
cor(val_cor_tillid$`F12 Regner med at kunne spare op i de kommende 12 måneder`, samlet_indikator_forbrug$realvaekst)
# 0.4664607
cor(val_cor_tillid$`F13 Familiens økonomiske situation lige nu: kan spare/penge slår til/ bruger mere end man tjener`, samlet_indikator_forbrug$realvaekst)
# 0.4828819


# TSS
tss <- var(samlet_indikator_forbrug$realvaekst) * 
  (length(samlet_indikator_forbrug$realvaekst) - 1)
df_r2$TSS <- tss


# Stabiliserer :))
# Vi blev enige om at fjerne 24 kvartaller fra sættet, så i alt 6 år

# Loop 1, fjerne kvartaller (24) fra 2000
# Datasættet bliver gradvist mindre, og ender på 82 observationer
sub_loop1_lm_liste <- rep(list(rep(list())), 2047)
loop1_lm_liste_samlet <- rep(list(rep(list(rep(list())))), 24)

for (i in 1:24) {
  sub_loop1_samlet_df <- samlet_indikator_forbrug
  sub_loop1_samlet_df <- sub_loop1_samlet_df[-(1:i), ]
  for (j in 3:ncol(sub_loop1_samlet_df)) {
    loop1_lm_indikator <- lm(sub_loop1_samlet_df$realvaekst ~ sub_loop1_samlet_df[[j]])
    sub_loop1_lm_liste[[j-2]] <- summary(loop1_lm_indikator)
  }
  loop1_lm_liste_samlet[[i[]]] <- sub_loop1_lm_liste
}

View(loop1_lm_liste_samlet)

# Vi mangler at slå gennemsnit, put a pin in it


# Loop 2, fjerne kvartaller (24) fra 2026
# Datasættet bliver gradvist mindre, og ender på 82 observationer
sub_loop2_lm_liste <- rep(list(rep(list())), 2047)
loop2_lm_liste_samlet <- rep(list(rep(list(rep(list())))), 24)

for (h in 1:1) {
  sub_loop2_samlet_df <- samlet_indikator_forbrug
  for (i in 1:24) {
    sub_loop2_samlet_df <- sub_loop2_samlet_df[-nrow(sub_loop2_samlet_df), ]
    for (j in 3:ncol(sub_loop2_samlet_df)) {
      loop2_lm_indikator <- lm(sub_loop2_samlet_df$realvaekst ~ sub_loop2_samlet_df[[j]])
      sub_loop2_lm_liste[[j-2]] <- summary(loop2_lm_indikator)
    }
    loop2_lm_liste_samlet[[i[]]] <- sub_loop2_lm_liste
  } 
}

View(loop2_lm_liste_samlet)

# Loop 3, fjerne kvartaller (24) i rul
# Datasættet bliver har altid 82 observationer, men i et rullende interval
sub_loop3_lm_liste <- rep(list(rep(list())), 2047)
loop3_lm_liste_samlet <- rep(list(rep(list(rep(list())))), 24)

for (i in 1:24) {
  sub_loop3_samlet_df <- samlet_indikator_forbrug
  loop3_start <- 0
  loop_int_længde <- nrow(sub_loop3_samlet_df)-25
  sub_loop3_samlet_df <- sub_loop3_samlet_df[(loop3_start + i):(loop_int_længde + i), ]
  for (j in 3:ncol(sub_loop3_samlet_df)) {
    loop3_lm_indikator <- lm(sub_loop3_samlet_df$realvaekst ~ sub_loop3_samlet_df[[j]])
    sub_loop3_lm_liste[[j-2]] <- summary(loop3_lm_indikator)
  }
  loop3_lm_liste_samlet[[i[]]] <- sub_loop3_lm_liste
}

View(loop3_lm_liste_samlet)

# Loop 4, fjerne kvartaller (24) fra begge ender af datasæt
# OBS: Har lavet 12 intervaller i stedet for 24, fordi jeg fjerner 2 ad gangen
# Så man ender med stadig at have fjernet 24 kvartaller i alt
# Datasættet bliver gradvist mindre, og ender på 82 observationer
sub_loop4_lm_liste <- rep(list(rep(list())), 2047)
loop4_lm_liste_samlet <- rep(list(rep(list(rep(list())))), 12)

for (h in 1:1) {
  sub_loop4_samlet_df <- samlet_indikator_forbrug
  for (i in 1:12) {
    sub_loop4_samlet_df <- sub_loop4_samlet_df[-1, ]
    sub_loop4_samlet_df <- sub_loop4_samlet_df[-nrow(sub_loop4_samlet_df), ]
    for (j in 3:ncol(sub_loop4_samlet_df)) {
      loop4_lm_indikator <- lm(sub_loop4_samlet_df$realvaekst ~ sub_loop4_samlet_df[[j]])
      sub_loop4_lm_liste[[j-2]] <- summary(loop4_lm_indikator)
    }
    loop4_lm_liste_samlet[[i[]]] <- sub_loop4_lm_liste
  } 
}

View(loop4_lm_liste_samlet)




#Casper laver PCA, PLS og RR
library(pls)

#PCA nu (tid er fra 2000 til 2026K2)
df_pca_11x <- df_kvartal[-107, 2:ncol(df_kvartal)]  #rækkerne skal være ens i X og Y, så vi bruger 106 i stedet for 107
names(df_pca_11x) <- trimws(substr(names(df_pca_11x), 1, 3))  #de første 3 tegn er F-nummeret, trimws fjerner mellemrummet efter F2, F3 osv.
head(df_pca_11x)

#        F2       F3         F4         F5         F9        F6         F7         F8       F10      F12      F13
#1 2.333333 13.00000 -3.3333333 -8.0000000 -6.3333333 14.000000  -7.333333  5.3333333 -7.333333 27.33333 24.00000
#2 7.333333 15.00000  2.0000000 -0.6666667 -2.0000000 10.000000  -9.000000  9.0000000 -6.333333 29.33333 26.33333
#3 6.333333 15.00000  1.6666667 -1.0000000 -8.0000000  9.666667  -9.666667  7.3333333 -8.333333 24.66667 25.33333
#4 4.666667 13.00000 -0.6666667 -6.0000000 -0.3333333  8.666667  -4.000000  0.3333333 -8.666667 22.33333 23.33333
#5 4.333333 14.33333  0.0000000 -5.0000000 -6.0000000 13.333333 -11.000000  0.3333333 -7.333333 27.66667 24.66667
#6 5.666667 12.66667 -5.6666667 -7.0000000 -6.0000000  9.666667  -8.333333 -2.6666667 -6.333333 27.33333 25.33333
dim(df_pca_11x)
#[1] 106  11

#?prcomp
pca_model <- prcomp(df_pca_11x, scale. = TRUE)  #scale. = TRUE så alle spørgsmål tæller lige meget
summary(pca_model)
#Importance of components:
#                         PC1    PC2    PC3    PC4     PC5     PC6     PC7     PC8     PC9    PC10    PC11
#Standard deviation     2.4834 1.3491 1.1047 0.7707 0.68761 0.48383 0.44673 0.35986 0.27606 0.24866 0.15600
#Proportion of Variance 0.5607 0.1655 0.1109 0.0540 0.04298 0.02128 0.01814 0.01177 0.00693 0.00562 0.00221
#Cumulative Proportion  0.5607 0.7261 0.8371 0.8911 0.93404 0.95532 0.97347 0.98524 0.99217 0.99779 1.00000

pca_model$rotation[, 1:11]  #vi tager 1 til 11

#round(pca_model$rotation[, 1:11], 2)  #wow se
#.    PC1   PC2   PC3   PC4   PC5   PC6   PC7   PC8   PC9   PC10  PC11
#F2  -0.37 -0.06  0.12 -0.29  0.22 -0.22  0.21  0.25  0.17 -0.20  0.70
#F3  -0.34 -0.30  0.05 -0.06  0.10 -0.46  0.30  0.27 -0.37  0.00 -0.51
#F4  -0.33  0.09 -0.39 -0.04  0.46  0.06 -0.02 -0.43  0.27 -0.43 -0.27
#F5  -0.25 -0.31 -0.32  0.51 -0.55 -0.27 -0.02 -0.18  0.11 -0.14  0.21
#F9  -0.33 -0.06 -0.04 -0.46 -0.48  0.47 -0.21  0.13 -0.21 -0.32 -0.10
#F6  -0.34 -0.27  0.18 -0.17 -0.06  0.26  0.30 -0.42  0.23  0.60 -0.03
#F7   0.02  0.63 -0.26 -0.35 -0.33 -0.36  0.29 -0.23 -0.10  0.13  0.01
#F8  -0.27  0.23 -0.52  0.30  0.16  0.38  0.18  0.41 -0.19  0.32  0.10
#F10 -0.36  0.13  0.02 -0.05  0.12 -0.28 -0.78 -0.04 -0.10  0.37  0.04
#F12 -0.26  0.31  0.48  0.38  0.06  0.14  0.12 -0.34 -0.51 -0.20  0.14
#F13 -0.28  0.39  0.36  0.23 -0.19 -0.01  0.03  0.34  0.58 -0.05 -0.30


?loadings
pca_model_mloadings <- pca_model$loadings  #vi tager 1 til 11

library(pls)  #tid til at bruge pls

#Det før var PCA. her er Baums PCR
df_pcr <- data.frame(df_vaekst_kvartal$realvaekst, df_pca_11x)  #Y og de 11 spørgsmål i samme dataframe

pcr_model <- pcr(df_vaekst_kvartal$realvaekst ~ ., data = df_pca_11x, scale = TRUE, validation = "CV")  #Y hentes fra den anden dataframe
summary(pcr_model)

#Data: 	X dimension: 106 11 
#Y dimension: 106 1
#Fit method: svdpc
#Number of components considered: 11

#VALIDATION: RMSEP
#Cross-validated (CV) using 10 random segments.
#          (Intercept)  1 comps  2 comps  3 comps  4 comps  5 comps  6 comps  7 comps  8 comps  9 comps  10 comps  11 comps
#CV           2.767    2.335    2.064    2.063    2.044    2.047    2.059    2.025    2.024    2.006     2.018     2.028
#adjCV        2.767    2.331    2.059    2.058    2.038    2.041    2.062    2.015    2.014    1.993     2.005     2.014

#TRAINING: % variance explained
#                                    1 comps  2 comps  3 comps  4 comps  5 comps  6 comps  7 comps  8 comps  9 comps  10 comps  11 comps
#X                                  56.07    72.61    83.71    89.11     93.4    95.53    97.35    98.52    99.22     99.78     100.0
#df_vaekst_kvartal$realvaekst (y)   31.36    47.27    48.23    49.96     50.5    50.50    54.97    56.00    58.39     58.53      58.6

vægt_første_komponent <- pcr_model$loadings[, 1]^2  #kolonne 1 (PC1) kvadreret = vægt pr. spørgsmål
sum(vægt_første_komponent)  #skal give 1 and it doooo

sort(vægt_første_komponent, decreasing = TRUE)  #de tre øverste er svaret på 2.2
#.    F2          F10           F3           F6           F9           F4          F13           F8          F12           F5           F7 
#0.1388591918 0.1313238597 0.1182017294 0.1180716174 0.1104692826 0.1057644269 0.0758846190 0.0713365668 0.0678825997 0.0619002141 0.0003058925 

#så det tid til opgave 2.2
#Hvilke 3 spørgsmål i forbrugerforventningsundersøgelsen har de højeste vægte? 
#Diskutér om spørgsmålene giver analytisk mening.






