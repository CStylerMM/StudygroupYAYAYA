#Opgave 1

#tid til at lave vores egen mikroindikator
#er mikroindikatoren bedre end dst og DI?
#brug data fra 1. kvartal til 3.kvartal 2026 (nyeste tid)

#opgave 1.1 Lav alle kombinationer af de 12 spørgsmål i forbrugerundersøgelsen af DST. I skal bruge data fra 1.
#kvartal 2000 til og med 3. kvartal 2025.

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

# F11 udeladt: serien ophører efter 2025M04 (17 NA'er i 2025M05-2026M09)
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

indikator_liste <- rep(list(rep(list())), 11)
z_length <- length(df_kvartal[ , 2:ncol(df_kvartal)])

#vi vender fortegnet 

df_kvartal$`F6 Priser i dag, sammenlignet med for et år siden` <- df_kvartal$`F6 Priser i dag, sammenlignet med for et år siden` * -1
df_kvartal$`F8 Arbejdsløsheden om et år, sammenlignet med i dag` <- df_kvartal$`F8 Arbejdsløsheden om et år, sammenlignet med i dag` * -1

samlet_indikator_df_combi <- data.frame(indikator_liste)

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
r2 <- sapply(lm_indikator_liste, function(x) x$r.squared)

View(lm_indikator_liste)
# Dataframe med kombinationer, r^2 og RSS
df_r2 <- data.frame(
  kombination = names(samlet_indikator_forbrug)[3:ncol(samlet_indikator_forbrug)],
  adj_r2 = adj_r2,
  RSS = rss,
  r2 = r2
)

#skal vi køre r^2 i stedet for adj. r^2, adj. R² straffer for antal parametre så muligvis bare køre
#adj. r^2
# Top 10 😎
nrow(df_r2)
head(df_r2)

top10 <- head(df_r2[order(df_r2$adj_r2, decreasing = TRUE), ], 10)
top10

#korrelation
y <- samlet_indikator_forbrug$realvaekst
x_vores <- samlet_indikator_forbrug[["F4.F9.F7.F10.F12.F13"]]
x_di    <- samlet_indikator_forbrug[["F2.F4.F9.F10"]]

cor(x_vores, y)
cor(x_di, y)

sammenligning$korrelation <- c(cor(x_vores, y), cor(x_di, y))
sammenligning
sammenligning$korrelation^2 #Med én regressor er R² lig korrelationen i anden.

bedste <- df_r2[which.max(df_r2$adj_r2), c("kombination", "adj_r2", "RSS")]
di     <- df_r2[df_r2$kombination == "F2.F4.F9.F10", c("kombination", "adj_r2", "RSS")]

sammenligning <- rbind(bedste, di)
sammenligning$indikator <- c("Vores bedste", "DI-FTI")
sammenligning

bedste$kombination        # skal være F4.F9.F7.F10.F12.F13
nrow(di)                  # skal være 1

#korrelation
y <- samlet_indikator_forbrug$realvaekst
x_vores <- samlet_indikator_forbrug[["F4.F9.F7.F10.F12.F13"]]
x_di    <- samlet_indikator_forbrug[["F2.F4.F9.F10"]]

cor(x_vores, y)
cor(x_di, y)

sammenligning$korrelation <- c(cor(x_vores, y), cor(x_di, y))
sammenligning
sammenligning$korrelation^2 #Med én regressor er R² lig korrelationen i anden.

sammenligning

#Opgave 1.3


# antal spørgsmål i hver kombination
df_r2$antal <- sapply(strsplit(df_r2$kombination, ".", fixed = TRUE), length)

# 1) bedste indikator for hvert antal spørgsmål (1 til 11)
bedste_pr_antal <- do.call(rbind, lapply(1:11, function(k) {
  d <- df_r2[df_r2$antal == k, ]
  d[which.max(d$adj_r2), c("kombination", "antal", "adj_r2", "RSS")]
}))
bedste_pr_antal

# 2) de tre bedste uden F7
uden_F7 <- df_r2[!grepl("F7", df_r2$kombination), ]
head(uden_F7[order(-uden_F7$adj_r2), c("kombination", "antal", "adj_r2")], 3)

# 3) enkeltspørgsmålene (til diskussionen af hvert spørgsmål)
df_r2[df_r2$antal == 1, c("kombination", "adj_r2")]


#1.4

#er oktober tallene udgivet?

max(FTIALLEINDI$TID)

lm_vores <- lm(y ~ x_vores)
lm_di    <- lm(y ~ x_di)

x_k3_vores <- samlet_indikator_df_combi[107, "F4.F9.F7.F10.F12.F13"]
x_k3_di    <- samlet_indikator_df_combi[107, "F2.F4.F9.F10"]

predict(lm_vores, newdata = data.frame(x_vores = x_k3_vores), interval = "prediction")
predict(lm_di,    newdata = data.frame(x_di = x_k3_di),       interval = "prediction")


pred_k3_vores <- predict(lm_vores, newdata = data.frame(x_vores = x_k3_vores))
pred_k3_di    <- predict(lm_di,    newdata = data.frame(x_di = x_k3_di))

data.frame(kvartal = c("2026K3", "2026K4 (scenarie)"),
           vores = round(rep(pred_k3_vores, 2), 2),
           di    = round(rep(pred_k3_di, 2), 2))