#Samlet opg.1 opg.2 opg 3.
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



#Så det opgave 2

#Opgave 2: PCA, PCR, PLS og RR
#vi starter med X og Y fra opgave 1

library(dkstat)

#X: spørgsmålene fra forbrugerundersøgelsen, månedligt fra DST
forv_meta <- dst_meta(table = "FORV1", lang = "da")
forv_alle <- dst_get_data(table = "FORV1",
                          query = list(INDIKATOR = "*", Tid = "*"),
                          meta_data = forv_meta, lang = "da")

#uden F1 (totalen) og F11, og kun fra 2000
df_11spg <- forv_alle[!grepl("^F11|^F1 ", forv_alle$INDIKATOR), ]
df_11spg <- df_11spg[df_11spg$TID >= as.Date("2000-01-01"), ]
sum(is.na(df_11spg$value))   #har vi nogle na'er - nope
unique(df_11spg$INDIKATOR)
#månederne laves til kvartaler: 3 måneder pr. kolonne, så gennemsnit
måneder <- sort(unique(df_11spg$TID))
length(måneder) %% 3   #skal være 0, ellers er sidste kvartal ikke fuldt

første_måned <- måneder[seq(1, length(måneder), by = 3)]
df_kvartal <- data.frame(kvartal = paste0(format(første_måned, "%Y"), "K",
                                          (as.numeric(format(første_måned, "%m")) - 1) %/% 3 + 1))

for (i in unique(df_11spg$INDIKATOR)) {
  df_spm <- df_11spg[df_11spg$INDIKATOR == i, ]
  df_spm <- df_spm[order(df_spm$TID), ]
  df_kvartal[[i]] <- colMeans(matrix(df_spm$value, nrow = 3))   #ny kolonne pr. spørgsmål
}
dim(df_kvartal)   #kvartal + 11 spørgsmål = 12 kolonner

#Y: årlig realvækst i husholdningernes forbrugsudgift
df_forbrug <- dst_get_data(table = "NKH1",
                           TRANSAKT = "P.31 Husholdningernes forbrugsudgifter",
                           PRISENHED = "2020-priser, kædede værdier",
                           SÆSON = "Sæsonkorrigeret",
                           Tid = "*",
                           meta_data = dst_meta(table = "NKH1", lang = "da"),
                           lang = "da")
df_forbrug <- df_forbrug[order(df_forbrug$TID), ]

ts_forbrug <- ts(df_forbrug$value, start = c(1990, 1), frequency = 4)
vaekst <- (exp(diff(log(as.numeric(ts_forbrug)), lag = 4)) - 1) * 100
ts_realvaekst <- window(ts(vaekst, start = c(1991, 1), frequency = 4), start = c(2000, 1))

df_vaekst_kvartal <- data.frame(kvartal = round(as.numeric(time(ts_realvaekst)), 2),
                                realvaekst = as.numeric(ts_realvaekst))

#F6 F7 og F8 vendes, så højere tal er bedre stemning for alle

##Vi har egentlig også gjort dette i opgave 1
##Men nu tilføjer vi også F7
#df_kvartal$`F6 Priser i dag, sammenlignet med for et år siden` <- df_kvartal$`F6 Priser i dag, sammenlignet med for et år siden` * -1
df_kvartal$`F8 Arbejdsløsheden om et år, sammenlignet med i dag` <- df_kvartal$`F8 Arbejdsløsheden om et år, sammenlignet med i dag` * -1
df_kvartal$`F7 Priser om et år, sammenlignet med i dag` <- df_kvartal$`F7 Priser om et år, sammenlignet med i dag` * -1
#X og Y skal have lige mange rækker: X har et kvartal mere end Y, det sidste tages fra
df_pca_11x <- df_kvartal[1:nrow(df_vaekst_kvartal), 2:ncol(df_kvartal)]
names(df_pca_11x) <- trimws(substr(names(df_pca_11x), 1, 3))   #F2, F3 osv.
dim(df_pca_11x)   #skal give 106 x 11


#okay okay lad os kigge på det
#trin 1: problemet, almindelig regression (OLS) på alle 11 spørgsmål
df_pcr <- data.frame(realvaekst = df_vaekst_kvartal$realvaekst, df_pca_11x)   #Y først, så de 11 X

summary(lm(realvaekst ~ ., data = df_pcr))
#AHA: R² = 0,586, men mange spørgsmål er ikke signifikante hver for sig
#og nogle har mærkelige fortegn. Spørgsmålene stjæler forklaring fra hinanden.
#0,586 er også tallet, PCR skal ramme igen til sidst (tjek)

round(cor(df_pca_11x), 2)
#AHA: mange tal tæt på 1, så spørgsmålene siger næsten det samme = multikollinearitet


#trin 2: PCA, kun på X
pca_model <- prcomp(df_pca_11x, scale. = TRUE)   #scale. = TRUE, ellers vinder det største tal

summary(pca_model)
#AHA: kig på Cumulative Proportion. PC1 = 56 %, to komponenter = 73 %, tre = 84 %
#11 spørgsmål kan koges ned til 2-3 uden at miste meget af X. Y er ikke med endnu


#trin 3: PCR, PCA + regression, med cross validation
library(pls)
set.seed(1)   #så CV giver samme tal hver gang
pcr_model <- pcr(realvaekst ~ ., data = df_pcr, scale = TRUE, validation = "CV")

summary(pcr_model)
#AHA 1, øverst (CV/RMSEP): fejlen falder 2,77 til ca. 2,10 ved to komponenter og flader så ud
#AHA 2, nederst (% variance explained, Y): to komponenter = 47 %, alle 11 = 58,6 %
#58,6 % er det samme som OLS i trin 1, så alt er sat rigtigt op

validationplot(pcr_model, val.type = "RMSEP")
#AHA: skarp albue ved 2. Efter det er det bare støj omkring 2,0-2,15


#trin 4: opskriften på komponenterne og vægtene
round(pca_model$rotation[, 1:2], 2)
#AHA: PC1 har samme fortegn på næsten alle = "den generelle stemning". F7 er næsten 0
#PC2 er næsten kun F7, den fanger det, der ikke følger stemningen

vægt_PC1 <- pcr_model$loadings[, 1]^2   #PC1 kvadreret
sum(vægt_PC1)   #skal give 1

sort(vægt_PC1, decreasing = TRUE)
#AHA: F2 (0,139), F10 (0,131), F3 (0,118) er top 3 = svaret på 2.2
#F6 ligger lige efter F3, F7 er 0,0003

#opg 2.2
#Hvilke 3 spørgsmål i forbrugerforventningsundersøgelsen har de højeste vægte?
#Diskutér om spørgsmålene giver analytisk mening

library(ggplot2)

vægt_PC1 <- pcr_model$loadings[, 1]^2   #vægtene fra PC1, kvadreret så summen er 1
top3 <- sort(vægt_PC1, decreasing = TRUE)[1:3]
round(top3, 3)   #F2 0,139, F10 0,131, F3 0,118

#plot af alle vægtene, så man kan se, hvor store forskellene er
vægt_data <- data.frame(spørgsmål = names(vægt_PC1), vægt = vægt_PC1)

ggplot(vægt_data, aes(x = reorder(spørgsmål, vægt), y = vægt)) +
  geom_col(fill = "hotpink") +
  geom_text(aes(label = round(vægt, 3)), hjust = 1.2, size = 4.5, color = "white") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title   = "Hvilke spørgsmål vejer mest i første komponent?",
       x       = NULL,
       y       = "Vægt (kvadreret loading, sum = 1)",
       caption = "Kilde: Danmarks Statistik, FORV1") +
  theme_minimal()

#SVAR:
#De tre højeste vægte er F2 (familiens økonomi i dag), F10 (hvor meget familien vil
#bruge på større forbrugsgoder de næste 12 mdr.) og F3 (familiens økonomi om et år).
#
#F2 og F10 giver analytisk mening: de handler om husstandens egen økonomi og dens
#købsplaner, og det er tæt på selve forbrugsbeslutningen. De to indgår også i DI's
#egen indikator.
#F3 giver mindre mening: det er en forventning til fremtiden, og DI smed det spørgsmål ud.
#
#Vægtene er fra PCA, som kun kigger på X. De siger, hvilke spørgsmål der følger den
#fælles stemning, ikke hvilke der bedst forudsiger forbruget.
#F3 og F6 er reelt lige tunge (0,1182 mod 0,1181), så nummer 3 er et hårsbred.
#F7 (priser om et år) vejer næsten ingenting (0,0003).

#opg 2.3 trin 1: modellens bud på 3. kvartal 2025
df_kvartal[103, 1]   #tjek: skal vise 3. kvartal 2025

x_2025K3 <- df_pca_11x[103, ]   #de 11 svar for det kvartal

predict(pcr_model, newdata = x_2025K3, ncomp = 2)   #modellens bud (årlig realvækst, %)
df_pcr$realvaekst[103]   #hvad det faktisk blev

#2.3 trin 2: er 0,07 mærkeligt?
mean(df_pcr$realvaekst)    #hvad er normal vækst i vores data?
range(df_pcr$realvaekst)   #laveste og højeste vækst

bud <- as.numeric(predict(pcr_model, newdata = x_2025K3, ncomp = 2))
df_pcr$realvaekst[103] - bud   #fejlen, sammenlign med RMSEP omkring 2,1

round(rbind(x_2025K3, gennemsnit = colMeans(df_pca_11x)), 1)   #svarene i 2025K3 mod et normalt kvartal

#2.3 trin 3: modellens vægte og et regnestykke i hånden
koef <- as.vector(coef(pcr_model, ncomp = 2))   #koefficienter på standardiserede spørgsmål
gns_x <- colMeans(df_pca_11x)
sd_x <- apply(df_pca_11x, 2, sd)

vægte_DI <- koef / sd_x   #omregnet så det er vækst-procentpoint pr. nettotalpoint
names(vægte_DI) <- names(df_pca_11x)
round(sort(vægte_DI, decreasing = TRUE), 3)   #modellens vægte, ligesom DI's

bud_manuelt <- mean(df_pcr$realvaekst) + sum(koef * (as.numeric(x_2025K3) - gns_x) / sd_x)
bud_manuelt   #skal give 0,0697, ellers er der en skalafejl

#2.3 trin 4: hvem trækker bruddet?
bidrag <- vægte_DI * (as.numeric(x_2025K3) - gns_x)   #vægt gange afvigelse fra normalen
round(sort(bidrag), 2)   #negative trækker ned, positive trækker op
mean(df_pcr$realvaekst) + sum(bidrag)   #gennemsnit + bidrag = modellens bud, skal give 0,07

round(sort(abs(setNames(koef, names(df_pca_11x))), decreasing = TRUE), 2)   #vægte på standardiserede spørgsmål

#SVAR 2.3:
#Modellen siger 0,07 % mod faktisk 2,36 %, en fejl på 2,3 procentpoint. Det er omkring en
#typisk prognosefejl (RMSEP ca. 2,1), så fejlen er ikke usædvanlig, men tallet er ikke
#til at stole på af tre grunde:
#1) Modellen forklarer kun 47 % af variationen i forbruget. Indkomst, beskæftigelse og
#   rente er ikke med.
#2) Prognosen er et sammenspil af modsatrettede kræfter: syv pessimistiske spørgsmål
#   trækker ned (ca. -2,3), mens F7 (forventede priser) alene trækker op (+0,7). Et enkelt
#   spørgsmål med en skæv skala flytter dermed meget.
#3) Nogle koefficienter har fortegn, der ikke giver mening (F3, F5 negative), og det er
#   resterne af multikollinearitet. Og 3. kvartal 2025 indgår i træningsdata, så det er en
#   tilpasning og ikke en ægte prognose.

#2.4: faktisk realvækst 2016
mean(df_pcr$realvaekst[65:68])   #2016K1 til 2016K4, række 65 er (2016-2000)*4+1

#opg 2.4
#DI skrev i oktober 2016, at forbrugertillidsindikatoren "tilsiger en årlig realvækst
#i privatforbruget på lidt over 2 pct. i 2016".
#I vores data blev gennemsnittet af de fire kvartaler i 2016 ca. 3,3 pct. (4,3 / 3,9 / 2,2 / 2,9).
#DI ramte altså ca. 1 procentpoint for lavt, hvilket er mindre end en typisk prognosefejl
#(RMSEP ca. 2,1), så DI var ikke dårlig, bare forsigtig.
#
#Forbehold: 3,3 er gennemsnit af kvartalsvise årsvækster, ikke den officielle årsvækst.
#Nationalregnskabet er revideret siden 2016, og vores Y er husholdningernes forbrugsudgift,
#mens DI skriver om privatforbruget. De to kan afvige lidt. Og DI skrev i oktober, før
#4. kvartal var kendt.

##Så det opgave 3.
#Opgave 3
library(dkstat)
#vi starter opg 3.1 med at hive forbrugsgoder ind via eurostat api
forbrugsgoder_meta <- dst_meta(table = "NKHC021", lang = "da")

#de 8 spørgsmål der indgår i vores mikro er  (F2,F3,F9,F10,F11,F12,F13,)
unique(forbrugsgoder_meta$variables)
forbrugsgoder_meta$values$FORMAAAL
forbrugsgoder_meta$values$PRISENHED
forbrugsgoder_meta$values$SÆSON


forbrugsgoder_query <- list(
  FORMAAAL  = "*",
  PRISENHED = "Løbende priser",
  SÆSON     = "Ikke sæsonkorrigeret",
  Tid       = "*"
)

forbrugsgoder_mikro <- dst_get_data(
  table     = "NKHC021",
  query     = forbrugsgoder_query,
  meta_data = forbrugsgoder_meta,
  lang      = "da"
)

#lad os sige fra 2000 og op efter. så det passer tidligere datasæt
forbrugsgoder_mikro <- forbrugsgoder_mikro[forbrugsgoder_mikro$TID >= as.Date("2000-01-01"),]
sum(is.na(forbrugsgoder_mikro))  #har vi nogle NA'er? - næij

#vi reshaper dataframe med baseR
rm(formaal)
# start med tidskolonnen fra første formål
forbrugsgoder_wide <- data.frame(TID = forbrugsgoder_mikro$TID[forbrugsgoder_mikro$FORMAAAL == forbrugsgoder_mikro$FORMAAAL[1]])
names(forbrugsgoder_mikro)
#laver loop så den sætter værdien under kolonnerne formaaal
for (f in unique(forbrugsgoder_mikro$FORMAAAL)) {
  forbrugsgoder_wide[[f]] <- forbrugsgoder_mikro$value[forbrugsgoder_mikro$FORMAAAL == f]
}

#lad os kigge på overall år til år hvilket forbrugdgode fører
#vi har ikke brug for "i alt" til dette, så vi laver et nyt df
forbrugsgoder_år1 <- forbrugsgoder_wide[, names(forbrugsgoder_wide) != "CPT I alt"]

#så laver vi kvartaler om til et år jubii
forbrugsgoder_år1$År <- as.numeric(format(forbrugsgoder_år1$TID, "%Y"))

#vi sætter lige "År" ved siden af "TID"
forbrugsgoder_år1 <- forbrugsgoder_år1[, c(1, 17, 2:16)]

#vi skal fjerne "TID" eller driller aggregate.

uden_tid <- forbrugsgoder_år1[, names(forbrugsgoder_år1) != "TID"]
#aggregate time yeah baby
forbrugsgoder_år2 <- aggregate(x   = uden_tid[, names(uden_tid) != "År"],
                               by  = list(År = uden_tid$År),
                               FUN = sum)

# fjerner 2026 pga vi ikke færdige med 2026 endnu :(
forbrugsgoder_år2 <- forbrugsgoder_år2[forbrugsgoder_år2$År < 2026, ]

#yay plot tid yay

library(ggplot2)

plotdata <- data.frame(
  formaal = sub("^CP[A-Z] ", "", names(forbrugsgoder_år2)[-1]),   # fjerner koden "CPA " osv.
  total   = colSums(forbrugsgoder_år2[, -1]) / 1000               # fra mio. til mia. kr.
)

ggplot(plotdata, aes(x = reorder(formaal, total), y = total)) +
  geom_col(fill = "hotpink") +
  geom_text(aes(label = round(total)), hjust = 1.2, size = 4.5, color = "white") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title    = "Hvad bruger danskerne penge på?",
       subtitle = "Samlet privatforbrug 2000-2025 fordelt på formål",
       x        = NULL,
       y        = "Mia. kr., løbende priser",
       caption  = "Kilde: Danmarks Statistik, NKHC021") +
  theme_minimal()

#her kan vi se det generelle billede af hvad danskerne bruger deres penge på^
#lad os finde ud af hvad de bruger i K4
# 1. Find rækkerne fra 4. kvartal (oktober)
K4 <- forbrugsgoder_år1[format(forbrugsgoder_år1$TID, "%m") == "10" & forbrugsgoder_år1$År < 2026, ]

# 2. Behold kun tallene (fjern TID og År)
K4_rental <- K4[, !(names(K4) %in% c("TID", "År"))]

# 3. Samme for hele året
år_rental <- forbrugsgoder_år2[, names(forbrugsgoder_år2) != "År"]

# 4. Andel af forbruget, i procent
K4_pct <- colSums(K4_rental) / sum(colSums(K4_rental)) * 100   #  K4
år_pct <- colSums(år_rental) / sum(colSums(år_rental)) * 100   # hele året

#lad os kigge på det
K4medårdata <- data.frame(
  formål  = sub("^CP[A-Z] ", "", names(K4_pct)),
  forskel = K4_pct - år_pct
)

ggplot(K4medårdata, aes(x = reorder(formål, forskel), y = forskel, fill = forskel > 0)) +
  geom_col() +
  geom_text(aes(label = round(forskel, 1), hjust = ifelse(forskel > 0, 1.2, -1.)),
            color = "white", size = 3.5) +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "hotpink", "FALSE" = "grey40"), guide = "none") +
  scale_y_continuous(expand = expansion(mult = 0.1)) +
  labs(title    = "Hvilke forbrugsgoder fylder mest i 4. kvartal?",
       subtitle = "Andel af forbruget i K4 minus andel i hele året, 2000-2025 (procentpoint)",
       x        = NULL,
       y        = "Procentpoint over/under årsgennemsnittet",
       caption  = "Kilde: Danmarks Statistik, NKHC021") +
  theme_minimal()
#niiiice
# hvor meget højere/lavere er K4 end et gennemsnitligt kvartal, pr. forbrugsgode
K4_udsving <- colMeans(as.matrix(K4_rental) / (as.matrix(år_rental) / 4))
sort(K4_udsving, decreasing = TRUE)   # kig på tallene


K4_udsving_data <- data.frame(
  formål  = sub("^CP[A-Z] ", "", names(K4_udsving)),
  udsving = (K4_udsving-1)*100
)

ggplot(K4_udsving_data, aes(x = reorder(formål, udsving), y = udsving)) +
  geom_col(fill = "hotpink") +
  geom_text(aes(label = round(udsving, 1)), hjust = -0.2, size = 3.5) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title    = "Hvor meget højere er forbruget i 4. kvartal end i et gennemsnitligt kvartal?",
       subtitle = "Gennemsnit 2000-2025, procent over et normalt kvartal",
       x        = NULL,
       y        = "Procent over gennemsnitligt kvartal",
       caption  = "Kilde: Danmarks Statistik, NKHC021") +
  theme_minimal()
#waooow elektricitet og tøj fører med 19,5 % og 17 %.
#hvorfor bruger danskerne mest på elektricitet - kan det være der er røvkoldt om vinteren??
#burde man subsette de her goder måske - og kigge på hvilke kunne være indenfor julehandel.
#Ja måske hvem ved.


#opgave 3.1 (men hvor jeg har kaldt den opg.3.1 rigtig.R)
#Opgave 3.1 - kan vi forudsige om julehandlen i 2025 er større end i 2024?
#vi starter med at hente forbruget i faste priser, så inflation ikke gør alle år til OP

library(dkstat)

forbrug_meta <- dst_meta(table = "NKHC021", lang = "da")

forbrug_query <- list(
  FORMAAAL  = "*",
  PRISENHED = "2020-priser, kædede værdier",
  SÆSON     = "Ikke sæsonkorrigeret",
  Tid       = "*"
)

forbrug_faste <- dst_get_data(
  table     = "NKHC021",
  query     = forbrug_query,
  meta_data = forbrug_meta,
  lang      = "da"
)

head(forbrug_faste)
unique(forbrug_faste$PRISENHED)  
#faste priser jepsi eller 2020-kædede kan vi jo sige hmm
#fra lang til bred, 
df_forbrug_bred <- data.frame(TID = forbrug_faste$TID[forbrug_faste$FORMAAAL == forbrug_faste$FORMAAAL[1]])
for (f in unique(forbrug_faste$FORMAAAL)) {
  df_forbrug_bred[[f]] <- forbrug_faste$value[forbrug_faste$FORMAAAL == f]
}

#kun fra 2000 og frem, så det passer med de andre data
df_forbrug_bred <- df_forbrug_bred[df_forbrug_bred$TID >= as.Date("1999-01-01"), ]

#kun K4 (oktober)
df_K4 <- df_forbrug_bred[format(df_forbrug_bred$TID, "%m") == "10", ]
dim(df_K4)   #skal give 26 rækker, 2000 til 2025

#julegoderne fra vores plot, uden elektricitet (det er vinter, ikke jul)
julegoder <- c("CPC Beklædning og fodtøj",
               "CPJ Information og kommunikation",
               "CPF Boligudstyr, husholdningstjenester mv.",
               "CPO Andre varer og tjenester",
               "CPA Fødevarer mv.")

#læg julegoderne sammen til ét tal pr. år
df_K4$julehandel <- rowSums(df_K4[, julegoder])
df_K4[, c("TID", "julehandel")]

#vækst i julehandlen, K4 mod K4 året før (i procent)
df_K4$vækst <- c(NA, diff(log(df_K4$julehandel)) * 100)   #første år har ingen forgænger, så NA

#OP hvis julehandlen er større end året før, ellers NED
df_K4$retning <- ifelse(df_K4$vækst >= 0, "OP", "NED")

df_K4[, c("TID", "vækst", "retning")]
table(df_K4$retning)   #hvor mange OP og NED?

#Nu skal vi bruge DST's forbrugertillidsindikator som X
forv_meta <- dst_meta(table = "FORV1", lang = "da")

forv_meta$variables
forv_meta$values$INDIKATOR   #hvad hedder de forskellige spørgsmål og totalen?

#forbrugertillidsindikatoren F1 fra DST, den kommer månedligt
forv_query <- list(
  INDIKATOR = "Forbrugertillidsindikatoren",
  Tid       = "*"
)

forv_F1 <- dst_get_data(
  table     = "FORV1",
  query     = forv_query,
  meta_data = forv_meta,
  lang      = "da"
)

#SÅ 
#Forbrugertilliden fra 2000 og frem, før det er der  jo mange NA'errrrr
forv_F1 <- forv_F1[forv_F1$TID >= as.Date("2000-01-01"), ]
sum(is.na(forv_F1$value))   #hvor mange NA er der tilbage? nul fætter guf

#år og kvartal på hver måned
forv_F1$År <- as.numeric(format(forv_F1$TID, "%Y"))
forv_F1$kvartal <- (as.numeric(format(forv_F1$TID, "%m")) - 1) %/% 3 + 1

#gennemsnit af de tre måneder i hvert kvartal
df_forv_kvartal <- aggregate(value ~ År + kvartal, data = forv_F1, FUN = mean)

#kun K3 (juli til september)
df_forv_K3 <- df_forv_kvartal[df_forv_kvartal$kvartal == 3, ]
df_forv_K3

#Sæt julehandel og forbrugertilliden sammen på år
df_K4$År <- as.numeric(format(df_K4$TID, "%Y"))

df_model <- merge(df_K4[, c("År", "julehandel", "vækst", "retning")],
                  df_forv_K3[, c("År", "value")],
                  by = "År")

#value er ikke særlig sigende, så den får et bedre navn
names(df_model)[names(df_model) == "value"] <- "tillid"

df_model

#lad os træne på 2001-2024, gemmer 2025 til endelig test (2000 har NA og falder væk)
df_træn <- df_model[df_model$År >= 2001 & df_model$År <= 2024, ]
df_test <- df_model[df_model$År == 2025, ]

#logistisk regression: retning (OP/NED) forklaret af forbrugertilliden i K3
glm_julehandel <- glm(as.factor(retning) ~ tillid, data = df_træn, family = "binomial")
summary(glm_julehandel)

#sandsynlighed for OP i 2025
predict(glm_julehandel, newdata = df_test, type = "response")

#prøver ændring i tillid i stedet for niveau
df_model$tillid_ændring <- c(NA, diff(df_model$tillid))

#konfusionsmatrix - ja tak.
p_træn <- predict(glm_julehandel, type = "response")
forudsagt_træn <- ifelse(p_træn > 0.5, "OP", "NED")
table(forudsagt_træn, df_træn$retning)

#træner igen, nu hvor tillid_ændring er kommet med
df_træn <- df_model[df_model$År >= 2001 & df_model$År <= 2024, ]
df_test <- df_model[df_model$År == 2025, ]

glm_ændring <- glm(as.factor(retning) ~ tillid_ændring, data = df_træn, family = "binomial")
summary(glm_ændring)

#sandsynlighed for OP i 2025
predict(glm_ændring, newdata = df_test, type = "response")
## fy for pyffer, glm_ændring er IKKE særlig god
## p = 0,28 så tillid_ændring siger ikke noget, og AIC er 27,4 mod 16,9 for glm_julehandel
## den rammer 2025 (70 % OP) fordi den ligger tæt på "gæt altid OP", ikke fordi den ved noget





