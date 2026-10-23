#Opgave 2: PCA, PCR, PLS og ikke RR
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
df_11spg <- df_11spg[df_11spg$TID < as.Date("2026-10-01"), ]
sum(is.na(df_11spg$value))   #har vi nogle na'er - nope
unique(df_11spg$INDIKATOR)
f11 <- forv_alle[grepl("^F11", forv_alle$INDIKATOR) & !is.na(forv_alle$value), ]
max(f11$TID)         #sidste måling af F11, skal vise 2025-04-01
max(forv_alle$TID)   #sidste måned i tabellen i øvrigt
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

#F6 og F8 vendes, så højere tal er bedre stemning. Samme som i opgave 1
df_kvartal$`F6 Priser i dag, sammenlignet med for et år siden` <- df_kvartal$`F6 Priser i dag, sammenlignet med for et år siden` * -1
df_kvartal$`F8 Arbejdsløsheden om et år, sammenlignet med i dag` <- df_kvartal$`F8 Arbejdsløsheden om et år, sammenlignet med i dag` * -1

#X har et kvartal mere end Y (2026K3). Det kvartal gemmes til prognosen
df_x_alle <- df_kvartal[, 2:ncol(df_kvartal)]
names(df_x_alle) <- trimws(substr(names(df_x_alle), 1, 3))   #F2, F3 osv.
df_pca_11x <- df_x_alle[1:nrow(df_vaekst_kvartal), ]
x_2026K3 <- df_x_alle[107, ]
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

kor <- cor(df_pca_11x)
kor[upper.tri(kor, diag = TRUE)] <- NA
par_idx <- which(!is.na(kor), arr.ind = TRUE)
kor_par <- data.frame(spm1 = rownames(kor)[par_idx[, 1]],
                      spm2 = colnames(kor)[par_idx[, 2]],
                      r    = round(kor[par_idx], 2))
head(kor_par[order(-abs(kor_par$r)), ], 10)   #de 10 stærkeste par = bilaget
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

## Standardiserede regressionsvægte fra PCR med 2 komponenter
df_vaegte <- data.frame(
  spoergsmaal = names(coef(pcr_model, ncomp = 2)[, 1, 1]),
  vaegt = as.numeric(coef(pcr_model, ncomp = 2)[, 1, 1])
)

## Sorter efter størrelse, så den største står øverst
df_vaegte <- df_vaegte[order(df_vaegte$vaegt), ]
df_vaegte$spoergsmaal <- factor(df_vaegte$spoergsmaal, levels = df_vaegte$spoergsmaal)

library(ggplot2)

ggplot(df_vaegte, aes(x = vaegt, y = spoergsmaal)) +
  geom_col(fill = "#FF4FA3") +
  geom_text(aes(label = round(vaegt, 2),
                hjust = ifelse(vaegt > 0, 1.2, 1.2),
                colour = vaegt > 0)) +
  scale_colour_manual(values = c("TRUE" = "white", "FALSE" = "black"), guide = "none") +  labs(title = "Hvilke spørgsmål vejer mest i regressionen?",
       x = "Vægt (standardiseret regressionskoefficient)", y = NULL,
       caption = "Kilde: Danmarks Statistik, FORV1") +
  theme_minimal()
ggsave("regressionsvaegte.png", width = 6.3, height = 4, dpi = 300)
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
df_kvartal[103, 1]   #tjek: skal vise 2025K3
x_2025K3 <- df_pca_11x[103, ]   #de 11 svar for det kvartal
predict(pcr_model, newdata = x_2025K3, ncomp = 2)   #modellens bud (årlig realvækst, %)
df_pcr$realvaekst[103]   #hvad det faktisk blev
#opg 2.3 trin 1: modellens bud på 3. kvartal 2025
#2025K3 uden at modellen har set kvartalet: træn kun til og med 2025K2 (række 102)
pcr_til_2025K2 <- pcr(realvaekst ~ ., data = df_pcr[1:102, ], scale = TRUE)
predict(pcr_til_2025K2, newdata = x_2025K3, ncomp = 2)

#2026K3: første kvartal med tillid men uden forbrug, ligesom i opgave 1.4
df_kvartal[107, 1]   #tjek: skal vise 2026K3
predict(pcr_model, newdata = x_2026K3, ncomp = 2)

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

