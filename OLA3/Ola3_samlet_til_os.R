#### OLA3 - til os ####
#Den rydded version med vores egne kommentarer. ## er rettelser og noter fra oprydningen
#Scriptet er det samme som aflevering-versionen, bare med flere noter

#### Opgave 1.1: Kombinationer af spørgsmålene i forbrugerundersøgelsen (FORV1) ####

#tid til at bruge dkstat
library(dkstat)

#DST giver forbrugertilliden månedligt, så den skal omregnes til kvartaler
forv_meta <- dst_meta(table = "FORV1", lang = "da")

#Her henter vi ALLE indikatorer og alle måneder. "*" betyder "alle"
FTIALLEINDI <- dst_get_data(
  table = "FORV1",
  query = list(INDIKATOR = "*", Tid = "*"),
  meta_data = forv_meta,
  lang = "da"
)

#F1 er den samlede indikator og fjernes
#F11 udeladt: serien ophører efter 2025M04, så den mangler i 2025K3 og senere
#Opgaven siger 12 spørgsmål, men uden F11 har vi 11 (2047 kombinationer i stedet for 4095)
#Vi beholder kun januar 2000 og frem, og kun til og med september 2026, så oktober ikke ødelægger kvartalet
FTIALLEINDI_11_spg <- FTIALLEINDI[!grepl("^F11|^F1 ", FTIALLEINDI$INDIKATOR), ]
FTIALLEINDI_11_spg <- FTIALLEINDI_11_spg[FTIALLEINDI_11_spg$TID >= as.Date("2000-01-01") &
                                          FTIALLEINDI_11_spg$TID < as.Date("2026-10-01"), ]

#### Fra måneder til kvartaler ####

#Sorterede måneder, bruges til kvartalsnavnene
maaneder <- sort(unique(FTIALLEINDI_11_spg$TID))
foerste_maaned <- maaneder[seq(1, length(maaneder), by = 3)]   #første måned i hvert kvartal

#Kvartalsnavne ud fra første måned i hvert kvartal, fx 2000K1
df_kvartal <- data.frame(kvartal = paste0(format(foerste_maaned, "%Y"), "K",
                                          (as.numeric(format(foerste_maaned, "%m")) - 1) %/% 3 + 1))

#Vi laver én kolonne pr. spørgsmål. Hver kolonne i matricen er et kvartal med tre måneder
for (i in unique(FTIALLEINDI_11_spg$INDIKATOR)) {
  df_spm <- FTIALLEINDI_11_spg[FTIALLEINDI_11_spg$INDIKATOR == i, ]
  df_spm <- df_spm[order(df_spm$TID), ]
  df_kvartal[[i]] <- colMeans(matrix(df_spm$value, nrow = 3))
}

## rettet: kortere kolonnenavne, så "F2 Familiens økonomiske ..." bliver til "F2"
names(df_kvartal) <- c("kvartal", sub(" .*", "", names(df_kvartal)[-1]))

## rettet: rækkefølgen styrer navnene på kombinationerne, fx F4.F9.F7.F10.F12.F13.
## Den skal stå som den er, ellers passer navnene i resten af scriptet ikke
col_names <- c("F2", "F3", "F4", "F5", "F9", "F6", "F7", "F8", "F10", "F12", "F13")
df_kvartal <- df_kvartal[, c("kvartal", col_names)]
dim(df_kvartal)   #107 rækker og 12 kolonner

#### Vend fortegn ####
#F6 (priser i dag) og F8 (arbejdsløshed om et år) vendes, så høj værdi altid betyder godt for forbruget
#F7 (priser om et år) vendes ikke: stigende forventede priser giver incitament til at fremrykke køb
#Linjerne må kun køres én gang. Kører man dem to gange, vender fortegnet tilbage
#Det skal ske FØR kombinationerne laves. I den gamle version blev de lavet EFTER, og opgave 4 brugte derfor forkerte fortegn
df_kvartal$F6 <- df_kvartal$F6 * -1
df_kvartal$F8 <- df_kvartal$F8 * -1

#### Kombinationerne ####
#Alle kombinationer af de 11 spørgsmål: 2^11 - 1 = 2047
#Vi laver en liste med indeks, fx c(3, 5, 7) for F4, F9 og F7
kombi_index <- unlist(lapply(1:11, function(k) combn(11, k, simplify = FALSE)), recursive = FALSE)
length(kombi_index)   #skal give 2047

X_kvartal <- as.matrix(df_kvartal[, col_names])   #107 kvartaler og 11 spørgsmål

## rettet: de tre gamle loops er samlet til én. Resultatet er det samme: 107 x 2047
samlet_indikator_df_combi <- as.data.frame(
  sapply(kombi_index, function(idx) rowMeans(X_kvartal[, idx, drop = FALSE]))
)
names(samlet_indikator_df_combi) <- sapply(kombi_index, function(idx) paste(col_names[idx], collapse = "."))
dim(samlet_indikator_df_combi)   #skal være 107 x 2047

#Kontrol: vinderen udregnet direkte fra df_kvartal skal give det samme (skal give TRUE)
all.equal(unname(rowMeans(df_kvartal[, c("F4", "F9", "F7", "F10", "F12", "F13")])),
          samlet_indikator_df_combi[["F4.F9.F7.F10.F12.F13"]])


#### Opgave 1.2: R² og sammenligning med DI's indikator ####

nkh1_meta <- dst_meta(table = "NKH1", lang = "da")

df_forbrug <- dst_get_data(table = "NKH1",
                           TRANSAKT = "P.31 Husholdningernes forbrugsudgifter",
                           PRISENHED = "2020-priser, kædede værdier",
                           SÆSON = "Sæsonkorrigeret",
                           Tid = "*", meta_data = nkh1_meta, lang = "da")
df_forbrug <- df_forbrug[order(df_forbrug$TID), ]

#derefter laver vi forbruget om til en tidsserie og regner væksten i forhold til samme kvartal året før
ts_forbrug <- ts(df_forbrug$value, start = c(1990, 1), frequency = 4)
vaekst <- (exp(diff(log(as.numeric(ts_forbrug)), lag = 4)) - 1) * 100
ts_realvaekst <- window(ts(vaekst, start = c(1991, 1), frequency = 4), start = c(2000, 1))

df_vaekst_kvartal <- data.frame(kvartal = as.numeric(time(ts_realvaekst)),
                                realvaekst = as.numeric(ts_realvaekst))

## Y har 106 kvartaler, X har 107. 2026K3 tages ud af X, fordi der ingen vækst er endnu
y <- df_vaekst_kvartal$realvaekst
samlet_indikator_forbrug <- cbind(df_vaekst_kvartal, samlet_indikator_df_combi[-107, ])
kombinationer <- names(samlet_indikator_df_combi)

#Loop lm: én regression pr. kombination
## rettet: i stedet for en loop med summary-lister laver vi modellerne direkte
modeller <- lapply(kombinationer, function(k) lm(y ~ samlet_indikator_forbrug[[k]]))

df_r2 <- data.frame(kombination = kombinationer,
                    r2     = sapply(modeller, function(m) summary(m)$r.squared),
                    adj_r2 = sapply(modeller, function(m) summary(m)$adj.r.squared),
                    RSS    = sapply(modeller, function(m) sum(resid(m)^2)))

#Top 10
head(df_r2[order(-df_r2$r2), ], 10)

#korrelation
y <- samlet_indikator_forbrug$realvaekst
x_vores <- samlet_indikator_forbrug[["F4.F9.F7.F10.F12.F13"]]
x_di    <- samlet_indikator_forbrug[["F2.F4.F9.F10"]]

bedste <- df_r2[which.max(df_r2$r2), ]
di     <- df_r2[df_r2$kombination == "F2.F4.F9.F10", ]
bedste$kombination   # skal være F4.F9.F7.F10.F12.F13

df_sammenligning <- data.frame(
  indikator   = c("Vores bedste", "DI-FTI"),
  kombination = c(bedste$kombination, di$kombination),
  r2          = c(bedste$r2, di$r2),
  RSS         = c(bedste$RSS, di$RSS),
  korrelation = c(cor(x_vores, y), cor(x_di, y))
)
df_sammenligning
df_sammenligning$korrelation^2   #Med én regressor er R² lig korrelationen i anden

#Figur 1: faktisk vækst, vores bedste indikator og DI-FTI
kvartal <- df_vaekst_kvartal$kvartal

png("tre_linjer.png", width = 6.3, height = 4, units = "in", res = 300)
par(mar = c(4.5, 4.5, 4.5, 1))
plot(kvartal, y, type = "l", lwd = 2.5, col = "grey60",
     xlab = "År", ylab = "Årlig realvækst, pct.",
     main = "Indikatorerne følger faldene i 2009 og 2022-23,\nmen rammer ikke toppene i 2005 og 2021",
     cex.main = 0.95)
abline(h = 0, lty = 3)
lines(kvartal, fitted(lm(y ~ x_vores)), col = "red", lwd = 2)
lines(kvartal, fitted(lm(y ~ x_di)), col = "blue", lwd = 2)
legend("topleft", legend = c("Faktisk vækst", "Vores bedste indikator", "DI-FTI"),
       col = c("grey60", "red", "blue"), lty = 1, lwd = c(2.5, 2, 2), cex = 0.8, bty = "n")
dev.off()

#Kontrol: samme periode som DI's artikel (2000K1-2016K2 er de første 66 kvartaler)
#DI's egne tal i artiklen: R² 0,54 og korrelation 0,73
p <- 1:66
c(R2_vores  = summary(lm(y[p] ~ x_vores[p]))$r.squared,
  R2_di     = summary(lm(y[p] ~ x_di[p]))$r.squared,
  cor_vores = cor(x_vores[p], y[p]),
  cor_di    = cor(x_di[p], y[p]))

#R² i to delperioder. 2016K3-2026K2 er de sidste 40 kvartaler
## rettet: de to gamle delperiode-blokke er samlet i én funktion
r2_periode <- function(r) round(c(vores = summary(lm(y[r] ~ x_vores[r]))$r.squared,
                                  di    = summary(lm(y[r] ~ x_di[r]))$r.squared), 2)
rbind(`2000K1-2016K2` = r2_periode(1:66),
      `2016K3-2026K2` = r2_periode(67:106))
## Tjek: tallene for de sidste 40 kvartaler i rapporten (1.2.4) skal matche den nederste række her.
## Tallene 0,57 og 0,44 er IKKE regnet endnu. Kør linjen før de bruges


#### Opgave 1.3 ####

#Vi tæller mikro- og makrospørgsmål i hver kombination
mikro <- c("F2", "F3", "F9", "F10", "F12", "F13")
makro <- c("F4", "F5", "F6", "F7", "F8")

spm_liste <- strsplit(df_r2$kombination, ".", fixed = TRUE)
df_r2$antal_mikro <- sapply(spm_liste, function(x) sum(x %in% mikro))
df_r2$antal_makro <- sapply(spm_liste, function(x) sum(x %in% makro))
df_r2$antal       <- sapply(spm_liste, length)

#Gitter: højeste R² for hver kombination af antal mikro (rækker) og makro (kolonner)
gitter <- matrix(NA, nrow = 7, ncol = 6,
                 dimnames = list(paste0("mikro", 0:6), paste0("makro", 0:5)))
for (a in 0:6) {
  for (b in 0:5) {
    d <- df_r2[df_r2$antal_mikro == a & df_r2$antal_makro == b, ]
    if (nrow(d) > 0) gitter[a + 1, b + 1] <- max(d$r2)
  }
}
round(gitter, 3)

#Vi inddeler hver kombination i en gruppe efter mikro/makro-sammensætning
df_r2$gruppe <- ifelse(df_r2$antal_makro == 0, "ren mikro",
                ifelse(df_r2$antal_mikro == 0, "ren makro",
                ifelse(df_r2$antal_mikro > df_r2$antal_makro, "mikro-tung",
                ifelse(df_r2$antal_mikro < df_r2$antal_makro, "makro-tung", "ligelig"))))

#Bedste indikator i hver gruppe, og hvor mange kombinationer der var at vælge imellem
bedste_pr_gruppe <- do.call(rbind, lapply(split(df_r2, df_r2$gruppe), function(d) {
  r <- d[which.max(d$r2), c("kombination", "antal_mikro", "antal_makro", "r2", "RSS")]
  r$antal_kombi <- nrow(d)
  r
}))
bedste_pr_gruppe

#### Skridt 1: RSS, RMSE, støjtest og t-/p-værdier ####

#RMSE = typisk fejl i procentpoint, regnet ud fra RSS
n <- length(y)
df_r2$RMSE <- sqrt(df_r2$RSS / n)

#Kontrol: R² = 1 - RSS/TSS (skal give TRUE)
all.equal(1 - df_r2$RSS / sum((y - mean(y))^2), df_r2$r2)

## rettet: r2 stod to gange i tabellen. Nu står den én gang
bedste_pr_gruppe$RMSE <- df_r2$RMSE[match(bedste_pr_gruppe$kombination, df_r2$kombination)]

#DI-FTI som reference. DI har 3 mikro (F2, F9, F10) og 1 makro (F4)
di_raekke <- df_r2[df_r2$kombination == "F2.F4.F9.F10", ]
df_rmse <- rbind(
  bedste_pr_gruppe[, c("kombination", "antal_mikro", "antal_makro", "r2", "RSS", "RMSE")],
  data.frame(kombination = "F2.F4.F9.F10", antal_mikro = 3, antal_makro = 1,
             r2 = di_raekke$r2, RSS = di_raekke$RSS, RMSE = di_raekke$RMSE,
             row.names = "DI-FTI")
)
df_rmse

#Til sammenligning: hvor meget selve væksten svinger
sd(y)

#--- B) Støjtest: hvad sker der med adj. R², når ét spørgsmål fjernes? ---
## rettet: X er nu de 106 kvartaler med de korte navne. Fortegnene er vendt én gang (se ovenfor)
X <- df_kvartal[-107, col_names]

#Korrelation mellem hvert spørgsmål og forbrugsvæksten
round(cor(X, y), 2)

#Hvor ofte er F10 negativ? F10 er forventet forbrug af større forbrugsgoder de næste 12 mdr.
range(X$F10)
mean(X$F10)
sum(X$F10 > 0)   #antal kvartaler med positivt nettotal

#Funktion: for hvert spørgsmål i en kombination fjerner vi det og ser, hvordan adj. R² ændrer sig
#Negativ ændring = spørgsmålet bidrager. Tæt på 0 eller positiv = ingen gevinst
stoejtest <- function(kombination) {
  spm <- strsplit(kombination, ".", fixed = TRUE)[[1]]
  if (length(spm) < 2) return(NA)   #med ét spørgsmål er der intet at fjerne
  adj_alle <- summary(lm(y ~ rowMeans(X[, spm, drop = FALSE])))$adj.r.squared
  sapply(spm, function(q) {
    rest <- setdiff(spm, q)
    summary(lm(y ~ rowMeans(X[, rest, drop = FALSE])))$adj.r.squared - adj_alle
  })
}

#Vi kører den for alle gruppevinderne og for DI
stoej_resultat <- lapply(c(bedste_pr_gruppe$kombination, "F2.F4.F9.F10"), stoejtest)
names(stoej_resultat) <- c(bedste_pr_gruppe$kombination, "F2.F4.F9.F10")
lapply(stoej_resultat, round, 4)

#--- C) t- og p-værdier pr. spørgsmål (multipel regression) ---
#Her får hvert spørgsmål sin egen vægt. Det er IKKE samme model som indikatoren (gennemsnit)
multi_resultat <- lapply(c(bedste_pr_gruppe$kombination, "F2.F4.F9.F10"), function(k) {
  spm <- strsplit(k, ".", fixed = TRUE)[[1]]
  if (length(spm) < 2) return(NA)
  d <- data.frame(y = y, X[, spm, drop = FALSE])
  round(summary(lm(y ~ ., data = d))$coefficients, 3)
})
names(multi_resultat) <- c(bedste_pr_gruppe$kombination, "F2.F4.F9.F10")
multi_resultat

#Hænger vinderens spørgsmål sammen? Høje korrelationer gør t-værdierne ustabile
round(cor(X[, c("F4", "F9", "F7", "F10", "F12", "F13")]), 2)

#1) bedste indikator for hvert antal spørgsmål (1 til 11)
bedste_pr_antal <- do.call(rbind, lapply(1:11, function(k) {
  d <- df_r2[df_r2$antal == k, ]
  d[which.max(d$r2), c("kombination", "antal", "r2", "RSS")]
}))
bedste_pr_antal

#2) de tre bedste uden F7
uden_F7 <- df_r2[!grepl("F7", df_r2$kombination), ]
head(uden_F7[order(-uden_F7$r2), c("kombination", "antal", "r2")], 3)

#3) enkeltspørgsmålene (til diskussionen af hvert spørgsmål)
df_r2[df_r2$antal == 1, c("kombination", "r2")]

#### Figur 2: seks paneler, gruppevindere og DI-FTI ####
#Rækkefølge som i tabel 2: vinderen først, DI-FTI til sidst
raekkefolge <- c("mikro-tung", "ligelig", "makro-tung", "ren mikro", "ren makro")
kombi  <- c(bedste_pr_gruppe[raekkefolge, "kombination"], "F2.F4.F9.F10")
titler <- c(raekkefolge, "DI-FTI")
farver <- c("blue", "red", "orange", "purple", "darkgreen", "grey30")

png("seks_paneler.png", width = 6.3, height = 8, units = "in", res = 300)
par(mfrow = c(3, 2), mar = c(3, 3.5, 3, 1), oma = c(2, 1.5, 4, 0))
par(cex = 1)   # skal stå EFTER mfrow, ellers sætter R teksten til 0,66

for (i in 1:6) {
  x    <- samlet_indikator_forbrug[[kombi[i]]]
  fit  <- lm(y ~ x)
  r2_i <- format(round(summary(fit)$r.squared, 2), nsmall = 2, decimal.mark = ",")

  plot(kvartal, y, type = "l", lwd = 2.5, col = "grey70",
       ylim = range(y), xlab = "", ylab = "",
       main = paste0(titler[i], " (R² = ", r2_i, ")"), cex.main = 1, cex.axis = 0.9)
  abline(h = 0, lty = 3)
  lines(kvartal, fitted(fit), col = farver[i], lwd = 2)
}

mtext("Ren mikro og DI-FTI undervurderer faldet i 2009 mest,\nog ingen af indikatorerne rammer toppen i 2021",
      side = 3, outer = TRUE, line = 1, font = 2, cex = 1)
mtext("År", side = 1, outer = TRUE, line = 0.5, cex = 0.9)
mtext("Årlig realvækst, pct.", side = 2, outer = TRUE, line = 0, cex = 0.9)
dev.off()

#Afvigelser: kvartalsnavne først (2021.25 bliver til "2021K2")
kv_navn <- paste0(floor(kvartal), "K", round((kvartal - floor(kvartal)) * 4) + 1)

#Gæt og residual (faktisk minus gæt) for vores bedste og DI
gaet_vores <- fitted(lm(y ~ x_vores))
gaet_di    <- fitted(lm(y ~ x_di))

afvigelse <- data.frame(
  kvartal    = kv_navn,
  faktisk    = round(y, 1),
  gaet_vores = round(gaet_vores, 1),
  res_vores  = round(y - gaet_vores, 1),
  gaet_di    = round(gaet_di, 1),
  res_di     = round(y - gaet_di, 1)
)

#De 10 kvartaler hvor vores bedste misser mest, og hvor DI misser mest
head(afvigelse[order(-abs(afvigelse$res_vores)), ], 10)
head(afvigelse[order(-abs(afvigelse$res_di)), ], 10)

#Krisene: residual = faktisk vækst minus gættet. Positiv = væksten var højere end gættet
res_f10   <- resid(lm(y ~ samlet_indikator_forbrug[["F10"]]))
res_vores <- resid(lm(y ~ x_vores))
res_di    <- resid(lm(y ~ x_di))

#Række 36-38 er 2008K4-2009K2 (finanskrisen), række 90-94 er 2022K2-2023K2 (energikrisen)
r_krise <- c(36:38, 90:94)
data.frame(kvartal   = kv_navn[r_krise],
           faktisk   = round(y[r_krise], 1),
           res_f10   = round(res_f10[r_krise], 1),
           res_vores = round(res_vores[r_krise], 1),
           res_di    = round(res_di[r_krise], 1))

#Typisk fejl (RMSE) i hver krise: finanskrisen 2008K3-2009K4 (række 35-40) og energikrisen
rmse_vindue <- function(r) c(f10   = sqrt(mean(res_f10[r]^2)),
                             vores = sqrt(mean(res_vores[r]^2)),
                             di    = sqrt(mean(res_di[r]^2)))
round(rbind(finanskrise = rmse_vindue(35:40),
            energikrise = rmse_vindue(90:94)), 2)

#Opsving efter nedgang: er fejlen anderledes i kvartaler efter en negativ vækst?
foer_neg <- c(FALSE, y[-length(y)] < 0)    #TRUE hvis forrige kvartal havde negativ vækst
d_y      <- c(NA, diff(y))                 #ændring i vækst fra forrige kvartal
opsving  <- foer_neg & !is.na(d_y) & d_y > 0
fortsat  <- foer_neg & !is.na(d_y) & d_y <= 0

data.frame(kvartal   = kv_navn[opsving],
           faktisk   = round(y[opsving], 1),
           res_vores = round(res_vores[opsving], 1),
           res_di    = round(res_di[opsving], 1),
           res_f10   = round(res_f10[opsving], 1))

#Gennemsnit og median fejl i de tre grupper
gns <- function(g) c(n = sum(g), vores = mean(res_vores[g]), di = mean(res_di[g]), f10 = mean(res_f10[g]))
med <- function(g) c(vores = median(res_vores[g]), di = median(res_di[g]), f10 = median(res_f10[g]))
round(rbind(opsving = gns(opsving), fortsat_nedgang = gns(fortsat), oevrige = gns(!foer_neg)), 2)
round(rbind(opsving = med(opsving), fortsat_nedgang = med(fortsat), oevrige = med(!foer_neg)), 2)

#Hvilke kvartaler er nedgangen fortsætter?
data.frame(kvartal   = kv_navn[fortsat],
           faktisk   = round(y[fortsat], 1),
           res_vores = round(res_vores[fortsat], 1),
           res_di    = round(res_di[fortsat], 1),
           res_f10   = round(res_f10[fortsat], 1))


#### Opgave 1.4: Forudsigelse af forbruget i 2026K3 og 2026K4 ####

#Er oktober udgivet? Står der 2026-09-01, er oktober ikke med
max(FTIALLEINDI$TID)

#Vi estimerer de to modeller på de 106 kvartaler, hvor væksten kendes
lm_vores <- lm(y ~ x_vores)
lm_di    <- lm(y ~ x_di)

#2026K3: indikatorværdien findes (række 107), men væksten er ikke offentliggjort
x_k3_vores <- samlet_indikator_df_combi[107, "F4.F9.F7.F10.F12.F13"]
x_k3_di    <- samlet_indikator_df_combi[107, "F2.F4.F9.F10"]

#Forudsigelse for 2026K3 med 95 % prediktionsinterval
predict(lm_vores, newdata = data.frame(x_vores = x_k3_vores), interval = "prediction")
predict(lm_di,    newdata = data.frame(x_di    = x_k3_di),    interval = "prediction")

#Månedsserier for de to indikatorer (gennemsnit af spørgsmålene i hver måned)
maanedlig <- function(koder) {
  d <- FTIALLEINDI[sub(" .*", "", FTIALLEINDI$INDIKATOR) %in% koder, ]
  tapply(d$value, d$TID, mean)
}
m_vores <- maanedlig(c("F4", "F9", "F7", "F10", "F12", "F13"))
m_di    <- maanedlig(c("F2", "F4", "F9", "F10"))

#Kontrol: de tre seneste måneder
tail(round(m_vores, 1), 3)

#Vi har ikke november og december, så vi tester, om første måned kan stå for hele kvartalet
lav_kvartaler <- function(m) {
  m <- m[names(m) >= "2000-01-01"]
  m <- m[1:(3 * floor(length(m) / 3))]
  matrix(m, nrow = 3)
}
mat_v <- lav_kvartaler(m_vores)
mat_d <- lav_kvartaler(m_di)

kv_v <- colMeans(mat_v)
kv_d <- colMeans(mat_d)

#Kontroller: 107 kvartaler, og sidste kvartal skal give K3-værdien (skal give TRUE)
dim(mat_v)
all.equal(unname(kv_v[length(kv_v)]), x_k3_vores)

#Hænger første måned sammen med kvartalets gennemsnit?
c(cor_vores = cor(mat_v[1, ], kv_v), cor_di = cor(mat_d[1, ], kv_d))

#Typisk afvigelse mellem første måned og kvartalsgennemsnittet
c(sd_afv_vores = sd(mat_v[1, ] - kv_v), sd_afv_di = sd(mat_d[1, ] - kv_d))

#K4: to bud på indikatoren, seneste måned og seneste tre måneder
n_m      <- length(m_vores)
x0_vores <- as.numeric(m_vores[n_m])
x0_di    <- as.numeric(m_di[n_m])

bud <- data.frame(
  bud_K4  = c("Seneste måned alene", "Seneste tre måneder (gennemsnit)"),
  x_vores = c(x0_vores, mean(as.numeric(m_vores[(n_m - 2):n_m]))),
  x_di    = c(x0_di,    mean(as.numeric(m_di[(n_m - 2):n_m])))
)
bud$prognose_vores <- round(predict(lm_vores, newdata = data.frame(x_vores = bud$x_vores)), 2)
bud$prognose_di    <- round(predict(lm_di,    newdata = data.frame(x_di    = bud$x_di)), 2)
bud

#K4 med prediktionsinterval, hvis seneste måned bruges som mål for kvartalet
predict(lm_vores, newdata = data.frame(x_vores = x0_vores), interval = "prediction")
predict(lm_di,    newdata = data.frame(x_di    = x0_di),    interval = "prediction")

#Følsomhed: hvad sker der, hvis tilliden skifter med ét typisk kvartalsskift?
sd_vores <- sd(diff(samlet_indikator_df_combi[["F4.F9.F7.F10.F12.F13"]]))
sd_di    <- sd(diff(samlet_indikator_df_combi[["F2.F4.F9.F10"]]))

scenarie <- function(model, navn, x0, s) {
  nydata <- setNames(data.frame(c(x0 - s, x0, x0 + s)), navn)
  round(predict(model, newdata = nydata), 2)
}

data.frame(
  scenarie_K4 = c("Lavere tillid (-1 sd)", "Uændret (seneste måned)", "Højere tillid (+1 sd)"),
  vores = scenarie(lm_vores, "x_vores", x0_vores, sd_vores),
  di    = scenarie(lm_di,    "x_di",    x0_di,    sd_di)
)


#### Opgave 1.5: Bedste mikroindikator ####

#1) Mikro er husholdningens egen økonomi og adfærd. F11 er ikke med
#2) Vi tjekker for hver kombination, om ALLE dens spørgsmål er mikro
er_mikro <- sapply(spm_liste, function(x) all(x %in% mikro))

#3) Kontrol: med 6 mikrospørgsmål skal der være 2^6 - 1 = 63 kombinationer
sum(er_mikro)

#4) Vi beholder kun de rene mikrokombinationer og finder den bedste (højest R²)
df_mikro     <- df_r2[er_mikro, ]
bedste_mikro <- df_mikro[which.max(df_mikro$r2), ]
bedste_mikro

#5) Bedste mikro for hvert antal spørgsmål (1 til 6)
## Her bruges almindelig R², ikke adj. R². Tjek at tallene i 1.5.3 i rapporten er R² og ikke adj. R²
bedste_mikro_pr_antal <- do.call(rbind, lapply(1:6, function(k) {
  d <- df_mikro[df_mikro$antal == k, ]
  d[which.max(d$r2), c("kombination", "antal", "r2", "RSS")]
}))
bedste_mikro_pr_antal

#6) Vi sætter tre indikatorer under hinanden: vores bedste, DI-FTI og bedste mikro
x_mikro <- samlet_indikator_forbrug[[bedste_mikro$kombination]]

#7) Korrelation med forbrugsvæksten for hver af de tre
df_15 <- data.frame(
  indikator   = c("Vores bedste", "DI-FTI", "Bedste mikro"),
  kombination = c(bedste$kombination, di$kombination, bedste_mikro$kombination),
  r2          = c(bedste$r2, di$r2, bedste_mikro$r2),
  RSS         = c(bedste$RSS, di$RSS, bedste_mikro$RSS),
  korrelation = c(cor(x_vores, y), cor(x_di, y), cor(x_mikro, y))
)
df_15

#Kontrol: R² skal være lig korrelationen i anden (skal ligge tæt på r2)
df_15$korrelation^2


#### Opgave 2: PCA, PCR, PLS og ikke RR ####
#vi starter med X og Y fra opgave 1

#tid til at bruge pls
library(pls)

#X er de 11 spørgsmål uden 2026K3. Y er realvæksten
df_pcr <- data.frame(realvaekst = y, df_kvartal[-107, col_names])
dim(df_pcr)   #106 rækker og 12 kolonner

#trin 1: problemet, almindelig regression (OLS) på alle 11 spørgsmål
summary(lm(realvaekst ~ ., data = df_pcr))
## R² = 0,586, men mange spørgsmål er ikke signifikante hver for sig, og nogle har mærkelige fortegn

#Korrelationer: mange tal tæt på 1, så spørgsmålene siger næsten det samme
kor <- cor(df_pcr[, -1])
kor[upper.tri(kor, diag = TRUE)] <- NA
par_idx <- which(!is.na(kor), arr.ind = TRUE)
kor_par <- data.frame(spm1 = rownames(kor)[par_idx[, 1]],
                      spm2 = colnames(kor)[par_idx[, 2]],
                      r    = round(kor[par_idx], 2))
head(kor_par[order(-abs(kor_par$r)), ], 10)   #de 10 stærkeste par

#trin 2: PCA, kun på X
pca_model <- prcomp(df_pcr[, -1], scale. = TRUE)   #scale. = TRUE, ellers vinder det største tal
summary(pca_model)
## to komponenter = 73 % af X

#trin 3: PCR, PCA + regression, med cross validation
set.seed(1)   #så CV giver samme tal hver gang
pcr_model <- pcr(realvaekst ~ ., data = df_pcr, scale = TRUE, validation = "CV")
summary(pcr_model)
validationplot(pcr_model, val.type = "RMSEP")
## skarp albue ved 2 komponenter

#trin 4: opskriften på komponenterne og vægtene
round(pca_model$rotation[, 1:2], 2)

vaegt_PC1 <- pcr_model$loadings[, 1]^2   #PC1 kvadreret
sum(vaegt_PC1)   #skal give 1
sort(vaegt_PC1, decreasing = TRUE)   #F2, F10 og F3 er størst

#### Opgave 2.2: standardiserede regressionsvægte ####
library(ggplot2)

df_vaegte <- data.frame(
  spoergsmaal = names(coef(pcr_model, ncomp = 2)[, 1, 1]),
  vaegt = as.numeric(coef(pcr_model, ncomp = 2)[, 1, 1])
)

#Sorter efter størrelse, så den største står øverst
df_vaegte <- df_vaegte[order(df_vaegte$vaegt), ]
df_vaegte$spoergsmaal <- factor(df_vaegte$spoergsmaal, levels = df_vaegte$spoergsmaal)

ggplot(df_vaegte, aes(x = vaegt, y = spoergsmaal)) +
  geom_col(fill = "#FF4FA3") +
  geom_text(aes(label = round(vaegt, 2),
                hjust = ifelse(vaegt > 0, 1.2, 1.2),
                colour = vaegt > 0)) +
  scale_colour_manual(values = c("TRUE" = "white", "FALSE" = "black"), guide = "none") +
  labs(title = "Hvilke spørgsmål vejer mest i regressionen?",
       x = "Vægt (standardiseret regressionskoefficient)", y = NULL,
       caption = "F7, F13 og F12 vejer tungest. Kilde: Danmarks Statistik, FORV1") +
  theme_classic()
ggsave("regressionsvaegte.png", width = 6.3, height = 4, dpi = 300)

#SVAR 2.2:
#De tre højeste vægte er F7 (priser om et år), F13 (økonomisk råderum) og F12 (opsparing).
#Vægtene er fra PCR-modellen og siger, hvad modellen faktisk bruger til at forudsige væksten.
#PC1-vægtene er fra PCA, som kun kigger på X. De siger, hvilke spørgsmål der følger den fælles stemning.
#F3 og F6 er næsten lige tunge i PC1 (0,1182 mod 0,1181).
#F7 vejer næsten ingenting i PC1 (0,0003), men er størst i regressionen.


#### Opgave 2.3: Forudsig forbruget ####

#Modellens bud på 3. kvartal 2025 (række 103). Modellen har set kvartalet
x_2025K3 <- df_kvartal[103, col_names]
df_kvartal[103, 1]   #skal vise 2025K3
predict(pcr_model, newdata = x_2025K3, ncomp = 2)
df_pcr$realvaekst[103]   #det faktiske tal

#2025K3 uden at modellen har set kvartalet: træn kun til og med 2025K2 (række 102)
pcr_til_2025K2 <- pcr(realvaekst ~ ., data = df_pcr[1:102, ], scale = TRUE)
predict(pcr_til_2025K2, newdata = x_2025K3, ncomp = 2)

#2026K3: første kvartal med tillid men uden forbrug, ligesom i opgave 1.4
x_2026K3 <- df_kvartal[107, col_names]
df_kvartal[107, 1]   #skal vise 2026K3
predict(pcr_model, newdata = x_2026K3, ncomp = 2)

#2.3 trin 2: er 0,07 mærkeligt?
mean(df_pcr$realvaekst)    #normal vækst i vores data
range(df_pcr$realvaekst)   #laveste og højeste vækst

bud <- as.numeric(predict(pcr_model, newdata = x_2025K3, ncomp = 2))
df_pcr$realvaekst[103] - bud   #fejlen, sammenlign med RMSEP omkring 2,1

round(rbind(x_2025K3, gennemsnit = colMeans(df_pcr[, -1])), 1)   #svarene i 2025K3 mod et normalt kvartal

#2.3 trin 3: modellens vægte og et regnestykke i hånden
koef      <- as.vector(coef(pcr_model, ncomp = 2))   #koefficienter på standardiserede spørgsmål
gns_x     <- colMeans(df_pcr[, -1])
sd_x      <- apply(df_pcr[, -1], 2, sd)
vaegte_DI <- koef / sd_x   #omregnet så det er vækst-procentpoint pr. nettotalpoint
names(vaegte_DI) <- col_names
round(sort(vaegte_DI, decreasing = TRUE), 3)

bud_manuelt <- mean(df_pcr$realvaekst) + sum(koef * (as.numeric(x_2025K3) - gns_x) / sd_x)
bud_manuelt   #skal give 0,0697

#2.3 trin 4: hvem trækker bruddet?
bidrag <- vaegte_DI * (as.numeric(x_2025K3) - gns_x)   #vægt gange afvigelse fra normalen
round(sort(bidrag), 2)   #negative trækker ned, positive trækker op
mean(df_pcr$realvaekst) + sum(bidrag)   #skal give 0,07


#### Opgave 2.4: DI's forudsigelse for 2016 ####

#2016K1 til 2016K4 er række 65-68
mean(df_pcr$realvaekst[65:68])


#### Opgave 3: Kan vi forudsige om julehandlen i 2026 er større end i 2025? ####
#Laver det så det er 1) julehandel pr. år 2) forbrugertillid 3) model 4) validering 5) gæt for 2026

#### 1) Julehandel pr. år ####

forbrug_meta <- dst_meta(table = "NKHC021", lang = "da")

forbrug_faste <- dst_get_data(
  table = "NKHC021",
  query = list(FORMAAAL = "*",
               PRISENHED = "2020-priser, kædede værdier",   #kædede værdier så inflation ikke gør alle år til OP
               SÆSON = "Ikke sæsonkorrigeret",
               Tid = "*"),
  meta_data = forbrug_meta,
  lang = "da"
)

#fra lang til bred, så hver forbrugsgruppe får sin egen kolonne
df_forbrug_bred <- data.frame(TID = forbrug_faste$TID[forbrug_faste$FORMAAAL == forbrug_faste$FORMAAAL[1]])
for (f in unique(forbrug_faste$FORMAAAL)) {
  df_forbrug_bred[[f]] <- forbrug_faste$value[forbrug_faste$FORMAAAL == f]
}

#fra 1999, så vi har et år før 2000 og kan regne vækst for 2000
df_forbrug_bred <- df_forbrug_bred[df_forbrug_bred$TID >= as.Date("1999-01-01"), ]

#kun K4 (oktober)
df_K4 <- df_forbrug_bred[format(df_forbrug_bred$TID, "%m") == "10", ]

#vores 8 julegrupper: A B C F J K M O
julegoder <- names(df_forbrug_bred)[substr(names(df_forbrug_bred), 1, 3) %in%
                                      c("CPA", "CPB", "CPC", "CPF", "CPJ", "CPK", "CPM", "CPO")]
julegoder

#læg julegrupperne sammen til ét tal pr. år
df_K4$julehandel <- rowSums(df_K4[, julegoder])

#vækst i julehandlen, K4 mod K4 året før (i procent)
df_K4$vækst <- c(NA, diff(log(df_K4$julehandel)) * 100)   #første år har ingen forgænger, så NA

#OP hvis julehandlen er større end året før, ellers NED
df_K4$retning <- ifelse(df_K4$vækst >= 0, "OP", "NED")
table(df_K4$retning)

#### 2) Forbrugertillid ####

forv_F1 <- dst_get_data(
  table = "FORV1",
  query = list(INDIKATOR = "Forbrugertillidsindikatoren", Tid = "*"),
  meta_data = forv_meta,
  lang = "da"
)

forv_F1 <- forv_F1[forv_F1$TID >= as.Date("2000-01-01"), ]   #før 2000 er der NA
forv_F1$År      <- as.numeric(format(forv_F1$TID, "%Y"))
forv_F1$kvartal <- (as.numeric(format(forv_F1$TID, "%m")) - 1) %/% 3 + 1

#gennemsnit af de tre måneder i hvert kvartal
df_forv_kvartal <- aggregate(value ~ År + kvartal, data = forv_F1, FUN = mean)

#kun K3 (juli til september), den kender vi før julen
df_forv_K3 <- df_forv_kvartal[df_forv_kvartal$kvartal == 3, ]

#tilliden for 2026 har vi, men der er ingen julehandel at sætte den sammen med endnu
sum(forv_F1$År == 2026 & forv_F1$kvartal == 3)   #skal give 3 måneder
tillid_ny <- df_forv_K3$value[df_forv_K3$År == 2026]

#### 3) Modellen ####

df_K4$År <- as.numeric(format(df_K4$TID, "%Y"))

df_model <- merge(df_K4[, c("År", "julehandel", "vækst", "retning")],
                  df_forv_K3[, c("År", "value")],
                  by = "År")
names(df_model)[names(df_model) == "value"] <- "tillid"
df_model <- df_model[df_model$År >= 2000, ]

#logistisk regression: retning (OP/NED) forklaret af forbrugertilliden i K3
glm_julehandel <- glm(as.factor(retning) ~ tillid, data = df_model, family = "binomial")
summary(glm_julehandel)
## Gamle output er fjernet, fordi de var fra en tidligere filtrering. Indsæt de nye tal fra konsollen

## tillid_ændring blev prøvet og droppet: p = 0,28 og AIC 27,4 mod 16,9 for tillid

#### 4) Validering (opgave 3.2) ####

#vi tager ét år ud ad gangen, træner på resten, og lader modellen gætte det år vi tog ud
df_model$p_uden_aaret <- NA   #her gemmer vi sandsynligheden for OP
for (i in 1:nrow(df_model)) {
  glm_uden_i <- glm(as.factor(retning) ~ tillid, data = df_model[-i, ], family = "binomial")
  df_model$p_uden_aaret[i] <- predict(glm_uden_i, newdata = df_model[i, ], type = "response")
}

#grænsen er 0,5: over det gætter vi OP
forudsagt <- ifelse(df_model$p_uden_aaret > 0.5, "OP", "NED")

#konfusionsmatrix, OP først så "rigtige OP" ligger øverst til venstre
konfusion <- table(forudsagt = factor(forudsagt, levels = c("OP", "NED")),
                   faktisk   = factor(df_model$retning, levels = c("OP", "NED")))
konfusion

TP <- konfusion["OP", "OP"]     #sagde OP, var OP
TN <- konfusion["NED", "NED"]   #sagde NED, var NED
FP <- konfusion["OP", "NED"]    #sagde OP, var NED
FN <- konfusion["NED", "OP"]    #sagde NED, var OP

(TP + TN) / sum(konfusion)   #accuracy: hvor mange år rammer modellen
TP / (TP + FN)               #sensitivitet: hvor mange af de rigtige OP-år fanger den
TN / (TN + FP)               #specificitet: hvor mange af de rigtige NED-år fanger den

max(table(df_model$retning)) / nrow(df_model)   #baseline: accuracy hvis vi altid gætter det hyppigste

#### 5) Gættet for 2026 ####

p_ny_aar <- predict(glm_julehandel, newdata = data.frame(tillid = tillid_ny), type = "response")
p_ny_aar   #sandsynlighed for at julehandlen i 2026 er større end i 2025
ifelse(p_ny_aar > 0.5, "OP", "NED")
## K4 2026 udkommer først i 2027, så gættet kan ikke tjekkes endnu


#### Opgave 4: Stabilitet ####
#tid til at lave vores egen mikroindikator
#er mikroindikatoren bedre end DST og DI?
#Vi bruger samme X og y som i opgave 1 og tester, hvor stabil hver kombination er

## rettet: X og fortegn er de samme som i opgave 1. I den gamle version blev F6 og F8 vendt efter kombinationerne var lavet.
## Det betyder at tallene i opgave 4 skal kørres igen

#Adj. R² for alle 2047 kombinationer i de kvartaler der er med i et vindue
adj_r2_vindue <- function(raekker) {
  sapply(kombinationer, function(k) {
    summary(lm(y[raekker] ~ samlet_indikator_forbrug[[k]][raekker]))$adj.r.squared
  })
}

#Fire slags vinduer. 24 + 24 + 24 + 12 = 84 kørsler
vinduer_1 <- lapply(1:24, function(i) (i + 1):106)        #fjern de første i kvartaler
vinduer_2 <- lapply(1:24, function(i) 1:(106 - i))        #fjern de sidste i kvartaler
vinduer_3 <- lapply(1:24, function(i) i:(i + 81))         #rullende vindue på 82 kvartaler
vinduer_4 <- lapply(1:12, function(k) (k + 1):(106 - k))  #fjern k fra begge ender

## rettet: de fire loops er samlet i én kodelinje pr. loop. Samme beregninger som før
adj_loop <- lapply(list(vinduer_1, vinduer_2, vinduer_3, vinduer_4),
                   function(v) t(sapply(v, adj_r2_vindue)))

#Gennemsnit pr. loop, og så gennemsnittet af de fire loop-gennemsnit
gns_pr_loop <- sapply(adj_loop, colMeans)
samlet_gns  <- rowMeans(gns_pr_loop)

## Vinderens navn, så [VINDER] i rapporten kan udfyldes
names(which.max(samlet_gns))
sort(samlet_gns, decreasing = TRUE)[1:10]
rank(-samlet_gns)[["F4.F9.F7.F10.F12.F13"]]   #placering af vores bedste
samlet_gns[["F2.F4.F9.F10"]]                  #DI-FTI


#### Opgave 5: Vind fra DMI ####
#### Opgave 5.1 og 5.2 ####

#tid til at bruge httr, jsonlite, dplyr og ggplot2
library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)

#Trin 0: test af API'et med opgavens egne data (vindstyrke i Århus Syd, 19. nov. 2022)
baseurl <- "https://opendataapi.dmi.dk/v2/metObs/collections/observation/items?"
res <- GET(paste0(baseurl, "stationId=06074&datetime=2022-11-19T00:00:00Z/2022-11-20T00:00:00Z&parameterId=wind_speed_past1h"))
res$status_code   # 200 = OK
resdf <- fromJSON(httr::content(res, type = "text"))
resdf$numberReturned   # ca. 25 (1 døgn x 24 timer + 1)

windforceparam <- "wind_speed_past1h"
windirparam    <- "wind_dir_past1h"
sid       <- "06074"   # Århus Syd
sidanholt <- "06079"   # Anholt Havn

## rettet: de manuelle trin 2 og 3 var kopier af hinanden. Funktionen i trin 4 gør det samme for begge parametre
#Henter én parameter for én station og laver tidspunkterne om til tid
getdatafromdmi <- function(param, station, frd, tod) {
  varurl <- paste0("parameterId=", param, "&stationId=", station, "&datetime=", frd, "/", tod, "&limit=10000")
  res <- GET(paste0(baseurl, varurl))
  resdf <- fromJSON(httr::content(res, type = "text"))
  obsdf <- resdf$features$properties
  windspeeddf <- obsdf[, c("observed", "value")]
  windspeeddf <- arrange(windspeeddf, observed)
  windspeeddf$observed <- as.POSIXct(windspeeddf$observed, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  return(windspeeddf)
}

#Styrke og retning for én station i en periode, samlet i én tabel
hent_vind <- function(station, fd, td) {
  styrke  <- getdatafromdmi(windforceparam, station, fd, td)
  retning <- getdatafromdmi(windirparam, station, fd, td)
  nrow(styrke) == nrow(retning)   # skal være TRUE, ellers passer cbind ikke
  data.frame(observed = styrke$observed, force = styrke$value, dir = retning$value)
}

#Trin 6: graf over vindstyrke, stormen i november 2022
totalwind       <- hent_vind(sid, "2022-11-14T00:00:00Z", "2022-11-21T00:00:00Z")
totalwindanholt <- hent_vind(sidanholt, "2022-11-14T00:00:00Z", "2022-11-21T00:00:00Z")

ggplot(totalwind, aes(x = observed, y = force)) +
  geom_line(color = "red") +
  geom_line(data = totalwindanholt, aes(x = observed, y = force), color = "blue") +
  theme_classic()

#Trin 7: hvilken retning kom vinden fra? dir = retningen vinden kommer FRA, 360 = nord, 0 = vindstille
kompas        <- c(0, 45, 135, 225, 315, 360)
retning_navne <- c("N", "Ø", "S", "V", "N")
table(cut(totalwind$dir, breaks = kompas, labels = retning_navne, include.lowest = TRUE))
table(cut(totalwindanholt$dir, breaks = kompas, labels = retning_navne, include.lowest = TRUE))

#Trin 8: stormen i oktober 2023 (Babet)
babet_aarhus <- hent_vind(sid, "2023-10-19T00:00:00Z", "2023-10-22T00:00:00Z")
babet_anholt <- hent_vind(sidanholt, "2023-10-19T00:00:00Z", "2023-10-22T00:00:00Z")

table(cut(babet_aarhus$dir, breaks = kompas, labels = retning_navne, include.lowest = TRUE))
table(cut(babet_anholt$dir, breaks = kompas, labels = retning_navne, include.lowest = TRUE))

#Trin 9: den forbedrede graf med styrke og retning. Pilene peger dér hvor vinden blæser hen
babet_aarhus$station <- "Århus Syd"
babet_anholt$station <- "Anholt Havn"
begge <- rbind(babet_aarhus, babet_anholt)
pile  <- begge[seq(1, nrow(begge), 3), ]   #en pil for hver 3. time

ggplot(begge, aes(x = observed, y = force, color = station)) +
  geom_line() +
  geom_text(data = pile, aes(angle = -90 - dir), label = "→", size = 6.5, show.legend = FALSE) +
  scale_color_manual(values = c("Anholt Havn" = "blue", "Århus Syd" = "red")) +
  labs(title = "Stormen Babet, oktober 2023",
       x = "Tid (UTC)", y = "Vindhastighed (m/s)", color = "Station") +
  theme_classic()
