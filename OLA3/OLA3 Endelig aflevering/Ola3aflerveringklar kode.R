#### OLA3 - aflevering ####
#Scriptet henter data, laver alle beregningerne og laver figurerne til rapporten

#### Opgave 1.1: Kombinationer af spørgsmålene ####

#Vi bruger dkstat til at hente data fra Danmarks Statistik
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

#F1 er den samlede indikator, og F11 stopper i april 2025, så de tager vi ud
#Vi tager kun måneder fra 2000 og frem, og kun til og med september 2026 (fulde kvartaler)
FTIALLEINDI_11_spg <- FTIALLEINDI[!grepl("^F11|^F1 ", FTIALLEINDI$INDIKATOR), ]
FTIALLEINDI_11_spg <- FTIALLEINDI_11_spg[FTIALLEINDI_11_spg$TID >= as.Date("2000-01-01") &
                                          FTIALLEINDI_11_spg$TID < as.Date("2026-10-01"), ]

#Vi laver måneder om til kvartaler
#Første måned i hvert kvartal bruges til navnet, fx 2000K1
maaneder <- sort(unique(FTIALLEINDI_11_spg$TID))
foerste_maaned <- maaneder[seq(1, length(maaneder), by = 3)]

df_kvartal <- data.frame(kvartal = paste0(format(foerste_maaned, "%Y"), "K",
                                          (as.numeric(format(foerste_maaned, "%m")) - 1) %/% 3 + 1))

#Hvert spørgsmål får sin egen kolonne. Gennemsnittet af tre måneder giver kvartalet
for (i in unique(FTIALLEINDI_11_spg$INDIKATOR)) {
  df_spm <- FTIALLEINDI_11_spg[FTIALLEINDI_11_spg$INDIKATOR == i, ]
  df_spm <- df_spm[order(df_spm$TID), ]
  df_kvartal[[i]] <- colMeans(matrix(df_spm$value, nrow = 3))
}

#Kortere navne: "F2 Familiens økonomiske ..." bliver til "F2"
names(df_kvartal) <- c("kvartal", sub(" .*", "", names(df_kvartal)[-1]))

#Rækkefølgen her bestemmer navnene på kombinationerne (fx F4.F9.F7.F10.F12.F13)
col_names <- c("F2", "F3", "F4", "F5", "F9", "F6", "F7", "F8", "F10", "F12", "F13")
df_kvartal <- df_kvartal[, c("kvartal", col_names)]
dim(df_kvartal)   #107 rækker og 12 kolonner

#F6 og F8 vendes, så en høj værdi altid er godt for forbruget
#F7 vendes ikke. Kør kun disse to linjer én gang, ellers vender de tilbage
df_kvartal$F6 <- df_kvartal$F6 * -1
df_kvartal$F8 <- df_kvartal$F8 * -1

#Alle kombinationer af de 11 spørgsmål: 2^11 - 1 = 2047
kombi_index <- unlist(lapply(1:11, function(k) combn(11, k, simplify = FALSE)), recursive = FALSE)
length(kombi_index)   #skal give 2047

X_kvartal <- as.matrix(df_kvartal[, col_names])   #107 kvartaler og 11 spørgsmål

#Hver kombination er gennemsnittet af de spørgsmål, der er med i den
samlet_indikator_df_combi <- as.data.frame(
  sapply(kombi_index, function(idx) rowMeans(X_kvartal[, idx, drop = FALSE]))
)

#Navnet viser hvilke spørgsmål der er med, fx F4.F9.F7.F10.F12.F13
names(samlet_indikator_df_combi) <- sapply(kombi_index, function(idx) paste(col_names[idx], collapse = "."))
dim(samlet_indikator_df_combi)   #skal være 107 x 2047

#Kontrol: vinderen udregnet direkte skal give det samme
all.equal(unname(rowMeans(df_kvartal[, c("F4", "F9", "F7", "F10", "F12", "F13")])),
          samlet_indikator_df_combi[["F4.F9.F7.F10.F12.F13"]])


#### Opgave 1.2: R² og sammenligning med DI ####

#Y er årlig realvækst i husholdningernes forbrugsudgift
nkh1_meta <- dst_meta(table = "NKH1", lang = "da")
df_forbrug <- dst_get_data(table = "NKH1",
                           TRANSAKT = "P.31 Husholdningernes forbrugsudgifter",
                           PRISENHED = "2020-priser, kædede værdier",
                           SÆSON = "Sæsonkorrigeret",
                           Tid = "*", meta_data = nkh1_meta, lang = "da")
df_forbrug <- df_forbrug[order(df_forbrug$TID), ]

#Forbruget laves om til en tidsserie. Væksten er i forhold til samme kvartal året før
ts_forbrug <- ts(df_forbrug$value, start = c(1990, 1), frequency = 4)
vaekst <- (exp(diff(log(as.numeric(ts_forbrug)), lag = 4)) - 1) * 100
ts_realvaekst <- window(ts(vaekst, start = c(1991, 1), frequency = 4), start = c(2000, 1))

df_vaekst_kvartal <- data.frame(kvartal = as.numeric(time(ts_realvaekst)),
                                realvaekst = as.numeric(ts_realvaekst))

#Y har 106 kvartaler. X har 107, så 2026K3 tages ud, fordi der ingen vækst er endnu
y <- df_vaekst_kvartal$realvaekst
samlet_indikator_forbrug <- cbind(df_vaekst_kvartal, samlet_indikator_df_combi[-107, ])
kombinationer <- names(samlet_indikator_df_combi)

#En regression pr. kombination: væksten forklaret af kombinationens gennemsnit
modeller <- lapply(kombinationer, function(k) lm(y ~ samlet_indikator_forbrug[[k]]))

df_r2 <- data.frame(kombination = kombinationer,
                    r2     = sapply(modeller, function(m) summary(m)$r.squared),
                    adj_r2 = sapply(modeller, function(m) summary(m)$adj.r.squared),
                    RSS    = sapply(modeller, function(m) sum(resid(m)^2)))

#Top 10 efter R²
head(df_r2[order(-df_r2$r2), ], 10)

#Vores bedste indikator og DI-FTI
x_vores <- samlet_indikator_forbrug[["F4.F9.F7.F10.F12.F13"]]
x_di    <- samlet_indikator_forbrug[["F2.F4.F9.F10"]]

bedste <- df_r2[which.max(df_r2$r2), ]
di     <- df_r2[df_r2$kombination == "F2.F4.F9.F10", ]
bedste$kombination   #skal være F4.F9.F7.F10.F12.F13

df_sammenligning <- data.frame(
  indikator   = c("Vores bedste", "DI-FTI"),
  kombination = c(bedste$kombination, di$kombination),
  r2          = c(bedste$r2, di$r2),
  RSS         = c(bedste$RSS, di$RSS),
  korrelation = c(cor(x_vores, y), cor(x_di, y))
)
df_sammenligning
df_sammenligning$korrelation^2   #med én forklarende variabel er R² lig korrelationen i anden

#Figur 1: faktisk vækst og de to indikatorers gæt
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

#DI's periode: 2000K1-2016K2 er de første 66 kvartaler
p <- 1:66
c(R2_vores  = summary(lm(y[p] ~ x_vores[p]))$r.squared,
  R2_di     = summary(lm(y[p] ~ x_di[p]))$r.squared,
  cor_vores = cor(x_vores[p], y[p]),
  cor_di    = cor(x_di[p], y[p]))

#R² i to delperioder. Den anden periode er de sidste 40 kvartaler (2016K3-2026K2)
r2_periode <- function(r) round(c(vores = summary(lm(y[r] ~ x_vores[r]))$r.squared,
                                  di    = summary(lm(y[r] ~ x_di[r]))$r.squared), 2)
rbind(`2000K1-2016K2` = r2_periode(1:66),
      `2016K3-2026K2` = r2_periode(67:106))


#### Opgave 1.3: Mikro og makro ####

mikro <- c("F2", "F3", "F9", "F10", "F12", "F13")
makro <- c("F4", "F5", "F6", "F7", "F8")

#Vi tæller hvor mange mikro- og makrospørgsmål hver kombination har
spm_liste <- strsplit(df_r2$kombination, ".", fixed = TRUE)
df_r2$antal_mikro <- sapply(spm_liste, function(x) sum(x %in% mikro))
df_r2$antal_makro <- sapply(spm_liste, function(x) sum(x %in% makro))
df_r2$antal       <- sapply(spm_liste, length)

#Gitter: den højeste R² for hver kombination af antal mikro (rækker) og makro (kolonner)
gitter <- matrix(NA, nrow = 7, ncol = 6,
                 dimnames = list(paste0("mikro", 0:6), paste0("makro", 0:5)))
for (a in 0:6) {
  for (b in 0:5) {
    d <- df_r2[df_r2$antal_mikro == a & df_r2$antal_makro == b, ]
    if (nrow(d) > 0) gitter[a + 1, b + 1] <- max(d$r2)
  }
}
round(gitter, 3)

#Hver kombination får en gruppe ud fra sammensætningen
df_r2$gruppe <- ifelse(df_r2$antal_makro == 0, "ren mikro",
                ifelse(df_r2$antal_mikro == 0, "ren makro",
                ifelse(df_r2$antal_mikro > df_r2$antal_makro, "mikro-tung",
                ifelse(df_r2$antal_mikro < df_r2$antal_makro, "makro-tung", "ligelig"))))

#Den bedste kombination i hver gruppe, og hvor mange der var at vælge imellem
bedste_pr_gruppe <- do.call(rbind, lapply(split(df_r2, df_r2$gruppe), function(d) {
  r <- d[which.max(d$r2), c("kombination", "antal_mikro", "antal_makro", "r2", "RSS")]
  r$antal_kombi <- nrow(d)
  r
}))
bedste_pr_gruppe

#RMSE: typisk fejl i procentpoint, regnet ud fra RSS
n <- length(y)
df_r2$RMSE <- sqrt(df_r2$RSS / n)
all.equal(1 - df_r2$RSS / sum((y - mean(y))^2), df_r2$r2)   #skal give TRUE

bedste_pr_gruppe$RMSE <- df_r2$RMSE[match(bedste_pr_gruppe$kombination, df_r2$kombination)]

#DI-FTI med som reference: 3 mikro (F2, F9, F10) og 1 makro (F4)
di_raekke <- df_r2[df_r2$kombination == "F2.F4.F9.F10", ]
df_rmse <- rbind(
  bedste_pr_gruppe[, c("kombination", "antal_mikro", "antal_makro", "r2", "RSS", "RMSE")],
  data.frame(kombination = "F2.F4.F9.F10", antal_mikro = 3, antal_makro = 1,
             r2 = di_raekke$r2, RSS = di_raekke$RSS, RMSE = di_raekke$RMSE,
             row.names = "DI-FTI")
)
df_rmse
sd(y)   #hvor meget væksten svinger i sig selv

#Støjtest: hvad sker der med adj. R², når ét spørgsmål fjernes?
X <- df_kvartal[-107, col_names]   #de 106 kvartaler

round(cor(X, y), 2)   #korrelation mellem hvert spørgsmål og væksten

#For hvert spørgsmål i en kombination fjerner vi det og ser på adj. R²
#Negativ ændring = spørgsmålet bidrager. Tæt på nul eller positiv = ingen gevinst
stoejtest <- function(kombination) {
  spm <- strsplit(kombination, ".", fixed = TRUE)[[1]]
  if (length(spm) < 2) return(NA)
  adj_alle <- summary(lm(y ~ rowMeans(X[, spm, drop = FALSE])))$adj.r.squared
  sapply(spm, function(q) {
    rest <- setdiff(spm, q)
    summary(lm(y ~ rowMeans(X[, rest, drop = FALSE])))$adj.r.squared - adj_alle
  })
}

stoej_resultat <- lapply(c(bedste_pr_gruppe$kombination, "F2.F4.F9.F10"), stoejtest)
names(stoej_resultat) <- c(bedste_pr_gruppe$kombination, "F2.F4.F9.F10")
lapply(stoej_resultat, round, 4)

#Multipel regression: hvert spørgsmål får sin egen vægt. Det er ikke det samme som gennemsnittet
multi_resultat <- lapply(c(bedste_pr_gruppe$kombination, "F2.F4.F9.F10"), function(k) {
  spm <- strsplit(k, ".", fixed = TRUE)[[1]]
  if (length(spm) < 2) return(NA)
  d <- data.frame(y = y, X[, spm, drop = FALSE])
  round(summary(lm(y ~ ., data = d))$coefficients, 3)
})
names(multi_resultat) <- c(bedste_pr_gruppe$kombination, "F2.F4.F9.F10")
multi_resultat

#Korrelationer mellem vinderens spørgsmål
round(cor(X[, c("F4", "F9", "F7", "F10", "F12", "F13")]), 2)

#Bedste kombination for hvert antal spørgsmål (1 til 11)
bedste_pr_antal <- do.call(rbind, lapply(1:11, function(k) {
  d <- df_r2[df_r2$antal == k, ]
  d[which.max(d$r2), c("kombination", "antal", "r2", "RSS")]
}))
bedste_pr_antal

#De tre bedste uden F7
uden_F7 <- df_r2[!grepl("F7", df_r2$kombination), ]
head(uden_F7[order(-uden_F7$r2), c("kombination", "antal", "r2")], 3)

#Enkeltspørgsmålene
df_r2[df_r2$antal == 1, c("kombination", "r2")]

#Figur 2: seks paneler med gruppevinderne og DI-FTI
raekkefolge <- c("mikro-tung", "ligelig", "makro-tung", "ren mikro", "ren makro")
kombi  <- c(bedste_pr_gruppe[raekkefolge, "kombination"], "F2.F4.F9.F10")
titler <- c(raekkefolge, "DI-FTI")
farver <- c("blue", "red", "orange", "purple", "darkgreen", "grey30")

png("seks_paneler.png", width = 6.3, height = 8, units = "in", res = 300)
par(mfrow = c(3, 2), mar = c(3, 3.5, 3, 1), oma = c(2, 1.5, 4, 0))
par(cex = 1)   #skal stå efter mfrow, ellers bliver teksten for lille

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

#Hvor rammer indikatorerne forkert? Kvartalsnavne først, fx 2021.25 bliver til 2021K2
kv_navn <- paste0(floor(kvartal), "K", round((kvartal - floor(kvartal)) * 4) + 1)

res_vores <- resid(lm(y ~ x_vores))
res_di    <- resid(lm(y ~ x_di))
res_f10   <- resid(lm(y ~ samlet_indikator_forbrug[["F10"]]))

df_afvigelse <- data.frame(kvartal = kv_navn,
                           faktisk = round(y, 1),
                           gaet_vores = round(fitted(lm(y ~ x_vores)), 1),
                           res_vores = round(res_vores, 1),
                           gaet_di = round(fitted(lm(y ~ x_di)), 1),
                           res_di = round(res_di, 1))

#De 10 kvartaler hvor vores bedste og DI misser mest
head(df_afvigelse[order(-abs(df_afvigelse$res_vores)), ], 10)
head(df_afvigelse[order(-abs(df_afvigelse$res_di)), ], 10)

#Krisene: finanskrisen (række 36-38 er 2008K4-2009K2) og energikrisen (række 90-94 er 2022K2-2023K2)
r_krise <- c(36:38, 90:94)
data.frame(kvartal   = kv_navn[r_krise],
           faktisk   = round(y[r_krise], 1),
           res_f10   = round(res_f10[r_krise], 1),
           res_vores = round(res_vores[r_krise], 1),
           res_di    = round(res_di[r_krise], 1))

#Typisk fejl i krisevinduerne
rmse_vindue <- function(r) c(f10   = sqrt(mean(res_f10[r]^2)),
                             vores = sqrt(mean(res_vores[r]^2)),
                             di    = sqrt(mean(res_di[r]^2)))
round(rbind(finanskrise = rmse_vindue(35:40),
            energikrise = rmse_vindue(90:94)), 2)

#Opsving efter en nedgang
foer_neg <- c(FALSE, y[-length(y)] < 0)   #TRUE hvis forrige kvartal var negativt
d_y      <- c(NA, diff(y))                #ændring i væksten fra forrige kvartal
opsving  <- foer_neg & !is.na(d_y) & d_y > 0
fortsat  <- foer_neg & !is.na(d_y) & d_y <= 0

data.frame(kvartal   = kv_navn[opsving],
           faktisk   = round(y[opsving], 1),
           res_vores = round(res_vores[opsving], 1),
           res_di    = round(res_di[opsving], 1),
           res_f10   = round(res_f10[opsving], 1))

#Gennemsnit og median i de tre grupper
gns <- function(g) c(n = sum(g), vores = mean(res_vores[g]), di = mean(res_di[g]), f10 = mean(res_f10[g]))
med <- function(g) c(vores = median(res_vores[g]), di = median(res_di[g]), f10 = median(res_f10[g]))
round(rbind(opsving = gns(opsving), fortsat_nedgang = gns(fortsat), oevrige = gns(!foer_neg)), 2)
round(rbind(opsving = med(opsving), fortsat_nedgang = med(fortsat), oevrige = med(!foer_neg)), 2)

#De kvartaler hvor nedgangen fortsætter
data.frame(kvartal   = kv_navn[fortsat],
           faktisk   = round(y[fortsat], 1),
           res_vores = round(res_vores[fortsat], 1),
           res_di    = round(res_di[fortsat], 1),
           res_f10   = round(res_f10[fortsat], 1))


#### Opgave 1.4: Forudsigelse af 2026K3 og 2026K4 ####

#Er oktober udgivet? Står der 2026-09-01, er den ikke
max(FTIALLEINDI$TID)

#Modellerne trænes på de 106 kvartaler hvor væksten kendes
lm_vores <- lm(y ~ x_vores)
lm_di    <- lm(y ~ x_di)

#2026K3: indikatorværdien findes (række 107), men væksten er ikke udgivet endnu
x_k3_vores <- samlet_indikator_df_combi[107, "F4.F9.F7.F10.F12.F13"]
x_k3_di    <- samlet_indikator_df_combi[107, "F2.F4.F9.F10"]

predict(lm_vores, newdata = data.frame(x_vores = x_k3_vores), interval = "prediction")
predict(lm_di,    newdata = data.frame(x_di    = x_k3_di),    interval = "prediction")

#Månedlige værdier: gennemsnit af spørgsmålene i hver måned
maanedlig <- function(koder) {
  d <- FTIALLEINDI[sub(" .*", "", FTIALLEINDI$INDIKATOR) %in% koder, ]
  tapply(d$value, d$TID, mean)
}
m_vores <- maanedlig(c("F4", "F9", "F7", "F10", "F12", "F13"))
m_di    <- maanedlig(c("F2", "F4", "F9", "F10"))
tail(round(m_vores, 1), 3)

#Kan første måned stå for hele kvartalet? Vi tester på de tidligere kvartaler
lav_kvartaler <- function(m) {
  m <- m[names(m) >= "2000-01-01"]
  m <- m[1:(3 * floor(length(m) / 3))]
  matrix(m, nrow = 3)
}
mat_v <- lav_kvartaler(m_vores)
mat_d <- lav_kvartaler(m_di)

kv_v <- colMeans(mat_v)
kv_d <- colMeans(mat_d)

dim(mat_v)   #107 kvartaler
all.equal(unname(kv_v[length(kv_v)]), x_k3_vores)   #skal give TRUE

c(cor_vores = cor(mat_v[1, ], kv_v), cor_di = cor(mat_d[1, ], kv_d))
c(sd_afv_vores = sd(mat_v[1, ] - kv_v), sd_afv_di = sd(mat_d[1, ] - kv_d))

#K4 (oktober til december) mangler. Vi laver to bud ud fra de seneste måneder
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

predict(lm_vores, newdata = data.frame(x_vores = x0_vores), interval = "prediction")
predict(lm_di,    newdata = data.frame(x_di    = x0_di),    interval = "prediction")

#Følsomhed: hvad sker der hvis tilliden flytter sig ét typisk kvartalsskift?
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

#Kun kombinationer hvor alle spørgsmål er mikro. Med 6 mikrospørgsmål er der 2^6 - 1 = 63
er_mikro <- sapply(spm_liste, function(x) all(x %in% mikro))
sum(er_mikro)   #skal give 63

df_mikro     <- df_r2[er_mikro, ]
bedste_mikro <- df_mikro[which.max(df_mikro$r2), ]
bedste_mikro

#Bedste mikroindikator for hvert antal spørgsmål
bedste_mikro_pr_antal <- do.call(rbind, lapply(1:6, function(k) {
  d <- df_mikro[df_mikro$antal == k, ]
  d[which.max(d$r2), c("kombination", "antal", "r2", "RSS")]
}))
bedste_mikro_pr_antal

#Vores bedste, DI-FTI og bedste mikro i samme tabel
x_mikro <- samlet_indikator_forbrug[[bedste_mikro$kombination]]

df_15 <- data.frame(
  indikator   = c("Vores bedste", "DI-FTI", "Bedste mikro"),
  kombination = c(bedste$kombination, di$kombination, bedste_mikro$kombination),
  r2          = c(bedste$r2, di$r2, bedste_mikro$r2),
  RSS         = c(bedste$RSS, di$RSS, bedste_mikro$RSS),
  korrelation = c(cor(x_vores, y), cor(x_di, y), cor(x_mikro, y))
)
df_15
df_15$korrelation^2   #skal ligge tæt på r2


#### Opgave 2: PCA, PCR og PLS ####

#Vi bruger pls til PCR
library(pls)

#X er de 11 spørgsmål fra opgave 1 uden 2026K3. Y er realvæksten
df_pcr <- data.frame(realvaekst = y, df_kvartal[-107, col_names])
dim(df_pcr)   #106 rækker og 12 kolonner

#Trin 1: almindelig regression på alle 11 spørgsmål. Det er her problemet viser sig
summary(lm(realvaekst ~ ., data = df_pcr))
#R² er 0,586, men mange spørgsmål er ikke signifikante, og nogle har mærkelige fortegn

#Korrelationer mellem spørgsmålene: mange er tæt på 1
kor <- cor(df_pcr[, -1])
kor[upper.tri(kor, diag = TRUE)] <- NA
par_idx <- which(!is.na(kor), arr.ind = TRUE)
kor_par <- data.frame(spm1 = rownames(kor)[par_idx[, 1]],
                      spm2 = colnames(kor)[par_idx[, 2]],
                      r    = round(kor[par_idx], 2))
head(kor_par[order(-abs(kor_par$r)), ], 10)

#Trin 2: PCA, kun på X. scale. = TRUE, så store tal ikke vinder bare fordi de er store
pca_model <- prcomp(df_pcr[, -1], scale. = TRUE)
summary(pca_model)   #to komponenter forklarer ca. 73 % af X

#Trin 3: PCR. Cross validation vælger antal komponenter
set.seed(1)   #så CV giver det samme resultat hver gang
pcr_model <- pcr(realvaekst ~ ., data = df_pcr, scale = TRUE, validation = "CV")
summary(pcr_model)   #fejlen falder til ca. 2,10 ved to komponenter
validationplot(pcr_model, val.type = "RMSEP")

#Trin 4: vægte for hvert spørgsmål
round(pca_model$rotation[, 1:2], 2)

#PC1-vægte: kvadreret loading, sum = 1
vaegt_PC1 <- pcr_model$loadings[, 1]^2
sum(vaegt_PC1)   #skal give 1
sort(vaegt_PC1, decreasing = TRUE)   #F2, F10 og F3 har de største

#Opgave 2.2: standardiserede regressionsvægte fra PCR med to komponenter
library(ggplot2)

df_vaegte <- data.frame(
  spoergsmaal = names(coef(pcr_model, ncomp = 2)[, 1, 1]),
  vaegt = as.numeric(coef(pcr_model, ncomp = 2)[, 1, 1])
)

#Sorteret så den største står øverst
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


#### Opgave 2.3: Forudsig forbruget ####

#Modellen med to komponenter forudsiger 2025K3 (række 103). Modellen har set det kvartal
x_2025K3 <- df_kvartal[103, col_names]
df_kvartal[103, 1]                                   #skal vise 2025K3
predict(pcr_model, newdata = x_2025K3, ncomp = 2)    #modellens bud
df_pcr$realvaekst[103]                               #det faktiske tal

#Ægte prognose: træn kun til og med 2025K2 (række 102)
pcr_til_2025K2 <- pcr(realvaekst ~ ., data = df_pcr[1:102, ], scale = TRUE)
predict(pcr_til_2025K2, newdata = x_2025K3, ncomp = 2)

#2026K3: tillid findes, men forbrug er endnu ikke udgivet
x_2026K3 <- df_kvartal[107, col_names]
df_kvartal[107, 1]                                   #skal vise 2026K3
predict(pcr_model, newdata = x_2026K3, ncomp = 2)

#Er 0,07 mærkeligt? Vi sammenligner med normal vækst og fejlen
mean(df_pcr$realvaekst)                              #normal vækst
bud <- as.numeric(predict(pcr_model, newdata = x_2025K3, ncomp = 2))
df_pcr$realvaekst[103] - bud                         #fejlen, sammenlign med RMSEP på ca. 2,1
round(rbind(x_2025K3, gennemsnit = colMeans(df_pcr[, -1])), 1)

#Modellens vægte omregnet til vækst pr. nettotalpoint, og et regnestykke i hånden
koef      <- as.vector(coef(pcr_model, ncomp = 2))
gns_x     <- colMeans(df_pcr[, -1])
sd_x      <- apply(df_pcr[, -1], 2, sd)
vaegte_DI <- koef / sd_x
names(vaegte_DI) <- col_names
round(sort(vaegte_DI, decreasing = TRUE), 3)

bud_manuelt <- mean(df_pcr$realvaekst) + sum(koef * (as.numeric(x_2025K3) - gns_x) / sd_x)
bud_manuelt   #skal give 0,0697

#Hvem trækker prognosen ned og op? Negative tal trækker ned
bidrag <- vaegte_DI * (as.numeric(x_2025K3) - gns_x)
round(sort(bidrag), 2)
mean(df_pcr$realvaekst) + sum(bidrag)   #skal give 0,07


#### Opgave 2.4: DI's forudsigelse for 2016 ####

#Gennemsnittet af de fire kvartaler i 2016 (række 65-68)
mean(df_pcr$realvaekst[65:68])


#### Opgave 3: Bliver julehandlen større i 2026 end i 2025? ####

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
TN / (TN + FP)                #specificitet
max(table(df_model$retning)) / nrow(df_model)   #baseline: altid gæt på det hyppigste

#5) Gættet for 2026
p_ny_aar <- predict(glm_julehandel, newdata = data.frame(tillid = tillid_ny), type = "response")
p_ny_aar   #sandsynlighed for at julehandlen i 2026 er større end i 2025
ifelse(p_ny_aar > 0.5, "OP", "NED")

table(df_model$retning)
mean(df_model$retning == "OP")
df_model$År[df_model$retning == "NED"]
addmargins(konfusion)
#### Opgave 4: Stabilitet ####

#Vi bruger samme X og y som i opgave 1, og tester hvor stabil hver kombination er
#Adj. R² for alle 2047 kombinationer i de kvartaler der er med i et vindue
adj_r2_vindue <- function(raekker) {
  sapply(kombinationer, function(k) {
    summary(lm(y[raekker] ~ samlet_indikator_forbrug[[k]][raekker]))$adj.r.squared
  })
}

#Fire slags vinduer: 24 + 24 + 24 + 12 = 84 kørsler
vinduer_1 <- lapply(1:24, function(i) (i + 1):106)        #fjern de første i kvartaler
vinduer_2 <- lapply(1:24, function(i) 1:(106 - i))        #fjern de sidste i kvartaler
vinduer_3 <- lapply(1:24, function(i) i:(i + 81))         #rullende vindue på 82 kvartaler
vinduer_4 <- lapply(1:12, function(k) (k + 1):(106 - k))  #fjern k fra begge ender

adj_loop <- lapply(list(vinduer_1, vinduer_2, vinduer_3, vinduer_4),
                   function(v) t(sapply(v, adj_r2_vindue)))

#Gennemsnit pr. loop, og så gennemsnittet af de fire loop
gns_pr_loop <- sapply(adj_loop, colMeans)
samlet_gns  <- rowMeans(gns_pr_loop)

#Vinderen og DI-FTI
names(which.max(samlet_gns))
sort(samlet_gns, decreasing = TRUE)[1:10]
rank(-samlet_gns)[["F4.F9.F7.F10.F12.F13"]]   #placering af vores bedste
samlet_gns[["F2.F4.F9.F10"]]                  #DI-FTI


#### Opgave 5: Vind fra DMI ####

library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)

#Test af API'et: vindstyrke i Århus Syd 19. november 2022
baseurl <- "https://opendataapi.dmi.dk/v2/metObs/collections/observation/items?"
res <- GET(paste0(baseurl, "stationId=06074&datetime=2022-11-19T00:00:00Z/2022-11-20T00:00:00Z&parameterId=wind_speed_past1h"))
res$status_code   #200 betyder OK
resdf <- fromJSON(httr::content(res, as = "text", encoding = "UTF-8"))
resdf$numberReturned

windforceparam <- "wind_speed_past1h"
windirparam    <- "wind_dir_past1h"
sid       <- "06074"   #Århus Syd
sidanholt <- "06079"   #Anholt Havn

#Henter én parameter for én station og laver tidspunkterne om til tid
getdatafromdmi <- function(param, station, frd, tod) {
  varurl <- paste0("parameterId=", param, "&stationId=", station, "&datetime=", frd, "/", tod, "&limit=10000")
  res <- GET(paste0(baseurl, varurl))
  resdf <- fromJSON(httr::content(res, as = "text", encoding = "UTF-8"))
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
  stopifnot(nrow(styrke) == nrow(retning))   #stopper, hvis antallet ikke passer
  data.frame(observed = styrke$observed, force = styrke$value, dir = retning$value)
}

#Stormen i november 2022
totalwind       <- hent_vind(sid, "2022-11-14T00:00:00Z", "2022-11-21T00:00:00Z")
totalwindanholt <- hent_vind(sidanholt, "2022-11-14T00:00:00Z", "2022-11-21T00:00:00Z")

ggplot(totalwind, aes(x = observed, y = force)) +
  geom_line(color = "red") +
  geom_line(data = totalwindanholt, aes(x = observed, y = force), color = "blue") +
  theme_classic()

#Hvilken retning kom vinden fra? dir er den retning vinden kommer FRA
#Fire sektorer: N, Ø, S, V. Vi lægger 45 grader til, så N går fra 315 til 45
retning_navne <- c("N", "Ø", "S", "V")
sektor <- function(d) {
  cut((d + 45) %% 360, breaks = c(0, 90, 180, 270, 360), labels = retning_navne, right = FALSE)
}
table(sektor(totalwind$dir))
table(sektor(totalwindanholt$dir))

#Stormen Babet, oktober 2023
babet_aarhus <- hent_vind(sid, "2023-10-19T00:00:00Z", "2023-10-23T00:00:00Z")
babet_anholt <- hent_vind(sidanholt, "2023-10-19T00:00:00Z", "2023-10-23T00:00:00Z")

table(sektor(babet_aarhus$dir))
table(sektor(babet_anholt$dir))

max(babet_aarhus$force, na.rm = TRUE)
babet_aarhus$observed[which.max(babet_aarhus$force)]
max(babet_anholt$force, na.rm = TRUE)
babet_anholt$observed[which.max(babet_anholt$force)]

#Graf med styrke og retning. Pilene peger dér hvor vinden blæser hen
babet_aarhus$station <- "Århus Syd"
babet_anholt$station <- "Anholt Havn"
begge <- rbind(babet_aarhus, babet_anholt)

#En pil for hver 3. time, for hver station for sig
pile <- begge %>%
  group_by(station) %>%
  filter(row_number() %% 3 == 1) %>%
  ungroup()

ggplot(begge, aes(x = observed, y = force, color = station)) +
  geom_line() +
  geom_text(data = pile, aes(angle = -90 - dir), label = "→", size = 9, show.legend = FALSE) +
  scale_color_manual(values = c("Anholt Havn" = "blue", "Århus Syd" = "red")) +
  labs(title = "Stormen Babet, oktober 2023",
       x = "Tid (UTC)", y = "Vindhastighed (m/s)", color = "Station") +
  theme_classic()

