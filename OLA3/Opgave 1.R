#### Opgave 1.1 – Kombinationer af spørgsmålene i forbrugerundersøgelsen ####
# Opgaven: lav alle kombinationer af spørgsmålene i DST's forbrugerundersøgelse (FORV1)
# Periode: 2000K1 til 2026K3 (opdateret til den seneste periode)
# Resultat: samlet_indikator_df_combi (107 kvartaler x 2047 kombinationer)
# Én kombination = ét ligevægtet gennemsnit af de valgte spørgsmåls nettotal

library(dkstat)

# --- Hent data ---
# DST giver forbrugertilliden månedligt, så den skal omregnes til kvartaler
voresfti1 <- dst_meta(table = "FORV1", lang = "da")  # metadata: hvilke indikatorer og tidspunkter findes

unique(voresfti1)
voresfti1$values$INDIKATOR  # viser de 13 indikatorer: F1 (samlet indikator) og F2-F13 (enkeltspørgsmål)

# Denne forespørgsel henter 8 udvalgte spørgsmål. Den hører til opgave 1.5 (mikroindikator)
# og bruges ikke i beregningerne herunder. Alle regressioner bygger på FTIALLEINDI længere nede
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
# Her henter vi ALLE indikatorer og alle måneder. "*" betyder "alle"
FTIALLEINDI_query <- list(
  INDIKATOR = "*",
  Tid = "*"
)

# Data kommer i long format: én række pr. kombination af indikator og måned
FTIALLEINDI<- dst_get_data(
  table = "FORV1",
  query = FTIALLEINDI_query,
  meta_data = voresfti1,
  lang = "da"
)
max(FTIALLEINDI$TID)


# --- Rens data ---
# Først ser vi, hvilke indikatorer der er hentet (skal være 13: F1-F13)
unique(FTIALLEINDI$INDIKATOR)

# F1 fjernes, fordi den er den samlede indikator og ikke et enkeltspørgsmål
# F11 udeladt: serien ophører efter 2025M04 (17 NA'er i 2025M05-2026M09)
# Opgaven siger 12  spørgsmål, men uden F11 har vi 11 spørgsmål (2047 kombinationer i stedet for 4095)
FTIALLEINDI_11_spg <- FTIALLEINDI[!grepl("^F11|^F1 ", FTIALLEINDI$INDIKATOR), ]

# Vi beholder kun januar 2000 og frem (opgavens startpunkt)
FTIALLEINDI_11_spg <- FTIALLEINDI_11_spg[FTIALLEINDI_11_spg$TID >= as.Date("2000-01-01"), ]
# Kun fulde kvartaler: afgræns til og med september 2026, så oktober ikke ødelægger kvartalsberegningen
FTIALLEINDI_11_spg <- FTIALLEINDI_11_spg[FTIALLEINDI_11_spg$TID < as.Date("2026-10-01"), ]


# Kontrol: der skal ikke være manglende værdier tilbage
sum(is.na(FTIALLEINDI_11_spg$value))  #antal NA i value (1460 i alle. nu det 0 jibiii

# --- Fra måneder til kvartaler ---
# Vi viser fremgangsmåden på ét spørgsmål (F2) først, så vi kan se, at den virker
df_spm <- FTIALLEINDI_11_spg[grepl("^F2 ", FTIALLEINDI_11_spg$INDIKATOR), ]
df_spm <- df_spm[order(df_spm$TID), ]  # sorteres efter tid, så månederne står i rækkefølge

# Hver kolonne i matricen er ét kvartal: tre måneder (jan-feb-mar osv.) pr. kolonne
matrix(df_spm$value, nrow = 3)[, 1:2]  #kig på de to første kvartaler

# Gennemsnit af hver kolonne = ét kvartalsgennemsnit
spm_kvartal <- colMeans(matrix(df_spm$value, nrow = 3))
head(spm_kvartal)

# Vi laver kvartalsnavne (fx 2000K1) ud fra den første måned i hvert kvartal
måneder <- sort(unique(FTIALLEINDI_11_spg$TID))
format(måneder[1], "%m")  #skal være "01"
length(måneder) %% 3      #skal være 0, ellers er sidste kvartal ikke fuldt

første_måned <- måneder[seq(1, length(måneder), by = 3)]  #første måned i hvert kvartal
df_kvartal <- data.frame(kvartal = paste0(format(første_måned, "%Y"), "K",
                                          (as.numeric(format(første_måned, "%m")) - 1) %/% 3 + 1))

# Vi gentager kvartalsberegningen for alle spørgsmål og gemmer hvert i sin egen kolonne
# Resultat: df_kvartal med 107 rækker (kvartaler) og 12 kolonner (kvartal + 11 spørgsmål)
spørgsmål <- unique(FTIALLEINDI_11_spg$INDIKATOR)

for (i in spørgsmål) {
  df_spm <- FTIALLEINDI_11_spg[FTIALLEINDI_11_spg$INDIKATOR == i, ]
  df_spm <- df_spm[order(df_spm$TID), ]
  df_kvartal[[i]] <- colMeans(matrix(df_spm$value, nrow = 3))  #ny kolonne pr. spørgsmål
}

# Disse to linjer bliver lavet igen længere nede under "Indikator projekt".
# De bruges ikke her, men de er med i scriptet som det er
indikator_liste <- rep(list(rep(list())), 11)
z_length <- length(df_kvartal[ , 2:ncol(df_kvartal)])

# --- Vend fortegn ---
# F6 (priser i dag) og F8 (arbejdsløshed om et år) vendes, så høj værdi altid betyder "godt for forbruget"
# Det skal ske FØR kombinationerne laves. Linjerne må kun køres én gang: kører man dem to gange, vender fortegnet tilbage
# F7 (priser om et år) vendes ikke: stigende forventede priser giver incitament til at fremrykke køb (fremrykningshypotesen)
df_kvartal$`F6 Priser i dag, sammenlignet med for et år siden` <- df_kvartal$`F6 Priser i dag, sammenlignet med for et år siden` * -1
df_kvartal$`F8 Arbejdsløsheden om et år, sammenlignet med i dag` <- df_kvartal$`F8 Arbejdsløsheden om et år, sammenlignet med i dag` * -1

# Denne linje står igen efter det opdaterede loop, hvor den bruges.
# Her kører den på en liste, der endnu ikke er fyldt ud
samlet_indikator_df_combi <- data.frame(indikator_liste)

# Kontrol: 107 rækker og 12 kolonner (kvartal + 11 spørgsmål)
dim(df_kvartal)  #kolonner skal være 12: kvartal + 11 spørgsmål
head(df_kvartal)

# --- Test af kombinationsmetoden på ét kvartal ---
# Først tester vi på kvartal 1 for at se, at metoden giver de rigtige antal
første_kvartal <- unlist(df_kvartal[1, 2:ncol(df_kvartal)])  # de 11 spørgsmål i 2000K1 som en talrække

kombi_par <- combn(første_kvartal, 2)  # alle par af spørgsmål: 2 rækker, én kolonne pr. par
dim(kombi_par)

choose(11, 2)  # antal par, skal være det samme som antal kolonner

indikator_par <- colMeans(kombi_par)  # gennemsnit pr. par = indikatorværdi for 2000K1
head(indikator_par)

sum(choose(11, 1:11))  # samlet antal kombinationer, skal være 2047

# Test: alle kombinationsstørrelser (1 til 11 spørgsmål) for alle kvartaler, uden kolonnenavne
# Denne version bliver ikke brugt videre. Den dokumenterer, at antallene stemmer
antal_spm <- ncol(df_kvartal) - 1  #11 spørgsmål
alle_kombi <- list()  # ét element pr. kombinationsstørrelse

for (j in 1:antal_spm) {
  kombi_matrix <- matrix(NA, nrow = nrow(df_kvartal), ncol = choose(antal_spm, j))  # tom matrix til størrelse j
  for (i in 1:nrow(df_kvartal)) {
    kvartal_tal <- unlist(df_kvartal[i, 2:ncol(df_kvartal)])  # spørgsmålene i kvartal i
    kombi_matrix[i, ] <- colMeans(combn(kvartal_tal, j))  # gennemsnit af hver kombination
  }
  alle_kombi[[j]] <- kombi_matrix
}

sapply(alle_kombi, ncol)  #11 55 165 330 462 462 330 165 55 11 1
sum(sapply(alle_kombi, ncol))  #skal give 2047 og det gør det

### --- Indikator projekt - alle indikatore --- ###
# Første version af kombinationsloopet, uden kolonnenavne. Den bliver overskrevet af den opdaterede version herunder
# Bemærk: linjen med "l <- length(...)" bruger i, før i er defineret i loopet (den bruger en gammel værdi af i)
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


# --- Den endelige kombinationsalgoritme (svaret på 1.1) ---
# Opdateret loop med navngivet kolonner.
# For hver kombinationsstørrelse (1 til 11 spørgsmål) laves en matrix med 107 rækker (kvartaler)
# og én kolonne pr. kombination. Hver celle er gennemsnittet af de valgte spørgsmål i det kvartal
indikator_liste <- rep(list(rep(list())), 11)  # tom liste: ét element pr. kombinationsstørrelse
z_length <- length(df_kvartal[ , 2:ncol(df_kvartal)])

# Kortnavne på spørgsmålene. Rækkefølgen SKAL passe til rækkefølgen af kolonnerne i df_kvartal
col_names <- c("F2", "F3", "F4", "F5", "F9", "F6", "F7", "F8", "F10", "F12", "F13" )

for (z_length in 1:z_length) {  # z_length = antal spørgsmål i kombinationen
  # l = antal kombinationer af den størrelse (fx 462 for 5 eller 6 spørgsmål)
  l <- length(colMeans(matrix(as.numeric(combn(df_kvartal[i,2:ncol(df_kvartal)], z_length)), nrow = z_length)))
  df_combi_d <- matrix(0, nrow = nrow(df_kvartal), ncol = l)  # tom matrix til størrelsen
  names <- combn(col_names, z_length)
  # navnene laves, fx "F4,F9,F10". I dataframet bliver kommaerne til punktummer
  names <- combn(col_names, z_length, paste, collapse = ",")
  colnames(df_combi_d) <- names
  for (i in 1:nrow(df_kvartal)) {  # i = kvartal
    # kombinationerne af kvartalets spørgsmål, og gennemsnittet af hver kombination
    df_combi_d[i, ] <- colMeans(matrix(as.numeric(combn(df_kvartal[i,2:ncol(df_kvartal)], z_length)), nrow = z_length))
    indikator_liste[[z_length]] <- df_combi_d
  }
}


# Alle matricer samles i ét dataframe: 107 rækker (2000K1-2026K3) og 2047 kolonner
# Kolonnenavnene viser, hvilke spørgsmål der indgår, fx "F4.F9.F7.F10.F12.F13"
samlet_indikator_df_combi <- data.frame(indikator_liste)

# Kontrol: vinderen genskabt manuelt direkte fra df_kvartal (skal give TRUE)
kol <- sapply(c("F4", "F9", "F7", "F10", "F12", "F13"),
              function(f) grep(paste0("^", f, " "), names(df_kvartal)))
manuel <- rowMeans(df_kvartal[, kol])
all.equal(manuel, samlet_indikator_df_combi[["F4.F9.F7.F10.F12.F13"]])

udsnit_11 <- cbind(kvartal = df_kvartal$kvartal,
                   round(samlet_indikator_df_combi[, c("F2", "F4", "F2.F4.F9.F10", "F4.F9.F7.F10.F12.F13")], 2))
udsnit_11[c(1:5, 103:107), ]


#### Slut på opgave 1.1 ####






#### Opgave 1.2 – R² og sammenligning med DI's indikator ####


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

#vi finder R2
names(lm_indikator_liste[[1]])
rss <- sapply(lm_indikator_liste, function(x) sum(x$residuals^2))
r2 <- sapply(lm_indikator_liste, function(x) x$r.squared)

View(lm_indikator_liste)
# Dataframe med kombinationer, r^2 og RSS
df_r2 <- data.frame(
  kombination = names(samlet_indikator_forbrug)[3:ncol(samlet_indikator_forbrug)],
  RSS = rss,
  r2 = r2
)

# Top 10 😎
nrow(df_r2)
head(df_r2)

top10 <- head(df_r2[order(df_r2$r2, decreasing = TRUE), ], 10)
top10

#korrelation
y <- samlet_indikator_forbrug$realvaekst
x_vores <- samlet_indikator_forbrug[["F4.F9.F7.F10.F12.F13"]]
x_di    <- samlet_indikator_forbrug[["F2.F4.F9.F10"]]

cor(x_vores, y)
cor(x_di, y)

bedste <- df_r2[which.max(df_r2$r2), c("kombination", "r2", "RSS")]
di     <- df_r2[df_r2$kombination == "F2.F4.F9.F10", c("kombination", "r2", "RSS")]

sammenligning <- rbind(bedste, di)
sammenligning$indikator <- c("Vores bedste", "DI-FTI")
sammenligning

sammenligning$korrelation <- c(cor(x_vores, y), cor(x_di, y))
sammenligning
sammenligning$korrelation^2 #Med én regressor er R² lig korrelationen i anden.


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

sammenligning$r2 <- df_r2$r2[match(sammenligning$kombination, df_r2$kombination)]

sammenligning <- sammenligning[, c("indikator", "kombination", "r2", "RSS", "korrelation")]
sammenligning

# Figur 1: faktisk vækst, vores bedste indikator og DI-FTI
kvartal <- df_vaekst_kvartal$kvartal   # x-aksen (2000.00 = 2000K1, 2000.25 = 2000K2 osv.)

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

# Kontrol: samme periode som DI's artikel (2000K1-2016K2 er de første 66 kvartaler)
# DI's egne tal i artiklen: R2 0,54 og korrelation 0,73 for deres indikator
p <- 1:66
c(R2_vores  = summary(lm(y[p] ~ x_vores[p]))$r.squared,
  R2_di     = summary(lm(y[p] ~ x_di[p]))$r.squared,
  cor_vores = cor(x_vores[p], y[p]),
  cor_di    = cor(x_di[p], y[p]))


# C) Er forspringet stabilt? Sammenlign R2 i første og anden del af perioden
q1 <- 1:66     # 2000K1-2016K2 (DI's periode)
q2 <- 67:106   # 2016K3-2026K2
r2_del <- function(r) c(vores = summary(lm(y[r] ~ x_vores[r]))$r.squared,
                        di    = summary(lm(y[r] ~ x_di[r]))$r.squared,
                        f10   = summary(lm(y[r] ~ samlet_indikator_forbrug[["F10"]][r]))$r.squared)
round(rbind(`2000K1-2016K2` = r2_del(q1), `2016K3-2026K2` = r2_del(q2)), 3)



# D udvidet: R2 i fire delperioder for fem indikatorer
# Række 1-80 er 2000K1-2019K4, række 81-92 er 2020K1-2022K4, række 93-106 er 2023K1-2026K2
ind <- list(vores   = x_vores,
            ligelig = samlet_indikator_forbrug[["F4.F9.F7.F12"]],
            F10     = samlet_indikator_forbrug[["F10"]],
            makro   = samlet_indikator_forbrug[["F4.F7"]],
            DI      = x_di)

r2_udsnit5 <- function(r) round(sapply(ind, function(x) summary(lm(y[r] ~ x[r]))$r.squared), 2)

rbind(`Alle data`                 = r2_udsnit5(1:106),
      `Før Corona, 2000K1-2019K4` = r2_udsnit5(1:80),
      `Uden 2020K1-2022K4`        = r2_udsnit5(setdiff(1:106, 81:92)),
      `Kun 2023K1 og frem`        = r2_udsnit5(93:106))






#Opgave 1.3

# Vi tæller mikro- og makrospørgsmål i hver kombination
mikro <- c("F2", "F3", "F9", "F10", "F12", "F13")
makro <- c("F4", "F5", "F6", "F7", "F8")

spm <- strsplit(df_r2$kombination, ".", fixed = TRUE)
df_r2$antal_mikro <- sapply(spm, function(x) sum(x %in% mikro))
df_r2$antal_makro <- sapply(spm, function(x) sum(x %in% makro))

# Gitter: højeste R2 for hver kombination af antal mikro (rækker) og antal makro (kolonner)
gitter <- matrix(NA, nrow = 7, ncol = 6,
                 dimnames = list(paste0("mikro", 0:6), paste0("makro", 0:5)))
for (a in 0:6) {
  for (b in 0:5) {
    d <- df_r2[df_r2$antal_mikro == a & df_r2$antal_makro == b, ]
    if (nrow(d) > 0) gitter[a + 1, b + 1] <- max(d$r2)
  }
}
round(gitter, 3)

View(gitter)

# Vi inddeler hver kombination i en gruppe efter mikro/makro-sammensætning
df_r2$gruppe <- ifelse(df_r2$antal_makro == 0, "ren mikro",
                       ifelse(df_r2$antal_mikro == 0, "ren makro",
                              ifelse(df_r2$antal_mikro > df_r2$antal_makro, "mikro-tung",
                                     ifelse(df_r2$antal_mikro < df_r2$antal_makro, "makro-tung", "ligelig"))))

# Bedste indikator i hver gruppe, og hvor mange kombinationer der var at vælge imellem
bedste_pr_gruppe <- do.call(rbind, lapply(split(df_r2, df_r2$gruppe), function(d) {
  r <- d[which.max(d$r2), c("kombination", "antal_mikro", "antal_makro", "r2", "RSS")]
  r$antal_kombi <- nrow(d)
  r
}))
bedste_pr_gruppe

#### Skridt 1: RSS, RMSE, støjtest og t-/p-værdier for gruppevinderne ####

# --- A) RSS, TSS og RMSE ---
# TSS = udsvingene i væksten omkring dens gennemsnit (samme tal for alle modeller)
n   <- length(y)
tss <- sum((y - mean(y))^2)

# RMSE = typisk fejl i procentpoint, regnet ud fra RSS. Lægges på alle 2047 kombinationer
df_r2$RMSE <- sqrt(df_r2$RSS / n)

# Kontrol: R2 = 1 - RSS/TSS skal give samme tal som r2 (skal give TRUE)
all.equal(1 - df_r2$RSS / tss, df_r2$r2)

# Vi henter r2 og RMSE til gruppevinderne ved at matche på kombinationsnavnet
raekker <- match(bedste_pr_gruppe$kombination, df_r2$kombination)
bedste_pr_gruppe$r2   <- df_r2$r2[raekker]
bedste_pr_gruppe$RMSE <- df_r2$RMSE[raekker]

# DI-FTI tilføjes som reference. DI har 3 mikro (F2, F9, F10) og 1 makro (F4)
di_raekke <- df_r2[df_r2$kombination == "F2.F4.F9.F10", ]
di_tabel <- data.frame(kombination = "F2.F4.F9.F10", antal_mikro = 3, antal_makro = 1,
                       r2 = di_raekke$r2, r2 = di_raekke$r2,
                       RSS = di_raekke$RSS, RMSE = di_raekke$RMSE)

tabel_rmse <- rbind(
  bedste_pr_gruppe[, c("kombination", "antal_mikro", "antal_makro", "r2", "r2", "RSS", "RMSE")],
  di_tabel
)
rownames(tabel_rmse)[nrow(tabel_rmse)] <- "DI-FTI"
tabel_rmse

# Til sammenligning: hvor meget selve væksten svinger
sd(y)


# --- B) Støjtest: hvad sker der med adj. R2, når ét spørgsmål fjernes? ---
# De rå spørgsmål for de 106 kvartaler (F6 og F8 er allerede fortegnsvendt i df_kvartal)
X <- df_kvartal[-107, -1]
names(X) <- sub(" .*", "", names(X))   # kortnavne: F2, F3, F4 ...

# Korrelation mellem hvert spørgsmål og forbrugsvæksten (F6 og F8 er fortegnsvendt)
round(cor(X, y), 2)


# Hvor ofte er F10 negativ? F10 er forventet forbrug af større forbrugsgoder de næste 12 mdr.
# sammenlignet med de seneste 12 mdr. Et negativt nettotal er ikke uden videre pessimisme
range(X$F10)
mean(X$F10)
sum(X$F10 > 0)                        # antal kvartaler med positivt nettotal
df_kvartal$kvartal[-107][X$F10 > 0]   # hvilke kvartaler (række 107 er 2026K3, som X ikke har med)


# Funktion: for hvert spørgsmål i en kombination fjerner vi det og ser, hvordan adj. R2 ændrer sig
# Negativ ændring = spørgsmålet bidrager. Tæt på 0 eller positiv = ingen gevinst
stoejtest <- function(kombination) {
  spm <- strsplit(kombination, ".", fixed = TRUE)[[1]]
  if (length(spm) < 2) return(NA)   # med ét spørgsmål er der intet at fjerne
  adj_alle <- summary(lm(y ~ rowMeans(X[, spm, drop = FALSE])))$adj.r.squared
  sapply(spm, function(q) {
    rest <- setdiff(spm, q)
    summary(lm(y ~ rowMeans(X[, rest, drop = FALSE])))$adj.r.squared - adj_alle
  })
}

# Vi kører den for alle gruppevindere og for DI
stoej_resultat <- lapply(c(bedste_pr_gruppe$kombination, "F2.F4.F9.F10"), stoejtest)
names(stoej_resultat) <- c(bedste_pr_gruppe$kombination, "F2.F4.F9.F10")
lapply(stoej_resultat, round, 4)




# --- C) t- og p-værdier pr. spørgsmål (multipel regression) ---
# Her får hvert spørgsmål sin egen vægt. Det er IKKE samme model som indikatoren (gennemsnit)
# Vi viser koefficienttabellen: Estimate, Std. Error, t value, Pr(>|t|)
multi_resultat <- lapply(c(bedste_pr_gruppe$kombination, "F2.F4.F9.F10"), function(k) {
  spm <- strsplit(k, ".", fixed = TRUE)[[1]]
  if (length(spm) < 2) return(NA)
  d <- data.frame(y = y, X[, spm, drop = FALSE])
  round(summary(lm(y ~ ., data = d))$coefficients, 3)
})
names(multi_resultat) <- c(bedste_pr_gruppe$kombination, "F2.F4.F9.F10")
multi_resultat

# Hænger vinderens spørgsmål sammen? Høje korrelationer gør t-værdierne ustabile
round(cor(X[, c("F4", "F9", "F7", "F10", "F12", "F13")]), 2)


# antal spørgsmål i hver kombination
df_r2$antal <- sapply(strsplit(df_r2$kombination, ".", fixed = TRUE), length)

# 1) bedste indikator for hvert antal spørgsmål (1 til 11)
bedste_pr_antal <- do.call(rbind, lapply(1:11, function(k) {
  d <- df_r2[df_r2$antal == k, ]
  d[which.max(d$r2), c("kombination", "antal", "r2", "RSS")]
}))
bedste_pr_antal

# 2) de tre bedste uden F7
uden_F7 <- df_r2[!grepl("F7", df_r2$kombination), ]
head(uden_F7[order(-uden_F7$r2), c("kombination", "antal", "r2")], 3)

# 3) enkeltspørgsmålene (til diskussionen af hvert spørgsmål)
df_r2[df_r2$antal == 1, c("kombination", "r2")]


#### Figur 2: seks paneler, gruppevindere og DI-FTI ####
kvartal <- df_vaekst_kvartal$kvartal

# Rækkefølge som i tabel 2: vinderen først, DI-FTI til sidst
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
       main = paste0(titler[i], " (R² = ", r2_i, ")"),
       cex.main = 1, cex.axis = 0.9)
  abline(h = 0, lty = 3)
  lines(kvartal, fitted(fit), col = farver[i], lwd = 2)
}

# Fælles overskrift og aksetitler
mtext("Ren mikro og DI-FTI undervurderer faldet i 2009 mest,\nog ingen af indikatorerne rammer toppen i 2021",
      side = 3, outer = TRUE, line = 1, font = 2, cex = 1)
mtext("År", side = 1, outer = TRUE, line = 0.5, cex = 0.9)
mtext("Årlig realvækst, pct.", side = 2, outer = TRUE, line = 0, cex = 0.9)
dev.off()



#Nemmere at afkode: en tabel i stedet for mærkede punkter
# 1) Kvartalsnavne (2021.25 bliver til "2021K2")
kv_navn <- paste0(floor(kvartal), "K", round((kvartal - floor(kvartal)) * 4) + 1)

# 2) Gæt og residual (faktisk minus gæt) for vores bedste og DI
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

# 3) De 10 kvartaler hvor vores bedste misser mest
head(afvigelse[order(-abs(afvigelse$res_vores)), ], 10)

# 4) De 10 kvartaler hvor DI misser mest
head(afvigelse[order(-abs(afvigelse$res_di)), ], 10)

# Hvor rammer indikatorerne forkert i to kriser: finanskrisen og energikrisen?
# Residual = faktisk vækst minus modellens gæt. Positiv = væksten var højere end gættet
res_f10   <- resid(lm(y ~ samlet_indikator_forbrug[["F10"]]))   # F10 alene
res_vores <- resid(lm(y ~ x_vores))                              # vores bedste
res_di    <- resid(lm(y ~ x_di))                                 # DI-FTI

# Række 36-38 er 2008K4-2009K2 (finanskrisen), række 90-94 er 2022K2-2023K2 (energikrisen)
r_krise <- c(36:38, 90:94)
data.frame(kvartal   = kv_navn[r_krise],
           faktisk   = round(y[r_krise], 1),
           res_f10   = round(res_f10[r_krise], 1),
           res_vores = round(res_vores[r_krise], 1),
           res_di    = round(res_di[r_krise], 1))

# Typisk fejl (RMSE) i hver krise: finanskrisen 2008K3-2009K4 (række 35-40) og energikrisen
rmse_vindue <- function(r) c(f10   = sqrt(mean(res_f10[r]^2)),
                             vores = sqrt(mean(res_vores[r]^2)),
                             di    = sqrt(mean(res_di[r]^2)))
round(rbind(finanskrise = rmse_vindue(35:40),
            energikrise = rmse_vindue(90:94)), 2)



# Opsving efter nedgang: er fejlen anderledes i kvartaler efter en negativ vækst?
foer_neg <- c(FALSE, y[-length(y)] < 0)    # TRUE hvis forrige kvartal havde negativ vækst
d_y      <- c(NA, diff(y))                 # ændring i vækst fra forrige kvartal
opsving  <- foer_neg & !is.na(d_y) & d_y > 0    # forrige negativt, væksten stiger nu
fortsat  <- foer_neg & !is.na(d_y) & d_y <= 0   # forrige negativt, væksten falder igen

# De enkelte opsvingskvartaler og fejlene (positiv fejl = modellen gætter for lavt)
data.frame(kvartal   = kv_navn[opsving],
           faktisk   = round(y[opsving], 1),
           res_vores = round(res_vores[opsving], 1),
           res_di    = round(res_di[opsving], 1),
           res_f10   = round(res_f10[opsving], 1))

# Gennemsnitlig og median fejl i de tre grupper
gns <- function(g) c(n = sum(g), vores = mean(res_vores[g]), di = mean(res_di[g]), f10 = mean(res_f10[g]))
med <- function(g) c(vores = median(res_vores[g]), di = median(res_di[g]), f10 = median(res_f10[g]))
round(rbind(opsving = gns(opsving), fortsat_nedgang = gns(fortsat), oevrige = gns(!foer_neg)), 2)
round(rbind(opsving = med(opsving), fortsat_nedgang = med(fortsat), oevrige = med(!foer_neg)), 2)

# Hvilke kvartaler er 'nedgangen fortsætter'?
data.frame(kvartal   = kv_navn[fortsat],
           faktisk   = round(y[fortsat], 1),
           res_vores = round(res_vores[fortsat], 1),
           res_di    = round(res_di[fortsat], 1),
           res_f10   = round(res_f10[fortsat], 1))







#1.4

#er oktober tallene udgivet? nej

#### Opgave 1.4 – Forudsigelse af forbruget i 2026K3 og 2026K4 ####

# 1) Er oktober udgivet? Står der 2026-09-01, er oktober ikke med
max(FTIALLEINDI$TID)

# 2) Vi estimerer de to modeller på de 106 kvartaler, hvor væksten kendes
lm_vores <- lm(y ~ x_vores)
lm_di    <- lm(y ~ x_di)

# 3) 2026K3: indikatorværdien findes (række 107), men væksten er ikke offentliggjort
x_k3_vores <- samlet_indikator_df_combi[107, "F4.F9.F7.F10.F12.F13"]
x_k3_di    <- samlet_indikator_df_combi[107, "F2.F4.F9.F10"]

# Forudsigelse for 2026K3 med 95 % prediktionsinterval
predict(lm_vores, newdata = data.frame(x_vores = x_k3_vores), interval = "prediction")
predict(lm_di,    newdata = data.frame(x_di = x_k3_di),       interval = "prediction")

# 4) Månedsserier for de to indikatorer (gennemsnit af spørgsmålene i hver måned)
#    Bruger FTIALLEINDI (alle måneder), så oktober kommer med, når den er udgivet
maanedlig <- function(koder) {
  d <- FTIALLEINDI[sub(" .*", "", FTIALLEINDI$INDIKATOR) %in% koder, ]
  tapply(d$value, d$TID, mean)
}
m_vores <- maanedlig(c("F4", "F9", "F7", "F10", "F12", "F13"))
m_di    <- maanedlig(c("F2", "F4", "F9", "F10"))

# Kontrol: de tre seneste måneder (nu juli til september)
tail(round(m_vores, 1), 3)

# 5) Vi har ikke november og december, så vi tester, om første måned kan stå for hele kvartalet
#    Hver kolonne i matricen er ét kvartal (tre måneder), række 1 er første måned
lav_kvartaler <- function(m) {
  m <- m[names(m) >= "2000-01-01"]          # kun fra 2000
  m <- m[1:(3 * floor(length(m) / 3))]      # kun hele kvartaler
  matrix(m, nrow = 3)
}
mat_v <- lav_kvartaler(m_vores)
mat_d <- lav_kvartaler(m_di)

kv_v <- colMeans(mat_v)    # kvartalsgennemsnit
kv_d <- colMeans(mat_d)

# Kontroller: 107 kvartaler, og sidste kvartal skal give K3-værdien (skal give TRUE)
dim(mat_v)
all.equal(unname(kv_v[length(kv_v)]), x_k3_vores)

# Hænger første måned sammen med kvartalets gennemsnit?
c(cor_vores = cor(mat_v[1, ], kv_v), cor_di = cor(mat_d[1, ], kv_d))

# Typisk afvigelse i nettotalspoint mellem første måned og kvartalsgennemsnittet
c(sd_afv_vores = sd(mat_v[1, ] - kv_v), sd_afv_di = sd(mat_d[1, ] - kv_d))

# 6) K4: to bud på indikatoren, nu seneste måned og seneste tre måneder
#    Når oktober er med, bliver "seneste måned" oktober og "seneste tre" august til oktober
n_m <- length(m_vores)
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

# 7) K4 med prediktionsinterval, hvis seneste måned bruges som mål for kvartalet
predict(lm_vores, newdata = data.frame(x_vores = x0_vores), interval = "prediction")
predict(lm_di,    newdata = data.frame(x_di    = x0_di),    interval = "prediction")

# 8) Følsomhed: hvad sker der, hvis tilliden skifter med ét typisk kvartalsskift?
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

#### Slut på opgave 1.4 ####





#opgave 1.5

#### Opgave 1.5 – Bedste mikroindikator ####

# 1) Vi har defineret, hvilke spørgsmål der er mikro (husholdningens egen økonomi og adfærd)
#    F11 er ikke med, fordi den er udeladt af data (serien stopper efter 2025M04)
mikro <- c("F2", "F3", "F9", "F10", "F12", "F13")

# 2) Vi skærer hver kombination op i sine spørgsmål, fx "F4.F9.F10" bliver til "F4" "F9" "F10"
#    fixed = TRUE får R til at læse punktummet som et almindeligt tegn
spoergsmaal_i_kombi <- strsplit(df_r2$kombination, ".", fixed = TRUE)

# 3) Vi tjekker for hver kombination, om ALLE dens spørgsmål er mikro
#    TRUE = kombinationen består kun af mikrospørgsmål
er_mikro <- sapply(spoergsmaal_i_kombi, function(x) all(x %in% mikro))

# 4) Kontrol: med 6 mikrospørgsmål skal der være 2^6 - 1 = 63 kombinationer
sum(er_mikro)

# 5) Vi beholder kun de rækker i df_r2, der er rene mikrokombinationer
df_mikro <- df_r2[er_mikro, ]

# 6) Vi finder den mikrokombination, der har højest adj. R2
bedste_mikro <- df_mikro[which.max(df_mikro$r2), c("kombination", "r2", "RSS")]
bedste_mikro

# 7) Vi sætter tre indikatorer under hinanden: vores bedste, DI-FTI og bedste mikro
#    (bedste og di er lavet i opgave 1.2)
sammenligning_15 <- rbind(bedste, di, bedste_mikro)
sammenligning_15$indikator <- c("Vores bedste", "DI-FTI", "Bedste mikro")

# 8) Vi henter den mikroindikator, der hører til den bedste mikrokombination
#    Kolonnerne i samlet_indikator_forbrug hedder det samme som kombinationerne
x_mikro <- samlet_indikator_forbrug[[bedste_mikro$kombination]]

# 9) Vi regner korrelationen med forbrugsvæksten for hver af de tre indikatorer
#    Rækkefølgen skal passe til rækkerne i tabellen
sammenligning_15$korrelation <- c(cor(x_vores, y), cor(x_di, y), cor(x_mikro, y))

# 10) Vi sætter kolonnerne i en læsevenlig rækkefølge
sammenligning_15 <- sammenligning_15[, c("indikator", "kombination", "r2", "RSS", "korrelation")]
sammenligning_15

# 11) Kontrol: med én regressor er R2 lig korrelationen i anden
#     Tallene skal ligge lidt over adj. R2 i tabellen
sammenligning_15$korrelation^2

# 1) Antal spørgsmål i hver kombination (hvis df_r2$antal ikke allerede findes)
df_r2$antal <- sapply(strsplit(df_r2$kombination, ".", fixed = TRUE), length)

# 2) Vi beholder de rene mikrokombinationer igen
df_mikro <- df_r2[er_mikro, ]

# 3) Vi finder den bedste mikroindikator for hvert antal spørgsmål (1 til 6)
bedste_mikro_pr_antal <- do.call(rbind, lapply(1:6, function(k) {
  d <- df_mikro[df_mikro$antal == k, ]
  d[which.max(d$r2), c("kombination", "antal", "r2", "RSS")]
}))
bedste_mikro_pr_antal

# Vi henter R2 fra df_r2 ved at matche på kombinationsnavnet
sammenligning_15$r2 <- df_r2$r2[match(sammenligning_15$kombination, df_r2$kombination)]

# Vi sætter kolonnerne i en læsevenlig rækkefølge, med R2 før adj. R2
sammenligning_15 <- sammenligning_15[, c("indikator", "kombination", "r2", "r2", "RSS", "korrelation")]
sammenligning_15

# Kontrol: R2 skal være lig korrelationen i anden (samme tal som før)
all.equal(sammenligning_15$r2, sammenligning_15$korrelation^2)


#done 1.5
