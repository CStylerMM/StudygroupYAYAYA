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

df_kvartal_clean <- samlet_indikator_df_combi[-(107:108), ]

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

View(lm_indikator_liste)
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
# Vende fortegn om, i udvalgte spørgsmål f6, f8, for at få en fælles retning
# for hvad der er godt og hvad der er skidt

df_kvartal$`F6 Priser i dag, sammenlignet med for et år siden` <- df_kvartal$`F6 Priser i dag, sammenlignet med for et år siden` * -1
df_kvartal$`F8 Arbejdsløsheden om et år, sammenlignet med i dag` <- df_kvartal$`F8 Arbejdsløsheden om et år, sammenlignet med i dag` * -1

# Tjekke antagelse af korrelation er rigtigt
val_cor_tillid <- df_kvartal[-(107:108), ]

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


### --- Finde vinderen --- ###
# Trække gennemsnit ud i dem alle sammen
# Lave et samlet matrix for gennemsnit for alle loops
samlet_stabilisering_matrix <- matrix(0, 5, 2047)
colnames(samlet_stabilisering_matrix) <- colnames(samlet_indikator_forbrug[ ,3:ncol(samlet_indikator_forbrug)])

# Loop 1
for (x in 1:1) {
  r2_value_loop1_matrix <- matrix(0, 24, 2047)
  colnames(r2_value_loop1_matrix) <- colnames(samlet_indikator_forbrug[ ,3:ncol(samlet_indikator_forbrug)])
  for (h in 1:24) {
    for (i in 1:2047) {
      r2_value_loop1 <- loop1_lm_liste_samlet[[h]][[i]]$adj.r.squared
      r2_value_loop1_matrix[h, i] <- r2_value_loop1
    }
  }
  loop1_gns <- colMeans(r2_value_loop1_matrix)
  samlet_stabilisering_matrix[1, ] <- loop1_gns
}

View(r2_value_loop1_matrix)
View(samlet_stabilisering_matrix)

# Loop 2
for (x in 1:1) {
  r2_value_loop2_matrix <- matrix(0, 24, 2047)
  colnames(r2_value_loop2_matrix) <- colnames(samlet_indikator_forbrug[ ,3:ncol(samlet_indikator_forbrug)])
  for (h in 1:24) {
    for (i in 1:2047) {
      r2_value_loop2 <- loop2_lm_liste_samlet[[h]][[i]]$adj.r.squared
      r2_value_loop2_matrix[h, i] <- r2_value_loop2
    }
  }
  loop1_gns <- colMeans(r2_value_loop2_matrix)
  samlet_stabilisering_matrix[2, ] <- loop1_gns
}

View(r2_value_loop1_matrix)
View(samlet_stabilisering_matrix)

# Loop 3
for (x in 1:1) {
  r2_value_loop3_matrix <- matrix(0, 24, 2047)
  colnames(r2_value_loop3_matrix) <- colnames(samlet_indikator_forbrug[ ,3:ncol(samlet_indikator_forbrug)])
  for (h in 1:24) {
    for (i in 1:2047) {
      r2_value_loop3 <- loop3_lm_liste_samlet[[h]][[i]]$adj.r.squared
      r2_value_loop3_matrix[h, i] <- r2_value_loop3
    }
  }
  loop3_gns <- colMeans(r2_value_loop3_matrix)
  samlet_stabilisering_matrix[3, ] <- loop3_gns
}

View(r2_value_loop3_matrix)
View(samlet_stabilisering_matrix)

# Loop 4
for (x in 1:1) {
  r2_value_loop4_matrix <- matrix(0, 12, 2047)
  colnames(r2_value_loop4_matrix) <- colnames(samlet_indikator_forbrug[ ,3:ncol(samlet_indikator_forbrug)])
  for (h in 1:12) {
    for (i in 1:2047) {
      r2_value_loop4 <- loop4_lm_liste_samlet[[h]][[i]]$adj.r.squared
      r2_value_loop4_matrix[h, i] <- r2_value_loop4
    }
  }
  loop4_gns <- colMeans(r2_value_loop4_matrix)
  samlet_stabilisering_matrix[4, ] <- loop4_gns
}

View(r2_value_loop4_matrix)
View(samlet_stabilisering_matrix)

### --- Finde det samlede gennemsnit for alle indikatore --- ###
samlet_gns_alle_indikatore <- colMeans(samlet_stabilisering_matrix[1:4, ]) # NO NO at køre igen, kun 1 gang
samlet_stabilisering_matrix[5, ] <- samlet_gns_alle_indikatore
min(samlet_stabilisering_matrix[5, ])
max(samlet_stabilisering_matrix[5, ])

# Lave et samlet dataframe på det samlede gennemsnit
indikator_stabilitet_vinder <- data.frame(samlet_stabilisering_matrix[5, ])
min(indikator_stabilitet_vinder)
max(indikator_stabilitet_vinder)




#Forsøge at få gennemsnit på hver loop


#Gennemsnit på loop1
samlet_gns_loop1 <- colMeans(r2_value_loop1_matrix)
t_indikator_stabilitet_loop1 <- data.frame(colMeans(r2_value_loop1_matrix))

#Gennemsnit på Loop2
samlet_gns_loop2 <- colMeans(r2_value_loop2_matrix)
t_indikator_stabilitet_loop2 <- data.frame(colMeans(r2_value_loop2_matrix))

#Gennemsnit på loop3
samlet_gns_loop3 <- colMeans(r2_value_loop3_matrix)
t_indikator_stabilitet_loop3 <- data.frame(colMeans(r2_value_loop3_matrix))

#Gennemsnit på loop 4
samlet_gns_loop4 <- colMeans(r2_value_loop4_matrix)
t_indikator_stabilitet_loop4 <- data.frame(colMeans(r2_value_loop4_matrix))
