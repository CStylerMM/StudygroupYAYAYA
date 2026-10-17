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
df_forbrug_bred <- df_forbrug_bred[df_forbrug_bred$TID >= as.Date("2000-01-01"), ]

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

