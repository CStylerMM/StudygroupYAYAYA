#Opgave 3 - kan vi forudsige om julehandlen i 2026 er større end i 2025?
#Laver det så det er 1) julehandel pr. år  2) forbrugertillid  3) model  4) validering  5) gæt for 2026


#1) JULEHANDEL PR. ÅR

library(dkstat)   #tid til at bruge dkstat

forbrug_meta <- dst_meta(table = "NKHC021", lang = "da")

forbrug_query <- list(
  FORMAAAL  = "*",
  PRISENHED = "2020-priser, kædede værdier",   #kædede værdier så inflation ikke gør alle år til OP
  SÆSON     = "Ikke sæsonkorrigeret",
  Tid       = "*"
)

forbrug_faste <- dst_get_data(
  table     = "NKHC021",
  query     = forbrug_query,
  meta_data = forbrug_meta,
  lang      = "da"
)

#fra lang til bred, så hver forbrugsgruppe får sin egen kolonne
df_forbrug_bred <- data.frame(TID = forbrug_faste$TID[forbrug_faste$FORMAAAL == forbrug_faste$FORMAAAL[1]])
for (f in unique(forbrug_faste$FORMAAAL)) {
  df_forbrug_bred[[f]] <- forbrug_faste$value[forbrug_faste$FORMAAAL == f]
}

df_forbrug_bred <- df_forbrug_bred[df_forbrug_bred$TID >= as.Date("1999-01-01"), ]   #så passer det med de andre data

#kun K4 (oktober)
df_K4 <- df_forbrug_bred[format(df_forbrug_bred$TID, "%m") == "10", ]

#vores 8 julegrupper: A B C F J K M O 
julegoder <- names(df_forbrug_bred)[substr(names(df_forbrug_bred), 1, 3) %in% c("CPA", "CPB", "CPC", "CPF", "CPJ", "CPK", "CPM", "CPO")]
julegoder 

#læg julegrupperne sammen til ét tal pr. år
df_K4$julehandel <- rowSums(df_K4[, julegoder])

#vækst i julehandlen, K4 mod K4 året før (i procent)
df_K4$vækst <- c(NA, diff(log(df_K4$julehandel)) * 100)   #første år har ingen forgænger, så NA

#OP hvis julehandlen er større end året før, ellers NED
df_K4$retning <- ifelse(df_K4$vækst >= 0, "OP", "NED")
table(df_K4$retning)  


#2) Med FTI

forv_meta <- dst_meta(table = "FORV1", lang = "da")

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

forv_F1 <- forv_F1[forv_F1$TID >= as.Date("2000-01-01"), ]   #før 2000 er der NA

#år og kvartal på hver måned
forv_F1$År <- as.numeric(format(forv_F1$TID, "%Y"))
forv_F1$kvartal <- (as.numeric(format(forv_F1$TID, "%m")) - 1) %/% 3 + 1

#gennemsnit af de tre måneder i hvert kvartal
df_forv_kvartal <- aggregate(value ~ År + kvartal, data = forv_F1, FUN = mean)

#kun K3 (juli til september), den kender vi før julen
df_forv_K3 <- df_forv_kvartal[df_forv_kvartal$kvartal == 3, ]

#tilliden for 2026 har vi, men der er ingen julehandel at sætte den sammen med endnu
sum(forv_F1$År == 2026 & forv_F1$kvartal == 3)   #skal give 3 måneder
tillid_ny <- df_forv_K3$value[df_forv_K3$År == 2026]


#3) MODELLEN

df_K4$År <- as.numeric(format(df_K4$TID, "%Y"))

df_model <- merge(df_K4[, c("År", "julehandel", "vækst", "retning")],
                  df_forv_K3[, c("År", "value")],
                  by = "År")
names(df_model)[names(df_model) == "value"] <- "tillid"

df_model <- df_model[df_model$År >= 2001, ]   #2000 har ingen vækst, så den falder væk
df_model

#logistisk regression: retning (OP/NED) forklaret af forbrugertilliden i K3
glm_julehandel <- glm(as.factor(retning) ~ tillid, data = df_model, family = "binomial")
summary(glm_julehandel)
#Call:
#  glm(formula = as.factor(retning) ~ tillid, family = "binomial", 
#      data = df_model)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)  
#(Intercept)  1.36185    0.56579   2.407   0.0161 *
#  tillid       0.13639    0.06602   2.066   0.0389 *
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 27.554  on 24  degrees of freedom
#Residual deviance: 21.430  on 23  degrees of freedom
#AIC: 25.43

#Number of Fisher Scoring iterations: 4

## tillid_ændring blev prøvet og droppet: p = 0,28 og AIC 27,4 mod 16,9 for tillid


#4) VALIDERING (opgave 3.2)

#vi tager ét år ud ad gangen, træner på resten, og lader modellen gætte det år vi tog ud
df_model$p_uden_året <- NA   #her gemmer vi sandsynligheden for OP
for (i in 1:nrow(df_model)) {
  glm_uden_i <- glm(as.factor(retning) ~ tillid, data = df_model[-i, ], family = "binomial")   #trænet uden år i
  df_model$p_uden_året[i] <- predict(glm_uden_i, newdata = df_model[i, ], type = "response")   #gæt på år i
}

#grænsen er 0,5: over det gætter vi OP
forudsagt <- ifelse(df_model$p_uden_året > 0.5, "OP", "NED")

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
## slår modellen ikke baseline, har forbrugertilliden ikke tilføjet noget


#5) GÆTTET FOR 2026

df_ny_år <- data.frame(tillid = tillid_ny)
p_ny_år <- predict(glm_julehandel, newdata = df_ny_år, type = "response")
p_ny_år   #sandsynlighed for at julehandlen i 2026 er større end i 2025
ifelse(p_ny_år > 0.5, "OP", "NED")
## K4 2026 udkommer først i 2027, så gættet kan ikke tjekkes endnu
