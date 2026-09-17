### --- Færdig øvelse til d. 14/9 --- ###

library(ggplot2)

# Import data, excel

#Rense og organiserer
# Forbrugertillid
Forbrugertillidsindikatoren <- Forbrugertillidsindikatoren_måneder[ ,-(1:121)]
View(Forbrugertillidsindikatoren)

# Laves til kvartaller
Forbrugertillid_ts <- ts(t(Forbrugertillidsindikatoren), start = c(2000, 1), frequency = 12)
Forbrugertillid_kvt_ts <- aggregate(Forbrugertillid_ts, nfrequency = 4)/3

Forbrugertillid_kvt<- data.frame(round(as.numeric(Forbrugertillid_kvt_ts), 1))
Forbrugertillid_kvt$Tidsinterval <- paste0(floor(time(Forbrugertillid_kvt_ts)), "K", cycle(Forbrugertillid_kvt_ts))
names(Forbrugertillid_kvt)[1] <- "Forbrugertillid"

# Privatforbrug realvækst i procent
Privatforbrug_realvæækst_p <- Husholdningernes_FU_realvækst_p[ , -(1:43)]

Privatforbrug_RV_lang <- pivot_longer(Privatforbrug_realvæækst_p,
                                      cols = everything(),
                                      names_to = "Tidsinterval",
                                      values_to = "Privatforbrug")

# Samle begge datasæt i en dataframe
Forbrugertillid_Forbrug_samlet_kvt <- merge(Forbrugertillid_kvt, Privatforbrug_RV_lang, by = "Tidsinterval")

### --- Samlet data --- ###
# Korrelation
cor.forbrug.ftillid <- cor(Forbrugertillid_Forbrug_samlet_kvt$Privatforbrug, Forbrugertillid_Forbrug_samlet_kvt$Forbrugertillid)
cor.forbrug.ftillid
round((cor.forbrug.ftillid), 2) # korrelation på 0.24

# Linær regression
lm.forbrug.ftillid <- lm(Privatforbrug ~ Forbrugertillid, data = Forbrugertillid_Forbrug_samlet_kvt)
summary(lm.forbrug.ftillid)

# Plot
skalafaktor <- max(abs(Forbrugertillid_Forbrug_samlet_kvt$Forbrugertillid), na.rm = TRUE) / 
  max(abs(Forbrugertillid_Forbrug_samlet_kvt$Privatforbrug), na.rm = TRUE)

ggplot(Forbrugertillid_Forbrug_samlet_kvt, aes(x = Tidsinterval)) +
  geom_col(aes(y = Privatforbrug * skalafaktor, fill = "Forbrug")) +
  geom_line(aes(y = Forbrugertillid, group = 1, colour = "Forbrugertillid")) +
  scale_fill_manual(name = NULL, values = c("Forbrug" = "pink")) +
  scale_colour_manual(name = NULL, values = c("Forbrugertillid" = "hotpink4")) +
  scale_y_continuous(
    name = "Forbrugertillid",
    sec.axis = sec_axis(~ . / skalafaktor, name = "Realvækst")
  ) +
  scale_x_discrete(breaks = Forbrugertillid_Forbrug_samlet_kvt$Tidsinterval[seq(1, nrow(Forbrugertillid_Forbrug_samlet_kvt), by = 4)]) +
  labs(title = "Sammenhæng mellem Forbrugertillid & Privatforbrug",
       subtitle = "Korrelation på 0.24",
       caption = "Kilde: Danmarks Statistik",
       x = NULL) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1),
    legend.position = "top",
    legend.justification = "left"
  )

### --- Valdiering --- ###
# Lave subset for validering
Forbrugertillid_Forbrug_samlet_kvt_SUB <- Forbrugertillid_Forbrug_samlet_kvt[-(69:106), ]

# Korrelation
cor.forbrug.ftillid_validering <- cor(Forbrugertillid_Forbrug_samlet_kvt_SUB$Privatforbrug, Forbrugertillid_Forbrug_samlet_kvt_SUB$Forbrugertillid)
cor.forbrug.ftillid_validering
round((cor.forbrug.ftillid_validering), 2) # korrelation på 0.4

#Linær regression
lm.forbrug.ftillid_validering <- lm(Privatforbrug ~ Forbrugertillid, data = Forbrugertillid_Forbrug_samlet_kvt_SUB)
summary(lm.forbrug.ftillid_validering)

# Plot
skalafaktor_v <- max(abs(Forbrugertillid_Forbrug_samlet_kvt_SUB$Forbrugertillid), na.rm = TRUE) / 
  max(abs(Forbrugertillid_Forbrug_samlet_kvt_SUB$Privatforbrug), na.rm = TRUE)skalafaktor_v

ggplot(Forbrugertillid_Forbrug_samlet_kvt_SUB, aes(x = Tidsinterval)) +
  geom_col(aes(y = Privatforbrug * skalafaktor, fill = "Forbrug")) +
  geom_line(aes(y = Forbrugertillid, group = 1, colour = "Forbrugertillid")) +
  scale_fill_manual(name = NULL, values = c("Forbrug" = "pink")) +
  scale_colour_manual(name = NULL, values = c("Forbrugertillid" = "hotpink4")) +
  scale_y_continuous(
    name = "Forbrugertillid",
    sec.axis = sec_axis(~ . / skalafaktor, name = "Realvækst")
  ) +
  scale_x_discrete(breaks = Forbrugertillid_Forbrug_samlet_kvt_SUB$Tidsinterval[seq(1, nrow(Forbrugertillid_Forbrug_samlet_kvt_SUB), by = 4)]) +
  labs(title = "Sammenhæng mellem Forbrugertillid & Privatforbrug",
       subtitle = "Korrelation på 0.4",
       caption = "Kilde: Danmarks Statistik",
       x = NULL) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1),
    legend.position = "top",
    legend.justification = "left"
  )


### --- ANDET --- ###

# Plottet forklaret

skalafaktor <- max(abs(Forbrugertillid_Forbrug_samlet_kvt$Forbrugertillid), na.rm = TRUE) / 
  max(abs(Forbrugertillid_Forbrug_samlet_kvt$Privatforbrug), na.rm = TRUE)
# Skalafaktor: gør de to variables størrelser sammenlignelige visuelt,
# så søjler og linje kan vises på samme graf uden at den ene "forsvinder"

ggplot(Forbrugertillid_Forbrug_samlet_kvt, aes(x = Tidsinterval)) +
  
  # Søjler: privatforbrug (opskaleret med skalafaktor, så den passer visuelt til venstre akse)
  geom_col(aes(y = Privatforbrug * skalafaktor, fill = "Forbrug")) +
  
  # Linje: forbrugertillid (uskaleret, læses direkte på venstre akse)
  geom_line(aes(y = Forbrugertillid, group = 1, colour = "Forbrugertillid")) +
  
  # Bestemmer de faktiske farver bag "Forbrug" og "Forbrugertillid" i legenden
  scale_fill_manual(name = NULL, values = c("Forbrug" = "pink")) +
  scale_colour_manual(name = NULL, values = c("Forbrugertillid" = "hotpink4")) +
  
  # To y-akser: venstre viser Forbrugertillid, højre viser Realvækst (de rigtige, ikke-skalerede tal)
  scale_y_continuous(
    name = "Forbrugertillid",
    sec.axis = sec_axis(~ . / skalafaktor, name = "Realvækst")
  ) +
  
  # Viser kun hvert 4. tidsinterval på x-aksen (svarer til ét label om året, da data er kvartalsvis)
  scale_x_discrete(breaks = Forbrugertillid_Forbrug_samlet_kvt$Tidsinterval[seq(1, nrow(Forbrugertillid_Forbrug_samlet_kvt), by = 4)]) +
  
  # Titel, undertitel, kildeangivelse og x-akse-navn (NULL fjerner "Tidsinterval"-teksten)
  labs(title = "Sammenhæng mellem Forbrugertillid & Privatforbrug",
       subtitle = "Korrelation på 0.24",
       caption = "Kilde: Danmarks Statistik",
       x = NULL) +
  
  # Grundtema: ren, hvid baggrund uden tung gitterlinje-styling
  theme_minimal() +
  
  # Finjusteringer: vinkler x-akse-teksten, og flytter legenden op til toppen (venstrejusteret)
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1),
    legend.position = "top",
    legend.justification = "left"
  )


# Linær regression graf
ggplot(Forbrugertillid_Forbrug_samlet_kvt, aes(x = Forbrugertillid, y = Privatforbrug)) +
  geom_point(colour = "hotpink4", alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", colour = "pink", fill = "pink1", alpha = 0.3) +
  labs(
    title = "Sammenhæng mellem Forbrugertillid & Privatforbrug",
    subtitle = "Lineær regression, korrelation på 0.24",
    x = "Forbrugertillid (Nettotal)",
    y = "Privatforbrug (Realvækst, pct.)",
    caption = "Kilde: Danmarks Statistik"
  ) +
  theme_minimal()


