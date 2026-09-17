### --- Færdig øvelse til d. 14/9 --- ###

# Pakker
library(ggplot2)
library(tidyr)

# Import data, excel
# Importererer "manuelt"

### --- Rense og ordne data --- ###

# Forbrugertillid
FBT_bred_clean <- Forbrugertillid_raw_DST[ , -(1:121)]

# Laves til kvartaller
FBT_ts <- ts(t(FBT_bred_clean), start = c(2000, 1), frequency = 12)
FBT_kvt_ts <- aggregate(FBT_ts, nfrequency = 4)/3

Forbrugertillid_kvt<- data.frame(round(as.numeric(FBT_kvt_ts), 1))
Forbrugertillid_kvt$Tidsinterval <- paste0(floor(time(FBT_kvt_ts)), "K", cycle(FBT_kvt_ts))
names(Forbrugertillid_kvt)[1] <- "Forbrugertillid"

# Privatforbrug
# Realvækst i procent, kontra året forinden
HusPriv_bred_br <- HusPriv_raw[ , -(1:39)]

(diff(log(as.numeric(HusPriv_bred_br)), lag = 4)) * 100
(exp(diff(log(as.numeric(HusPriv_bred_br)), lag = 4)) - 1) * 100

HusPriv_realVækst_p <- (diff(log(as.numeric(HusPriv_bred_br)), lag = 4)) * 100
HusPriv_realVækst_p <- (exp(diff(log(as.numeric(HusPriv_bred_br)), lag = 4)) - 1) * 100
HusPriv_realVækst_p <- data.frame(HusPriv_realVækst_p)

# Lave privatforbrug mia. kr. lang
HusPriv_bred_clean <- HusPriv_raw[ , -(1:43)]

HusPriv_lang <- pivot_longer(HusPriv_bred_clean,
                                      cols = everything(),
                                      names_to = "Tidsinterval",
                                      values_to = "Privatforbrug")

# Merge privatforbrug i kr. og procent
HusPriv_samlet <- cbind(HusPriv_lang, HusPriv_realVækst_p)
names(HusPriv_samlet)[names(HusPriv_samlet) == "HusPriv_realVækst_p"] <- "Realvækst"
HusPriv_samlet$Realvækst <- round(HusPriv_samlet$Realvækst, 2)


# Lave et helt samlet datasæt
samlet_df <- merge(Forbrugertillid_kvt, HusPriv_samlet, by = "Tidsinterval")

### --- Analyse på samlet data --- ###

# Korrelation
cor.forbrug.tillid <- cor(samlet_df$Realvækst, samlet_df$Forbrugertillid)
cor.forbrug.tillid
Korrelation_2000_2026 <- round((cor.forbrug.tillid), 2)
Korrelation_2000_2026 # Korrelation på 0.52

# Linær regression
lm.forbrug.tillid <- lm(Realvækst ~ Forbrugertillid, data = samlet_df)
summary(lm.forbrug.tillid)

# Plot
skalafaktor <- max(abs(samlet_df$Forbrugertillid), na.rm = TRUE) /
  max(abs(samlet_df$Realvækst), na.rm = TRUE)

ggplot(samlet_df, aes(x = Tidsinterval)) +
  geom_col(aes(y = Realvækst * skalafaktor, fill = "Realvækst")) +
  geom_line(aes(y = Forbrugertillid, group = 1, color = "Forbrugertillid")) +
  scale_fill_manual(name = NULL, values = c("Realvækst" = "pink")) +
  scale_colour_manual(name = NULL, values = c("Forbrugertillid" = "hotpink4")) +
  scale_y_continuous(name = "Forbrugertillid",
                     sec.axis = sec_axis(~ . / skalafaktor, name = "Realvækst")) +
  scale_x_discrete(breaks = samlet_df$Tidsinterval[seq(1, nrow(samlet_df), by = 4)]) +
  labs(title = "Sammenhæng mellem Forbrugertillid & Privatforbrug",
       subtitle = "Korrelation på 0.52",
       caption = "Kilde: Danmarks Statistik",
       x = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        legend.position = "top",
        legend.justification = "left")

### --- Validering --- ###
# Subset med valideringsdata
samlet_df_val <- samlet_df[-(69:106), ]

# Korrelation
cor.forbrug.tillid_val <- cor(samlet_df_val$Realvækst, samlet_df_val$Forbrugertillid)
cor.forbrug.tillid_val
Korrelation_2000_2016 <- round((cor.forbrug.tillid_val), 2)
Korrelation_2000_2016 # Korrelation på 0.56

# Linær regression
lm.forbrug.tillid_val <- lm(Realvækst ~ Forbrugertillid, data = samlet_df_val)
summary(lm.forbrug.tillid_val)

# Plot
skalafaktor_val <- max(abs(samlet_df_val$Forbrugertillid), na.rm = TRUE) /
  max(abs(samlet_df_val$Realvækst), na.rm = TRUE)

ggplot(samlet_df_val, aes(x = Tidsinterval)) +
  geom_col(aes(y = Realvækst * skalafaktor, fill = "Realvækst")) +
  geom_line(aes(y = Forbrugertillid, group = 1, color = "Forbrugertillid")) +
  scale_fill_manual(name = NULL, values = c("Realvækst" = "pink")) +
  scale_colour_manual(name = NULL, values = c("Forbrugertillid" = "hotpink4")) +
  scale_y_continuous(name = "Forbrugertillid",
                     sec.axis = sec_axis(~ . / skalafaktor, name = "Realvækst")) +
  scale_x_discrete(breaks = samlet_df_val$Tidsinterval[seq(1, nrow(samlet_df_val), by = 4)]) +
  labs(title = "Sammenhæng mellem Forbrugertillid & Privatforbrug",
       subtitle = "Korrelation på 0.56",
       caption = "Kilde: Danmarks Statistik",
       x = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        legend.position = "top",
        legend.justification = "left")



