### --- Multipel Linær regression af forbrugerforventninger --- ###

library(tidyr)

# Import data
ff_spg_raw
HusPriv_raw

# Rense for unødige rækker
ff_spg <- ff_spg_raw[ , -(2:304)]
ff_spg <- ff_spg[ , -(260:321)]
names(ff_spg)[1] <- "Spørgsmål"

# Lave 5 subsets at regne kvartaller på
ff_sub1 <- ff_spg[1,(2:ncol(ff_spg))]
ff_sub2 <- ff_spg[2,(2:ncol(ff_spg))]
ff_sub3 <- ff_spg[3,(2:ncol(ff_spg))]
ff_sub4 <- ff_spg[4,(2:ncol(ff_spg))]
ff_sub5 <- ff_spg[5,(2:ncol(ff_spg))]

# Regne kvartaller
# Sub 1
ff_sub1_kvt <- ts(t(ff_sub1), start = c(2000, 1), frequency = 12)
ff_sub1_kvt_ts <- aggregate(ff_sub1_kvt, nfrequency = 4) / 3

ff_sub1_df <- data.frame(round(as.numeric(ff_sub1_kvt_ts), 1))
ff_sub1_df$Tidsinterval <- paste0(floor(time(ff_sub1_kvt_ts)), "K", cycle(ff_sub1_kvt_ts))
names(ff_sub1_df)[1] <- "Spørgsmål 1"

# Sub 2
ff_sub2_kvt <- ts(t(ff_sub2), start = c(2000, 1), frequency = 12)
ff_sub2_kvt_ts <- aggregate(ff_sub2_kvt, nfrequency = 4) / 3

ff_sub2_df <- data.frame(round(as.numeric(ff_sub2_kvt_ts), 1))
ff_sub2_df$Tidsinterval <- paste0(floor(time(ff_sub2_kvt_ts)), "K", cycle(ff_sub2_kvt_ts))
names(ff_sub2_df)[1] <- "Spørgsmål 2"

#Sub 3
ff_sub3_kvt <- ts(t(ff_sub3), start = c(2000, 1), frequency = 12)
ff_sub3_kvt_ts <- aggregate(ff_sub3_kvt, nfrequency = 4) / 3

ff_sub3_df <- data.frame(round(as.numeric(ff_sub3_kvt_ts), 1))
ff_sub3_df$Tidsinterval <- paste0(floor(time(ff_sub3_kvt_ts)), "K", cycle(ff_sub3_kvt_ts))
names(ff_sub3_df)[1] <- "Spørgsmål 3"

# Sub 4
ff_sub4_kvt <- ts(t(ff_sub4), start = c(2000, 1), frequency = 12)
ff_sub4_kvt_ts <- aggregate(ff_sub4_kvt, nfrequency = 4) / 3

ff_sub4_df <- data.frame(round(as.numeric(ff_sub4_kvt_ts), 1))
ff_sub4_df$Tidsinterval <- paste0(floor(time(ff_sub4_kvt_ts)), "K", cycle(ff_sub4_kvt_ts))
names(ff_sub4_df)[1] <- "Spørgsmål 4"

# Sub 5
ff_sub5_kvt <- ts(t(ff_sub5), start = c(2000, 1), frequency = 12)
ff_sub5_kvt_ts <- aggregate(ff_sub5_kvt, nfrequency = 4) / 3

ff_sub5_df <- data.frame(round(as.numeric(ff_sub5_kvt_ts), 1))
ff_sub5_df$Tidsinterval <- paste0(floor(time(ff_sub5_kvt_ts)), "K", cycle(ff_sub5_kvt_ts))
names(ff_sub5_df)[1] <- "Spørgsmål 5"

# Lave et samlet data frame på kvartallerne 
liste_sub_sets <- list(ff_sub5_df, ff_sub4_df, ff_sub3_df, ff_sub2_df, ff_sub1_df)
samlet_df_kvt <- Reduce(function(x, y) merge(x, y, by = "Tidsinterval"), liste_sub_sets)

# Udregne realvækst i privatforbruget
HusPriv_bred_br <- HusPriv_raw[ , -(1:39)]
HusPriv_bred_br <- HusPriv_bred_br[ , -(91:110)]

(diff(log(as.numeric(HusPriv_bred_br)), lag = 4)) * 100
(exp(diff(log(as.numeric(HusPriv_bred_br)), lag = 4)) - 1) * 100

HusPriv_realVækst_p <- (diff(log(as.numeric(HusPriv_bred_br)), lag = 4)) * 100
HusPriv_realVækst_p <- (exp(diff(log(as.numeric(HusPriv_bred_br)), lag = 4)) - 1) * 100
HusPriv_realVækst_p <- data.frame(round((HusPriv_realVækst_p), 2))

# Lave et helt samlet datasæt
samlet_df <- cbind(samlet_df_kvt, HusPriv_realVækst_p)
names(samlet_df)[7] <- "Realvækst"

names(samlet_df)[2] <- "Spg_V"
names(samlet_df)[3] <- "Spg_IV"
names(samlet_df)[4] <- "Spg_III"
names(samlet_df)[5] <- "Spg_II"
names(samlet_df)[6] <- "Spg_I"


# Lave multipel linær reggression
lm.forbrug.forventninger <- lm(Realvækst ~ Spg_I + 
                                 Spg_II + 
                                 Spg_III + 
                                 Spg_IV + 
                                 Spg_V , 
                               data = samlet_df)

summary(lm.forbrug.forventninger)
coef <- round((summary(lm.forbrug.forventninger)$coef), 3)
coef






