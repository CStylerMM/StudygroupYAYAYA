### --- Øvelse 9/11 --- ###

# Importere data

# Organisere importeret data

# Privat forbrug

privatforbrug_tal <- privtforbrug_2015_2026[ ,-(1:3)]

PrivatForbrug <- pivot_longer(privatforbrug_tal,
  cols = everything(),
  names_to = "Tidsinterval",
  values_to = "Privatforbrug"
)

# Omdanne forbrug til procent
vækst_procent_forbrug <- (exp(diff(log(as.numeric(PrivatForbrug$Privatforbrug)), lag = 4)) - 1) * 100

length(vækst_procent_forbrug)
nrow(vækst_procent_forbrug)


# Omdanne Forbrugertillid til kvartaler

Forbrugertillid_2015_2026_TS <- Forbrugertillid_2015_2026[ , -1]

ForbrugerTillid_ts <- ts(t(Forbrugertillid_2015_2026_TS), start = c(2015, 1), frequency = 12)
ForbrugerTillid_kvt_ts <- aggregate(ForbrugerTillid_ts, nfrequency = 4)/3

ForbrugerTillid_kvt<- data.frame(round(as.numeric(ForbrugerTillid_kvt_ts), 1))
ForbrugerTillid_kvt$Tidsinterval <- paste0(floor(time(ForbrugerTillid_kvt_ts)), "K", cycle(ForbrugerTillid_kvt_ts))
names(ForbrugerTillid_kvt)[1] <- "Forbrugertillid"





# Samle begge datasæt i en dataframe
kvt_samlet <- merge(ForbrugerTillid_kvt, PrivatForbrug, by = "Tidsinterval")

# Korrelation
cor.forbrug.ftillid <- cor(PrivatForbrug$Privatforbrug, ForbrugerTillid_kvt$Forbrugertillid)
cor.forbrug.ftillid

# Linær regression
lm.forbrug.ftillid <- lm(Privatforbrug ~ Forbrugertillid, data = kvt_samlet)
summary(lm.forbrug.ftillid)

library(ggplot2)
ggplot(kvt_samlet, aes(Forbrugertillid, Privatforbrug)) +
  geom_point() +
  geom_smooth(method = "lm")






### --- ROD ---###

ForbrugerTillid <- pivot_longer(
  Forbrugertillid_2015_2026,
  cols = -1,
  names_to = "Tidsinterval",
  values_to = "Forbrugertillid"
)

ForbrugerTillid_måneder <- ForbrugerTillid[ , -1]

ForbrugerTillid <- ForbrugerTillid[-(139:140), ]






# Importerer data
# Rense importeret data
# Pivot longer?
library(tidyr)
?pivot_longer
ForbrugertillidLang <- pivot_longer(Forbrugertillid_2015_2026,
rm(tid)
#Privatforbrug er good
Privatforbrug <- data.frame(y[-(1:3), ])
#Privatforbrug er good
Forbrugertillid <- data.frame(x[-1,])
tid <- ts(start = c(2015,1), end = c(2026,8), frequency = 12, class = c("mts"))
Forbrugertillid2 <- ts(t())
tid2 <- 
chat
library(tidyr)

Forbrugertillid_long <- pivot_longer(
  Forbrugertillid_2015_2026,
  cols = -1,
  names_to = "Tidsinterval",
  values_to = "Forbrugertillid"
)
ForbrugertillidLang <- Forbrugertillid_long[ , -1]
rm(x2)
ForbrugertillidMåneder <- ts(t(Forbrugertillid), start = c(2015,1),frequency = 12)
ForbrugertillidKvt <- ts(t(Forbrugertillid), start = c(2015,1),frequency = 4)
ForbrugertillidKvtLang <- t(ForbrugertillidKvt)
t <- ts(start = c(2015, 1), end = c(2026, 8), frequency = 12)
tid <- paste0(floor(time(t)), "M", cycle(t))
labels
colnames(Forbrugertillid) <- Forbrugertillid["forbrugertillid", ]
tid <- ts(start = 2015, end = 2027, frequency = 12)
tid
rm(tid2)
x2  <- data.frame(x[-1,])
tid2
x <- t(Forbrugertillid_2015_2026)
y <- t(privtforbrug_2015_2026)
 rownames(x2) <- x2[,tid]
colnames(x) <-  x[1,]
length(Forbrugertillid)
Forbrugertillid <- data.frame(x[-1,])
?ts
