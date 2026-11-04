# # øvelser
library(tidyr)
df_fly <- data.frame(
  callsign = c("SAS123", "RYR45", "SAS908", "DLH7", "NAX21", "SAS14"),
  land     = c("Danmark", "Irland", "Danmark", "Tyskland", "Norge", "Sverige"),
  højde    = c(10500, 11200, 3200, 9800, 12000, 800),
  hastighed = c(240, 250, 160, 230, 260, 90)
)

df_selskab <- data.frame(
  kode    = c("SAS", "RYR", "DLH", "NAX"),
  selskab = c("SAS", "Ryanair", "Lufthansa", "Norwegian")
)
# Basis
 
# 1 Hvor mange rækker og kolonner har df_fly? Brug en funktion, så du ikke tæller selv.
dim(df_fly)
#6 og 4

# 2 Find gennemsnittet af højde og den højeste hastighed.
mean(df_fly$højde)
mean(df_fly$hastighed)
# 3 Vis kun kolonnen callsign som en vektor.
callsign <- (df_fly$callsign)
df_fly[,1]
# 4 Vis kun de fly, der flyver højere end 10000.
df_fly$højde>10000
df_fly$mere10000 <- df_fly$højde>10000
df_fly[,5]
str(df_fly)
# 5 Vis de fly, der er fra Danmark, og sortér dem efter højde, højest først.
flydk <- filter(df_fly$land,"Danmark")
unique(df_fly$land)

sort(df_fly$land,"Danmark", decr=F)

# 6 Lav en ny kolonne højde_km, som er højden i kilometer.
df_fly$højde_km <- df_fly$højde
# 7 Lav en kolonne lander, som er TRUE, hvis højde er under 1000, ellers FALSE
# Tekst og join
 
# 8 Lav en kolonne kode med de tre første bogstaver i callsign (hint: substr()).

# 9 Brug str_detect() til at tælle, hvor mange callsigns der starter med SAS.
# 10 Join df_fly med df_selskab på kode, så hvert fly får et selskabsnavn. Tjek bagefter,
# at antallet af rækker er det samme som før.