### --- Opgave 4.1 --- ###

#Kolonne 1: Klasser
rep(LETTERS[1:4], each = 9)
Klasser <- rep(LETTERS[1:4], each = 9)
#Kolonne 2: Uge
rep(seq(1, 9), times = 4)
Uger <- rep(seq(1, 9), times = 4)
# Kolonne 3: Score
sample(c(00,02,4,7,10,12), size=36, replace = TRUE)
Score <- sample(c(00,02,4,7,10,12), size=36, replace = TRUE)

#Lave en data frame med 3 kolonner, og indsætte vores værdier
df <- data.frame("Klasser" = Klasser, "Uge" = Uger, "Score" = Score)


### --- Opgave 4.2 --- ###

# Oprette ny tom dataframe, til nye værdier
kvt_df <- data.frame(Klasser = character(), Uge = numeric(), Score = numeric())

# Lave for Loop til data frame
for (i in 1:nrow(df)) {
  if (i %% 3 == 0) {
    gns <- round(mean(df$Score[(i-2):i]))
    ny_raekke <- data.frame(Klasser=df$Klasser[i], Uge=df$Uge[i], Score=gns)
    kvt_df <- rbind(kvt_df, ny_raekke)
  }
}

library(tidyr)

rm(kvt_df)

# i for(), indsættes "i" som symbol for funktionen
# derefter indsættes området i datasættes som loopet skal kører på (1:nrow - betyder række 1 til den sidste)
# efter det kommer if funktionen, hvor loopet defineres
# her er hvis "i" divideret med 3 giver exact 0, går den igang med loop
# det defineres med modulo-operatoren (%%), som finder en rest efter divition
# efter definationen op hvornår loopet skal gå igang, kan man definerer hvad loopet skal gøre
# HUSK at give funktionen et "slut resultat" så den printer noget af en eller anden art (kvt_df)


### --- Opgave 4.3 --- ###

#Aktiverer pakken tidyr
library(tidyr)

# Lave en ny dataframe
kvt_wide <- pivot_wider(kvt_df, names_from = Klasser, values_from = Score)

# Bruger pivot_wider til at lave et nyt "sorteret" dataframe.
# Den tager mange rækker og få kolonner, og laver det til få rækker og mange kolonner
# Først vælger man datasættet (kvf_df), derefter vælger man hvor names på nye kolonner skal komme fra (Klasser), og til sidst tager den values/indholdet i rækkerne (Score)
# HVIS man undlader at omtale enkelte konlonner, tages de automatisk med videre som en ny kolonne, derfor er uger stadig med i den nye dataframe


