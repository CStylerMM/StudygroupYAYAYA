## ============================================================
## Teen moms i Tyskland (Eurostat: demo_r_fagec)
## Kør scriptet oppefra og ned i ÉN omgang

#Okay opgave 1 her. Vi starter med at kigge i metadata (DSD) for at kunne finde ud af hvilke aldersgrupper og regioner tabellen har
#tid til at bruge restatapi
library(restatapi)

#henter "opskriften" på tabellen, ikke selve dataene endnu
dsd <- get_eurostat_dsd("demo_r_fagec")

#gal et overblik: dsd har tre kolonner (concept, code, name)
head(dsd)

#hvilke aldersgrupper findes der? Vi leder efter Y10-14 og Y15-19
alder <- dsd[dsd$concept == "age", ]
alder

#nu til geo: vi vil kun have Tyskland, så vi tager koder der starter med DE
#substr(x, 1, 2) tager de to første tegn i koden
tyskland <- dsd[dsd$concept == "geo" & substr(dsd$code, 1, 2) == "DE", ]
nrow(tyskland)
#55 stk

#nchar() tæller tegn: 2 tegn = hele Tyskland, 3 tegn = Länder (NUTS 1), 4 tegn = NUTS 2
table(nchar(tyskland$code))
# 2  3  4
# 1 16 38

#3 tegn er Länder, så de 16 koder plukker vi ud som filter
nuts1 <- tyskland$code[nchar(tyskland$code) == 3]
length(nuts1)

#tid til at hente selve dataene. Vi henter ALLE aldersgrupper for de 16 Länder, så filtrerer vi i R bagefter
dt <- get_eurostat_data("demo_r_fagec",
                        filters = list(geo = nuts1),
                        label = FALSE,
                        name = FALSE)

#gal et overblik: hvilke kolonner har vi?
str(dt)

#time er en faktor, så vi laver den til tal via character (ALDRIG direkte as.numeric på en faktor, så får vi 1,2,3 i stedet for årstal)
dt$time <- as.numeric(as.character(dt$time))

#kom alle 16 Länder med? (underviserens plot havde kun 9)
length(unique(dt$geo))
#16 juuuuuuup

#hvilke aldersgrupper kom med? Y15-19 findes ikke som egen kode, kun som enkeltaldre (Y15, Y16 osv.)
unique(dt$age)

#nu filtrerer vi i R: kun de unge mødre (Y10-14 og enkeltaldrene 15 til 19)
#Y_LT16 lader vi ligge, den overlapper med de andre og ville dobbelttælle
unge <- dt[dt$age %in% c("Y10-14", "Y15", "Y16", "Y17", "Y18", "Y19"), ]

#NA-tjek FØR vi lægger noget sammen (sum() med NA giver NA)
sum(is.na(unge$values))
#nul luksus, MEN hele rækker kan mangle uden at være NA, det tjekker vi lige om lidt

#tid til aggregate(): Y15 til Y19 lægges sammen til én gruppe, Y15-19
ung_15_19 <- unge[unge$age %in% c("Y15", "Y16", "Y17", "Y18", "Y19"), ]
y15_19 <- aggregate(values ~ geo + time, data = ung_15_19, FUN = sum)
y15_19$age <- "Y15-19"

#vigtigt tjek: hvor mange enkeltaldre lagde aggregate sammen pr. Land og år? Det skal være 5
#(Y15, Y16, Y17, Y18, Y19). Hvis ikke, er summen for lav uden at R siger et ord
antal <- aggregate(values ~ geo + time, data = ung_15_19, FUN = length)
table(antal$values)
#256 har 5, men 64 har færre. Hvilke år er det?
table(antal$time[antal$values < 5])
#2021-2024 er ufuldstændige (Y15 og Y16 mangler), og 2018-2020 mangler helt

#Y10-14 skal have samme kolonner, så vi kan sætte dem oven på hinanden
y10_14 <- unge[unge$age == "Y10-14", c("geo", "time", "values")]
y10_14$age <- "Y10-14"

#rbind() stabler de to tabeller
teen <- rbind(y10_14, y15_19)

#vi holder os til 2002-2017, hvor alle Länder har alle aldre
teen <- teen[teen$time <= 2017, ]

#tjek: begge aldersgrupper skal have 256 rækker (16 Länder x 16 år)
table(teen$age)

#opgave 4: navnene på NUTS-koderne ligger allerede i dsd (kolonnen "name"), så vi plukker geo-rækkerne ud
navne <- dsd[dsd$concept == "geo", c("code", "name")]

#geo i teen er en faktor og code i navne er tekst, så vi gør dem ens før vi merger
teen$geo <- as.character(teen$geo)

#tid til merge(): vi sætter navnene på teen via koden. by.x og by.y fordi kolonnerne hedder forskelligt
teen <- merge(teen, navne, by.x = "geo", by.y = "code")

#kolonnen hedder "name", så vi omdøber den til NUTS_label ligesom på underviserens plot
names(teen)[names(teen) == "name"] <- "NUTS_label"

#gal et overblik: skal give 512 rækker (16 Länder x 16 år x 2 aldersgrupper) og 16 navne
nrow(teen)
length(unique(teen$NUTS_label))
sort(unique(teen$NUTS_label))

#Okay, nu har vi 16 Länder med navne, 2002-2017, begge aldersgrupper. Tid til at plotte antal
#tid til at bruge ggplot2
library(ggplot2)

#linjer pr. Land, et panel pr. aldersgruppe. scales = "free_y" fordi Y10-14 er små tal og Y15-19 er tusinder
ggplot(teen, aes(x = time, y = values, color = NUTS_label)) +
  geom_line() +
  facet_wrap(~ age, scales = "free_y") +
  labs(title = "Teen moms i Tyskland, 2002-2017",
       x = "År",
       y = "Antal fødsler",
       color = "Land")

#NRW ligger øverst i antal, men det er også det største Land. Antal er ikke det samme som udbredelse
## ehm, derfor skal vi have andel af alle fødsler

#først nævneren: alle fødsler (TOTAL) pr. Land og år, hentet fra dt
total <- dt[dt$age == "TOTAL", c("geo", "time", "values")]

#geo er en faktor i dt, men en tekst i teen, så vi gør dem ens før merge
total$geo <- as.character(total$geo)

#omdøb values så vi ikke får to kolonner med samme navn
names(total)[names(total) == "values"] <- "fodsler_total"

#hvis scriptet er kørt før, ligger fodsler_total måske allerede i teen. Fjern den, ellers får vi .x og .y
teen$fodsler_total <- NULL

#tid til merge(): alle fødsler sættes på siden af teenagemødrene via Land og år
teen <- merge(teen, total, by = c("geo", "time"))

#gal et overblik: stadig 512 rækker? Ellers tabte merge noget, og så stopper vi her
nrow(teen)

#nu selve andelen i procent
teen$andel <- teen$values / teen$fodsler_total * 100

#kun Y15-19 til modellerne (Y10-14 er for små tal og mest støj)
#VIGTIGT: teen_15_19 laves EFTER andel, ellers arver den ikke kolonnen
teen_15_19 <- teen[teen$age == "Y15-19", ]

#listen over de 16 Länder, som loopet løber igennem (sort() gør dem alfabetiske)
lande <- sort(unique(teen_15_19$NUTS_label))

#tom liste hvor loopet gemmer én model pr. Land
modeller <- list()

#par(mfrow = c(4, 4)) deler plotvinduet i 4 rækker og 4 kolonner, så alle 16 Länder kan være i ét billede
par(mfrow = c(4, 4))

#for-loop: "for hvert Land i listen lande, gør det der står mellem { }"
for (l in lande) {
  
  #kun rækkerne for det Land vi er nået til
  d <- teen_15_19[teen_15_19$NUTS_label == l, ]
  
  #modellen gemmes i listen under Landets navn, så den ikke bliver overskrevet
  modeller[[l]] <- lm(andel ~ time, data = d)
  
  #plot: punkterne (andel over tid). ylim er ens for alle, så Länderne kan sammenlignes ærligt
  plot(d$time, d$andel, main = l, xlab = "År", ylab = "Andel (%)",
       ylim = range(teen_15_19$andel))
  
  #abline tegner den rette linje fra modellen ovenpå punkterne
  abline(modeller[[l]], col = "red")
}

#sæt plotvinduet tilbage til normal, ellers arver alle senere plots 4x4-opsætningen
par(mfrow = c(1, 1))

#gal et overblik: skal give 16
length(modeller)

#tallene fra de 16 modeller samlet i én tabel: sapply løber listen igennem og tager hældning og R2 fra hver model
resultat <- data.frame(land = names(modeller),
                       haeldning = sapply(modeller, function(m) coef(m)[2]),
                       r2 = sapply(modeller, function(m) summary(m)$r.squared),
                       row.names = NULL)

#sorter så det Land der falder mest (mest negative hældning) står øverst
resultat <- resultat[order(resultat$haeldning), ]
resultat$haeldning <- round(resultat$haeldning, 3)
resultat$r2 <- round(resultat$r2, 2)
resultat

## ehm, hældning = procentpoint pr. år. Med 16 datapunkter pr. Land skriver vi "faldende trend", ikke "årsag"
## næste: sammenlign Tyskland og Danmark (demo_fagec eller demo_fordagec)