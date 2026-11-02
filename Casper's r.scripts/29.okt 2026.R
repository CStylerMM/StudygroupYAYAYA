#Daniel/Marie dag
library(dplyr)
biler <- mtcars

#opg 1.1
head(biler)

#opg 1.2
str(biler)
structure(biler)
class(biler)
typeof(biler$cyl)


#opg 1.3
dim(biler)
nrow(biler)
ncol(biler)

#opg 1.4
mean(biler$mpg)

#opg 1.5

min(biler$hp)
max(biler$hp)
?grepl

#############################

#2.1 - Udvælg kolonnen mpg
structure(biler)
biler$mpg

#2.2 - Udvælg kolonnerne mpg og hp
biler[,c("mpg", "hp")]
select(biler,mpg,hp)


#2.3 - vis de første fem biler
head(biler[,c("mpg", "hp")])

#2.4 - find de biler med mere end 150 hestekræfter
biler[biler$hp>=150,]

#2.5 - find de biler med mindst 6 cylindre
biler[biler$cyl>=6,]
#bonuss
biler[biler$cyl>=6&biler[biler$hp>=150,]
,]

###############################
#3.1 - find alle biler med mere end 20 mgp
biler[biler$mpg>=20,]

#3.2 -  find alle biler med otte cylindre
biler[biler$cyl==8,]

#3.3 - find alle biler med mindst 150 hestekræfter
biler[biler$hp>=149,]

#3.4 - find alle biler med 5 cylindre opg mere end 20 mpg
biler[biler$cyl==5&biler$mpg>20,]

#3.5 - find biler med enten 4 eller 8 cylindre

biler[(biler$cyl==4|biler$cyl==8),]


#### Ekstra opgave: find biler, der både har mere end 150 hestekræfter og mindst 6 cylindre

biler[(biler$hp>150|biler$cyl>6),]
#filter

###############################
#4.1 - udvælg kun mpg,hp,wt og cyl
select(biler,mpg,hp,wt,cyl)

#4.2 - sortér bilerne efter mpg fra lavest til højest

sort(biler$mpg,decreasing = FALSE)
arrange(biler,mpg)

#4.3 - sortér bilerne efter hp fra højest til lavest
sort(biler$hp,decreasing = TRUE)

arrange(biler, desc(mpg),5)

#4.4 - find de 5 biler med højeste brændstoføkonomi
head(select(sort(biler$mpg,decreasing = TRUE),))
arrange(biler, desc(mpg))

#andre har gjort
biler[order(biler$mpg,decreasing =T),][1:5,]

#4.5 - udvælg alle variabler undtagen disp og carb
biler <- rm(biler$disp)
bileruden2 <- select(biler,!disp&!carb)
select(biler, -disp, -carb)

#udvælge er select brorrrrrzzzzz

#### OPGAVE 5: OPRET NYE VARIABLER ####

#5.1 - opret "hp_kw", der omregner hestekræfter til kilowatt- brug omregningen:
#  kW = hp * 0,7457

bilerM_KW <- bileruden2$hp_kw <- bileruden2$hp*0.7457
bileruden2 <- 
#toby way
#biler <- biler%>%
#  mutate(
#    kw_hp = hp * 0.7457
#  )


#5.2 - opret "weight_kg", der omregner bilens vægt til kilogram.Variablen wt er angivet i 1.000 pund
#kg = wt * 1000 * 0,453592
biler_weight_kg <- biler$weight_kg <- biler$wt*1000*0.453592

#5.3 Opret"high_mgp", som er TRUE, hvis bilen har mere end 20 mpg, og ellers FALSE

biler$high_mgp <- biler$mpg>20


biler <- biler %>%
  mutate(
    high_mgp=ifelse(biler$mpg>20, "over 20", "20 eller mindre")
  )

print(biler)
#5.4 - vis de første 10 biler med de nye variabler


bileruden2

#### OPGAVE 6 - DESKRIPTIV STATISTIK ####

#Beregn følgende for mpg:
#  Gennemsnit
mean(biler$mpg)
#Median
median(biler$mpg)

#median er altså det midterse i vores dataæst se daniel der tager hænder ud og trinvis går indad.

#Standardafvigelse
sd(biler$mpg)
#eksempel 
#vi metodisk gerne vil kgige på om der er nogen korrelaion mellem det her. har det her noget med hinanden at gøre.
#jo mere spredning, jo mere usikkerhed er der.

#Minimum
min(biler$mpg)
#Maximum
max(biler$mpg)
#Antal observationer
str(biler$mpg)
length(biler$mpg)
#Ekstra udfordring: Beregn første og tredje kvartil
summary(biler$mpg)
#hvad er første kvartil?
#det er median for 2. kvaril. så du tager 25% og 3. kvartil er 75% 

#så 1. er 25%
#så median er 50%
#så 3. er 75%



############################################
#### OPGAVE 7 - GRUPPÉR DATA ####

#UNDERSØG, OM BRÆNDSTOFØKONOMIEN Korrelerer MED ANTALLET AF CYLINDRE
cor(biler$mpg,biler$cyl)


#Gruppér bilerne efter cyl

cyldf <- biler %>%
  group_by(cyl)
  summarise(biler = n())                

#Beregn gennemsnitlig mpg for hver gruppe
cyldf <- biler %>%
  group_by(cyl) %>%
  summarise(
    gns_mpg = mean(mpg), #gør vi med den her xd
    biler = n()) 

#Beregn antallet af biler i hver gruppe
cyldf <- biler %>%
  group_by(cyl) %>%
  summarise(
    gns_mpg = mean(mpg),
    biler = n()) #lige her kammarat

#Sortér grupperne efter gennesmniutlig mpg fra højest til lavest
cyldf <- biler %>%
  group_by(cyl) %>%
  summarise(
    gns_mpg = mean(mpg),
    biler = n()) %>%
  arrange(desc(gns_mpg))

#ekstra: beregn standardafvigeslsen for mpg i hver gruppe
cyldf <- biler %>%
  group_by(cyl) %>%
  summarise(
    gns_mpg = mean(mpg),
    SD_mpg =sd(mpg), #right here budddydydyydydyd
    biler = n()) %>%
  arrange(desc(gns_mpg))
#### VISUALISERING ####

#Visualiser dine resultater, så jeg som modtager forsår det ved første øjekast









