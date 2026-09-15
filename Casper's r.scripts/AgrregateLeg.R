mymatrix <- matrix(NA,10,3)
list1 <- list(100:130, "R", list(TRUE,FALSE))
list1
#If statements

#Else, hvis ikke så gør den det her i stedet.

#Else if = lave en lang kæde af statemens, hvis de her er rigtige så gør det her

x <- 10
y <- 5
if(x>y) {
  print(x)
} else if(y==x) {
    print("x and y are equal")
} else {
  print(y)
}
#Tid til Loops
my_sequence <- seq(0,100,10)
for(item in my_sequence) {
  print (item)
}
#for hver item, putter man "in" ind. Så nu tager den vores item eller variable som her er 100. 
#og vi starter fra 0 vi går op til 100. vi rykker med 10 af gangen. vi vil gerne have hver 10. vist. det gør loopet

for (item in my_sequence) {
  if (item<=50) {
    next
  }
  print(item)
  }
#Den her kode skipper de første 50 - så de ikke bliver printet. Den printer kun over 50. det er hvis det er <50 <= er så tager den efter 50.

#Den næste kode skipper de SIDSTE 50. Så vi får den altså til at stoppe når den tælle rtil 50.
for (item in my_sequence) {
  if (item>50) {
    break
  }
  print(item)
}
#læg mærke til det er samme output. Med break går den ud af loopet. Med next laver man mere computing, fordi man ikke ryger ud af et loop.

#Wild loops = gør næsten det samme. Men laver de samme loops mange gange. 
x <- 5
iters <- 0
#iters er "Iters" er en forkortelse for iterationer — antallet af gange en løkke (loop) kører igennem.
while(iters<x) {
  print("study")
  iters <-iters+1
}
#først er det kør kode å mange gange som der er iters
#næste er det increment med +1 hver gang loopet kører.

while(TRUE) {
  print("study")
  break
}
#this logical expression is always true 
#But we immediatyly break out of the loop!
 
#Vi starter vores random nummer generaotr RNG!!!!

set.seed(12)
my_vector <- runif(25,-1,1) #Vi laver noget random data mellem -1 og 1
for (index in 1:length(my_vector)) { #Vi looper igennem sequence 1:25
  number <- my_vector [index] #Vi søger efter nummeret med index
  if (number<0) { #Vi checker om nummeret er under 0
    my_vector[index] <- 0 # Hvis det er, så sætter vi det til 0
  }
}
 print(my_vector)

 #lad os lave en logisk vektor.
 set.seed(12)
 my_vect2 <- runif(25,-1,1) 
 my_vect2 <- ifelse((my_vect2<0),
                    0,
                    my_vect2)


 print(my_vect2)
#vi laver en logisk test hor vi sætter det logisk til sandt eller falsk altså 1 eller 0. 
 
 #næste er lapply.
 
 #apply family of functions, som kan eksekvere på hver del af data struktur.
 #den tager en liste og skal have et reply den popper ud, vist..
 
 #her er eksempel
 data <- mtcars
 
 #nu kommer en funktion til dette.
 mpg_category <- function(mpg){
   if(mpg>30){
     return("High")
   } else if (mpg>20){
     return ("Medium")
   }
   return("Low")
 }
 
 #Herefter skal vi aplly til et element
 lapply(X=data$mpg, FUN=mpg_category)

 
 
deltagere <- list("deltager"="kurt") 
v1 <- runif(5,25,40)
v2 <- runif(2,40,60)
v3 <- runif(6,25,40)
v4 <- runif(3,40,60)

deltagere <- list("deltager"=
                    list("32"=list("navn"="kurt","alder"=45,
                                   "løb"=list("DHL"=v3,"ERM"=v4)),
                         "33"=list("navn"="Valde", "alder"=12,
                                   "løb"=list("DHL"=v1,"ERM"=v2)),
                         "34"=list("navn"="Jens","alder"=102,
                                   "løb"=list("DHL"=v4,"ERM"=v2)),
                         "35"=list("navn"="Jørn","alder"=29,
                                   "løb"=list("DHL"=v2,"ERM"=v1))
                    )
)

#tid til at lære loading og saving dat
dfestonia = read.csv("https://raw.githubusercontent.com/cphstud/RIntroData/refs/heads/master/estonia-passenger-list.csv")
saveRDS(dfestonia,"Estonia.rds") 
      
dfestonia[8,]             
#Filter, ny kolonne
dfestonia$AgeCat="A"
#Nu tager vi alle grown ups ud
#Subset alle voksne

newdf=dfestonia$Age>18
newdf
newAdults=dfestonia[newdf,]
newAdults
kids=dfestonia$Age<=18
newKids=dfestonia[kids,]
newKids

#lad os se efter dk og sverieg
#%in% hvor der ikke er noget logik - bruges ret tit til at filtrere 
unique(dfestonia$Country)
length(unique(dfestonia$Country))
dfestonia$Country%$%c("D")

#table er også god hihi
#hvor mange observationer er der talt?
table(dfestonia$Category)/sum(table(dfestonia$Category))
#hvor mange overlevede?
table(dfestonia$Survived)
table(dfestonia$Survived)/sum(table(dfestonia$Survived))
length(dfestonia$Survived)
ctrv=dfestonia$Country%in%c("Denmark","Sweden")
sum(ctrv)

#fyr det ind i dataen
dfdkse=dfestonia[ctrv,]
dfdkse
summary(dfestonia$Age)
#category variable vi laver selv
#hvilken alders range har vi?

myAgeCat <- function(age){
  retvalg=0
  #logik der indeler kategorier
  if(age<18){
    retval="I"
  } else if (age>=18 & age<25) {
    retval="Y"
  } else if (age>=25 & age<30){
    retval="A"
  } else if (age>=30 & age<30) {
    retval="A"
    
  return(retval)
}
}
dfestonia$AgeCat=sapply(dfestonia$Age,FUN=myAgeCat)
table(dfestonia$AgeCat)
