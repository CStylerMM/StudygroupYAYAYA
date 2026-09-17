### Project 2: Playing Cards ###

df <- data.frame(face = c("ace", "two", "six"),  
                 suit = c("clubs", "clubs", "clubs"), 
                 value = c(1, 2, 3))
df












### ROD FRA FORSØG PÅ BOG###
hand<-c("ace","king","queen","jack","ten")
hand

hand1<-c("ace","king","queen","jack","ten","spade","spade","spade","spade","spade")
hand2<-c("ace","spade","king","spade","queen","spade","jack","spade","ten","spade")

#For ikke at tjekke længden manuelt
length(hand1)
length(hand2)

#Matrice hand1
matrix(hand1,nrow = 5)
matrix(hand1,ncol =2)

#Matrice hand2
matrix(hand2, nrow = 5, byrow = TRUE)
matrix(hand2, ncol = 2, byrow = TRUE)

#Give attribute til en vector
die<-1:6
names(die)<-c("one","two","three","four","five","six")
die
attributes(die)
# Attribut navne ændre sig ikke i forhold til værdien på vectoren
# For at slette navne attribut print nedenstående
names(die)<-NULL
die
