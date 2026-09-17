#Scipt draft - Project 1: Weighted Dice

die<-1:6
sample(die,size=2,replace=TRUE)
dice<-sample(die,size=2,replace=TRUE)
roll<-function(){
  die<-1:6
  dice<-sample(die,size=2,replace=TRUE)
  sum(dice)
}

#Installing extensions
install.packages("ggplot2")
library("ggplot2")

#qplot
x<-c(-1,-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8,1)
y<-x^3
qplot(x, y)
x <- c(1, 2, 2, 2, 3, 3)
qplot(x, binwidth = 1)
x2 <- c(1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 4)
qplot(x2, binwidth = 1)

x3 <- c(0, 1, 1, 2, 2, 2, 3, 3, 4)
qplot(x3, binwidth = 1)

replicate(10, roll())
rolls <- replicate(10000, roll())
qplot(rolls, binwidth = 1)

#Weighted dices
roll <- function() {
  die <- 1:6
  dice <- sample(die, size = 2, replace = TRUE, 
                 prob = c(1/8, 1/8, 1/8, 1/8, 1/8, 3/8))
  sum(dice)
}






