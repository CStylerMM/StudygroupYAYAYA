#CLM 
#Cumulative Link Models
#install.packages("ordinal")
library(ordinal)

fm1 <- clm(rating~ temp + contact, data = wine)
pred <- predict(fm1,newdata=subset(wine,select = -rating))$fit
pred


#         1          2         3          4          5
# 1  0.206790132 0.57064970 0.1922909 0.02361882 0.00665041
# 2  0.206790132 0.57064970 0.1922909 0.02361882 0.00665041
# 3  0.053546010 0.37764614 0.4430599 0.09582084 0.02992711
# 4  0.053546010 0.37764614 0.4430599 0.09582084 0.02992711
# 5  0.020887709 0.20141572 0.5015755 0.20049402 0.07562701

#vi skal svare på hvad de 5 er 
# skala er enten 2 eller 3 brorrz


# kig i række 1. det højeste værdi er 2

# fm.nom <- clm(rating ~ temp, nominal = ~ contact, data=wine)​
# summary(fm.nom)​




#Hvis vi vil scale, bruger vi det her
# fm.sca <- clm(rating ~ temp, scale = ~ contact, data=wine)​
# summary(fm.sca)




