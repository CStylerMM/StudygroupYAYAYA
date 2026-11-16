#CLM 
#Cumulative Link Models
#install.packages("ordinal")
libary(ordinal)

fm1 <- clm(rating~temp + contakt,data=wine)
# fm.nom <- clm(rating ~ temp, nominal = ~ contact, data=wine)​
# summary(fm.nom)​




#Hvis vi vil scale, bruger vi det her
# fm.sca <- clm(rating ~ temp, scale = ~ contact, data=wine)​
# summary(fm.sca)




