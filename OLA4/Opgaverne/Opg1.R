#Hvad skal vi bruge?
library(chromote)
library(rvest)
library(stringr)
library(tryCatchLog)

?chromote
b <- ChromoteSession$new()
b$view()
b <- b$go_to("https://www.bilbasen.dk/brugt/bil/mercedes?includeengroscvr=true&includeleasing=false&page=2")

