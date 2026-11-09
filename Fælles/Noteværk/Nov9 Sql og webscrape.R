# install.packages(c("DBI", "RMariaDB"))
#Overstående er i forbindelse med SQL server
# file.edit("~/.Renviron") - jeg gemmer mine koder i en renviron fil - det bassicly en hemmelig fil med mine koder 

library(DBI)
con <- dbConnect(RMariaDB::MariaDB(),
                 host = "127.0.0.1",
                 user = "root",
                 password = Sys.getenv("MYSQL_PWD"),
                 dbname = "test")
dbListTables(con)
dbGetQuery(con, "SELECT * FROM personer")
dbWriteTable(con, "mtcars", mtcars, overwrite = TRUE)
dbGetQuery(con, "SELECT cyl, AVG(mpg) AS gns_mpg FROM mtcars GROUP BY cyl")
dbDisconnect(con)



#Nu det tid til noget chromote
library(chromote)
library(stringr)
library(tryCatchLog)


b <- ChromoteSession$new()
b$view()
b$Browser$getVersion()
b$go_to("https://www.r-project.org/")
# Saves to screenshot.png
b$screenshot()

# Takes a screenshot of elements picked out by CSS selector
b$screenshot("sidebar.png", selector = ".sidebar")


#lad os lege med lidt regulære udtryk
testtest <- c("Her bor i 2026 82.320 mennesker","Viggo er 30 år","Koster 30-25 kr.")
str_match(testtest, "\\d.+") #d står for digtis (så tal.)
str_match(testtest, "\\D.+") #alt med stort gør det omvendte.
str_match(testtest, "\\w.+") #Tager words og punktummer
str_match(testtest, "\\W.+") #Tager ikke words og punktummer?

str_match(testtest, "s.+")



