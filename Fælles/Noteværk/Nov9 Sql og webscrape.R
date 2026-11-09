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


#her
library(dplyr)


#indlæs
text=readRDS("Fælles/carsforreg.rds")

# tilføj kolonne med antal karakterer
text$chars=str_length(text$descs)
# sortér efter chars


# hent første beskrivelse ud
textet=text[8,2]
textet
# udskriv til en fil - vi kalder den textet egentlig.
writeLines(textet,"car.txt")

# vis de første 20 characterer i raw og identificer newline (Newline = nyt afsnits kode) her er det 0a
testraw=charToRaw(textet)
testraw

# split strengen op i tokens baseret på newline
testtext=unlist(str_split(textet,"\\x0a"))

# find alle tal
str_match_all(testtext, "\\d")
str_match_all(testtext, "\\d+")

# find alle tal med flere end 2 
r=unlist(str_match_all(testtext, "\\d{3,}"))

#Findes der et telefonnummer?
tlf=unlist(str_extract_all(testtext,"\\d{8,}"))
tlf2=unlist(str_extract_all(testtext,"[0-9]+"))
tlf
# Ja fanme ja 74720102
?str_extract_all

# saml all uppercasesekvenser som er længere end 3 karakterer

# 1) brug en quantifyer
myv <- unlist(str_match_all(testtext, "[A-Z]{4,}"))
myv

# words
# letters and numbers
str_match_all(testtext, "\\w+")
str_match_all(testtext, "\\W+")

# all white spaces
str_match_all(testtext, "\\s")
str_match_all(testtext, "\\S")



# 
str_match(myv, ".")
str_match(myv, ".+")
str_match(myv, "AB.*")
str_match(myv, "^[AB].+")




# find de hyppigste ord med mere end 3 karakterer
kpi <- unlist(str_extract_all(testtext, "\\w{4,}"))
kpi
kpi2 <- sort(str_to_lower(kpi))
sort(table(kpi2), decreasing = TRUE)


# lav det til en funktion
kpi <- function(text, ln) {
  text=testtest
  ln=4
  pattern <- str_c("\\w{", ln, ",}")
  tmp <- unlist(str_extract_all(text, pattern))
  tmp2 <- sort(str_to_lower(tmp))
  retval <- sort(table(tmp2), decreasing = TRUE)
  head(retval)
}

kpi(testtext, 4)

#føj kpilisten til dataframene
text$kpi <- sapply(text$descs, function(d) {
  paste(names(kpi(d, 4)), collapse = ", ")
})
head(text[, c("descs", "kpi")])

### GROUPS ######
testtext

#Findes der en adresse?
adr_pat <- "(\\w+) (\\d+[A-Z]?),? (\\d{4}) (\\w+)"

adr <- str_match(testtext, adr_pat)
adr <- adr[!is.na(adr[, 1]), , drop = FALSE]   # behold kun linjer med match
adr



pattern = '.*(\\d{3}).*(\\d{3}).*(\\d{4})'

phone_numbers = c(
  "(541) 471 3918",
  "(603)281-0308",
  "Home: 425-707-7220",
  "(814)-462-8074",
  "9704443106",
  "I don't have a phone."
)

phn=str_match(phone_numbers, pattern)
phndf=as.data.frame(phn)

#
#tid til at bruge DBI og RMariaDB
library(DBI)

con <- dbConnect(RMariaDB::MariaDB(),
                 host = "127.0.0.1",
                 user = "root",
                 password = Sys.getenv("MYSQL_PWD"),
                 dbname = "trytrynorthwind")  #nu peger vi på northwind i stedet for test

dbListTables(con)  #viser alle tabellerne i northwind

# 1. Show all data for the first 10 rows in the table products.
df_produkter <- dbGetQuery(con, "SELECT * FROM products LIMIT 10")  #dbGetQuery henter svaret som en dataframe
df_produkter

# 2. Show only productid, productname and unitinstock for the first 10 rows in the table products.
df_lager <- dbGetQuery(con, "SELECT productid, productname, unitsinstock FROM products LIMIT 10")  #kolonnen hedder unitsinstock med s
df_lager

# 3. Show all data for the first 10 rows in the table products sorted by decreasing unitsinstock.
df_mest_på_lager <- dbGetQuery(con, "SELECT * FROM products ORDER BY unitsinstock DESC LIMIT 10")  #DESC betyder faldende
df_mest_på_lager

# 4. Show all data for the first 10 rows in the table products where unitprice is less than 10.
df_billige <- dbGetQuery(con, "SELECT * FROM products WHERE unitprice < 10 LIMIT 10")  #WHERE filtrerer rækkerne
df_billige

# 5. Show all data for the first 10 rows in the table products where unitsinstock is between 0 and 10.
df_lavt_lager <- dbGetQuery(con, "SELECT * FROM products WHERE unitsinstock BETWEEN 0 AND 10 LIMIT 10")  #BETWEEN tager 0 og 10 med
df_lavt_lager

# 6. Show all data for the first 10 rows in the table products where productname is geitost.
df_geitost <- dbGetQuery(con, "SELECT * FROM products WHERE productname = 'geitost' LIMIT 10")  #enkelte citationstegn omkring teksten, inde i de dobbelte
df_geitost

# 7. Show only productname, productid and discontinued for the first 10 rows in the table products where supplierid is 7 or 9.
df_leverandør_7_9 <- dbGetQuery(con, "SELECT productname, productid, discontinued FROM products WHERE supplierid = 7 OR supplierid = 9 LIMIT 10")  #OR betyder enten det ene eller det andet
df_leverandør_7_9

# 10. Show all data for the first 10 rows in the table products where productname contains ost.
df_med_ost <- dbGetQuery(con, "SELECT * FROM products WHERE productname LIKE '%ost%' LIMIT 10")  #% betyder hvad som helst
df_med_ost

# 15. Show the number of rows in the table products.
df_antal_rækker <- dbGetQuery(con, "SELECT COUNT(*) FROM products")  #COUNT(*) tæller rækker
df_antal_rækker

# 16. Show the number of rows in the table products with a column name "Number of products".
df_antal_produkter <- dbGetQuery(con, "SELECT COUNT(*) AS `Number of products` FROM products")  #backticks fordi navnet har mellemrum
df_antal_produkter

# 17. Show the average of unitprice in the table products with a column name "Average price".
df_gennemsnitspris <- dbGetQuery(con, "SELECT AVG(unitprice) AS `Average price` FROM products")  #AVG tager gennemsnittet
df_gennemsnitspris

# 18. Show all different country in the table suppliers.
df_lande <- dbGetQuery(con, "SELECT DISTINCT country FROM suppliers")  #DISTINCT fjerner dubletter
df_lande
# 20. Show the first 10 productname and companyname for the tables products and suppliers where supplierid is equal in the two tables.
df_produkt_leverandør <- dbGetQuery(con, "
  SELECT products.productname, suppliers.companyname
  FROM products, suppliers
  WHERE products.supplierid = suppliers.supplierid
  LIMIT 10")  #to tabeller efter FROM, og WHERE kobler dem på supplierid
df_produkt_leverandør

# 21. Show the first productname and companyname in an inner join of the tables products and suppliers where supplierid is equal in the two tables.
df_første_produkt <- dbGetQuery(con, "
  SELECT products.productname, suppliers.companyname
  FROM products
  INNER JOIN suppliers ON products.supplierid = suppliers.supplierid
  LIMIT 1")  #samme resultat som 20, bare med INNER JOIN, og LIMIT 1 giver den første
df_første_produkt

# 22. Show the three most selling employees measured in quantity .
df_topsælgere <- dbGetQuery(con, "
  SELECT employees.employeeid, employees.firstname, employees.lastname, SUM(order_details.quantity) AS `Total quantity`
  FROM employees
  INNER JOIN orders ON employees.employeeid = orders.employeeid
  INNER JOIN order_details ON orders.orderid = order_details.orderid
  GROUP BY employees.employeeid, employees.firstname, employees.lastname
  ORDER BY `Total quantity` DESC
  LIMIT 3")  #employees til orders til order_details, SUM lægger quantity sammen pr. medarbejder
df_topsælgere
# 25. Show all data for all rows in the table shippers.
dbExecute(con, "DELETE FROM shippers WHERE shipperid > 6")  #nulstiller til de 6 originale, så koden kan køres igen
df_shippers <- dbGetQuery(con, "SELECT * FROM shippers")  #dbGetQuery henter, når vi vil se noget
df_shippers  #6 rækker er udgangspunktet

# 26. Insert a new shipper with companyname "Newshipper" and phone 11223344.
dbExecute(con, "INSERT INTO shippers (companyname, phone) VALUES ('Newshipper', '11223344')")  #dbExecute ændrer data og svarer med antal rækker, den rørte

# 27. Insert a new shipper with companyname of your own choise but with phone 99990000.
dbExecute(con, "INSERT INTO shippers (companyname, phone) VALUES ('Capshipper', '99990000')")

# 28. Change companyname "Newshipper" to "ABCDshipper".
dbExecute(con, "UPDATE shippers SET companyname = 'ABCDshipper' WHERE companyname = 'Newshipper'")

# 29. Change phone 11223344 to 55667788.
dbExecute(con, "UPDATE shippers SET phone = '55667788' WHERE phone = '11223344'")

# 30. Delete "ABCDshipper".
dbExecute(con, "DELETE FROM shippers WHERE companyname = 'ABCDshipper'")  #WHERE med, ellers sletter den alt

# 31. Insert a new shipper with companyname of your own choise but with phone 99990000.
dbExecute(con, "INSERT INTO shippers (companyname, phone) VALUES ('Dagshipper', '99990000')")

# 32. Change phone 99990000 to 12345678.
dbExecute(con, "UPDATE shippers SET phone = '12345678' WHERE phone = '99990000'")  #rammer både Capshipper og Dagshipper

df_shippers_til_sidst <- dbGetQuery(con, "SELECT * FROM shippers")  #tjek slutresultatet
df_shippers_til_sidst

dbDisconnect(con)  #luk forbindelsen, når du er færdig
