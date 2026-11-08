#search for gdp in the text field of the tables.
dst_search(string = "bnp", field = "text")
#Download the tables
#The dst_get_tables function downloads all the available tables that the search 
#function use when searching for a word or a phrase.
head(dst_get_tables(lang = "da"))
#META Data
#The dst_meta function retrieves meta data from the table you wan’t to take a closer look at. 
#It can be used to create the final request, but if you can figure out the structure of the query you can define it yourself.
#We’ll get some meta data from the AULAAR table. The AULAAR table has net unemployment numbers.
aulaar_meta <- dst_meta(table = "AULAAR", lang = "da")
#The ‘dst_meta’ function returns a list with 4 objects: - basics - variables - values - basic_query
#Basic
#lad os se hvad de kan
aulaar_meta$basics
## $id
## [1] "AULAAR"
## 
## $text
## [1] "Fuldtidsledige (netto)"
## 
## $description
## [1] "Fuldtidsledige (netto) efter køn, personer/pct. og tid"
## 
## $unit
## [1] "Antal"
## 
## $updated
## [1] "2024-04-16T08:00:00"
## 
## $footnote
## NULL
###There’s a table id, a short description, a unit description and when the table was updated.


#Variables
#The variables in the list has a short description of each variable as well as the id. 
#You might want to make sure that you have supplied all the ID’s where the elimination columns 
#is equal to FALSE. The IDs where eliminnation is equal FALSE are mandatory.
aulaar_meta$variables
##       id          text elimination
## 1    KØN           køn        TRUE
## 2 PERPCT personer/pct.       FALSE
## 3    Tid           tid       FALSE

#Values

#The values is a list object of all the values in each variable. You use the text column to construct your final query:
  
str(aulaar_meta$values)
#List of 3
#$ KØN   :'data.frame':	3 obs. of  2 variables:
#  ..$ id  : chr [1:3] "TOT" "M" "K"
#..$ text: chr [1:3] "I alt" "Mænd" "Kvinder"
#$ PERPCT:'data.frame':	2 obs. of  2 variables:
#  ..$ id  : chr [1:2] "L10" "L9"
#..$ text: chr [1:2] "Procent af arbejdsstyrken" "Ledige (1000 personer)"
#$ Tid   :'data.frame':	47 obs. of  2 variables:
#  ..$ id  : chr [1:47] "1979" "1980" "1981" "1982" ...
#..$ text: chr [1:47] "1979" "1980" "1981" "1982" ...


#Get data

#You need to build your query based on the text column that each variable contains in the meta_data$values list.
aulaar <- dst_get_data(
  table = "AULAAR", KØN = "Total", PERPCT = "Per cent of the labour force", Tid = 2013,
  lang = "en"
)
str(aulaar)

#In the request above I don’t supply the meta_data to the dst_get_data function, but this is possible as I will show below.
#It’s a good idea to supply the meta data to the dst_get_data function if you query the table more than once.
#If you don’t supply the meta data the dst_get_data function will request the meta data for the table and this will be very ineffecient.

#Let’s query the statbank using more than one value for each variable.
folk1a_meta <- dst_meta("folk1a", lang = "da")

str(dst_get_data(
  table = "folk1a",
  Tid = "*",
  CIVILSTAND = "*",
  ALDER = "*",
  OMRÅDE = c("Hele landet", "København", "Dragør", "Albertslund"),
  lang = "da",
  meta_data = folk1a_meta
))
## Classes 'dkstat_Denmark_municipality_07' and 'data.frame':   175260 obs. of  5 variables:
##  $ TID       : POSIXct, format: "2008-01-01" "2008-01-01" ...
##  $ CIVILSTAND: chr  "TOT I alt" "TOT I alt" "TOT I alt" "TOT I alt" ...
##  $ ALDER     : chr  "IALT Alder i alt" "IALT Alder i alt" "IALT Alder i alt" "IALT Alder i alt" ...
##  $ OMRÅDE    : chr  "000 Hele landet" "101 København" "155 Dragør" "165 Albertslund" ...
##  $ value     : int  5475791 509861 13261 27602 64412 7986 121 295 65722 7097 ...
#I can also build a query beforehand and then use the query in the query parameter. 
#This might be a good way to split your script up into smaller pieces and make it more structured.

#You might have noticed that I use the * as a value in the TID variable.
#You can use the star as a alternative to writing all the text values for the variable.

my_query <- list(
  OMRÅDE = c("Hele landet", "København", "Frederiksberg", "Odense"),
  CIVILSTAND = "Ugift",
  TID = "*"
)

str(dst_get_data(table = "folk1a", query = my_query, lang = "da"))

## Classes 'dkstat_Denmark_municipality_07' and 'data.frame':   276 obs. of  4 variables:
##  $ OMRÅDE    : chr  "000 Hele landet" "000 Hele landet" "000 Hele landet" "000 Hele landet" ...
##  $ CIVILSTAND: chr  "U Ugift" "U Ugift" "U Ugift" "U Ugift" ...
##  $ TID       : POSIXct, format: "2008-01-01" "2008-04-01" ...
##  $ value     : int  2552700 2563134 2564705 2568255 2575185 2584993 2584560 2588198 2593172 2604129 ...

str(dst_get_data(table = "AUP01", OMRÅDE = c("Hele landet"), TID = "*", lang = "da"))


#If you run into problems, then try to set the parse_dst_tid parameter to FALSE as there are few datasets with non-standard date formats.

#Don’t hesitate to submit an issue or question on github and I’ll try to help as much as I can.

