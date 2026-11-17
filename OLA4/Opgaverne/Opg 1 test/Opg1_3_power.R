#Opgave 1.3 - Power og Elgiganten
#Opgaven er en VURDERING: kan https://www.power.dk/kampagne/aktuelt/ webscrapes?
#Vi tjekker tre ting: robots.txt, den rå HTML, og siden efter JavaScript.
library(rvest)
library(stringr)

#1) må vi? robots.txt
robots <- readLines("https://www.power.dk/robots.txt", warn = FALSE)
head(robots, 30)  #kig efter Disallow-linjer der rammer /kampagne

#2) står tilbuddene i den rå HTML? (det rvest henter uden browser)
page_rå <- read_html("https://www.power.dk/kampagne/aktuelt/")
tekst_rå <- html_text2(page_rå)
nchar(tekst_rå)  #meget lille tal = tom side
str_count(tekst_rå, "kr")  #ingen priser = ingen tilbud i HTML'en

#3) og efter JavaScript? samme trick som Bilbasen
library(chromote)
a <- ChromoteSession$new()
a$view()
a$go_to("https://www.power.dk/kampagne/aktuelt/")
Sys.sleep(5)

html <- a$Runtime$evaluate("document.documentElement.outerHTML")$result$value
page_js <- read_html(html)
tekst_js <- html_text2(page_js)
nchar(tekst_js)  #nu burde der være indhold
str_count(tekst_js, "kr")  #og priser

#KONKLUSION TIL RAPPORTEN (tjek tallene selv når du har kørt det):
#Den rå HTML er stort set tom - tilbuddene renderes med JavaScript og hentes
#fra Powers egen API bagved. rvest alene kan derfor IKKE scrape siden, men
#med chromote (eller ved at finde JSON-kaldet i Chromes Network-fane under
#Fetch/XHR) kan den godt scrapes. Så svaret er: ja, det kan lade sig gøre,
#men kun med browser-rendering eller direkte mod deres API.
## husk også lige at kigge i Network-fanen og skriv endpointet ind her som bevis
