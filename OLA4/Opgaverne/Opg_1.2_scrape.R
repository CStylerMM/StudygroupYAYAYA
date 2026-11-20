# ---------- Opg 1.2: Biler fra Tyskland (autoscout24.de) ----------

## tanker til selve besvarelsen, efter scrapet:
## Er det billigere at hente biler fra Tyskland? Ja/Nej, hvorfor?
## Lav en til både DK og en til DE
## Tjek korrelationen
## Eventuel hypotesetest?
## Husk at kigge på afgiften, som kan være forskellig for benzinbil og elbiler

#hvad skal vi bruge? det samme som i 1.1
library(chromote)
library(rvest)

#samme mærke som i 1.1: Mercedes
#cy=D = biler fra Tyskland, atype=C = personbil, custtype=D = forhandler, ustate=U = brugt
#samme filtre som på Bilbasen, så vi sammenligner ens med ens
baseurl_de <- "https://www.autoscout24.de/lst/mercedes-benz?atype=C&cy=D&custtype=D&ustate=U&damaged_listing=exclude&sort=standard&desc=0"

## custtype=D er et kvalificeret gæt, så tjek tabellen med sælgertype længere nede


# ---------- starter chromote ----------

a <- ChromoteSession$new()
a$view()

#navigate i stedet for go_to, for go_to venter på at alle reklamer er loadet
a$Page$navigate(baseurl_de)

#venter på at JavaScript er færdig med at indlæse siden
Sys.sleep(5)

#kommer der en cookie-boks i vinduet fra a$view(), så klik accepter og kør de næste linjer igen

#skal sige "ChromoteSession" "R6"
class(a)


# ---------- html koderne ----------

#hele siden efter JavaScript er kørt
html <- a$Runtime$evaluate("document.documentElement.outerHTML")$result$value

#nu kan rvest læse den
page <- read_html(html)

#hver bil er en article, ligesom på Bilbasen
articles <- page %>%
  html_elements("article")

#skal være omkring 20 biler pr. side
length(articles)


# ---------- hvad ligger der på én bil? ----------

#autoscout lægger det meste som attributter på selve article-tagget, så vi kigger på den første bil
html_attrs(articles[[1]])

## ligger brændstof (benzin/el) blandt attributterne? Det skal bruges til afgiften

#tjek om filtrene virker: alle skal være Mercedes
articles %>%
  html_attr("data-make") %>%
  table()

#og alle skal være forhandler (d), ikke privat (p)
articles %>%
  html_attr("data-seller-type") %>%
  table()
