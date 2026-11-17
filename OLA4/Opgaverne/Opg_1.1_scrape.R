#Hvad skal vi bruge?
library(chromote)
library(rvest)
library(stringr)
library(tryCatchLog)
#forhandler, mercedes, kontantpris, personbil - køb ikke leasing
baseurl <- "https://www.bilbasen.dk/brugt/bil/mercedes?pricetype=Retail&sellertypes=dealer"

# ---------- starter chromote ----------

a <- ChromoteSession$new()
a$view()
a$go_to(baseurl)

#venter på at JavaScript er færdig med at indlæse siden
Sys.sleep(5)

#skal sige "ChromoteSession" "R6", ellers kan vi sgu ikke scrape
class(a)

# ---------- html koderne ----------

#hele siden efter JavaScript er kørt
html <- a$Runtime$evaluate("document.documentElement.outerHTML")$result$value

#nu kan rvest læse den
page <- read_html(html)

#hver bil er en article i html'en
articles <- page %>%
  html_elements("article")

#skal være 30 pga 30 biler yo
length(articles)

# ---------- henter felterne fra hver bil ----------

#tomme vektorer, ligesom i franskmandens video
links <- c()
carids <- c()
modeller <- c()
varianter <- c()
priser <- c()
årgange <- c()
kilometer <- c()
detaljer <- c()
steder <- c()
forhandler_logoer <- c()

#én article = én bil
for (article in articles) {
  
  #linket ligger i en attribut, ikke i teksten
  link <- article %>%
    html_element("a[data-e2e='search-result-link']") %>%
    html_attr("href")
  links <- c(links, link)
  
  #sidste tal i linket er bilens id vi bruger regulært udtryk for at hive det ud. \\d+$
  carid <- link %>%
    str_extract("\\d+$")
  carids <- c(carids, carid)
  
  #modellen ligger i attributten data-e2e-value, fx Mercedes CLA250+
  model <- article %>%
    html_element("h3[data-e2e='listing-make-model']") %>%
    html_attr("data-e2e-value")
  modeller <- c(modeller, model)
  
  #varianten er teksten under modellen, så vi fjerner modellen fra den samlede tekst
  variant <- article %>%
    html_element("[data-e2e='search-result-make-model']") %>%
    html_text2() %>%
    str_remove(fixed(model)) %>%
    str_trim()
  varianter <- c(varianter, variant)
  
  #prisen gemmes som rå tekst og renses senere
  pris <- article %>%
    html_element("[data-e2e='search-result-price']") %>%
    html_text2()
  priser <- c(priser, pris)
  
  #tre felter: årgang, km og km/l
  egenskaber <- article %>%
    html_elements(".Listing_properties___ptWv div") %>%
    html_text2()
  årgange <- c(årgange, egenskaber[1])
  kilometer <- c(kilometer, egenskaber[2])
  
  #alle li samles i én tekst, og den splittes senere, fordi rækkefølgen varierer
  detalje <- article %>%
    html_elements(".ListingDetails_listItem___omDg") %>%
    html_text2() %>%
    paste(collapse = " | ")
  detaljer <- c(detaljer, detalje)
  
  sted <- article %>%
    html_element(".Listing_location__nKGQz span") %>%
    html_text2()
  steder <- c(steder, sted)
  
  #samme logo betyder samme forhandler, så den bruges som forhandler-id
  forhandler_logo <- article %>%
    html_element(".Listing_location__nKGQz img") %>%
    html_attr("src")
  forhandler_logoer <- c(forhandler_logoer, forhandler_logo)
}

# ---------- samler det i en dataframe ----------

df_biler_side1 <- data.frame(
  carid = carids,
  link = links,
  model = modeller,
  variant = varianter,
  pris = priser,
  årgang = årgange,
  km = kilometer,
  detaljer = detaljer,
  sted = steder,
  forhandler_logo = forhandler_logoer
)

#antal rækker skal være lig length(articles)
nrow(df_biler_side1)

#Har vi fucking NA'er?- nope!
colSums(is.na(df_biler_side1))

head(df_biler_side1)

# ---------- loop over siderne ----------

#test først med 3 sider, der er i alt 95.
antal_sider <- 95
##Kommenteret ud så vi ikke scraber igen igen igen igen.
# for (side in 1:antal_sider) {
#   
#   #tryCatchLog fanger fejl, så en dårlig side ikke stopper hele kørslen
#   tryCatchLog({
#     
#     #page= styrer hvilken side vi står på
#     a$go_to(paste0(baseurl, "&page=", side))
#     
#     #tilfældig pause på 5-8 sekunder, både til JavaScript og så vi ikke hamrer på serveren
#     Sys.sleep(sample(5:8, 1))
#     
#     #hele siden efter JavaScript er kørt
#     html <- a$Runtime$evaluate("document.documentElement.outerHTML")$result$value
#     
#     #nu kan rvest læse den
#     page <- read_html(html)
#     
#     #hver bil er en article i html'en
#     articles <- page %>%
#       html_elements("article")
#     
#     #viser hvor langt vi er, og hvor mange biler hver side gav
#     print(paste("Side", side, "har", length(articles), "biler"))
#     
#     #én article = én bil
#     for (article in articles) {
#       
#       #linket ligger i en attribut, ikke i teksten
#       link <- article %>%
#         html_element("a[data-e2e='search-result-link']") %>%
#         html_attr("href")
#       links <- c(links, link)
#       
#       #sidste tal i linket er bilens id
#       carid <- link %>%
#         str_extract("\\d+$")
#       carids <- c(carids, carid)
#       
#       #modellen ligger i attributten data-e2e-value, fx Mercedes CLA250+
#       model <- article %>%
#         html_element("h3[data-e2e='listing-make-model']") %>%
#         html_attr("data-e2e-value")
#       modeller <- c(modeller, model)
#       
#       #varianten er teksten under modellen, så vi fjerner modellen fra den samlede tekst
#       variant <- article %>%
#         html_element("[data-e2e='search-result-make-model']") %>%
#         html_text2() %>%
#         str_remove(fixed(model)) %>%
#         str_trim()
#       varianter <- c(varianter, variant)
#       
#       #prisen gemmes som rå tekst og renses senere
#       pris <- article %>%
#         html_element("[data-e2e='search-result-price']") %>%
#         html_text2()
#       priser <- c(priser, pris)
#       
#       #tre felter: årgang, km og km/l
#       egenskaber <- article %>%
#         html_elements(".Listing_properties___ptWv div") %>%
#         html_text2()
#       årgange <- c(årgange, egenskaber[1])
#       kilometer <- c(kilometer, egenskaber[2])
#       
#       #alle li samles i én tekst, og den splittes senere, fordi rækkefølgen varierer
#       detalje <- article %>%
#         html_elements(".ListingDetails_listItem___omDg") %>%
#         html_text2() %>%
#         paste(collapse = " | ")
#       detaljer <- c(detaljer, detalje)
#       
#       sted <- article %>%
#         html_element(".Listing_location__nKGQz span") %>%
#         html_text2()
#       steder <- c(steder, sted)
#       
#       #samme logo betyder samme forhandler, så den bruges som forhandler-id
#       forhandler_logo <- article %>%
#         html_element(".Listing_location__nKGQz img") %>%
#         html_attr("src")
#       forhandler_logoer <- c(forhandler_logoer, forhandler_logo)
#     }
#   })
# }

# ---------- tjek før vi samler ----------

#alle skal have samme længde, ellers fejler data.frame
lengths(list(links, carids, modeller, varianter, priser, årgange, kilometer, detaljer, steder, forhandler_logoer))

# ---------- samler det i en dataframe ----------

#scrapedate skal bruges i opgave 2.3
df_mercedes_OLA <- data.frame(
  carid = carids,
  link = links,
  model = modeller,
  variant = varianter,
  pris = priser,
  årgang = årgange,
  km = kilometer,
  detaljer = detaljer,
  sted = steder,
  forhandler_logo = forhandler_logoer,
  scrapedate = Sys.Date()
)

nrow(df_mercedes_OLA)


#gemmer som rds
saveRDS(df_mercedes_OLA, "df_mercedes_OLA.rds")

#viser hvor filen blev gemt
getwd()


# ---------- Lad os rense vores DF ----------
