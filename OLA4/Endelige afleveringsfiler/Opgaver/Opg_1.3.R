# ---------- Opg 1.3: Kan Powers kampagneside webscrapes? ----------
library(rvest)
library(stringr)
library(dplyr)
library(chromote)
library(jsonlite)

# ---------- 1. må vi? robots.txt ----------
#robots.txt er sidens egne regler for scrapere. Disallow betyder "her må I ikke komme"
robots <- readLines("https://www.power.dk/robots.txt", warn = FALSE)
#hvor mange linjer har filen?
length(robots)

#viser kun de linjer vi skal bruge: hvem reglerne gælder for, og om /kampagne eller hele siden er spærret
robots[str_detect(robots, regex("user-agent|disallow.*kampagne|^disallow: /$", ignore_case = TRUE))]

#viser alle 28 linjer
robots


# ---------- 2. står tilbuddene i den rå HTML? ----------
#read_html henter siden uden browser, så JavaScript bliver ikke kørt
page_rå <- read_html("https://www.power.dk/kampagne/aktuelt/")
tekst_rå <- html_text2(page_rå)

#antal tegn synlig tekst. er tallet lille, er siden tom
nchar(tekst_rå)

#fx "2.999 kr." eller "2.999,-"
pris_mønster <- "\\d[\\d.]*\\s?(kr\\.?|,-)"
#antal priser i den rå HTML
str_count(tekst_rå, pris_mønster)


# ---------- 3. og efter JavaScript? samme metode som Bilbasen ----------
a <- ChromoteSession$new()
a$Page$navigate("https://www.power.dk/kampagne/aktuelt/")
#giver JavaScript tid til at hente tilbuddene
Sys.sleep(8)

html_js <- a$Runtime$evaluate("document.documentElement.outerHTML")$result$value
page_js <- read_html(html_js)

#antal tilbudskort efter JavaScript (i den rå HTML var der 0)
page_js |> html_elements("article.product-item") |> length()


# ---------- 4. hent felterne fra hvert tilbud (HTML) ----------
#hvert tilbud er et <article class="product-item">, prisen ligger i <data class="main-price" value="...">
hent_tilbud <- function(kort) {
  link <- kort |> html_element(".product-item-title-container a") |> html_attr("href")
  
  data.frame(
    #tallet efter "p-" i linket
    produkt_id    = str_extract(link, "(?<=p-)\\d+"),
    navn          = kort |> html_element(".product-item-title-container a") |> html_text2(),
    link          = paste0("https://www.power.dk", link),
    pris          = kort |> html_element("data.main-price") |> html_attr("value") |> as.numeric(),
    besparelse    = kort |> html_element(".savings-label") |> html_text2(),
    førpris_tekst = kort |> html_element("pwr-product-previous-price-disclaimer") |> html_text2(),
    lager         = kort |> html_element("pwr-product-stock-label[type='webshort']") |> html_text2()
  )
}

#siden bygger kun de tilbud man kan se, og fjerner dem igen når man scroller forbi
#så vi scroller fra top til bund i skridt på 800 pixels og samler kortene undervejs
sidehøjde <- a$Runtime$evaluate("document.body.scrollHeight")$result$value
liste_tilbud <- list()

for (position in seq(0, sidehøjde, by = 800)) {
  #går til positionen
  a$Runtime$evaluate(paste0("window.scrollTo(0, ", position, ")"))
  #venter på at tilbuddene bliver bygget
  Sys.sleep(2)
  
  kort <- read_html(a$Runtime$evaluate("document.documentElement.outerHTML")$result$value) |>
    #hvert tilbud er et <article class="product-item">
    html_elements("article.product-item")
  
  #skriver i konsollen ved hvert skridt
  cat("position", position, ":", length(kort), "kort\n")
  if (length(kort) > 0) liste_tilbud[[length(liste_tilbud) + 1]] <- lapply(kort, hent_tilbud) |> bind_rows()
}

df_power <- bind_rows(liste_tilbud) |>
  #fjerner dubletter på produkt-id
  distinct(produkt_id, .keep_all = TRUE)

#antal forskellige tilbud
nrow(df_power)
head(df_power)

#tilbud bag pilene i karrusellerne kommer ikke med, fx Gorenje-vaskemaskinen fra "Udvalgte tilbud"
df_power |> filter(str_detect(navn, "Gorenje")) |> select(navn, pris)


# ---------- 5. hent tilbuddene direkte fra Powers API ----------
#fundet i Chrome DevTools under Network, Fetch/XHR og filter: products. siden henter tilbuddene fra /api/v2/products
api_url <- "https://www.power.dk/api/v2/products?ids=2165908%2C2323626%2C2560639%2C3199486%2C3275702%2C4008995%2C4123359%2C4125280%2C4126596%2C4148875%2C4157177%2C4157294%2C4161440%2C4193715%2C4204328%2C4206178%2C4206198%2C4231859%2C4238275%2C4251464%2C4252867%2C4253039%2C4266300%2C4266301%2C4266317%2C4268029%2C4313164%2C4317058%2C4317069%2C4317071%2C4318138%2C4523413%2C4526586%2C4528037&allowWebStatus8=false"

api_data <- fromJSON(api_url)

#hvilken type data får vi? (data.frame eller list)
class(api_data)
#hvilke felter er der?
names(api_data)
#antal tilbud. skal give 34, for der er 34 id'er i URL'en
nrow(api_data)

df_power_api <- api_data |>
  select(productId, title, manufacturerName, categoryName,
         price, previousPrice, url, webStockTextShort) |>
  #hvor meget man sparer i kr.
  mutate(besparelse = previousPrice - price,
         link = paste0("https://www.power.dk", url))

head(df_power_api)


# ---------- 6. Tjek: HTML-scraping mod API ----------
#er Gorenje-vaskemaskinen med nu? (den manglede i HTML-scrapingen)
df_power_api |> filter(str_detect(title, "Gorenje")) |> select(title, price, previousPrice)

#er alle tilbud fra HTML-scrapingen også i API'et?
sum(df_power$produkt_id %in% df_power_api$productId)

saveRDS(df_power_api, "df_power_api.rds")
