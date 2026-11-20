# ==========================================================
# Opgave 1.3 – Kan Powers kampagneside webscrapes?
# ==========================================================
library(rvest)
library(stringr)
library(dplyr)
library(chromote)
library(jsonlite)

# ---------- 1. må vi? robots.txt ----------
# robots.txt er sidens egne regler for robotter (scrapere). Disallow = "her må I ikke komme"
robots <- readLines("https://www.power.dk/robots.txt", warn = FALSE)
length(robots)   # hvor mange linjer har filen?

# vis kun de linjer, der er relevante: hvem reglerne gælder for, og om /kampagne eller hele sitet er spærret
robots[str_detect(robots, regex("user-agent|disallow.*kampagne|^disallow: /$", ignore_case = TRUE))]

robots   # vis alle 28 linjer


# ---------- 2. står tilbuddene i den rå HTML? ----------
# read_html henter siden UDEN browser, så JavaScript ikke bliver kørt
page_rå <- read_html("https://www.power.dk/kampagne/aktuelt/")
tekst_rå <- html_text2(page_rå)

nchar(tekst_rå)                                   # antal tegn synlig tekst – meget lille = tom side

pris_mønster <- "\\d[\\d.]*\\s?(kr\\.?|,-)"       # fx "2.999 kr.", "2.999,-"
str_count(tekst_rå, pris_mønster)                 # antal priser i den rå HTML


# ---------- 3. og efter JavaScript? samme metode som Bilbasen ----------
a <- ChromoteSession$new()
a$Page$navigate("https://www.power.dk/kampagne/aktuelt/")
Sys.sleep(8)                                       # giv JavaScript tid til at hente tilbuddene

html_js <- a$Runtime$evaluate("document.documentElement.outerHTML")$result$value
page_js <- read_html(html_js)

page_js |> html_elements("article.product-item") |> length()   # antal tilbudskort efter JavaScript (rå HTML: 0)


# ---------- 4. hent felterne fra hvert tilbud (HTML) ----------
# hvert tilbud er et <article class="product-item">, prisen ligger i <data class="main-price" value="...">
hent_tilbud <- function(kort) {
  link <- kort |> html_element(".product-item-title-container a") |> html_attr("href")
  
  data.frame(
    produkt_id    = str_extract(link, "(?<=p-)\\d+"),                         # tallet efter "p-" i linket
    navn          = kort |> html_element(".product-item-title-container a") |> html_text2(),
    link          = paste0("https://www.power.dk", link),
    pris          = kort |> html_element("data.main-price") |> html_attr("value") |> as.numeric(),
    besparelse    = kort |> html_element(".savings-label") |> html_text2(),
    førpris_tekst = kort |> html_element("pwr-product-previous-price-disclaimer") |> html_text2(),
    lager         = kort |> html_element("pwr-product-stock-label[type='webshort']") |> html_text2()
  )
}

# siden bygger kun de tilbud, der er i syne, og fjerner dem igen, når man scroller forbi
# -> scroll fra top til bund i skridt på 800 pixels og saml alle kort undervejs
sidehøjde <- a$Runtime$evaluate("document.body.scrollHeight")$result$value
liste_tilbud <- list()

for (position in seq(0, sidehøjde, by = 800)) {
  a$Runtime$evaluate(paste0("window.scrollTo(0, ", position, ")"))   # gå til positionen
  Sys.sleep(2)                                                       # vent på, at tilbuddene bliver bygget
  
  kort <- read_html(a$Runtime$evaluate("document.documentElement.outerHTML")$result$value) |>
    html_elements("article.product-item")                            # hvert tilbud er et <article class="product-item">
  
  cat("position", position, ":", length(kort), "kort\n")             # log ved hvert skridt
  if (length(kort) > 0) liste_tilbud[[length(liste_tilbud) + 1]] <- lapply(kort, hent_tilbud) |> bind_rows()
}

df_power <- bind_rows(liste_tilbud) |>
  distinct(produkt_id, .keep_all = TRUE)                             # fjern dubletter via produkt-id

nrow(df_power)                       # antal forskellige tilbud
head(df_power)

# tilbud bag karrusel-pilene kommer ikke med – fx Gorenje-vaskemaskinen fra "Udvalgte tilbud"
df_power |> filter(str_detect(navn, "Gorenje")) |> select(navn, pris)


# ---------- 5. hent tilbuddene direkte fra Powers API ----------
# fundet i Chrome DevTools -> Network -> Fetch/XHR -> filter: products. Siden henter tilbuddene fra /api/v2/products
api_url <- "https://www.power.dk/api/v2/products?ids=2165908%2C2323626%2C2560639%2C3199486%2C3275702%2C4008995%2C4123359%2C4125280%2C4126596%2C4148875%2C4157177%2C4157294%2C4161440%2C4193715%2C4204328%2C4206178%2C4206198%2C4231859%2C4238275%2C4251464%2C4252867%2C4253039%2C4266300%2C4266301%2C4266317%2C4268029%2C4313164%2C4317058%2C4317069%2C4317071%2C4318138%2C4523413%2C4526586%2C4528037&allowWebStatus8=false"

api_data <- fromJSON(api_url)

class(api_data)        # hvilken type data får vi? (data.frame eller list)
names(api_data)        # hvilke felter er der?
nrow(api_data)         # antal tilbud – forventet 34 (antal id'er i URL'en)

df_power_api <- api_data |>
  select(productId, title, manufacturerName, categoryName,
         price, previousPrice, url, webStockTextShort) |>
  mutate(besparelse = previousPrice - price,                       # hvor meget man sparer i kr.
         link = paste0("https://www.power.dk", url))

head(df_power_api)


# ---------- 6. kontrol: HTML-scraping vs. API ----------
# er Gorenje-vaskemaskinen med nu? (den manglede i HTML-scrapingen)
df_power_api |> filter(str_detect(title, "Gorenje")) |> select(title, price, previousPrice)

# er alle tilbud fra HTML-scrapingen også i API'et?
sum(df_power$produkt_id %in% df_power_api$productId)

saveRDS(df_power_api, "df_power_api.rds")