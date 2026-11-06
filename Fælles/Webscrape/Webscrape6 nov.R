library(rvest)
library(dplyr)
library(stringr)



url <- "http://books.toscrape.com/"
page <- read_html(url)

# hver bog ligger i en <article class="product_pod">
boxes <- page |> html_elements("article.product_pod")

books <- tibble(
  title  = boxes |> html_element("h3 a") |> html_attr("title"),
  price  = boxes |> html_element("p.price_color") |> html_text() |>
    str_extract("[\\d.]+") |> as.numeric(),
  rating = boxes |> html_element("p.star-rating") |> html_attr("class") |>
    str_remove("star-rating ")
)

