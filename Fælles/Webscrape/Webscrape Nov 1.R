library(rvest)   # webscraping
library(dplyr)   # datamanipulation (bruger %>%)
library(tryCatchLog) #til at catche de logs vi får
# install.packages(c("rvest", "dplyr"))
# Step 1: hent siden

url  <- "http://books.toscrape.com"
page <- read_html(url)

# Step 2: find alle bog-elementer
books <- page %>% html_nodes("article.product_pod")

# Step 3: tomme vektorer til resultater
titles         <- c()
prices         <- c()
availabilities <- c()
ratings        <- c()

# Step 4: loop gennem hver bog
for (book in books) {
  # Titel
  title <- book %>%
    html_node("h3 a") %>%
    html_attr("title")
  titles <- c(titles, title)
  
# Pris
  price <- book %>%
    html_node("p.price_color") %>%
    html_text()
  prices <- c(prices, price)
  
  # Availability og rating følger samme mønster
  
# Availability
  availability <- book %>%
    html_node("p.instock.availability") %>%
    html_text(trim = TRUE)
  availabilities <- c(availabilities, availability)
  
# Rating
  rating <- book %>%
    html_node("p.star-rating") %>%
    html_attr("class")
  rating <- gsub("star-rating", "", rating)
  ratings <- c(ratings, rating)
}   # slut på for (book in books)

# Saml side 1 i et data frame
df <- data.frame(
  Title        = titles,
  Price        = prices,
  Availability = availabilities,
  Rating       = ratings
)
print(df)

# Side 2-50
for (page_num in 2:50) {
  url   <- paste0("http://books.toscrape.com/catalogue/page-", page_num, ".html")
  page  <- read_html(url)
  books <- page %>% html_nodes("article.product_pod")
  
  for (book in books) {
    # samme krop som ovenfor: title, price, availability, rating
  }
}

# Saml alt til sidst
df <- data.frame(
  Title        = titles,
  Price        = prices,
  Availability = availabilities,
  Rating       = ratings
)
print(df)
