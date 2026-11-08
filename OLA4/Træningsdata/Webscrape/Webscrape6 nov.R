library(rvest)
library(dplyr)
library(stringr)



url <- "http://books.toscrape.com/"
url <- "http://books.toscrape.com/catalogue/page-1.html"
baseurl <- url <- "http://books.toscrape.com/catalogue/page"


#læser med rvest
page <- read_html(url)

# hver bog ligger i en <article class="product_pod">
boxes <- page %>% html_elements("article.product_pod")

booklist <- page %>% html_elements(".product_pod")
# Vi ud skriver nedenstående, da vi har fundet de 20 bøger med product_pod
# booklist <- page %>% html_elements("div") %>% html_attrs()
# booklist <- page %>% html_elements("li")



#undersøge en bog
testbog <- booklist[[4]]
price <- testbog %>% html_elements(".price_color") %>% html_text()
#for at få titlen. tager vi h3 tager atrributter (a) og hiver også titel ud.
titel <- testbog %>% html_elements("h3") %>% html_elements("a") %>%  html_attr("title")

#udtræk alle bøger på siden
prices=list()
titles=list()



#lad os loope de loop in 20 fordi der 20 bøger pr side brorroroororor
for(i in 20){
newurl <- str_c(baseurl,i,".html")
page <- read_html(newurl)
booklist <- page %>% html_elements(".product_pod")

    for (book in bookslist) {
      #book <- booklist[[1]]
      price <- testbog %>% html_elements(".price_color") %>% html_text()
      prices <- c(prices, price)
      titel <- testbog %>% html_elements("h3") %>% html_elements("a") %>%  html_attr("title")
      titles <- c(titles,title)
    }
}

dfbooks <- data.frame(
  titles=unlist(titles),
  prices=unlist(prices)
)


#et sted skal der inddrages log_print(url) der senere kan hjælpe os med at se på NA'er.





