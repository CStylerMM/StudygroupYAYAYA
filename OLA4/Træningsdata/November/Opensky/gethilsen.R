#laver en func der retunere hej mellemrum parameter
library(stringr)

gethilsen <- function(navn) {
  retval <-str_c("Hej ",navn) 
  return(retval)
}

gethilsen("Hej")
