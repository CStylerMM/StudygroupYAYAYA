#Util
library(httr)
library(jsonlite)
library(dplyr)


clientId="casperstegler@gmail.com-api-client"
clientSecret="EgpDTuSaWMPBZtM5TgUdJp0SYTcu8h28"


# Request an access token

getToken <- function() {
  token=NULL
  response <- POST(
    url = "https://auth.opensky-network.org/auth/realms/opensky-network/protocol/openid-connect/token",
    body = list(
      grant_type = "client_credentials",
      client_id = clientId,
      client_secret = clientSecret
    ),
    encode = "form"
  )
  
  # Parse the JSON response
  response$status_code
  if (response$status_code == 200) {
    content <- content(response, as = "parsed", type = "application/json")
    token <- content$access_token
  }
  return(token)
}


getRawFromUrl <- function(url,token) {
  rawres=GET(
    url,
    add_headers(Authorization = paste("Bearer", token, sep = " "))
  )
  status <- rawres$status_code
  if(status==401){
    token <- getToken() 
  }
  return(rawres)
}


