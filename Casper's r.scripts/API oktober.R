library(httr)
library(jsonlite)
#anvender baseurl
url ="https://api.smk.dk/api/v1/art/search?keys=*&offset=0&rows=10"

#råt resultat
rawres=GET(url=url)
if (rawres$status_code==200) {
  #continue
}
#status code 200, så der response

#hiver content ud råt
rawcontent=rawres$content
#hiver ud, som tekst
rawcontent=httr::content(rawres, as ="text")

#laver det til df ud fra json
realcontent=fromJSON(rawcontent)

#wow se der er 10 artworks
artworks=realcontent$items


#Lad os prøve opensky api
#install.packages("openSkies")
library(openSkies)
# Install the development version from GitHub:
devtools::install_github("luisgasco/openskyr")
library(openskyr)
#Retrieve any state vector of the OpenSky data with authentication (you need to register in opensky-network):
state_vectors_df <- get_state_vectors(username="casperstegler@gmail.com",password="Rfm.@s&LC9MRNG4")

#Retrieve flights for a certain airport which departed within a given time interval [begin, end]:
  
data_airport_df <- get_airport_data(username="casperstegler@gmail.com",password="Rfm.@s&LC9MRNG4word",
                                    airport = "EKCH",
                                    begin=1793207400000,end=1793218183697, option = "departures")

?openskyR                                   
?get_airport_data
#Retrieve flights for a particular aircraft within a certain time interval:
  
data_specific_flight <- get_flights_data(username="casperstegler@gmail.com",password="Rfm.@s&LC9MRNG4word",
                                         icao24 = c(45cab6,45cab5)
                                           icao24="3c675a",begin=1517184000,end=1517270400)
#Retrieves flights for a certain time interval [begin, end]:
  
data_all_flight <- get_flights_data(username="casperstegler@gmail.com",password="Rfm.@s&LC9MRNG4word",
                                      begin=1517227200,end=1517230800)
#Retrieve the trajectory for a certain aircraft at a given time:
  
data_flight_track <- get_track_data(username=username,password="Rfm.@s&LC9MRNG4word",
                                      icao24="494103",time=1587126600)





