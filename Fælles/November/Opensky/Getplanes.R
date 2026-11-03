#Getplanes
source("Fælles/November/Opensky/Util.R")
rawres <- getRawFromUrl(toturl,token)

# lav dette til en function i util - getRawFromUrl(url)
rawres=GET(
  url = toturl,
  add_headers(Authorization = paste("Bearer", token, sep = " "))
)

# tjek statuskode
rawres$status_code 
current=content(rawres, as="text")
currentJS=fromJSON(current)
currentDF=currentJS$states
currentDF=as.data.frame(currentDF)

# scrape variables
page=read_html("https://openskynetwork.github.io/opensky-api/rest.html")
tabs=page |> html_elements("table") |> html_table()
vars=tabs[[5]]
colnames(vars)

#lav dette til en vector der kan bruges i colnames
#vars=rename(vars, Property=`3‐Ltr`)
names=unlist(vars$Property)
names=names[1:(length(names)-1)]
saveRDS(names,"vars.rds")


colnames(currentDF)=names

# find operator from callsign
currentDF$cslet=str_extract(currentDF$callsign,"[A-Z]+")

# how many in the air now?

# find list of operators from FAA and get these operators with rvest
airurl="https://www.faa.gov/air_traffic/publications/atpubs/cnt_html/chap3_section_3.html"
page=read_html(airurl)
tabclass=".table.table-scroll"
tabs=page |> html_elements(tabclass) |> html_table()
tabsdf=bind_rows(tabs)
tabsdf=rename(tabsdf, operator=`3‐Ltr`)
saveRDS(tabsdf, "operators.rds")

# join operator with the current df
currwithop=currentDF |> left_join(tabsdf)

