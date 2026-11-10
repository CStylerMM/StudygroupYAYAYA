#Hvad skal vi bruge?
library(chromote)
library(rvest)
library(stringr)
library(tryCatchLog)

a <- ChromoteSession$new()  
a$view()
a$go_to("https://www.bilbasen.dk/brugt/bil/mercedes?includeengroscvr=true&includeleasing=false&page=2")  #ingen a <- foran
Sys.sleep(5)  #indlæser JS

class(a)  #skal sige "ChromoteSession" "R6" ellers kan vi sgu ikke scrape :/

#lad os finde html koderne
html <- a$Runtime$evaluate("document.documentElement.outerHTML")$result$value  #hele siden efter JavaScript er kørt
page <- read_html(html)  #nu kan rvest læse den

kasser <- page %>% html_elements("article")  # hver bil er en article
length(kasser)  #skal være ca. 30

#okay okay. nu ved vi der er 30 biler i de her kasser. Lad os dykke dybere.

#har fundet noget om beskrivelsen her
kasser[[1]] %>%
  html_elements(".Listing_description__sCNNM") %>%
  html_text2() #Jeg bruger html text2 der jeg synes det pænere xd

#Hvilke klasser er der?
kasser[[1]] %>%
  html_attrs() 

#Fandt noget om modellen hmm - skal dog fikse syntaks siger lige nu
##"Mercedes 200\n2,0 D aut. 4d"
kasser[[1]] %>%
  html_elements("[data-e2e='search-result-make-model']") %>%
  html_text2()

#Noget om pris her
kasser[[1]] %>%
  html_elements("[data-e2e='search-result-price']") %>%
  html_text2()

#Der er 3 forskellige outputs - men tror oprindelsesåret er i den her.
kasser[[1]] %>%
  html_elements(".Listing_properties___ptWv div") %>%
  html_text2()

#Her er også oprindelsesår, men bonus vi ved her det er automat gear + diesel
kasser[[1]] %>%
  html_elements(".ListingDetails_listItem___omDg") %>%
  html_text2()

#Hvor bliver den solgt? - fanme i hørsholm do
kasser[[1]] %>%
  html_elements(".Listing_location__nKGQz") %>%
  html_text2()

#De så søde, de har givet os et direkte url til bilens egen side
kasser[[1]] %>%
  html_elements("a[data-e2e='search-result-link']") %>%
  html_attr("href")

###------ Lad os samle det vi har fundet og definere det. ###------ 

#kasse 
kasse <- kasser[[1]]

#link børge
link <- kasse %>%
  html_element("a[data-e2e='search-result-link']") %>%
  html_attr("href")  #element uden s giver kun det første match

#carid børge
carid <- link %>%
  str_extract("\\d+$")  #sidste tal i linket er bilens id

#model
make_model <- kasse %>%
  html_element("h3[data-e2e='listing-make-model']") %>%
  html_attr("data-e2e-value")  #Mercedes 200, uden \n

#variant (automat,el,eller manuel  vi får også om det diesel,osv)
variant <- kasse %>%
  html_element("[data-e2e='search-result-make-model']") %>%
  html_text2() %>%
  str_remove(fixed(make_model)) %>%
  str_trim()  #fixed() = match som ren tekst, så + og ( ikke driller

#pris. pris pris.
pris <- kasse %>%
  html_element("[data-e2e='search-result-price']") %>%
  html_text2() %>%
  str_remove_all("[^0-9]") %>%
  as.numeric()  #fjerner alt der ikke er et tal, så 394.900 kr. bliver 394900

#år,km og et 3. ukendt tal indtil videre...
properties <- kasse %>%
  html_elements(".Listing_properties___ptWv div") %>%
  html_text2()  #3 felter: år, km og et tredje

#kort detaljer 
detaljer <- kasse %>%
  html_elements(".ListingDetails_listItem___omDg") %>%
  html_text2() %>%
  paste(collapse = " | ")  #alle tags i én tekst, så splitter vi dem senere

#tekst boks g
beskrivelse <- kasse %>%
  html_element(".Listing_description__sCNNM") %>%
  html_text2()

#hvor er den?
sted <- kasse %>%
  html_element(".Listing_location__nKGQz") %>%
  html_text2()

#Yep logo time
forhandlerlogo <- kasse %>%
  html_element("img[alt='Forhandlerlogo']") %>%
  html_attr("src")  #logoet er vores forhandler-id indtil videre


#lad os lave det fårking frame mand
df_bil <- data.frame(
  carid = carid,
  link = link,
  model = make_model,
  variant = variant,
  pris = pris,
  årgang = properties[1],
  km = properties[2],
  detaljer = detaljer,
  beskrivelse = beskrivelse,
  sted = sted,
  forhandlerlogo = forhandlerlogo,
  scrapedate = Sys.Date()  #sat scrapedate ind da vi skal bruge det senere.
)

df_bil

####------ Funktion tid. Lad os lave en række brorrzzzz ------####

hent_bil <- function(kasse) {
  link <- kasse %>%
    html_element("a[data-e2e='search-result-link']") %>%
    html_attr("href")
  
  carid <- link %>%
    str_extract("\\d+$")
  
  make_model <- kasse %>%
    html_element("h3[data-e2e='listing-make-model']") %>%
    html_attr("data-e2e-value")
  
  variant <- kasse %>%
    html_element("[data-e2e='search-result-make-model']") %>%
    html_text2() %>%
    str_remove(fixed(make_model)) %>%
    str_trim()
  
  pris <- kasse %>%
    html_element("[data-e2e='search-result-price']") %>%
    html_text2() %>%
    str_remove_all("[^0-9]") %>%
    as.numeric()
  
  properties <- kasse %>%
    html_elements(".Listing_properties___ptWv div") %>%
    html_text2()
  
  detaljer <- kasse %>%
    html_elements(".ListingDetails_listItem___omDg") %>%
    html_text2() %>%
    paste(collapse = " | ")
  
  beskrivelse <- kasse %>%
    html_element(".Listing_description__sCNNM") %>%
    html_text2()
  
  sted <- kasse %>%
    html_element(".Listing_location__nKGQz") %>%
    html_text2()
  
  forhandlerlogo <- kasse %>%
    html_element("img[alt='Forhandlerlogo']") %>%
    html_attr("src")
  
  data.frame(
    carid = carid,
    link = link,
    model = make_model,
    variant = variant,
    pris = pris,
    årgang = properties[1],
    km = properties[2],
    detaljer = detaljer,
    beskrivelse = beskrivelse,
    sted = sted,
    forhandlerlogo = forhandlerlogo,
    scrapedate = Sys.Date()
  )
}

#Det virkede sgu
hent_bil(kasser[[2]])

#lad os teste hele siden af

####------ én side bliver til 30 rækker + vi laver også lige log på ------####

baseurl <- "https://www.bilbasen.dk/brugt/bil/mercedes?includeengroscvr=true&includeleasing=false"

hent_side <- function(side_nr) {
  url <- paste0(baseurl, "&page=", side_nr)
  a$go_to(url)  
  Sys.sleep(sample(5:8, 1))  #tid til JavaScript og en tilfældig pause, så vi ikke spammer
  
  html <- a$Runtime$evaluate("document.documentElement.outerHTML")$result$value
  kasser <- read_html(html) %>%
    html_elements("article")
  
  cat("side", side_nr, ":", length(kasser), "biler\n")  #log i konsollen
  
  if (length(kasser) == 0) {
    stop(paste("ingen biler på side :(", side_nr))  #så tryLog logger den, og den havner i fejlede
  }
  
  lapply(kasser, hent_bil) %>%
    bind_rows()
}

#test på 3 sider først, ikke 114
library(dplyr)
df_test <- lapply(1:3, hent_side) %>%
  bind_rows()

#DEN TOG FANME ALLE 90!!!!
nrow(df_test)  

####------ Vil lige kigge efter NA'er inden vi fyrer derudaf ------####
#biler uden pris?
sum(is.na(df_test$pris))  

#biler uden årgang?
sum(is.na(df_test$årgang))

#biler uden km?
sum(is.na(df_test$km))

#Jeg tænker, El biler ser mærkelige ud, lad so tjekke
df_test %>%
  filter(str_detect(detaljer, "\\bEl\\b")) %>%
  select(model, årgang, km, detaljer) %>%
  head() 
#\\b = ordgrænse, så vi kun finder "El" som helt ord og ikke "El" inde i fx Elegance


####------ Alle 114 sider ------####
#tryLog fanger fejl uden at stoppe
library(tryCatchLog)  

#her gemmer vi hver side
liste_sider <- list()  

#her noterer vi sider, der fejler
fejlede <- c()  

for (i in 1:114) {
  resultat <- tryLog(hent_side(i))  #fejler en side, logges den og loopet kører videre
  
  if (inherits(resultat, "try-error")) {
    fejlede <- c(fejlede, i)  #husker hvilken side der fejlede
  } else {
    liste_sider[[i]] <- resultat
  }
  
  saveRDS(liste_sider, "bilbasen_midlertidig.rds")  #sikkerhedskopi efter hver side
}

df_mercedes <- bind_rows(liste_sider) %>%
  distinct(carid, .keep_all = TRUE)  #nye biler skubber andre ned, så samme bil kan stå to gange

fejlede  #sider vi skal prøve igen
nrow(df_mercedes)  #ca. 3.400
colSums(is.na(df_mercedes))  #NA i hver kolonne

saveRDS(df_mercedes, "OLA4/Opgaverne/df_mercedes.rds")  #den færdige dataframe, til opgave 2
write.csv(df_mercedes, "OLA4/Opgaverne/df_mercedes.csv", row.names = FALSE)  #samme data som csv, hvis nogen i gruppen vil åbne den i Excel
