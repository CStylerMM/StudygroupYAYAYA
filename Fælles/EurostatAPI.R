library(tidyverse)
#installer først
library(eurostat)
search <- search_eurostat("population", type = "table")
elderly_pop <- get_eurostat("tps00028",filter = list(geo = "AT")) 
#"tps bla bla er selve Proportion of population aged 65 and over og list bruger vi til at sige
#kun østrig eller AT

#nu vil vi have en figur der viser populationen over de 65 og så med en x og y værdi
#kigger du i elderly_pop kan du se vi har nogle kolonner. som f.eks time og values

#dem tager vi ud med select og laver et ggplot med.
elderly_pop %>% select(time, values) %>% ggplot(aes(x=time, y=values))+
  geom_line()
#nu laver vi noget med eustat kilde omkring unge møder (teen moms)
library(restatapi)
library(dplyr)

#retrieval - external
allTabs <- get_eurostat_toc()

#search for variables in title
colnames(allTabs)
sel = allTabs %>% select(c(title,code)) %>% filter(str_detect(title, "birth")) %>%
  filter(str_detect(title,"mother"))
# vi bruger så demo_r_fagec3 efter at kigge på sel 

#meta data på
youngmomsmeta <- get_eurostat_dsd("demo_r_fagec3")
unique(youngmomsmeta$concept)         #Hvad er der?
#"freq" "age"  "unit" "geo" 

