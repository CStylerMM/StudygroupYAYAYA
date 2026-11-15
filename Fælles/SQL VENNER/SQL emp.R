#worldUeb

library(DBI)


con <- dbConnect(
  RMariaDB::MariaDB(),
  host = "localhost", port = 3306,
  user = "root", password = rstudioapi::askForPassword("MySQL password"),
  dbname = "world"
)


#find de 10 mest urbaniserede lande og de 10 mindst urbaniserede

#ved sql


query='select ct.name, (sum(c.population)/ct.population) as "uc'


dfW <- dbGetQuery(wc,query)
view(dfW)