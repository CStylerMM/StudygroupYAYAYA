library(DBI)

con <- dbConnect(
  RMariaDB::MariaDB(),
  host = "localhost", port = 3306,
  user = "root", password = rstudioapi::askForPassword("MySQL password"),
  dbname = "bilbasen"
)

df_mercedes <- readRDS("~/Dataanalyse/StudygroupYAYAYA/OLA4/Opgaverne/df_mercedes_inkl_cvr.rds")

dbWriteTable(con, "mercedes", df_mercedes, overwrite = TRUE)
dbGetQuery(con, "SELECT COUNT(*) FROM mercedes")
