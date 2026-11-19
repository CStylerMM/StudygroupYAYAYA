library(DBI)
library(RMariaDB)
library(dplyr)
library(stringr)
# ---------- Lad os få data ind i databasen ----------

# starter med forbindelse til databasen.
con <- dbConnect(MariaDB(),
                 db = "bilbasen",
                 host = "localhost",
                 user = "root",
                 password = Sys.getenv("MYSQL_PWD"),
                 port = 3306)



# tjek at vi er inde i den rigtige database
dbGetQuery(con, "SELECT DATABASE()")
dbListTables(con)

# ---------- Gør data klar til de tre tabeller ----------

#kør Opg_1.1.R først, så df_mercedes_OLA ligger i environment

#forhandler: én række pr. kombination af logo og sted, og hver får et nummer
#kolonnenavnene skal hedde præcis det samme som i databasen
df_forhandler <- df_mercedes_OLA %>%
  distinct(forhandler_logo, sted) %>%
  mutate(forhandler_id = row_number()) %>%
  rename(logo = forhandler_logo)

#biler: forhandlerens nummer hentes via logo og sted
#carid skal være tal, for den er INT i databasen. str_extract gav os tekst
#\\D betyder alt der ikke er et ciffer, så "126.000 km" bliver til 126000
df_biler <- df_mercedes_OLA %>%
  left_join(df_forhandler, join_by(forhandler_logo == logo, sted)) %>%
  mutate(carid = as.integer(carid),
         år = as.integer(str_extract(årgang, "\\d{4}$")),
         km = as.integer(str_remove_all(km, "\\D")),
         sold = FALSE) %>%
  select(carid, forhandler_id, link, model, variant, år, km, beskrivelse, email, telefon, sold)
names(df_mercedes_OLA)


#pris: prisid laver databasen selv, så den skal ikke med
df_pris <- df_mercedes_OLA %>%
  mutate(carid = as.integer(carid),
         pris = as.integer(pris_kr)) %>%
  select(carid, pris, scrapedate)


# ---------- Tjek før vi sender noget ----------

#skal give ca. 455, 2836 og 2836
nrow(df_forhandler)
nrow(df_biler)
nrow(df_pris)

#carid må ikke være NA og ikke gå igen, for det er primærnøglen. Begge skal give 0
sum(is.na(df_biler$carid))
sum(duplicated(df_biler$carid))

#passer teksten ind i kolonnerne? link, variant og email max 255, sted max 100
max(str_length(df_biler$link), na.rm = TRUE)
max(str_length(df_biler$variant), na.rm = TRUE)
max(str_length(df_biler$email), na.rm = TRUE)
max(str_length(df_forhandler$sted), na.rm = TRUE)

# ---------- Send data til databasen ----------

#append = TRUE betyder, at R putter rækker i de tabeller, vi lavede i 2.1, i stedet for at lave nye
#rækkefølgen er vigtig: forhandler før biler, biler før pris, ellers brokker fremmednøglerne sig
dbWriteTable(con, "forhandler", df_forhandler, append = TRUE, row.names = FALSE)
dbWriteTable(con, "biler", df_biler, append = TRUE, row.names = FALSE)
dbWriteTable(con, "pris", df_pris, append = TRUE, row.names = FALSE)

#tjek: skal give 455, 2836 og 2836
dbGetQuery(con, "SELECT COUNT(*) FROM forhandler")
dbGetQuery(con, "SELECT COUNT(*) FROM biler")
dbGetQuery(con, "SELECT COUNT(*) FROM pris")

#og viewet fra 2.1 skal nu vise den nyeste pris for hver bil
dbGetQuery(con, "SELECT * FROM seneste_pris LIMIT 5")
