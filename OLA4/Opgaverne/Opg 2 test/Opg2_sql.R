#Opgave 2 - SQL
#Vi gemmer Mercedes-bilerne fra opgave 1.1 i MySQL.
#Skemaet ligger i sql/bilbasen_schema.sql (skal på github, opgave 2.1)

library(DBI)

con <- dbConnect(
  RMariaDB::MariaDB(),
  host = "localhost", port = 3306,
  user = "root", password = rstudioapi::askForPassword("MySQL password"),
  dbname = "bilbasen"
)

####------ 2.1 Opret skemaet ------####
#Nemmest: åbn sql/bilbasen_schema.sql i DBeaver/Workbench og kør den der.
#Tjek bagefter herfra:
dbListTables(con)  #skal vise bil, forhandler, pris

####------ 2.2 Gem kørsel 1 i databasen ------####
library(dplyr)
library(stringr)

df_mercedes <- readRDS("OLA4/Opgaverne/df_mercedes.rds")  #2995 biler uden CVR

#samme rensning som i opgave 1 - årgang "6/2019" deles op, km bliver et tal
kørsel1 <- df_mercedes %>%
  mutate(
    måned = as.numeric(str_extract(årgang, "^\\d+")),
    år = as.numeric(str_extract(årgang, "\\d{4}$")),
    km = as.numeric(str_remove_all(km, "[^0-9]")),
    carid = as.numeric(carid)
  )

#først forhandlerne, så vi kan slå forhandler_id op bagefter
df_forhandlere <- kørsel1 %>%
  distinct(logo = forhandlerlogo) %>%
  filter(!is.na(logo))

dbExecute(con, "INSERT IGNORE INTO forhandler (logo) VALUES (?)",
          params = list(df_forhandlere$logo))  #IGNORE springer dem over der findes i forvejen

df_forhandler_map <- dbGetQuery(con, "SELECT forhandler_id, logo FROM forhandler")

#så bilerne - med forhandler_id joinet på
df_biler <- kørsel1 %>%
  left_join(df_forhandler_map, by = c("forhandlerlogo" = "logo")) %>%
  transmute(carid, link, model, variant, måned, år, beskrivelse, sted,
            forhandler_id, sold = FALSE)

dbAppendTable(con, "bil", df_biler)  #Append fordi tabellen allerede findes med carid som PK

#og priserne i tidsserie-tabellen
df_priser <- kørsel1 %>%
  transmute(carid, pris, km, scrapedate = as.Date(scrapedate))

dbAppendTable(con, "pris", df_priser)

dbGetQuery(con, "SELECT COUNT(*) FROM bil")  #2995
dbGetQuery(con, "SELECT COUNT(*) FROM pris")  #2995

#INSERT-statements til github (opgaven kræver de ligger der som .sql)
#vi skriver de første 10 som eksempel + en note om at resten kørte via R
eksempler <- head(df_priser, 10)
inserts <- sprintf("INSERT INTO pris (carid, pris, km, scrapedate) VALUES (%d, %d, %d, '%s');",
                   eksempler$carid, eksempler$pris, eksempler$km, eksempler$scrapedate)
writeLines(c("-- OLA4 opgave 2.2: INSERT-statements",
             "-- Alle 2995 biler er indsat fra R med dbAppendTable (se Opg2_sql.R).",
             "-- De første 10 som eksempel på formatet:", "", inserts),
           "OLA4/Opgaverne/sql/bilbasen_inserts.sql")

####------ 2.3 Kørsel-II: simuleret ny scraping ------####
#Krav: scrapedate +1 dag, 2 nye biler, 3 ændrede priser, 5 solgte (mangler)
set.seed(46)  #uge 46 hehe - så får vi de samme "tilfældige" biler hver gang

kørsel2 <- kørsel1
kørsel2$scrapedate <- as.Date(kørsel1$scrapedate[1]) + 1  #én dag senere

solgte_rækker <- sample(nrow(kørsel2), 5)
kørsel2 <- kørsel2[-solgte_rækker, ]  #5 biler er væk = solgt

ændrede_rækker <- sample(nrow(kørsel2), 3)
kørsel2$pris[ændrede_rækker] <- round(kørsel2$pris[ændrede_rækker] * c(0.95, 1.05, 0.90))  #3 nye priser

df_nye_biler <- kørsel1[sample(nrow(kørsel1), 2), ]  #2 "nye" biler, kopier med nyt carid
df_nye_biler$carid <- c(99000001, 99000002)  #fake id'er der ikke findes i forvejen
df_nye_biler$scrapedate <- kørsel2$scrapedate[1]
kørsel2 <- bind_rows(kørsel2, df_nye_biler)

####------ Funktionen der sammenligner og opdaterer ------####
opdater_database <- function(con, ny_kørsel) {

  #hent forrige kørsel fra databasen: seneste pris pr. bil der ikke er solgt
  df_db <- dbGetQuery(con, "
    SELECT b.carid, p.pris
    FROM bil b
    JOIN pris p ON p.carid = b.carid
    WHERE b.sold = FALSE
      AND p.scrapedate = (SELECT MAX(p2.scrapedate) FROM pris p2 WHERE p2.carid = b.carid)")

  dato <- as.Date(ny_kørsel$scrapedate[1])

  #a) nye records: carid i den nye kørsel som databasen ikke kender
  df_nye <- ny_kørsel %>%
    filter(!carid %in% df_db$carid)

  #b) ændrede records: samme carid, men prisen er en anden
  df_ændrede <- ny_kørsel %>%
    inner_join(df_db, by = "carid", suffix = c("", "_gammel")) %>%
    filter(pris != pris_gammel)

  #c) missing records: carid i databasen som den nye kørsel ikke har = solgt
  df_solgte <- df_db %>%
    filter(!carid %in% ny_kørsel$carid)

  #a) nye biler ind i bil-tabellen + deres første pris
  if (nrow(df_nye) > 0) {
    df_map <- dbGetQuery(con, "SELECT forhandler_id, logo FROM forhandler")
    dbAppendTable(con, "bil", df_nye %>%
      left_join(df_map, by = c("forhandlerlogo" = "logo")) %>%
      transmute(carid, link, model, variant, måned, år, beskrivelse, sted,
                forhandler_id, sold = FALSE))
    dbAppendTable(con, "pris", df_nye %>%
      transmute(carid, pris, km, scrapedate = dato))
  }

  #b) ændrede priser: NY række i pris-tabellen - vi overskriver ikke, det er en tidsserie
  if (nrow(df_ændrede) > 0) {
    dbAppendTable(con, "pris", df_ændrede %>%
      transmute(carid, pris, km, scrapedate = dato))
  }

  #c) solgte: markeres, slettes ikke
  if (nrow(df_solgte) > 0) {
    dbExecute(con, "UPDATE bil SET sold = TRUE WHERE carid = ?",
              params = list(df_solgte$carid))
  }

  cat("Nye:", nrow(df_nye), "| Ændrede priser:", nrow(df_ændrede),
      "| Solgte:", nrow(df_solgte), "\n")
}

opdater_database(con, kørsel2)  #forventet: Nye: 2 | Ændrede priser: 3 | Solgte: 5

#tjek at det sidder i skabet
dbGetQuery(con, "SELECT COUNT(*) FROM bil WHERE sold = TRUE")  #5
dbGetQuery(con, "SELECT COUNT(*) FROM bil")  #2997 (2995 + 2 nye)
dbGetQuery(con, "
  SELECT b.carid, p.pris, p.scrapedate
  FROM bil b
  JOIN pris p ON p.carid = b.carid
  WHERE b.carid IN (SELECT carid FROM pris GROUP BY carid HAVING COUNT(*) > 1)
  ORDER BY b.carid, p.scrapedate")  #de 3 biler med to priser - prishistorikken virker!

dbDisconnect(con)
