# ---------- Opg 2.3: Opdater databasen ud fra en simuleret kørsel ----------

library(DBI)
library(RMariaDB)
library(dplyr)

con <- dbConnect(MariaDB(),
                 db = "bilbasen",
                 host = "localhost",
                 user = "root",
                 password = Sys.getenv("MYSQL_PWD"),
                 port = 3306)


# ---------- Trin 1: Byg kørsel II ----------

#kørsel 1 hentes fra databasen: bilerne med deres pris, koblet på carid
#prisid   ikke med, den laver databasen selv
df_kørsel1 <- dbReadTable(con, "biler") %>%
  left_join(dbReadTable(con, "pris"), join_by(carid)) %>%
  select(-prisid)

#set.seed gør, at vi trækker de samme "tilfældige" biler hver gang
set.seed(47)

#kørsel II er scrapet dagen efter
df_kørsel2 <- df_kørsel1 %>%
  mutate(scrapedate = scrapedate + 1)

#5 biler forsvinder = solgt
solgte_id <- sample(df_kørsel2$carid, 5)
df_kørsel2 <- df_kørsel2 %>%
  filter(!carid %in% solgte_id)

#3 biler sættes 10.000 kr. ned
ændrede_id <- sample(df_kørsel2$carid, 3)
df_kørsel2 <- df_kørsel2 %>%
  mutate(pris = ifelse(carid %in% ændrede_id, pris - 10000, pris))

#2 nye biler: kopier af de to første rækker, med et carid der ikke findes
df_nye <- df_kørsel2 %>%
  slice(1:2) %>%
  mutate(carid = c(9999991, 9999992))

df_kørsel2 <- bind_rows(df_kørsel2, df_nye)


# ---------- Tjek kørsel II ----------

#2836 - 5 + 2 = 2833
nrow(df_kørsel2)

#2026-11-18
unique(df_kørsel2$scrapedate)

solgte_id
ændrede_id

# ---------- Trin 2: Sammenlign kørsel II med databasen ----------

#forrige kørsel hentes fra databasen: nyeste pris for hver bil, der ikke er solgt
#JOIN kobler viewet med biler på carid
df_forrige <- dbGetQuery(con, "
  SELECT s.carid, s.pris
  FROM seneste_pris s
  JOIN biler b ON s.carid = b.carid
  WHERE b.sold = FALSE")

#a) nye: i kørsel II, men ikke i databasen
df_nye_biler <- anti_join(df_kørsel2, df_forrige, join_by(carid))

#c) solgte: i databasen, men ikke i kørsel II
df_solgte <- anti_join(df_forrige, df_kørsel2, join_by(carid))

#b) ændrede: i begge, men prisen er forskellig
df_ændrede <- inner_join(df_kørsel2, df_forrige, join_by(carid), suffix = c("_ny", "_gammel")) %>%
  filter(pris_ny != pris_gammel)

#giver 2, 5 og 3
nrow(df_nye_biler)
nrow(df_solgte)
nrow(df_ændrede)

#og det skal være de samme id'er, som vi lavede om i trin 1
df_solgte$carid
df_ændrede %>% select(carid, pris_gammel, pris_ny)


# ---------- Trin 3: Opdatér databasen ----------

#a) nye biler: først ind i biler, så deres første pris ind i pris (fremmednøglen kræver, at bilen findes først)
dbWriteTable(con, "biler",
             df_nye_biler %>%
               select(carid, forhandler_id, link, model, variant, år, km, beskrivelse, email, telefon, sold),
             append = TRUE, row.names = FALSE)

dbWriteTable(con, "pris",
             df_nye_biler %>%
               select(carid, pris, scrapedate),
             append = TRUE, row.names = FALSE)

#b) ændrede priser: en NY række i pris med den nye dato. Den gamle pris bliver liggende, det er tidsserien
dbWriteTable(con, "pris",
             df_ændrede %>%
               select(carid, pris = pris_ny, scrapedate),
             append = TRUE, row.names = FALSE)

#c) solgte biler: slettes ikke, men markeres med sold = TRUE
#paste(collapse = ", ") samler de 5 id'er til én tekst, ligesom i scrapet
dbExecute(con, paste0("UPDATE biler SET sold = TRUE WHERE carid IN (",
                      paste(df_solgte$carid, collapse = ", "), ")"))

# ---------- Tjek at databasen er opdateret ----------

#biler: 2836 + 2 nye = 2838
dbGetQuery(con, "SELECT COUNT(*) FROM biler")

#pris: 2836 + 2 nye + 3 ændrede = 2841
dbGetQuery(con, "SELECT COUNT(*) FROM pris")

#solgte: 5
dbGetQuery(con, "SELECT COUNT(*) FROM biler WHERE sold = TRUE")

#prishistorik for de 3 ændrede: to rækker hver, gammel og ny pris
dbGetQuery(con, paste0("SELECT carid, pris, scrapedate FROM pris WHERE carid IN (",
                       paste(df_ændrede$carid, collapse = ", "), ") ORDER BY carid, scrapedate"))

#viewet viser den nye pris for de 3
dbGetQuery(con, paste0("SELECT * FROM seneste_pris WHERE carid IN (",
                       paste(df_ændrede$carid, collapse = ", "), ")"))

dbDisconnect(con)

