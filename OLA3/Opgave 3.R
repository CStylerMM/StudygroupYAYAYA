#Opgave 3
library(dkstat)
#vi starter opg 3.1 med at hive forbrugsgoder ind via eurostat api
forbrugsgoder_meta <- dst_meta(table = "NKHC021", lang = "da")

#de 8 spørgsmål der indgår i vores mikro er  (F2,F3,F9,F10,F11,F12,F13,)
unique(forbrugsgoder_meta$variables)
forbrugsgoder_meta$values$FORMAAAL
forbrugsgoder_meta$values$PRISENHED
forbrugsgoder_meta$values$SÆSON


forbrugsgoder_query <- list(
  FORMAAAL  = "*",
  PRISENHED = "Løbende priser",
  SÆSON     = "Ikke sæsonkorrigeret",
  Tid       = "*"
)

forbrugsgoder_mikro <- dst_get_data(
  table     = "NKHC021",
  query     = forbrugsgoder_query,
  meta_data = forbrugsgoder_meta,
  lang      = "da"
)

#lad os sige fra 2000 og op efter. så det passer tidligere datasæt
forbrugsgoder_mikro <- forbrugsgoder_mikro[forbrugsgoder_mikro$TID >= as.Date("2000-01-01"),]
sum(is.na(forbrugsgoder_mikro))  #har vi nogle NA'er? - næij

#vi reshaper dataframe med baseR
rm(formaal)
# start med tidskolonnen fra første formål
forbrugsgoder_wide <- data.frame(TID = forbrugsgoder_mikro$TID[forbrugsgoder_mikro$FORMAAAL == forbrugsgoder_mikro$FORMAAAL[1]])
names(forbrugsgoder_mikro)
#laver loop så den sætter værdien under kolonnerne formaaal
for (f in unique(forbrugsgoder_mikro$FORMAAAL)) {
  forbrugsgoder_wide[[f]] <- forbrugsgoder_mikro$value[forbrugsgoder_mikro$FORMAAAL == f]
}

#lad os kigge på overall år til år hvilket forbrugdgode fører
#vi har ikke brug for "i alt" til dette, så vi laver et nyt df
forbrugsgoder_år1 <- forbrugsgoder_wide[, names(forbrugsgoder_wide) != "CPT I alt"]

#så laver vi kvartaler om til et år jubii
forbrugsgoder_år1$År <- as.numeric(format(forbrugsgoder_år1$TID, "%Y"))

#vi sætter lige "År" ved siden af "TID"
forbrugsgoder_år1 <- forbrugsgoder_år1[, c(1, 17, 2:16)]

#vi skal fjerne "TID" eller driller aggregate.

uden_tid <- forbrugsgoder_år1[, names(forbrugsgoder_år1) != "TID"]
#aggregate time yeah baby
forbrugsgoder_år2 <- aggregate(x   = uden_tid[, names(uden_tid) != "År"],
                               by  = list(År = uden_tid$År),
                               FUN = sum)

# fjerner 2026 pga vi ikke færdige med 2026 endnu :(
forbrugsgoder_år2 <- forbrugsgoder_år2[forbrugsgoder_år2$År < 2026, ]

#yay plot tid yay

library(ggplot2)

plotdata <- data.frame(
  formaal = sub("^CP[A-Z] ", "", names(forbrugsgoder_år2)[-1]),   # fjerner koden "CPA " osv.
  total   = colSums(forbrugsgoder_år2[, -1]) / 1000               # fra mio. til mia. kr.
)

ggplot(plotdata, aes(x = reorder(formaal, total), y = total)) +
  geom_col(fill = "hotpink") +
  geom_text(aes(label = round(total)), hjust = 1.2, size = 4.5, color = "white") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title    = "Hvad bruger danskerne penge på?",
       subtitle = "Samlet privatforbrug 2000-2025 fordelt på formål",
       x        = NULL,
       y        = "Mia. kr., løbende priser",
caption  = "Kilde: Danmarks Statistik, NKHC021") +
  theme_minimal()

#her kan vi se det generelle billede af hvad danskerne bruger deres penge på^
#lad os finde ud af hvad de bruger i K4
# 1. Find rækkerne fra 4. kvartal (oktober)
K4 <- forbrugsgoder_år1[format(forbrugsgoder_år1$TID, "%m") == "10" & forbrugsgoder_år1$År < 2026, ]

# 2. Behold kun tallene (fjern TID og År)
K4_rental <- K4[, !(names(K4) %in% c("TID", "År"))]

# 3. Samme for hele året
år_rental <- forbrugsgoder_år2[, names(forbrugsgoder_år2) != "År"]

# 4. Andel af forbruget, i procent
K4_pct <- colSums(K4_rental) / sum(colSums(K4_rental)) * 100   #  K4
år_pct <- colSums(år_rental) / sum(colSums(år_rental)) * 100   # hele året

#lad os kigge på det
K4medårdata <- data.frame(
  formål  = sub("^CP[A-Z] ", "", names(K4_pct)),
  forskel = K4_pct - år_pct
)

ggplot(K4medårdata, aes(x = reorder(formål, forskel), y = forskel, fill = forskel > 0)) +
  geom_col() +
  geom_text(aes(label = round(forskel, 1), hjust = ifelse(forskel > 0, 1.2, -1.)),
            color = "white", size = 3.5) +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "hotpink", "FALSE" = "grey40"), guide = "none") +
  scale_y_continuous(expand = expansion(mult = 0.1)) +
  labs(title    = "Hvilke forbrugsgoder fylder mest i 4. kvartal?",
       subtitle = "Andel af forbruget i K4 minus andel i hele året, 2000-2025 (procentpoint)",
       x        = NULL,
       y        = "Procentpoint over/under årsgennemsnittet",
       caption  = "Kilde: Danmarks Statistik, NKHC021") +
  theme_minimal()
#niiiice
# hvor meget højere/lavere er K4 end et gennemsnitligt kvartal, pr. forbrugsgode
K4_udsving <- colMeans(as.matrix(K4_rental) / (as.matrix(år_rental) / 4))
sort(K4_udsving, decreasing = TRUE)   # kig på tallene


K4_udsving_data <- data.frame(
  formål  = sub("^CP[A-Z] ", "", names(K4_udsving)),
  udsving = (K4_udsving-1)*100
)

ggplot(K4_udsving_data, aes(x = reorder(formål, udsving), y = udsving)) +
  geom_col(fill = "hotpink") +
  geom_text(aes(label = round(udsving, 1)), hjust = -0.2, size = 3.5) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title    = "Hvor meget højere er forbruget i 4. kvartal end i et gennemsnitligt kvartal?",
       subtitle = "Gennemsnit 2000-2025, procent over et normalt kvartal",
       x        = NULL,
       y        = "Procent over gennemsnitligt kvartal",
       caption  = "Kilde: Danmarks Statistik, NKHC021") +
  theme_minimal()
#waooow elektricitet og tøj fører med 19,5 % og 17 %.
#hvorfor bruger danskerne mest på elektricitet - kan det være der er røvkoldt om vinteren??
#burde man subsette de her goder måske - og kigge på hvilke kunne være indenfor julehandel.
#Ja måske hvem ved.


