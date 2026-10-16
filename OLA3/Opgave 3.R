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
