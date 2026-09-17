#Installer og aktiver pakken
install.packages("benchmarkme")
install.packages("benchmarkmeData")
library("benchmarkme")
library("benchmarkmeData")

#Skal lige finde ud af hvad det betyder...
bm_matrix_cal_manip

#Måle hvor god computeren er
res=benchmark_matrix_cal()

rescpu=get_cpu()
resram=get_ram()
rescpu$vendor_id

# *Pro tip, :: efter en pakke, viser alle pakkens funktioner --> benchmarkme::

library(benchmarkme)
library(dplyr)

# Kør benchmarken (tager et par minutter)
  res <- benchmark_std(runs = 3)

#Aggreger tiderne for de relevante tests
  agg <- res %>%
  group_by(test) %>%
  summarise(elapsed = mean(elapsed))

crossprod_tid <- agg$elapsed[agg$test == "cross_product"]
lm_tid        <- agg$elapsed[agg$test == "lm"]
sort_tid      <- agg$elapsed[agg$test == "sort"]

#Systeminfo direkte fra benchmarkme
  cpu <- get_cpu()
ram <- get_ram()

#se hvad get_cpu() faktisk giver dig på din maskine
str(cpu)

resultat <- data.frame(
  Ram          = as.numeric(ram) / 1e9,   # bytes -> GB
  cpuhastighed = cpu$model_name,           # eneste sted clockhastigheden gemmer sig som tekst
  cores        = cpu$no_of_cores,
  crossprod    = crossprod_tid,
  lml          = lm_tid,
  sortl        = sort_tid,
  vendor       = cpu$vendor_id,
  user         = Sys.info()[["user"]]      # kommer fra Sys.info(), ikke fra selve benchmarken
)

resultat

#Nyyyyt
res <- benchmark_std(runs = 3)

#Aggreger tiderne for de relevante tests
  agg <- res %>%
  group_by(test) %>%
  summarise(elapsed = mean(elapsed))

crossprod_tid <- agg$elapsed[agg$test == "cross_product"]
lm_tid        <- agg$elapsed[agg$test == "lm"]
sort_tid      <- agg$elapsed[agg$test == "sort"]

#Systeminfo direkte fra benchmarkme
  cpu <- get_cpu()
ram <- get_ram()

#se hvad get_cpu() faktisk giver dig på din maskine
str(cpu)

resultat <- data.frame(
  Ram          = as.numeric(ram) / 1e9,
  cpuhastighed = cpu$model_name,
  cores        = cpu$no_of_cores,
  crossprod    = crossprod_tid,
  lml          = lm_tid,
  sortl        = sort_tid,
  vendor       = ifelse(length(cpu$vendor_id) == 0, NA, cpu$vendor_id),
  user         = Sys.info()[["user"]]
)

resultat


#Igen...
res <- benchmark_std(runs = 3)

#Aggreger tiderne for de relevante tests
  agg <- res %>%
  group_by(test) %>%
  summarise(elapsed = mean(elapsed))

crossprod_tid <- agg$elapsed[agg$test == "cross_product"]
lm_tid        <- agg$elapsed[agg$test == "lm"]
sort_tid      <- agg$elapsed[agg$test == "sort"]

#Systeminfo direkte fra benchmarkme
  cpu <- get_cpu()
ram <- get_ram()

#se hvad get_cpu() faktisk giver dig på din maskine
str(cpu)

resultat <- data.frame(
  Ram          = as.numeric(ram) / 1e9,
  cpuhastighed = cpu$model_name,
  cores        = cpu$no_of_cores,
  crossprod    = crossprod_tid,
  lml          = lm_tid,
  sortl        = sort_tid,
  vendor       = ifelse(length(cpu$vendor_id) == 0, NA, cpu$vendor_id),
  user         = Sys.info()[["user"]]
)

resultat
