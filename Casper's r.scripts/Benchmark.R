#benchmark
install.packages("benchmarkme")
library(benchmarkme)
bm_matrix_cal_manip
get_cpu()
get_ram()
benchmark_matrix_cal()

res=benchmark_matrix_cal()
rescpu=get_cpu()
resrom=get_ram()
rescpu$vendor_id

library(benchmarkme)
library(dplyr)

# --- Kør benchmarken (tager et par minutter) ---
res <- benchmark_std(runs = 3)

# --- Aggreger tiderne for de relevante tests ---
agg <- res %>%
  group_by(test) %>%
  summarise(elapsed = mean(elapsed))

crossprod_tid <- agg$elapsed[agg$test == "cross_product"]
lm_tid        <- agg$elapsed[agg$test == "lm"]
sort_tid      <- agg$elapsed[agg$test == "sort"]

# --- Systeminfo direkte fra benchmarkme ---
cpu <- get_cpu()
ram <- get_ram()

# se hvad get_cpu() faktisk giver dig på din maskine
str(cpu)

resultat <- data.frame(
  Ram          = as.numeric(ram) / 1e9,
  cpuhastighed = "Apple M2, 3.49 GHz",   # slået op manuelt, indsæt dit rigtige tal
  cores        = cpu$no_of_cores,
  crossprod    = crossprod_tid,
  lml          = lm_tid,
  sortl        = sort_tid,
  vendor       = ifelse(length(cpu$vendor_id) == 0, NA, cpu$vendor_id),
  user         = Sys.info()[["user"]]
)

resultat

str(cpu)
length(cpu$model_name)
length(cpu$no_of_cores)
length(cpu$vendor_id)
resultat

system("sysctl -a | grep hw.cpufrequency", intern = TRUE)
sysctl

#her leger vi med rounder og laver det som en funktion

myRounder  <- function(myNumber){
  kurt=0
  kurt=as.integer(myNumber)
  return(kurt) 
}

tal="10,4"
tal=10.4
#typecasting vi laver et tal om til noget andet.
tal2=as.numeric(tal)
typeof(tal2)
mNewNumber=myRounder(tal2)
