library(dplyr)

store.compact <- function(filename){
  temp <- read.table(filename, header = TRUE, sep = ",")
  temp <- select(temp, 
                 unique_transaction_id, 
                 dollarsobligated,
                 maj_agency_cat,
                 state,
                 congressionaldistrict) 
  write.table(temp, "ContractSpending.csv", 
              append = TRUE, 
              row.names = FALSE, 
              col.names = FALSE,
              sep = ",")
  rm(temp)
}