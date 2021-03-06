library(dplyr)

store.compact <- function(infilename, outfilename){
  temp <- read.table(infilename, header = TRUE, sep = ",", stringsAsFactors = FALSE)
  temp <- select(temp, 
                 unique_transaction_id, 
                 dollarsobligated,
                 maj_agency_cat,
                 vendorcountrycode,
                 state,
                 congressionaldistrict,
                 placeofperformancecongressionaldistrict,
                 placeofperformancezipcode) 
  
  write.table(temp, outfilename, 
              append = TRUE, 
              row.names = FALSE, 
              col.names = FALSE,
              sep = ",")
  rm(temp)
}