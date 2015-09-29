library(dplyr)

contracts <- read.csv("ContractSpendingClean.csv", header = TRUE) %>% tbl_df()
agencies <- read.csv("agency_codes.csv", header = TRUE) %>% tbl_df()
congress <- read.csv("CongressData.csv", header = TRUE) %>% tbl_df()