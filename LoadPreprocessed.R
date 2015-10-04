library(dplyr)

contracts <- readRDS("contracts.rds") %>% tbl_df()
agencies <- readRDS("agency_codes.rds") %>% tbl_df()
congress <- readRDS("CongressData.rds") %>% tbl_df()
#congress <- read.csv("CongressData.csv", stringsAsFactors = FALSE) %>% tbl_df()

zip2county <- readRDS("zip2county.rds")