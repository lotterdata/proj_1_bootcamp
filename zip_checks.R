library(dplyr)

test <- readRDS("contracts.rds")
nrow(test[str_detect(test$pop_zip,'[:alpha:]') & str_length(test$pop_cd)==4,])
nrow(test[str_length(test$pop_zip)!=5 & str_length(test$pop_cd)==4,])