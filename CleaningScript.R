library(stringr)
library(dplyr)

cd_code <- function(state, district){
  if(str_length(as.character(district) < 2))
    str_c(as.character(state),'0',as.character(district))
  else
    str_c(as.character(state),as.character(district))
  }

Vectorize(cd_code)

cstest <- readRDS("contracts_raw.rds")

agency_codes <- unique(cstest$V3)
lower <- agency_codes[str_detect(agency_codes,'[:lower:]+')]
lower <- array(lower)
agency_codes <- data.frame(id = str_sub(lower,1,4), Agency = str_sub(lower,7))
source("ShortAgencyName.R")
for(code in agency_codes$id){
  agency_codes$ShortName[agency_codes$id==code] <- Short.Agency.Names(code)
}

write.table(agency_codes,
            "agency_codes.csv", 
            row.names = FALSE,
            sep = ",")
rm(lower, agency_codes)

cstest <- mutate(cstest, Agency.Code = str_sub(V3,1,4)) %>%
          filter(., str_detect(V4, 'UNITED STATES'))

colnames(cstest) <- c('ID', 'Dollars.Obligated', 'oldagnc', 'country','State', 
                      'Congress.District', 'pop_cd','pop_zip','Agency.Code')

cstest <- filter(cstest, !(str_sub(pop_cd,1,2) %in% c('PR','VI','GU','DC','AS','MP'))) %>%
          filter(., !(pop_cd == 'CA00' | pop_cd == 'WV00')) %>%
          filter(., str_length(Congress.District) != 0 & str_length(State) == 2)

# code <- filter(cstest, pop_cd != '')
# no_code <- filter(cstest, pop_cd == '')
# for(i in seq(1:nrow(no_code))){
#   no_code$pop_cd[i] = cd_code(no_code$State,no_code$Congress.Districr)
# }
# 
# cstest <- rbind(code,no_code)
          

cstest <-select(cstest, ID, Agency.Code, Dollars.Obligated, State, Congress.District, pop_cd, pop_zip)
cstest$Agency.Code <- as.numeric(cstest$Agency.Code )
cstest$pop_zip <- sapply(cstest$pop_zip, function(x) str_sub(x,1,5))
#write.table(cstest, "ContractSpendingClean.csv", row.names = FALSE, sep = ',')
saveRDS(cstest,"contracts.rds")

rm(cstest)