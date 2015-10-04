library(stringr)
library(dplyr)

cstest <- readRDS("contracts_raw.rds")

# Create a mapping between agency codes and agency names, as implied by the contract data
# Each name appears twice in the raw files, so we pick the version that includes lowercase letters
agency_codes <- unique(cstest$V3)
lower <- agency_codes[str_detect(agency_codes,'[:lower:]+')]
lower <- array(lower)
agency_codes <- data.frame(id = str_sub(lower,1,4), Agency = str_sub(lower,7))
source("ShortAgencyName.R")
for(code in agency_codes$id){
  agency_codes$ShortName[agency_codes$id==code] <- Short.Agency.Names(code)
}
agency_codes$id <- as.character(agency_codes$id)
saveRDS(agency_codes,"agency_codes.rds")
rm(lower, agency_codes, code, Short.Agency.Names)


# Various cleaning tasks for contract data

# Create new field containing agency code with no title
cstest <- mutate(cstest, Agency.Code = str_sub(V3,1,4)) 
# Add field names that were omitted during storage
colnames(cstest) <- c('ID', 'Dollars.Obligated', 'oldagnc', 'country','State', 
                      'Congress.District', 'pop_cd','pop_zip','Agency.Code')

# Filter out non-state juridictions, non-existent district codes
cstest <- filter(cstest, !(str_sub(pop_cd,1,2) %in% c('PR','VI','GU','DC','AS','MP'))) %>%
          filter(., !(pop_cd == 'CA00' | pop_cd == 'WV00')) 

# Select fields needed downstream, and shorten zip to 5 digits (as used in county mapping)
cstest <-select(cstest, ID, Agency.Code, Dollars.Obligated, State, pop_cd, pop_zip)
cstest$Agency.Code <- as.character(cstest$Agency.Code )
cstest$pop_zip <- sapply(cstest$pop_zip, function(x) str_sub(x,1,5))
saveRDS(cstest,"contracts.rds")

rm(cstest)