library(stringr)
library(dplyr)

cd_code <- function(state, district, pop_cd){
  if(str_length(pop_cd) == 4)
    return(pop_cd)
  else if(as.numeric(district) < 10)
          return(str_c(State,'0',as.character(district)))
  else
    return(str_c(State,'0',as.character(district)))
}

Vectorize(cd_code)

cstest <- read.csv("ContractSpending.csv", header = FALSE)

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

colnames(cstest) <- c('ID', 'Dollars.Obligated', 'oldagnc', 'country','State', 'Congress.District', 'pop_cd','Agency.Code' )

cstest <- filter(cstest, !(str_sub(pop_cd,1,2) %in% c('PR','VI','GU','DC','AS','MP'))) 
          

cstest <-select(cstest, ID, Agency.Code, Dollars.Obligated, State, Congress.District, pop_cd)
write.table(cstest, "ContractSpendingClean.csv", row.names = FALSE, sep = ',')

rm(cstest)