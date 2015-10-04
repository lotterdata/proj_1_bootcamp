library(dplyr)
county.data <- read.csv("zcta_county_rel_10.csv", header = TRUE, colClasses = 'character') %>% 
               data.frame() %>% 
               tbl_df() %>% 
               select(ZCTA5, GEOID, ZPOPPCT) %>%
               rename(ZIPCODE = ZCTA5)
county.data$ZPOPPCT <- as.numeric(county.data$ZPOPPCT)

maxpop <- summarise(group_by(county.data, ZIPCODE), mx = max(ZPOPPCT))

county.data <- inner_join(county.data, maxpop, by = 'ZIPCODE') %>%
               filter(., ZPOPPCT ==  mx) %>%
               select(ZIPCODE, GEOID)

saveRDS(county.data, "zip2county.rds")
rm(county.data)
