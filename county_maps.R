#source("LoadPreprocessed.R")
library(stringr)
library(maps)
library(mapproj)
library(dplyr)

data(county.fips) 
county.fips <- county.fips

by_zip <- group_by(contracts, pop_zip) %>%
          summarise(., Total = sum(Dollars.Obligated)) %>%
          filter(str_detect(pop_zip,'[:digit:]{5}')) %>%
          inner_join(.,zip2county, by = c('pop_zip'='ZIPCODE'))

by_county <- group_by(by_zip, GEOID) %>%
             summarise(., Total = sum(Total))
by_county$GEOID <- as.integer(by_county$GEOID)

by_county <- left_join(county.fips, by_county, by = c("fips" = "GEOID"))
by_county$Total[is.na(by_county$Total) | by_county$Total <= 0] <- 1
by_county <- arrange(by_county,fips)
# by_county$mag <- cut(by_county$Total, breaks = c(0,1e4,1e5,1e6,1e7,1e8,1e9,1e11))
by_county$mag <- cut(by_county$Total, breaks = c(0,1e9,1e11))

shades <- colorRampPalette(c("white","black"))(length(unique(by_county$mag )))
fills <- shades[by_county$mag]
map("county", fill = TRUE, col = fills, resolution = 0, lty = 0,
    projection = "polyconic", myborder = 0, mar = c(0,0,0,0))
map("state", col = "black", fill = FALSE, add = TRUE,
    lty = 1, lwd = 1, projection = "polyconic",
    myborder = 0, mar = c(0,0,0,0))

# legend.text <- c('$10,000 or less',
#                  '$10,001 to $100,000',
#                  '$100,001 to $1,000,000',
#                  '$1,000,001 to $10,000,000',
#                  '$10,000,001 to $100,000,000',
#                  '$100,000,001 to $1,000,000,000',
#                  '> $1,000,000,000')
# 
# legend(
#   'bottomleft', cex = 0.4,
#   legend = legend.text,
#        fill = shades,
#        title = 'Value of Federal Contracts')

                 