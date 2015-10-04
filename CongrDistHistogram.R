library(ggplot2)
library(dplyr)
library(scales)
source("LoadPreprocessed.R")

by_district <- group_by(contracts, pop_cd) %>%
               summarise(., Total = sum(Dollars.Obligated)/1e6) %>%
               rename(., DistrictCode = pop_cd) %>%
               semi_join(.,congress, by = "DistrictCode")
by_district$Total <- sapply(by_district$Total, function(x) ifelse(x<1,1,x))

district.plot <- ggplot(aes(x=Total), data = by_district) +
                 geom_histogram() +
                 scale_x_continuous(name = "Total Contracts (millions)",
                     labels = dollar) +
                 ggtitle('Counts of Congressional Districts by Total Contract Values') +
                 ylab('') +
                 theme_bw()
  
by_district_agency <- inner_join(contracts,agencies, by = c("Agency.Code" = "id")) %>%
                      group_by(., pop_cd, ShortName) %>%
                      summarise(., Total = sum(Dollars.Obligated)/1e6) %>%
                      rename(., DistrictCode = pop_cd) %>%
                      semi_join(.,congress, by = 'DistrictCode')
by_district_agency$Total <- sapply(by_district_agency$Total,  function(x) ifelse(x<1,1,x))

defense.plot <- ggplot(data=filter(by_district_agency,ShortName=='Defense'),
                       aes(x=Total)) +
                geom_histogram(binwidth = 100) +
                scale_x_continuous(name = "Total Contracts (millions)",
                     labels = dollar) +
                ggtitle('Counts of Congressional Districts by Total Defense Contract Values') +
                ylab('') +
                theme_bw()

by_district_agency_other <- filter(by_district_agency, 
                                   ShortName %in% c('NASA', 'Veterans Affairs',
                                                    'HHS', 'Energy'))

other.plot <- ggplot(data = by_district_agency_other, aes(x = Total)) +
              facet_grid(ShortName~.) +
              geom_histogram(binwidth = 10) +
              scale_x_continuous(name = "Total Contracts (millions)",
                                 labels = dollar) +
              ggtitle('Counts of Congressional Districts by Total Contract Values') +
              ylab('') +
              theme_bw()

other.plot.zoom <- other.plot + coord_cartesian(xlim = c(0,250))


                      


