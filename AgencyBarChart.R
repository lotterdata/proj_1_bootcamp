library(ggplot2)
library(dplyr)
library(scales)
#source("LoadPreprocessed.R")


by_agency.plot.data <- inner_join(contracts,agencies, by = c("Agency.Code" = "id")) %>%
                       group_by(.,ShortName) %>%
                       summarise(., Total = sum(Dollars.Obligated)/1e9) %>%
                       filter(., Total > 10)

agency.plot <- ggplot(aes(x=reorder(ShortName,Total), y=Total), data = 
                        by_agency.plot.data) +
               geom_bar(stat = "identity") +
               scale_x_discrete(name = "Agency") +
               scale_y_continuous(name = "Total Contracts (billions)",
                                  labels = dollar)

