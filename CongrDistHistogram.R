library(ggplot2)
library(dplyr)
library(scales)
source("LoadPreprocessed.R")

by_district <- group_by(contracts, pop_cd) %>%
               summarise(., Total = sum(Dollars.Obligated)/1e6) %>%
               rename(., DistrictCode = pop_cd) %>%
               semi_join(.,congress, by = "DistrictCode")

district.plot <- ggplot(aes(x=Total), data = by_district) +
                 geom_histogram() +
                 scale_x_continuous(name = "Total Contracts (millions)",
                     labels = dollar)

