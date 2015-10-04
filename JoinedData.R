#source("~/nycdsa/proj_1_bootcamp/CongrDistHistogram.R")
library(stringr)

# by_distr_comm <- inner_join(by_district, congress, by = "DistrictCode") %>%
#                  select(., DistrictCode,Seniority, Total, 8:19) 
# 
# distr.comm.plot <- ggplot(data = by_distr_comm, 
#                           aes(x=Appropriations,y=Total, 
#                               color = Leadership,
#                               size = Leadership)) +
#                    geom_jitter(position = position_jitter(width = 0.3)) +
#                    coord_trans(y = 'log10') +
#                    scale_size_discrete(range = c(2,4)) +
#                    geom_hline(yintercept = median(by_distr_comm$Total),
#                               color = 'black', linetype = 'solid', width = 2) +
#                    facet_grid(.~Seniority)+
#                    theme_bw()

by_distr_agc_comm <- inner_join(by_district_agency, congress, by = "DistrictCode") %>%
                     select(., DistrictCode, Seniority, ShortName, Total, 9:20) 


make.plot <- function(DeptName,CommName){
  plot.data <- filter(by_distr_agc_comm, ShortName == DeptName) 
  comm.bool <- select(plot.data,matches(CommName))[[2]]
  plot.data$Committee <- mapply(function(x,y) if(x) CommName 
                                   else if(y) 'Appropriations'
                                   else 'Other',
                                   comm.bool, 
                                   plot.data$Appropriations)
  plot.data$Committee <- factor(plot.data$Committee, 
                                   levels = c(CommName,'Appropriations', 'Leadership','Other'))
  
  return.plot <- ggplot(data = plot.data, 
                        aes(x = Committee, y = Total, color = Committee), guide = TRUE) +
                 geom_jitter(position = position_jitter(height = 0.2, width = 0.25), 
                             size = 2) +
                 coord_trans(y = 'log10') +
                 scale_size_discrete(range = c(2,4)) +
                 scale_y_continuous(name = "Total Contracts (millions)",
                                    labels = dollar) +
                 ggtitle(str_c('Department: ', DeptName)) +
                 scale_x_discrete(name = NULL, labels = NULL) +
                 geom_hline(yintercept = median(plot.data$Total),
                            color = 'black', linetype = 'solid', width = 2) +
                 ylab(NULL) +
                 theme_bw()
}

defense.comm.plot <- make.plot('Defense','ArmedServices')
agriculture.comm.plot <- make.plot('Agriculture','Agriculture')
veterans.comm.plot <- make.plot('Veterans Affairs','VeteransAffairs')
homeland.comm.plot <- make.plot('Homeland Security','HomelandSecurity')
interior.comm.plot <- make.plot('Interior','NaturalResources')
nasa.comm.plot <- make.plot('NASA','Science')

