library(stringr)
library(XML)
library(magrittr)
library(dplyr)

url <- "http://clerk.house.gov/committee_info/oal.aspx"
RepData <- readHTMLTable(url, header = TRUE) %>% data.frame() %>% tbl_df()
colnames(RepData) <- c("Member", "Committees")

get.state <- function(member_string){
  temp <- str_split(member_string, " ")
  return(temp[[1]][length(temp[[1]])])
}

get.district <- function(member_string){
  temp <- str_split(member_string, " ")
  text <- temp[[1]][length(temp[[1]])-1]
  if(str_detect(text,'[:digit:]+')){
    return(str_extract(text,'[:digit:]+'))
  }else{
    return('0')
  }
}

RepData$State = sapply(RepData$Member, get.state)
RepData$District = sapply(RepData$Member, get.district)

RepData <- filter(RepData, !(State %in% c('PR','VI','GU','DC','AS','MP'))) %>%
           mutate(., Agriculture = str_detect(Committees,'Agriculture')) %>%
           mutate(., Appropriations = str_detect(Committees,'Appropriations')) %>%
           mutate(., ArmedServices = str_detect(Committees,'Armed Services')) %>%
           mutate(., Education = str_detect(Committees,'Education')) %>%
           mutate(., EnergyCommerce = str_detect(Committees,'Energy and')) %>%
           mutate(., HomelandSecurity = str_detect(Committees,'Homeland')) %>%
           mutate(., NaturalResources = str_detect(Committees,'Natural')) %>%
           mutate(., ScienceSpaceTechnology = str_detect(Committees,'Science')) %>%
           mutate(., SmallBusiness = str_detect(Committees,'Small Business')) %>%
           mutate(., Transportation = str_detect(Committees,'Transportation')) %>%
           mutate(., VeteransAffairs = str_detect(Committees,'Veterans'))