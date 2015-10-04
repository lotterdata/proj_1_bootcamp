library(stringr)
library(XML)
library(dplyr)

url <- "http://pressgallery.house.gov/member-data/seniority"
SeniorData <- readHTMLTable(url, header = TRUE, stringsAsFactors = FALSE)[[1]] %>%
              select(., 2:4)
colnames(SeniorData) <- c('Member','PartyAndState','Terms')

get.state.senior <- function(ps){
  temp <- str_split(ps,",")
  return(temp[[1]][2])
}

get.last.name.senior <- function(memb){
  temp <- str_split(memb," ")[[1]][-1]
  temp <- ifelse(str_detect(temp,'\\*'),str_sub(temp,1,-2),temp)
  return(temp[[1]])
}

get.first.name.senior <- function(memb){
  temp <- str_split(memb," ")[[1]][1]
  temp <- ifelse(str_detect(temp,'\\*'),str_sub(temp,1,-2),temp)
  return(temp[[1]])
}

SeniorData$FirstName = sapply(SeniorData$Member,get.first.name.senior)
SeniorData$LastName = sapply(SeniorData$Member,get.last.name.senior)
SeniorData$State = sapply(SeniorData$PartyAndState,get.state.senior)

SeniorData <- select(SeniorData, Member, FirstName, LastName, State, Terms)
              
              


