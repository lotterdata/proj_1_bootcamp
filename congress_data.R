library(stringr)
library(XML)
library(dplyr)

url <- "http://clerk.house.gov/committee_info/oal.aspx"
RepData <- readHTMLTable(url, header = TRUE, stringsAsFactors = FALSE) %>% 
           data.frame() %>% tbl_df()
colnames(RepData) <- c("Member", "Committees")

get.last.name <- function(member_string){
  temp <- str_split(member_string, ",")
  return(temp[[1]][1]) 
}

get.first.name <- function(member_string){
  temp <- str_split(member_string, ",")
  temp <- temp[[1]][2]
  temp <- str_split(str_sub(temp, start = 2)," ")
  return(temp[[1]][1])
  return(temp)
}

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

get.code <- function(state, district){
  if(str_length(district) == 1)
    return(str_c(state,'0',district))
  else
    return(str_c(state,district))
}

RepData$FirstName <- sapply(RepData$Member, get.first.name)
RepData$LastName <- sapply(RepData$Member, get.last.name)
RepData$State <- sapply(RepData$Member, get.state)
RepData$District <- sapply(RepData$Member, get.district)
RepData$DistrictCode <- mapply(get.code, RepData$State, RepData$District) %>%
                        as.character(.)




url <- "http://pressgallery.house.gov/member-data/seniority"
SeniorData <- readHTMLTable(url, header = TRUE, stringsAsFactors = FALSE)[[1]] %>%
  select(., 2:4)
colnames(SeniorData) <- c('Member','PartyAndState','Terms')

get.state.senior <- function(ps){
  temp <- str_split(ps,",")
  return(str_sub(temp[[1]][2],2,3))
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

SeniorData <- select(SeniorData, FirstName, LastName, State, Terms)


RepData <- filter(RepData, !(State %in% c('PR','VI','GU','DC','AS','MP')))
LeftData <- left_join(RepData, SeniorData, by = c('State','FirstName','LastName'))
MissingLeft <- filter(LeftData,is.na(Terms))
FixerByLast <- left_join(MissingLeft,SeniorData, by = c('State','LastName')) %>%
               select(.,DistrictCode,Terms.y) %>%
               filter(.,!is.na(Terms.y))
for(d in FixerByLast$DistrictCode){
  LeftData$Terms[LeftData$DistrictCode == d] <- FixerByLast$Terms.y[FixerByLast$DistrictCode == d]
} 
MissingLeft <- filter(LeftData,is.na(Terms))
FixerByFirst <- left_join(MissingLeft,SeniorData, by = c('State','FirstName')) %>%
  select(.,DistrictCode,Terms.y) %>%
  filter(.,!is.na(Terms.y)) %>%
  filter(., !(DistrictCode=='NJ10' & Terms.y !=3))
for(d in FixerByFirst$DistrictCode){
  LeftData$Terms[LeftData$DistrictCode == d] <- FixerByFirst$Terms.y[FixerByFirst$DistrictCode == d]
}
LeftData$Terms[LeftData$DistrictCode == 'MO01'] <- 8
LeftData$Terms[LeftData$DistrictCode == 'WA03'] <- 3
LeftData$Terms <- as.integer(LeftData$Terms)

RepData <- inner_join(RepData, LeftData, by = "DistrictCode") %>%
           select(., ends_with('x'), DistrictCode, Terms) %>%
           select(., Member = Member.x, Committees = Committees.x, State = State.x, DistrictCode, Terms)
RepData$Seniority <- cut(RepData$Terms,
                         c(0,1,3,7,30),
                         c('1st Term', '2 to 3 Terms', '4 to 7 Terms', '8 or More Term'),
                         include.lowest = FALSE)

rm(SeniorData, FixerByFirst, FixerByLast, LeftData, MissingLeft, d, url)
rm(get.code, get.district, 
   get.first.name, get.first.name.senior, 
   get.last.name, get.last.name.senior,
   get.state,
   get.state.senior)


RepData <- mutate(RepData, Agriculture = str_detect(Committees,'Agriculture')) %>%
           mutate(., Appropriations = str_detect(Committees,'Appropriations')) %>%
           mutate(., ArmedServices = str_detect(Committees,'Armed Services')) %>%
           mutate(., Education = str_detect(Committees,'Education')) %>%
           mutate(., EnergyCommerce = str_detect(Committees,'Energy and')) %>%
           mutate(., HomelandSecurity = str_detect(Committees,'Homeland')) %>%
           mutate(., NaturalResources = str_detect(Committees,'Natural')) %>%
           mutate(., ScienceSpaceTechnology = str_detect(Committees,'Science')) %>%
           mutate(., SmallBusiness = str_detect(Committees,'Small Business')) %>%
           mutate(., Transportation = str_detect(Committees,'Transportation')) %>%
           mutate(., VeteransAffairs = str_detect(Committees,'Veterans')) %>%
           mutate(., Leadership = str_detect(Member,'Boehner.|Pelosi.|McCarthy.|Hoyer.|Clyburn.|Scalise.')) 

write.table(RepData, "CongressData.csv", row.names = FALSE, sep = ',')
saveRDS(RepData,"CongressData.rds")

rm(RepData)