### Read in the data from U.S. House 1976-2020 from https://electionlab.mit.edu/data
data <- read.csv("./data/1976-2020-house.csv")
extract_election_house <- function(year_input, state_input){
  ### Filter the data to only general elections that aren't
  ### special elections for the U.S. house and only the 
  ### Democratic and Republican parties
  source("packages.R")
  ### Bin all parties not DEMOCRAT or REPUBLICAN to OTHER
  data$party <- ifelse(data$party %in% c("DEMOCRAT", "REPUBLICAN"), data$party, "OTHER")
  total_district <- data%>%
    select(year, state, district, totalvotes)%>%
    filter(year == year_input, state == state_input)%>%
    distinct()
  data <- data%>%
    filter(special == "FALSE")%>%
    filter(office == "US HOUSE")%>%
    filter(stage == "GEN")%>%
    filter(year == year_input)%>%
    filter(state == state_input)%>%
    group_by(year, state, district, party)%>%
    summarize(candidatevotes = sum(candidatevotes))
  data <- left_join(data, total_district, by = "district")
  data <- data%>% 
    mutate(vote_prop = candidatevotes / totalvotes)%>%
    select(c(year.x, state.x, district, party, vote_prop, candidatevotes, totalvotes, state.y, year.y))
  ### Add a column with the shape information
  geometry <- congressional_districts(state = toupper(substr(state_input, 1, 2)), year = year_input)%>%
    select(geometry, NAMELSAD)
  ### Extract district from the full district name
  geometry$district <- as.integer(sapply(strsplit(geometry$NAMELSAD, "Congressional District "), "[[", 2))
  merged_data <- left_join(data, geometry, by = "district")%>%
    select(-c(state.y, year.y))
  colnames(merged_data)[1:2] <- c("year", "state")
  ### Should figure out a better way to visualize the data...
  return(merged_data)
}

### Test the function
house_data_CA <- extract_election_house(2020, "CALIFORNIA")

ggplot(data = house_data_CA%>%filter(party == "DEMOCRAT"), aes(geometry = geometry))+
  geom_sf(aes(fill=vote_prop))+
  labs(fill = "Percent of the vote the\nDemocratic candidate recieved", title = "2012 U.S. House of Representatives Results")+
  scale_fill_gradient2(low="red", midpoint = 0.5, high="blue")+
  theme_minimal()


data <- read.csv("./data/countypres_2000-2020.csv")
extract_election_pres <- function(year_input, state_input){
  library(tidyverse)
  library(stringr)
   if (!year_input %in% c(2000, 2004, 2008, 2012, 2016, 2020)){
    stop("Year must be either: 2000, 2004, 2008, 2012, 2016, or 2020")
  }
  if (!state_input %in% state.abb){
    stop("The state must be a two-letter code, with both letters capitalized.")
  }
  data$party <- ifelse(data$party %in% c("DEMOCRAT", "REPUBLICAN"), data$party, "OTHER")
  data <- data%>%
    filter(state_po == state_input)%>%
    filter(year == year_input)%>%
    select(c(year, state_po, county_name, party, candidatevotes, totalvotes))%>%
    mutate(vote_percent = candidatevotes / totalvotes)%>%
    group_by(year, state_po, county_name, party)%>%
    summarize(vote_percent = sum(vote_percent))
  data$county_name <- str_to_title(data$county_name)
  return(data)
}

pres_data_CA <- extract_election_pres(2010, "CA")


