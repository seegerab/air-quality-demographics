##########################################################################
### election_functions.R contains two functions: extract_election_house and 
### extract_elections_pres. 
###
### extract_election_house takes year_input (numeric that is an even year between 1976 and 2020)
### and state_input (two-letter postal abbreviation, all capitalized) and returns the percentage of the
### vote DEMOCRAT, REPUBLICAN or OTHER received for each congressional district in the state. The function also adds the 
### geometry of the congressional district, to make it possible to plot the percentage by district
### as a map in the future. The data input to the function can be found at https://electionlab.mit.edu/data,
### under U.S. House 1976 - 2020.
###
### extract_election_pres takes year input (numeric that is a presidential election year between 2000
### and 2004) and state_input (two-letter postal abbreviation, all capitalized) and returns the percentage of the 
### vote DEMOCRAT, REPUBLICAN, or OTHER received for each county in the state. The function also adds the geometry of the 
### county, to make it possible to plot the percentage by county as a map in the future. The data input to the function
### can be found at https://electionlab.mit.edu/data, under U.S. Presidential 2000 - 2020
###
##########################################################################

### Read in the data from U.S. House 1976-2020 from  saved as a .csv file
data <- read.csv("./data/1976-2020-house.csv")
extract_election_house <- function(year_input, state_input){
  ### Load in the necessary packages, sourced from a separate .R script
  source("packages.R")
  ### Error checking: make sure the year is a presidential year
  if (!year_input %in% c(2000, 2004, 2008, 2012, 2016, 2020)){
    stop("Year must be either: 2000, 2004, 2008, 2012, 2016, or 2020")
  }
  ### Make sure the state_input is in the correct form
  if (!state_input %in% state.abb){
    stop("The state must be a two-letter code, with both letters capitalized.")
  }
  ### Bin all parties not DEMOCRAT or REPUBLICAN to OTHER
  data$party <- ifelse(data$party %in% c("DEMOCRAT", "REPUBLICAN"), data$party, "OTHER")
  ### Filter to get the total votes by district
  ### This will be used to find the percent of the vote each party gets later
  total_district <- data%>%
    select(year, state, district, totalvotes)%>%
    filter(year == year_input, state == state_input)%>%
    distinct()
  ### Filter the data to only general elections that aren't
  ### special elections for the U.S. house and only the 
  ### Democratic and Republican parties
  data <- data%>%
    filter(special == "FALSE")%>%
    filter(office == "US HOUSE")%>%
    filter(stage == "GEN")%>%
    filter(year == year_input)%>%
    filter(state == state_input)%>%
    group_by(year, state, district, party)%>%
    summarize(candidatevotes = sum(candidatevotes))
  ### Join this data with the total votes per district (total_district)
  data <- left_join(data, total_district, by = "district")
  ### Create a variable with the percent of the vote each candidate gets
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
  ### Rename state.y to be state and year.y to be year
  colnames(merged_data)[1:2] <- c("year", "state")
  return(merged_data)
}

### Plot house result data from CA in 2020
house_data_CA <- extract_election_house(2020, "CALIFORNIA")
ggplot(data = house_data_CA%>%filter(party == "DEMOCRAT"), aes(geometry = geometry))+
  geom_sf(aes(fill=vote_prop))+
  labs(fill = "Percent of the vote the\nDemocratic candidate recieved", title = "2012 U.S. House of Representatives Results")+
  scale_fill_gradient2(low="red", midpoint = 0.5, high="blue")+
  theme_minimal()

### Read in the county level presidential results from 2000 to 2020
### The data comes from https://electionlab.mit.edu/data 
data <- read.csv("./data/countypres_2000-2020.csv")
extract_election_pres <- function(year_input, state_input){
  ### Error checking: make sure the year is a presidential year
   if (!year_input %in% c(2000, 2004, 2008, 2012, 2016, 2020)){
    stop("Year must be either: 2000, 2004, 2008, 2012, 2016, or 2020")
   }
  ### Make sure the state_input is in the correct form
  if (!state_input %in% state.abb){
    stop("The state must be a two-letter code, with both letters capitalized.")
  }
  ### Create an "OTHER" party that isn't DEMOCRAT or REPUBLICAN
  data$party <- ifelse(data$party %in% c("DEMOCRAT", "REPUBLICAN"), data$party, "OTHER")
  ### Filter the data to the correct year and state
  data <- data%>%
    filter(state_po == state_input)%>%
    filter(year == year_input)%>%
    select(c(year, state_po, county_name, party, candidatevotes, totalvotes))%>%
    ### Find the proportion of the vote each party got
    mutate(vote_percent = candidatevotes / totalvotes)%>%
    group_by(year, state_po, county_name, party)%>%
    ### Add up the percentage by party (this adds up all the OTHER votes)
    summarize(vote_percent = sum(vote_percent))
  ### Convert DEMOCRAT to Democrat, etc, for the three party categories
  data$county_name <- str_to_title(data$county_name)
  return(data)
}



