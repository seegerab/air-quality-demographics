### Read in the data from U.S. House 1976-2020 from https://electionlab.mit.edu/data
data <- read.csv("1976-2020-house.csv")
extract_election <- function(year_input, state_input){
  ### Load in necessary packages
  library(tidyverse)
  library(sf)
  library(tigris)
  ### Filter the data to only general elections that aren't
  ### special elections for the U.S. house and only the 
  ### Democratic and Republican parties
  data <- data%>%
    filter(party %in% c("DEMOCRAT", "REPUBLICAN"))%>%
    filter(special == "FALSE")%>%
    filter(office == "US HOUSE")%>%
    filter(stage == "GEN")%>%
    filter(year == year_input)%>%
    filter(state == state_input)%>%
    mutate(vote_prop = candidatevotes / totalvotes)%>%
    select(c(year, state, district, party, vote_prop))
  ### Add a column with the shape information
  geometry <- congressional_districts(state = toupper(substr(state_input, 1, 2)), year = year_input)%>%
    select(geometry, NAMELSAD)
  ### Extract district from the full district name
  geometry$district <- as.integer(sapply(strsplit(geometry$NAMELSAD, "Congressional District "), "[[", 2))
  merged_data <- left_join(data, geometry, by = "district")
  merged_data <- merged_data%>%
    filter(party == "DEMOCRAT")
  ### Should figure out a better way to visualize the data...
  return(merged_data)
}

### Test the function
test <- extract_election(2014, "OREGON")

ggplot(data = test, aes(geometry = geometry))+
  geom_sf(aes(fill=vote_prop))+
  labs(fill = "Percent of the vote the\nDemocratic candidate recieved", title = "2012 U.S. House of Representatives Results")+
  scale_fill_gradient2(low="red", midpoint = 0.5, high="blue")+
  theme_minimal()
