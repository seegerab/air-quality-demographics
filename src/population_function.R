library(tidycensus)
library(tidyverse)
### Get the variables for the ACS census; this is different from the decenniel data
census_var_acs <- load_variables(year = 2010, dataset = 'acs5', cache=T)
# All races except for hispanic or latino
census_race <- census_var_acs[grepl("^RACE$", census_var_acs$concept), ]
### as well as "Total!!", which are the different demographic groups
census_race1 <- census_race[grepl("^Estimate!!Total!!", census_race$label), ]
### Include the "Total" population
census_race1 <- rbind(census_race1, census_race%>%filter(label == "Estimate!!Total")) ###
### Create a variable with the race
census_race1$race <- substring(census_race1$label, 18)
### Only include the races alone, and "Two or more races", and the total
census_race1$race[census_race1$race == ""] <- "TotalAll"
### Remove the more specific "Two or more races" variables
census_race1 <- census_race1 %>%
  filter(!race %in% c("Two or more races!!Two races including Some other race", "Two or more races!!Two races excluding Some other race, and three or more races"))
#### Find the total hispanic or latino origin
race_hl <- census_var_acs[grepl("^HISPANIC OR LATINO ORIGIN BY RACE", census_var_acs$concept), ]
total_hl <- race_hl[grepl("^Estimate!!Total$", race_hl$label), ]
### Change the label to be "TotalHispanicLatino", to separate from the total population
total_hl$label <- "TotalHispanicLatino"
total_hl$race <- c("Hispanic or Latino")
all_races <- rbind(census_race1, total_hl) ### 
race_codes <- all_races[,1]
### These are the variables we want to select from the Census API 
race_codes <- as.vector(unlist(race_codes))

population_func <- function(year, state){
  library(tidyverse)
  library(tidycensus)
  ### Error checking for the inputs 
  ### Verify year is a numeric
  if (!is.numeric(year)){
    stop("Year must be a numeric value, not a character.")
  }
  ### Verify that the year is between 2005 and 2021
  if (year < 2005 | year > 2021){
    stop("American Community Survey Data is only available between 2005 and 2020.")
  }
  ### Check that state is a character
  if (!is.character(state)){
    stop("State must be a character.")
  }
  ### State must be a two-letter code, all caps
  if (!state %in% state.abb){
    stop("The state must be a two-letter code, with both letters capitalized.")
  }
  
  ### These are the codes for the race variables; see above for how they were extracted from all variables
  race_var <- c("B02001_002",
                "B02001_003",
                "B02001_004",
                "B02001_005",
                "B02001_006",
                "B02001_007",
                "B02001_008",
                "B02001_001",
                "B03002_001")
  ### Use the acs survey, not the decenniel survey
  data <- get_acs(geography = "county", 
                  variables = c(race_var), 
                  year = year, 
                  state = state,
                  geometry = TRUE)
  ### Create a column with the demographic group
  data$race <- case_when(
    data$variable == "B02001_002" ~ "Total White alone",
    data$variable == "B02001_003" ~ "Total Black or African American alone",
    data$variable == "B02001_004" ~ "Total American Indian and Alaska Native alone",
    data$variable == "B02001_005" ~ "Total Asian alone",
    data$variable == "B02001_006" ~ "Total Native Hawaiian and Other Pacific Islander alone",
    data$variable == "B02001_007" ~ "Total some other race alone",
    data$variable == "B02001_008" ~ "Total two or more races",
    data$variable == "B02001_001" ~ "Total population",
    data$variable == "B03002_001" ~ "Total hispanic latino"
  )
  data$year <- rep(year, nrow(data))
  data$state <- rep(state, nrow(data))
  ### Extract county and state from the NAME variable here
  
  return(data)
  
}
