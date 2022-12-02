### Alternate population function:
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
