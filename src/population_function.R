##########################################################################
### population_function.R includes code to extract the variable codes for the races 
### we're interested in, as well as the function population_func
###
### population_func takes year (numeric) and state (a two-letter postal code, all capitalized)
### as an input. It returns a dataframe with the percentage of each race for each county for the 
### input year and state. The returned data frame includes a geometry variable, allowing the 
### user to plot the data as a map in the future. 
###
##########################################################################


### Load in the necessary packages
source("packages.R")
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
  ### These are the codes for the race variables; see population_function.R for how these variables were extracted
  race_var <- c("B02001_002",
                "B02001_003",
                "B02001_004",
                "B02001_005",
                "B02001_006",
                "B02001_007",
                "B02001_008",
                "B02001_001",
                "B03002_001")
  ### Use the acs survey, not the decenniel survey, which has data for every year
  data <- get_acs(geography = "county", 
                  variables = c(race_var), 
                  year = year, 
                  state = state,
                  geometry = TRUE)
  ### Create a column with the demographic group
  data$race <- case_when(
    data$variable == "B02001_002" ~ "White",
    data$variable == "B02001_003" ~ "Black or African American",
    data$variable == "B02001_004" ~ "American Indian and Alaska Native",
    data$variable == "B02001_005" ~ "Asian",
    data$variable == "B02001_006" ~ "Native Hawaiian and Other Pacific Islander",
    data$variable == "B02001_007" ~ "Other",
    data$variable == "B02001_008" ~ "Two or more races",
    data$variable == "B02001_001" ~ "Total population",
    data$variable == "B03002_001" ~ "Hispanic latino"
  )
  ### Save the geometry variable as a data frame, along with GEOID and NAME 
  geometry <- as.data.frame(data%>%select(c(geometry, GEOID, NAME)))
  
  data <- as.data.frame(data)
  ### Create a new data frame that is just the total population
  total_population <- data%>%filter(race == "Total population")
  ### Include the total_population as a column, joined by GEOID
  data <- left_join(data, total_population, by = "GEOID")
  data <- data %>%
    select(c(GEOID, estimate.x, estimate.y, race.x))%>%
    ### Create a new variable that is the proportion of each race
    mutate(race_prop = estimate.x / estimate.y)
  colnames(data) <- c("GEOID", "population", "total_population", "race", "race_proportion")
  ### Include the geometry in the data
  data <- merge(data, geometry, by = "GEOID", all.x=TRUE)
  ### Include a variable with the year
  data$year <- rep(year, nrow(data)) 
  ### Filter to a subset of the columns
  data <- data[,c("GEOID", "population.x", "total_population.x", "race.x", "race_proportion.x", "NAME.x", "year")] # take out geometry.x
  colnames(data) <- c("GEOID", "population", "total_population", "race", "race_proportion", "NAME", "year")
  ### Extract the county name
  data$county <- sapply(strsplit(data$NAME, " County"), "[[", 1)
  data <- left_join(data, geometry, by = "GEOID")
  return(distinct(data))
}