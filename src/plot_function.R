##########################################################################
### plot_function.R includes the code plot_function
###
### plot_function takes year (numeric that is 2012, 2016, or 2020), state (a two-letter postal code, all capitalized)
### email used for the EPA API, and the key used for the EPA API. plot_function.R
### includes three separate functions - population_func, pollution_function, and extract_election_pres. See the 
### three separate scripts (population_function.R, epa_function.R, and election_functions.R) for more description
### of each of these functions.
###
### The function outputs three data frames as a list with three elements, each
### of which have the same year and state that the user inputs. 
###
##########################################################################
plot_function <- function(year, state, email = "seegerab@umich.edu", key = "greengazelle94"){
  ### Source the necessary packages
  source("packages.R")
  ### Error checking: year must be numeric
  if (!is.numeric(year)){
    stop("Year must be a numeric value, not a character.")
  }
  ### Verify that the year is a presidential election year between 2012 and 2020
  if (!year %in% c(2012, 2016, 2020)){
    stop("Year must be either 2012, 2016, or 2020.")
  }
  ### Check that state is a character
  if (!is.character(state)){
    stop("State must be a character.")
  }
  ### State must be a two-letter code, all caps
  if (!state %in% state.abb){
    stop("The state must be a two-letter code, with both letters capitalized.")
  }
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
  
  pollution_function <- function(year,state){
    ### Match state input with the FIPs code, which is required for the pollution function
      if (state == "CA"){
        state_code <- "06"
      }
      if (state == "OR"){
        state_code <- "41"
      }
      if (state == "WA"){
        state_code <- "53"
      }
      all_pollutants = c("ozone","so2","co","no2","pm25","pm10")
    ### Initialize an empty data frame with 61 columns
      all_data <- data.frame(matrix(nrow = 0, ncol = 61))
      ### For each pollutant in all_pollutants
      for (pollutant in all_pollutants){
        ### Find the parameter code for the pollutant 
        if(pollutant=="ozone"){param="44201"}
        if(pollutant=="so2"){param="42401"}
        if(pollutant=="co"){param="42101"}
        if(pollutant=="no2"){param="42602"}
        if(pollutant=="pm25"){param="88502"}
        if(pollutant=="pm10"){param="81102"}
        ### Extract the pollution data from the api
        URL <- paste0("https://aqs.epa.gov/data/api/annualData/byState?",
                      "email=", email,"&",
                      "key=", key ,"&",
                      "param=", param,"&",
                      "bdate=", year,"0101&",
                      "edate=", year,"1231&",
                      "state=", state_code)
        ### Extract data from the URL 
        partial_data <- data.frame(fromJSON(URL), row.names = NULL)
        ### If we're at the first parameter, rename the columns of the empty data
        ### frame to match what was extracted from the api
        if (param == "44201"){
          colnames(all_data) <- colnames(partial_data)}
        ### Include a column with the pollutant
        partial_data$pollutant <- rep(pollutant, nrow(partial_data))
        ### Add the specific pollutant data to all data
        all_data <- rbind(all_data, partial_data, row.names = NULL)

      }
        ### Remove the first four columns
        df <- subset(all_data, select = c(5:ncol(all_data)))
        ### Remove Data. from the column names
        colnames(df) <- gsub("Data.", "", colnames(df), fixed=TRUE)
        ### Find the mean pollutant concentration by county, year, and pollutant
        df <- df%>%
          group_by(county, year, pollutant)%>%
          summarize(mean_conc = mean(arithmetic_mean))%>%
          select(county, mean_conc, year, pollutant)%>%
          distinct()
        return(df)
  }
  ### Read in the presidential election data by county
  data <- read.csv("/data/countypres_2000-2020.csv")
  extract_election_pres <- function(year, state){
    ### If the party isn't DEMOCRAT or REPUBLICAN, rename the party to be OTHER 
    data$party <- ifelse(data$party %in% c("DEMOCRAT", "REPUBLICAN"), data$party, "OTHER")
    data <- data%>%
      ### Remove the state variable from the data
      select(-c(state))%>%
      ### Filter so that the state_po (state postal code) matches the state input and year matches the input year
      filter(state_po == state)%>%
      filter(year == year)%>%
      ### Select only the necessary columns
      select(c(year, state_po, county_name, party, candidatevotes, totalvotes))%>%
      ### Create a variable that is the percent of votes by party
      mutate(vote_percent = candidatevotes / totalvotes)%>%
      group_by(year, state_po, county_name, party)%>%
      ### Add up the percent of votes for each county (only applicable ot the OTHER category)
      summarize(vote_percent = sum(vote_percent))
    ### Convert the county to have the first letter be capitalized
    data$county <- str_to_title(data$county_name)
    return(data)
  }
  population_data <- population_func(year, state)
  ### Extract the geometry for each county from population_data in order to include a geometry variable to the pollution and election data
  county_geometry <- population_data%>%
    select(geometry, county)%>%
    ### Remove duplicated rows
    distinct()
  ### Include the geometry variable to the pollution and election data, joined by county
  pollution_data <- left_join(pollution_function(year, state), county_geometry, by= "county")
  election_data <- left_join(extract_election_pres(year, state), county_geometry, by = "county")
  ### Convert DEMOCRAT to be Democrat, etc.
  election_data$party <- str_to_title(election_data$party)
  ### Convert PM25 to be Pm25, etc.
  pollution_data$pollutant <- str_to_title(pollution_data$pollutant)
  ### Return a list with three elements. Each element is a data frame. 
  return(list(population_data, pollution_data, election_data))
}





