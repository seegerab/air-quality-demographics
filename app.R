#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# 

library(shiny)
library(shinythemes)
library(viridis)
library(sf)
library(tidyverse)

plot_function <- function(year, state, email = "seegerab@umich.edu", key = "greengazelle94"){
  library(stringr)
  library(tidyverse)
  library(sf)
  library(tigris)
  library(tidycensus)
  library(dplyr)
  library(tidycensus)
  library(tidyverse)
  library(stringr)
  library(jsonlite)
  library(ggplot2) 
  library(maps)
  library(usmap)
  library(RAQSAPI)
  if (!is.numeric(year)){
    stop("Year must be a numeric value, not a character.")
  }
  
  ### Verify that the year is between 2005 and 2021
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
  
  ### Have the function accept a variable number of years
  population_func <- function(year, state){
    ### Error checking for the inputs 
    ### Verify year is a numeric
    
    
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
    
    geometry <- as.data.frame(data%>%select(c(geometry, GEOID, NAME)))
    
    data <- as.data.frame(data)
    total_population <- data%>%filter(race == "Total population")
    data <- left_join(data, total_population, by = "GEOID")
    data <- data %>%
      select(c(GEOID, estimate.x, estimate.y, race.x))%>%
      ### Create a new variable that is the proportion of each race
      mutate(race_prop = estimate.x / estimate.y)
    colnames(data) <- c("GEOID", "population", "total_population", "race", "race_proportion")
    data <- merge(data, geometry, by = "GEOID", all.x=TRUE)
    total_population <- data %>% filter(race == "Total population")
    data <- left_join(data, total_population, by = "GEOID")
    data <- data %>% 
      # ### Create a new variable that is the proportion of each race
      mutate(race_prop = population.x / population.y)
    data$year <- rep(year, nrow(data)) 
    data <- data[,c("GEOID", "population.x", "total_population.x", "race.x", "race_proportion.x", "NAME.x", "year")] # take out geometry.x
    colnames(data) <- c("GEOID", "population", "total_population", "race", "race_proportion", "NAME", "year")
    data$county <- sapply(strsplit(data$NAME, " County"), "[[", 1)
    data <- left_join(data, geometry, by = "GEOID")
    ### Need to figure out why some rows are duplicating
    return(distinct(data))
  }
  
  
  
  # pollution_function <- function(year, state){
  #   if (state == "CA"){
  #     state_code <- "06"
  #   }
  #   if (state == "OR"){
  #     state_code <- "41"
  #   }
  #   if (state == "WA"){
  #     state_code <- "53"
  #   }
  # 
  #   
  #   
  #   all_pollutants = c("ozone","so2","co","no2","pm25.frm","pm25","pm10")
  #   # 
  #   # if(pollutant=="ozone"){param="44201"}
  #   # if(pollutant=="so2"){param="42401"}
  #   # if(pollutant=="co"){param="42101"}
  #   # if(pollutant=="no2"){param="42602"}
  #   # if(pollutant=="pm25.frm"){param="88101"}
  #   # if(pollutant=="pm25"){param="88502"}
  #   # if(pollutant=="pm10"){param="81102"}
  #   
  #   all_data <- data.frame(matrix(nrow = 0, ncol = 37))
  #   for (pollutant in all_pollutants){
  #     if(pollutant=="ozone"){param="44201"}
  #     if(pollutant=="so2"){param="42401"}
  #     if(pollutant=="co"){param="42101"}
  #     if(pollutant=="no2"){param="42602"}
  #     if(pollutant=="pm25.frm"){param="88101"}
  #     if(pollutant=="pm25"){param="88502"}
  #     if(pollutant=="pm10"){param="81102"}
  #     # print(pollutant)
  #     URL <- paste0("https://aqs.epa.gov/data/api/dailyData/byState?",
  #                   "email=", email,"&",
  #                   "key=", key ,"&", 
  #                   "param=", param,"&", 
  #                   "bdate=", year,"0101&",
  #                   "edate=", year,"1231&",
  #                   "state=", state_code)
  #     if (param == "44201"){
  #       colnames(all_data) <- colnames(data.frame(fromJSON(URL)))}
  #     partial_data <- data.frame(fromJSON(URL), row.names = NULL)
  #     partial_data$pollutant <- rep(pollutant, nrow(partial_data))
  #     all_data <- rbind(all_data, partial_data, row.names = NULL)
  #     
  #   }
  #   
  #   df <- subset(all_data, select = c(5:ncol(all_data)))
  #   colnames(df) <- gsub("Data.", "", colnames(df), fixed=TRUE)
  #   df$year <- format(as.Date(df$date_local, format="%Y-%m-%d"),"%Y")
  #   # return(colnames(df))
  #   # Arithmetic Mean (Daily)- The measure of central tendency obtained from the sum of the observed pollutant data values in the daily data set divided by the number of values that comprise the sum for the daily data set.
  #   
  #   new_df <- df %>%
  #     group_by(county, year, pollutant) %>%
  #     summarize(mean_aqi = mean(aqi, na.rm = TRUE),
  #               mean_conc = mean(arithmetic_mean, na.rm = TRUE))
  # 
  #   # return(new_df)
  #   # # Creating AQI categories
  #   new_df$aqi_cat <- as.factor(
  #     ifelse(new_df$mean_aqi >= 0 & new_df$mean_aqi <= 50, "Good",
  #            ifelse(new_df$mean_aqi >= 51 & new_df$mean_aqi <= 100, "Moderate",
  #                   ifelse(new_df$mean_aqi >= 101 & new_df$mean_aqi <= 150, "Unhealthy for Sensitive Groups",
  #                          ifelse(new_df$mean_aqi >= 151 & new_df$mean_aqi <= 200, "Unhealthy",
  #                                 ifelse(new_df$mean_aqi > 201 & new_df$mean_aqi <= 300, "Very Unhealthy",
  #                                        ifelse(new_df$mean_aqi >= 301 & new_df$mean_aqi <= 500, "Hazardous",0)))))))
  # 
  #   new_df <- new_df %>% select(county, mean_conc, mean_aqi, aqi_cat, year, pollutant)
  #   return(new_df)
  # }
  
  
  pollution_function <- function(year,state){
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
    
    all_data <- data.frame(matrix(nrow = 0, ncol = 61))
    print(dim(all_data))
    for (pollutant in all_pollutants){
      if(pollutant=="ozone"){param="44201"}
      if(pollutant=="so2"){param="42401"}
      if(pollutant=="co"){param="42101"}
      if(pollutant=="no2"){param="42602"}
      if(pollutant=="pm25"){param="88502"}
      if(pollutant=="pm10"){param="81102"}
      # print(pollutant)
      URL <- paste0("https://aqs.epa.gov/data/api/annualData/byState?",
                    "email=", email,"&",
                    "key=", key ,"&",
                    "param=", param,"&",
                    "bdate=", year,"0101&",
                    "edate=", year,"1231&",
                    "state=", state_code)
      if (param == "44201"){
        colnames(all_data) <- colnames(data.frame(fromJSON(URL)))}
      partial_data <- data.frame(fromJSON(URL), row.names = NULL)
      partial_data$pollutant <- rep(pollutant, nrow(partial_data))
      print(dim(partial_data))
      all_data <- rbind(all_data, partial_data, row.names = NULL)
      
    }
    df <- subset(all_data, select = c(5:ncol(all_data)))
    print(head(df))
    colnames(df) <- gsub("Data.", "", colnames(df), fixed=TRUE)
    # df$year <- format(as.Date(df$date_local, format="%Y-%m-%d"),"%Y")
    # return(colnames(df))
    df <- df%>%
      group_by(county, year, pollutant)%>%
      summarize(mean_conc = mean(arithmetic_mean))%>%
      select(county, mean_conc, year, pollutant)%>%
      distinct()
    
    return(df)
    
  }
  
  # For annual data, only the year portion of the bdate and edate are used and only whole years of data are returned. For example, bdate = 20171231 and edate = 20180101 will return full data for 2017 and 2018
  
  
  
  
  
  
  data <- read.csv("countypres_2000-2020.csv")
  
  extract_election_pres <- function(year, state){
    library(tidyverse)
    library(stringr)
    data$party <- ifelse(data$party %in% c("DEMOCRAT", "REPUBLICAN"), data$party, "OTHER")
    data <- data%>%
      select(-c(state))%>%
      filter(state_po == state)%>%
      filter(year == year)%>%
      select(c(year, state_po, county_name, party, candidatevotes, totalvotes))%>%
      mutate(vote_percent = candidatevotes / totalvotes)%>%
      group_by(year, state_po, county_name, party)%>%
      summarize(vote_percent = sum(vote_percent))
    data$county <- str_to_title(data$county_name)
    return(data)
  }
  population_data <- population_func(year, state)
  
  county_geometry <- population_data%>%
    select(geometry, county)%>%
    distinct()
  print(colnames(county_geometry))
  print(colnames(pollution_function(year, state)))
  # pollution <- p
  pollution_data <- left_join(pollution_function(year, state), county_geometry, by= "county")
  
  election_data <- left_join(extract_election_pres(year, state), county_geometry, by = "county")
  
  election_data$party <- str_to_title(election_data$party)
  
  pollution_data$pollutant <- str_to_title(pollution_data$pollutant)
  
  
  
  return(list(population_data, pollution_data, election_data))
  
  
}


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("lumen"),
                          
    # Application title
    titlePanel("Relationship between air pollutants, election results, and demographics"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          ### Ask the user what variable they want plotted alongside demographic data - either election results or pollution
          selectInput("choice", "What variable would you like to plot?",
                      choices = c( 
                        "Pollution", 
                        "Election"),
                      multiple = FALSE), 
          ### The user can select one demographic group
          selectInput("race", "Demographic Group",
                        choices = c( "White", 
                                    "Black or African American", 
                                    "American Indian and Alaska Native",
                                    "Asian",
                                    "Native Hawaiian and Other Pacific Islander",
                                    "Other",
                                    "Two or more races"),
                        multiple = FALSE),
            ### Users can specify one pollutant from the following
            selectInput("pollutant", "Pollutant",
                        choices = c("Pm25" , "Ozone", "Co", "So2", "Pm10", "No2"),
                        multiple = FALSE),
          ### Users can specify either CA, WA, or OR
            selectInput("state", "State",
                        choices = c("CA", "WA", "OR"),
                        ### May want to include a select multiple option later
                        multiple = FALSE),
          ### Because we're plotting election data, user can specify either 2012, 2016, or 2020
            selectInput("year", "Year",
                        choices = c("2012","2016","2020"),
                        ### May want to include a select multiple option later
                        multiple = FALSE),
          ### User can specify whether they want the percent of the Democratic vote or percent of the Republican vote
            selectInput("party", "Party",
                        choices = c("Democrat", "Republican"),
                        ### May want to include a select multiple option later
                        multiple = FALSE),
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
                  fluidRow(
                    ### Update the layout to plot 2 plots next to each other
                    splitLayout(cellWidths = c("50%", "50%"), plotOutput("plot1"), plotOutput("plot2"))
                  )

        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  ### data_list is a list with the 3 data frames in it
  ### The first element is the demographic data, the second element is the pollution data,
  ### and the third element is the election data
    data_list<- reactive({
        ### Call the plot_function to extract data for the user specified year and state
        all_data <- plot_function(as.numeric(input$year), input$state)
        return(all_data)
        })
    output$plot1 <- renderPlot({
      req(data_list())
      ### The population data is the first element of data_list
      ### Filter the population_data to the user specified race
      population_data <- data_list()[[1]]%>%filter(race == input$race)
      g <- ggplot(population_data, aes(fill = race_proportion, geometry = geometry))
      g + geom_sf() +
        ### Update the labels to reflect the user specified race, state, and year
        labs(fill = "% of the population", title = paste(unique(population_data$race),"Demographics For", unique(input$state), "For", unique(input$year), "By County"))+
        scale_fill_viridis_c(option = "plasma")+
        theme_minimal()

    })

    output$plot2 <- renderPlot({
      req(data_list())
      ### If the user input "Pollution" as the variable they want to plot,
      ### index into the second element of data_list. Otherwise, index into the 
      ### third element, which is the election data. 
      list_index <- ifelse(input$choice == "Pollution", 2, 3)
      if (list_index == 3){
        ### Filter to the party the user specified of data_list
        election_data <- data_list()[[3]]%>%filter(party == input$party)
        ### If the party specified was Democrat, the high_color is blue and the low_color is red
        ### Otherwise, if the party specified was Republican, the high_color is red and the low_color is blue
        if (input$party == "Democrat"){
          high_color = "blue"
          low_color = "red"
        }
        if (input$party == "Republican"){
          high_color = "red"
          low_color = "blue"
        }
        g <- ggplot(election_data, aes(fill = vote_percent, geometry = geometry))
        g + geom_sf() +
          ### Update the labels to include the user specified year, party, and state
          labs(fill = "% of the Vote", title = paste(unique(election_data$party),"Percent of Vote In", unique(input$state), "By County"))+
          scale_fill_gradient2(low=low_color, midpoint = 0.5, high=high_color)+
          theme_minimal()}
      else{
        ### Otherwise, plot the pollution data
        pollution_data <- data_list()[[2]]%>%filter(pollutant == input$pollutant)
        g <- ggplot(pollution_data, aes(fill = mean_conc, geometry = geometry))
        g + geom_sf() +
          labs(fill = "Pollutant Concentration", title = paste(unique(pollution_data$pollutant),"Concentrations For", unique(input$state), "For", unique(input$year), "By County"))+
          scale_fill_viridis_c(option = "inferno")+
          theme_minimal()
      }
      
    })

}

# Run the application 
shinyApp(ui = ui, server = server)











