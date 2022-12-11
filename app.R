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




# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("lumen"),
                          
    # Application title
    titlePanel("Relationship between air pollutants, housing cost, and demographics"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            ### User can specify one race
            ### There's a way to specify more than one race, but I don't know if we want to do that?
            selectInput("race", "Demographic Group",
                        choices = c("Total population", 
                                    "White", 
                                    "Black or African American", 
                                    "American Indian and Alaska Native",
                                    "Asian",
                                    "Native Hawaiian and Other Pacific Islander",
                                    "Other",
                                    "Two or more races",
                                    "Hispanic latino"),
                        multiple = FALSE),
            ### Users can specify one pollutant
            selectInput("pollutant", "Pollutant",
                        choices = c("Pm25" , "Ozone", "Co", "So2", "Pm10", "No2"),
                        multiple = FALSE),
            selectInput("state", "State",
                        choices = c("CA", "WA", "OR"),
                        ### May want to include a select multiple option later
                        multiple = FALSE),
            selectInput("year", "Year",
                        choices = c("2012","2016","2020"),
                        ### May want to include a select multiple option later
                        multiple = FALSE),
            selectInput("party", "Party",
                        choices = c("Democrat", "Republican"),
                        ### May want to include a select multiple option later
                        multiple = FALSE),
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
                  # fluidRow(
                  #   splitLayout(cellWidths = c("50%", "50%"), plotOutput("plot1"), plotOutput("plot1"))
                  # )
          plotOutput("plot1")

        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  ### I don't think the next two lines of code are necessary
  
    # output$year <- reactive(input$year)
    # output$state <- reactive(input$state)
    
    
  data_list<- reactive({
      all_data <- plot_function(as.numeric(input$year), input$state)
      ### Do something here to connect the input with the actual name of the race in the data
      population_data<- all_data[[1]] %>% filter(race == input$race) 
      return(list(population_data))
      })
    # election_data <- all_data[[3]]%>%filter(party== input$party)
    # pollutation_data<-all_data[[2]]%>%filter(pollutant== input$pollutant)
 #population_alone <- data_list()[[1]]

    
    
    
    # output$plot1 <- renderPlot({
    #  # req(population_data())
    #   req(data_list)
    #   g <- ggplot(data_list()[[1]], aes(fill = race_proportion, geometry = geometry))
    #   g + geom_sf() +   
    #     #labs(fill = "Total population", title = paste("Total population of", unique(population_data()$race), "for", unique(population_data()$state), "in", unique(population_data()$year)))+
    #     scale_fill_viridis_c(option = "plasma")+
    #     theme_minimal()
    # 
    # })
    
  output$plot2 <- renderPlot({
    # req(population_data())
    req(data_list)
    g <- ggplot(data_list()[[2]], aes(fill = mean_conc, geometry = geometry))
    g + geom_sf() +   
      #labs(fill = "Total population", title = paste("Total population of", unique(population_data()$race), "for", unique(population_data()$state), "in", unique(population_data()$year)))+
      scale_fill_viridis_c(option = "plasma")+
      theme_minimal()
    
  })

  

}

# Run the application 
shinyApp(ui = ui, server = server)



### To Do

### Marley

### Include year in the title of the plot DONE
### Figure out how to show two maps side-by-side -- can't do this until function is better/fixed? i think our
# population function should output a dataframe with combined EPA and census data, with geoid included so i can plot it

### Replace "Total Black or African American alone" with "African American" in the function, or here in the Shiny App
### Replace state abbr. with the full name 

### Abigail 

### Change year to 2012, 2016, 2020 DONE 

### Three plots 

### Find a third data source

### Join by the county name (no county in the name)






