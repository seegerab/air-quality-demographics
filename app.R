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
                                    "Total White alone", 
                                    "Total Black or African American alone", 
                                    "Total American Indian and Alaska Native alone",
                                    "Total Asian alone",
                                    "Total Native Hawaiian and Other Pacific Islander alone",
                                    "Total some other race alone",
                                    "Total two or more races",
                                    "Total hispanic latino"),
                        multiple = FALSE),
            ### Users can specify one pollutant
            selectInput("pollutant", "Pollutant",
                        choices = c("pm25", "CO2", "Ozone", "Other Pollutants Here..."),
                        multiple = FALSE),
            selectInput("state", "State",
                        choices = c("MI", "OH", "IN", "IL"),
                        ### May want to include a select multiple option later
                        multiple = FALSE),
            selectInput("year", "Year",
                        choices = c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020"),
                        ### May want to include a select multiple option later
                        multiple = FALSE),
            textInput("stateTextInput", h3("State"),
                      value = "")
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
                  # fluidRow(
                  #   splitLayout(cellWidths = c("50%", "50%"), plotOutput("plot1"), plotOutput("plot1"))
                  # )
          plotOutput("plot2")

        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  ### I don't think the next two lines of code are necessary
  
    # output$year <- reactive(input$year)
    # output$state <- reactive(input$state)
    
    
    population_data <- reactive({
      return(population_func(as.numeric(input$year), input$state)%>%
               filter(race == input$race))
      ### Do something here to connect the input with the actual name of the race in the data
      
    })
    
    
    output$plot1 <- renderPlot({
      req(population_data())
      g <- ggplot(population_data(), aes(y = estimate, x = year))
      g + geom_point()
    })
    
    output$plot2 <- renderPlot({
      req(population_data())
      g <- ggplot(population_data(), aes(fill = estimate, geometry = geometry))
      g + geom_sf() +   
        labs(fill = "Total population", title = paste("Total population of", unique(population_data()$race), "for", unique(population_data()$state)))+
        scale_fill_viridis_c(option = "plasma")+
        theme_minimal()

    })
    
  

}

# Run the application 
shinyApp(ui = ui, server = server)

