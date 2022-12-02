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
                        choices = c("Black", "Native American", "White", "Other Demographic Groups Here..."),
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
                        choices = c("2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020"),
                        ### May want to include a select multiple option later
                        multiple = FALSE),
            textInput("stateTextInput", h3("State"),
                      value = "")
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
                  # fluidRow(
                  #   splitLayout(cellWidths = c("50%", "50%"), plotOutput("distPlot1"), plotOutput("distPlot2"))
                  # )
          plotOutput("plot")
          # textOutput("vec")        # ALIGNS TO renderTable
          
        # textOutput("dim")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    # year <- reactive({
    #   as.numeric(input$year)
    # })
    # state <- reactive({
    #   getSymbols(input$state)
    # })
    # race <- reactive({
    #   getSymbols(input$race)
    # })
    output$year <- reactive(input$year)
    output$state <- reactive(input$state)
    
    
    population_data <- reactive({
      return(as.data.frame(population_func(as.numeric(input$year), input$state)))
    })
    
    
    output$plot <- renderPlot({
      req(population_data())
      g <- ggplot(population_data(), aes( y = estimate, x = year))
      g + geom_point()
      #browser()
    })
    
    # data <- population_data()
    # 
    # output$vec <- renderText(typeof(data))
    # 
    # 
    # output$valueOut <- renderTable(population_data())
    # output$plotOne <- renderPlot({
    #   ggplot(data=population_data, aes(x=year, y = estimate))+geom_point()
    # })
    # output$dim <- nrow(population_data)
    
    # output$distPlot1 <- renderPlot(plot(1:10 ~ 1:10,
    #      xlab = race()))
    # output$distPlot2 <- renderPlot(plot(1:10 ~ 1:10,
    #                      xlab = state()))
    
    # library(tidyverse)
    # source("population_function.R")

    ### Call the population_func function sourced from population_function.R
    # data <- population_func(year(),state())
    
    ### Match up the race the user input with the race variable in the data
    # race_var <- (unique(data$race))[str_detect(unique(tolower(data$race)), tolower(race_input))]
    # data <- data%>%
    #   filter(race == race_var, year == year_input)%>%
    #   arrange(desc(estimate))%>%
    #   slice(1:10)
    # 
    # outplot$distPlot1 <- renderPlot({
    #   ggplot(data = data, aes(x = estimate, y = NAME))+
    #     geom_bar(stat="identity")
    # })
    # 
    # # output$distPlot1 <- renderPlot({
    # #     # generate bins based on input$bins from ui.R
    # #     x    <- faithful[, 2]
    # #     bins <- seq(min(x), max(x), length.out = 10 + 1)
    # # 
    # #     # draw the histogram with the specified number of bins
    # #     hist(x, breaks = 10, col = 'darkgray', border = 'white',
    # #          xlab = paste('Waiting time to next eruption (in mins)', input$pollutant, input$stateTextInput, sep = ""),
    # #          main = 'Histogram of waiting times')
    # # })
    # 
    # output$distPlot2 <- renderPlot({
    #   # generate bins based on input$bins from ui.R
    #   x    <- faithful[, 2]
    #   bins <- seq(min(x), max(x), length.out = 10 + 1)
    # 
    #   # draw the histogram with the specified number of bins
    #   hist(x, breaks = 10, col = 'darkgray', border = 'white',
    #        xlab = paste('Waiting time to next eruption (in mins)'),
    #        main = 'Histogram of waiting times')
    # })


}

# shinyServer(function(input, output) 
# {
#   set.seed(1234)
#   pt1 <- qplot(rnorm(500),fill=I("red"),binwidth=0.2,title="plotgraph1")
#   pt2 <- reactive({
#     input$do2
#     if (input$do2){
#       return(qplot(rnorm(500),fill=I("blue"),binwidth=0.2,title="plotgraph2"))
#     } else {
#       return(NULL)
#     }
#   })
#   output$plotgraph1 = renderPlot({pt1})
#   output$plotgraph2 = renderPlot({pt2()})
# }
# )


# Run the application 
shinyApp(ui = ui, server = server)

