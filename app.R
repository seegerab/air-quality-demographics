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
            # sliderInput("bins",
            #             "Number of bins:",
            #             min = 1,
            #             max = 50,
            #             value = 30),
            # sliderInput("bins",
            #             "Number of bins:",
            #             min = 1,
            #             max = 50,
            #             value = 30),
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
                        choices = c("California", "Oregon", "Washington", "Nevada"),
                        multiple = TRUE)
            
        ),

        # Show a plot of the generated distribution
        # mainPanel(
        #           fluidRow(
        #             splitLayout(cellWidths = c("50%", "50%"), plotOutput("plotgraph1"), plotOutput("plotgraph2"))
        #           )
        # )
        mainPanel(
          plotOutput("distPlot1")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot1 <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = 10 + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = 10, col = 'darkgray', border = 'white',
             xlab = paste('Waiting time to next eruption (in mins)'),
             main = 'Histogram of waiting times')
    })

    output$distPlot2 <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2]
      bins <- seq(min(x), max(x), length.out = 10 + 1)

      # draw the histogram with the specified number of bins
      hist(x, breaks = 10, col = 'darkgray', border = 'white',
           xlab = paste('Waiting time to next eruption (in mins)'),
           main = 'Histogram of waiting times')
    })


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

