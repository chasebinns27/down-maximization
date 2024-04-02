
library(shiny)
library(dplyr)
library(httr)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Team Down Maximization Over Time"),

    sidebarLayout(
        sidebarPanel(
            selectInput("season",
                        "Select Season",
                        choices = c(2016:2023),
                        selected = 2023)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
