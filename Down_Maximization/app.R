
library(shiny)
library(dplyr)
library(httr)
library(shinybusy)
library(ggplot2)
library(nflplotR)
library(tidyr)

# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Chivo+Mono:wght@700&display=swap');
      h2 {
        font-family: 'Chivo Mono', monospace;
      }
      .siderbar h2 {
        font-family: 'Chivo Mono', monospace;
      }"))
  ),

    # Application title
    titlePanel("Who's Making the Best Use of Their Downs?"),
    
    sidebarLayout(  

    sidebarPanel(
            width = 2,
            selectInput(inputId = 'season',
                        label = "Select Season",
                        choices = c('2016',
                                    '2017',
                                    '2018',
                                    '2019',
                                    '2020',
                                    '2021',
                                    '2022',
                                    '2023'),
                        selected = '2023'),
            actionButton("goButton","View")
        ),

    mainPanel(
                  plotOutput("master_plot", height = "500px", width = "800px")
        )),
    add_busy_spinner(spin = "fading-circle")
)

server <- function(input, output) {
  
  csv_url <- eventReactive(input$goButton, { 
    paste0('https://media.githubusercontent.com/media/chasebinns27/down-maximization/main/pbp_data/pbp_',
           input$season, '.csv')})
  
  
  
  ###plot data
  output$master_plot <- renderPlot({
    
    pbp_response <- GET(csv_url())
    
    pbp <- read.csv(text = rawToChar(pbp_response$content))
    
    fourth_down_rankings <- pbp %>%
      filter(
        go_boost > 0,
        !is.na(go),
        wp < .90,
        wp > .10) %>%
      mutate(right = ifelse(go == 100, 1, 0),
             wrong = ifelse(go == 0, 1, 0)) %>%
      group_by(posteam) %>%
      summarize(right_decisions = sum(right),
                total_decisions = sum(right + wrong)) %>%
      ungroup() %>%
      mutate(right_pct = (right_decisions/total_decisions)*100)
    
    
    long_second_down_rankings <- pbp %>%
      filter((pass == 1 | rush == 1) & play_type %in% c('pass', 'run'),
             is.na(epa) == FALSE,
             is.na(two_point_conv_result),
             down == 2,
             ydstogo > 7,
             wp < .90,
             wp > .10) %>%
      mutate(rushing_play = ifelse(play_type == 'run', 1, 0),
             passing_play = ifelse(play_type == 'pass', 1, 0)) %>%
      group_by(posteam) %>%
      summarize(rushing_plays = sum(rushing_play),
                total_plays = sum(rushing_play + passing_play)) %>%
      mutate(run_rate = (rushing_plays / total_plays) * 100) %>%
      arrange(desc(run_rate))
    
    combined_rankings <- long_second_down_rankings %>%
      inner_join(fourth_down_rankings, by = 'posteam') %>%
      distinct(posteam, run_rate, go_rate = right_pct)
    
    run_rate_mean <- combined_rankings %>%
      summarize(mean_run_rate = mean(run_rate)) %>%
      pull()
    
    go_rate_mean <- combined_rankings %>%
      summarize(mean_go_rate = mean(go_rate)) %>%
      pull()
    
    centroid <- data.frame(
      quadrant = c("Maximizing downs", "Scared of late downs", "Wasting downs", "Comfortable with late downs"),
      x = c(go_rate_mean + 9, go_rate_mean - 9, go_rate_mean -9, go_rate_mean + 9),  # Adjust these values according to your plot
      y = c(run_rate_mean - 9, run_rate_mean -9, run_rate_mean + 9, run_rate_mean + 9)   # Adjust these values according to your plot
    )
    
    
    return(ggplot(combined_rankings, aes(x = go_rate, y = run_rate)) +
    geom_point() +
    geom_mean_lines(aes(x0 = go_rate , y0 = run_rate)) +
    geom_nfl_logos(aes(team_abbr = posteam), width = 0.065, alpha = 0.5) +
    geom_label(data = centroid, aes(x, y, label = quadrant), size = 5) +
    ggplot2::labs(
      x = "4th Down Go Rate When Analytics Say Go",
      y = "Run Rate on 2nd and 8+",
      caption = "Data: @nflfastR, @nfl4th"
    ) +
    scale_y_reverse() +
    geom_abline(slope = -1, intercept = seq(-40,50, 10), alpha = .2) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 20),  # Make title bold and big
      axis.title = element_text(face = "bold", size = 14) , # Make axis titles bold
      axis.text.x = element_text(size = 12),  # Adjust the size as needed
      axis.text.y = element_text(size = 12),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1))
    )})

}

# Run the application 
shinyApp(ui = ui, server = server)
