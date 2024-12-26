library(shiny)
library(bslib)
library(ggplot2)

ui <- page_fluid(
  navset_pill(
    nav_panel(
      "Survey",
      "Answer the questions below, and we'll provide some songs that you might like!",
      br(), br(), br(),
      uiOutput("genreSurveyUI"),
      br(), br(),
      sliderInput("energeticSurvey", 
                  "How energetic would you like the song?", 
                  value = 50, min = 0, max = 100),
      br(), br(),
      "What speed do you like your songs? (Tempo, not duration!)",
      sliderInput("tempoSurvey",
                  p("Slow-paced songs!", HTML('&emsp;'), "Fast-paced songs!"),
                  value = 50, min = 0, max = 100),
      br(), br(),
      "How do you like lyrics in your songs?",
      sliderInput("speechinessSurvey",
                  p("Almost no lyrics!", HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), 
                    HTML('&nbsp;'), HTML('&nbsp;'), "Lots of lyrics!"),
                  value = 50, min = 0, max = 100),
      br(), br(),
      "Looking for happy or sad songs?",
      sliderInput("valenceSurvey",
                  p("Gimme sad stuff!", HTML('&emsp;'), HTML('&emsp;'), HTML('&nbsp;'), 
                    "It's happy hour!"),
                  value = 50, min = 0, max = 100),
      br(), br(),
      "How long do you like your songs? (Duration, not tempo!)",
      sliderInput("durationSurvey",
                  p("Short songs!", HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), 
                    HTML('&emsp;'), HTML('&emsp;'), HTML('&nbsp;'), "Long songs!"),
                  value = 50, min = 0, max = 100)
    ),
    nav_panel("Top Songs", 
              card(tableOutput("summaryTable"))),
    nav_panel("Song Chart", 
              card(plotOutput("songPlot")))
  )
)

server <- function(input, output, session) {
  load("data/data-music.RData")
  
  
  output$genreSurveyUI <- renderUI({
    genres <- unique(data_music$genre)
    selectInput("genreSurvey", "Any genre that you're a big fan of?", genres)
  })
  
  output$summaryTable <- renderTable({
    genre <- tolower(input$genreSurvey)
    energetic <- input$energeticSurvey / 100
    tempo <- input$tempoSurvey / 100
    speechiness <- input$speechinessSurvey / 100
    valence <- input$valenceSurvey / 100
    duration <- input$durationSurvey * 1000
    
    data_music$preference_score <- rowMeans(
      cbind(
        abs(data_music$energy - energetic),
        abs(data_music$tempo / max(data_music$tempo) - tempo),
        abs(data_music$speechiness - speechiness),
        abs(data_music$valence - valence),
        abs(data_music$duration_ms - duration) / max(data_music$duration_ms)
      ),
      na.rm = TRUE
    )
    
    filtered_songs <- data_music[tolower(data_music$genre) == genre, ]
    filtered_songs <- filtered_songs[order(filtered_songs$preference_score, -filtered_songs$popularity), ]
    
    head(filtered_songs[, c("track_name", "artist_name", "popularity")], 10)
  })
  
  output$songPlot <- renderPlot({
    genre <- tolower(input$genreSurvey)
    energetic <- input$energeticSurvey / 100
    tempo <- input$tempoSurvey / 100
    speechiness <- input$speechinessSurvey / 100
    valence <- input$valenceSurvey / 100
    duration <- input$durationSurvey * 1000
    
    data_music$preference_score <- rowMeans(
      cbind(
        abs(data_music$energy - energetic),
        abs(data_music$tempo / max(data_music$tempo) - tempo),
        abs(data_music$speechiness - speechiness),
        abs(data_music$valence - valence),
        abs(data_music$duration_ms - duration) / max(data_music$duration_ms)
      ),
      na.rm = TRUE
    )
    
    ggplot(data_music, aes(x = preference_score, y = popularity, color = tolower(genre) == genre)) +
      geom_point() +
      labs(
        x = "Preference Score (Lower is Better)",
        y = "Popularity",
        title = "Scatterplot of Songs Based on Preferences"
      ) +
      scale_color_manual(values = c("FALSE" = "gray", "TRUE" = "blue")) +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)