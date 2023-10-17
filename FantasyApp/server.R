# libraries
library(shiny)
library(ggplot2)



# Define server

server <- shinyServer(function(input, output, session) {
  # ----------------------------------------------
  output$example_roster <- renderImage({
    return(list(
      src = "images/example_roster.png",
      contentType = "image/png",
      alt = "Roster"
    ))
  }, deleteFile = FALSE)
  
  
  output$fantasy_points <- DT::renderDataTable({
    fantasy_points
  })
  
})
