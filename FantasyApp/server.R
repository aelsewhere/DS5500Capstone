
library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    output$example_roster <- renderImage({
      return(list(
        src = "images/example_roster.png",
        contentType = "image/png",
        alt = "Roster"
      ))
    }, deleteFile = FALSE)
    
})
