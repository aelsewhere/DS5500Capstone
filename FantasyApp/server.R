# libraries
library(shiny)
library(ggplot2)
library(shinyjs)


server <- function(input, output, session) {
  # ----------------------------------------------
  observe({
    roster_filled <-
      vapply(
        mand_roster,
        function(field) {
          !is.null(input[[field]]) && input[[field]] != ""
        },
        logical(1)
      )
    roster_filled <- all(roster_filled)
    toggleState(id = "submit", condition = roster_filled)
  })

  output$example_roster <- renderImage(
    {
      return(list(
        src = "images/example_roster.png",
        contentType = "image/png",
        alt = "Roster"
      ))
    },
    deleteFile = FALSE
  )

  output$fantasy_points <- DT::renderDataTable({fantasy_points})

  output$input_a <- renderPrint({input$select})
}
