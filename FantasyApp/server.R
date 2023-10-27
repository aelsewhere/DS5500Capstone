# libraries
library(shiny)
library(ggplot2)
library(shinyjs)
library(DBI)
library(dplyr)
library(dbplyr)
library(pool)



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

  # Example Roster Image (Hidden)
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

  #output$fantasy_points <- DT::renderDataTable({fantasy_points})

  output$input_a <- renderPrint({input$select})

  # Save Roster
  formData <- reactive({
    data <- sapply(mand_roster, function(x) input[[x]])
    data <- c(data, timestamp = epochTime())
    data <- t(data)
    data
  })

  #saveData <- function(data){
  #  fileName <- sprintf("%s_%s.csv", humanTime(), digest::digest(data))
  #  write.csv(
  #   x=data,
  #   file=file.path(saved_roster, fileName),
  #   row.names=FALSE,
  #   quote=TRUE)
  #}

  #observeEvent(input$submit,{saveData(formData())})
  
  # for now, not saving to filepath. Roster will be saved
  # as a global variable. Resubmitting roster
  # will override current global variables


  observeEvent(input$submit,{
    #saveData(formData())
    shinyjs::reset("form")
    shinyjs::hide("form")
    shinyjs::show("roster_submit_msg")
  })

  observeEvent(input$resubmit_, {
    shinyjs::show("form")
    shinyjs::hide("roster_submit_mssg")
  })

  observe({
    roster_qb <<- input$myRosterQB
    roster_rb1 <<- input$myRosterRB1
    roster_rb2 <<- input$myRosterRB2
    roster_wr1 <<- input$myRosterWR1
    roster_wr2 <<- input$myRosterWR2
    roster_te <<- input$myRosterTE
    roster_flex <<- input$myRosterFlex
    roster_dst <<- input$myRosterDST
    roster_k <<- input$myRosterK
  })

  output$qb_pred_table <- renderDataTable({
    read.csv("Data/qb_pred.csv")
  })
  output$wr_pred_table <- renderDataTable({
    read.csv("Data/wr_pred.csv")
  })
  output$te_pred_table <- renderDataTable({
    read.csv("Data/te_pred.csv")
  })
  output$rb_pred_table <- renderDataTable({
    read.csv("Data/rb_pred.csv")
  })
  
}
