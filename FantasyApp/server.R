# libraries
library(shiny)
library(ggplot2)
library(shinyjs)
library(DBI)
library(dplyr)
library(dbplyr)
library(pool)



server <- function(input, output, session) {
  dir.create(saved_roster, showWarnings = FALSE, recursive = TRUE)
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

  output$input_a <- renderPrint({input$select})

  # Save Roster
  formData <- reactive({
    data <- sapply(mand_roster, function(x) input[[x]])
    data <- c(data, timestamp = epochTime())
    data <- t(data)
    data
  })

  saveData <- function(data){
    fileName <- sprintf("%s_%s.csv", humanTime(), digest::digest(data))
    write.csv(
     x=data,
     file=file.path(saved_roster, fileName),
     row.names=FALSE,
     quote=TRUE)
  }

  observeEvent(input$submit,{saveData(formData())})

  observeEvent(input$submit,{
    shinyjs::reset("form")
    shinyjs::hide("form")
    shinyjs::show("roster_submit_msg")

  })

  observeEvent(input$resubmit_, {
    shinyjs::show("form")
    shinyjs::hide("roster_submit_mssg")
  })

  observe({
    print(input$myRosterQB)
  })

  observe({
    roster_qb <- input$myRosterQB
    roster_rb1 <- input$myRosterRB1
    roster_rb2 <- input$myRosterRB2
    roster_wr1 <- input$myRosterWR1
    roster_wr2 <- input$myRosterWR2
    roster_te <- input$myRosterTE
    roster_flex <- input$myRosterFlex
    roster_dst <- input$myRosterDST
    roster_k <- input$myRosterK
  })

  output$qb_info <- renderPrint({
    player_info <- get_player_info(input$myRosterQB)
    print_player_info(player_info)
  })

  output$rb1_info <- renderPrint({
    player_info <- get_player_info(input$myRosterRB1)
    print_player_info(player_info)
  })

  output$rb2_info <- renderPrint({
    player_info <- get_player_info(input$myRosterRB2)
    print_player_info(player_info)
  })

  output$wr1_info <- renderPrint({
    player_info <- get_player_info(input$myRosterWR1)
    print_player_info(player_info)
  })

  output$wr2_info <- renderPrint({
    player_info <- get_player_info(input$myRosterWR2)
    print_player_info(player_info)
  })

  output$te_info <- renderPrint({
    player_info <- get_player_info(input$myRosterTE)
    print_player_info(player_info)
  })

  output$flex_info <- renderPrint({
    player_info <- get_player_info(input$myRosterFlex)
    print_player_info(player_info)
  })

  output$dst_info <- renderPrint({
    player_info <- get_player_info(input$myRosterDST)
    print_player_info(player_info)
  })

  output$k_info <- renderPrint({
    player_info <- get_player_info(input$myRosterK)
    print_player_info(player_info)
  })


  output$qb_pred_table <- DT::renderDataTable({
    DT::datatable(
      read.csv(
        "Data/qb_pred.csv"
      ),
      options = list(
        scrollX = TRUE
      )
    )
  })
  output$wr_pred_table <- DT::renderDataTable({
    DT::datatable(
      read.csv(
        "Data/wr_pred.csv"
      ),
      options = list(
        scrollX = TRUE
      )
    )
  })
  output$te_pred_table <- DT::renderDataTable({
    DT::datatable(
      read.csv(
        "Data/te_pred.csv"
      ),
      options = list(
        scrollX = TRUE
      )
    )
  })
  output$rb_pred_table <- DT::renderDataTable({
    DT::datatable(
      read.csv(
        "Data/rb_pred.csv"
      ),
      options = list(
        scrollX = TRUE
      )
    )
  })
}
