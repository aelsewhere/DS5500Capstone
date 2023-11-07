# libraries
libs <- c(
  "shiny", "shinyMobile", "ggplot2", "shinyjs", "DBI",
  "dplyr", "dbplyr", "pool"
)
invisible(lapply(libs, library, character.only = TRUE))

server <- function(input, output, session) {
  dir.create(saved_roster, showWarnings = FALSE, recursive = TRUE)
  # ----------------------------------------------
  observe({
    roster_filled <-
      vapply(
        full_roster,
        function(field) {
          !is.null(input[[field]]) && input[[field]] != ""
        },
        logical(1)
      )
    roster_filled <- all(roster_filled)
    toggleState(id = "submit", condition = roster_filled)
  })

  # Save Roster
  form_data <- reactive({
    data <- sapply(full_roster, function(x) input[[x]])
    data <- c(data, timestamp = epoch_time())
    data <- t(data)
    data
  })

  file_path <- reactiveVal(NULL)

  save_data <- function(data) {
    file_name <- sprintf("%s_%s.csv", human_time(), digest::digest(data))
    new_file_path <- file.path(saved_roster, file_name)

    file_path(new_file_path)

    write.csv(
      x = data,
      file = new_file_path,
      row.names = FALSE,
      quote = TRUE
    )
  }

  roster_data <- reactive({
    if(!is.null(file_path()) && file.exists(file_path())){
      roster <- read.csv(file_path())
      formatted_data <- paste(
        "Quarterback: ", roster$myRosterQB,
        "<br> Running Back: ", roster$myRosterRB1,
        "<br> Running Back: ", roster$myRosterRB2,
        "<br> Wide Reciever: ", roster$myRosterWR1,
        "<br> Wide Reciever: ", roster$myRosterWR2,
        "<br> Tight End: ", roster$myRosterTE,
        "<br> Flex: ", roster$myRosterFlex,
        "<br> Defense/Special Team: ", roster$myRosterDST,
        "<br> Kicker: ", roster$myRosterK
      )
      formatted_data
    }
  })


  observeEvent(input$submit,{save_data(form_data())})

  observeEvent(input$submit,{
    shinyjs::reset("roster_form")
    shinyjs::hide("roster_form")
    shinyjs::show("roster_submit_msg")
  })

  observeEvent(input$resubmit_, {
    shinyjs::show("roster_form")
    shinyjs::hide("roster_submit_msg")
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

  output$saved_roster_info <- renderUI({
    HTML(paste("Saved Roster Info:<br>", roster_data()))
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
  #output$upcoming_schedule <- DT::renderDataTable({
  #  DT::datatable("schedule")
  #})
}
