# libraries
libs <- c(
  "shiny", "shinyMobile", "ggplot2", "shinyjs", "DBI",
  "dplyr", "dbplyr", "pool"
)
invisible(lapply(libs, library, character.only = TRUE))

server <- function(input, output, session) {
  dir.create(saved_roster, showWarnings = FALSE, recursive = TRUE)

  # PREDICTION FLES
  qb_pred <- read.csv("Data/qb_pred.csv")
  rb_pred <- read.csv("Data/rb_pred.csv")
  wr_pred <- read.csv("Data/wr_pred.csv")
  te_pred <- read.csv("Data/te_pred.csv")

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

  output$saved_roster_info <- renderUI({
    HTML(paste("Saved Roster Info:<br>", roster_data()))
  })

  player_mapping <- build_player_mapping()


  ###### MY ROSTER DISPLAY - QUARTERBACK
  rv <- reactiveValues(selected_qb = NULL)

  qb_pred_filtered <- reactive({
    req(input$myRosterQB)
    print("IN QB PRED FILTERED")
    qb_name_mapping <- player_mapping[player_mapping$Full_name == input$myRosterQB, ]
    if (nrow(qb_name_mapping) == 0) {
      return(NULL)
    }
    qb_short_name <- qb_name_mapping$Short_name
    rv$selected_qb <- qb_name_mapping$Full_name
    qb_pred[qb_pred$player_name == qb_short_name, ]
  })

  observe({
    req(qb_pred_filtered())
    print("IN OBSERVE")
    print(qb_pred_filtered())
  })

  observe({
    selected_qb_value <- rv$selected_qb
    print(paste("Before Update: ", selected_qb_value))
    updateSelectizeInput(
      session,
      "myRosterQB",
      choices = qb_choices,
      selected = selected_qb_value
    )
    print(paste("After Update: ", selected_qb_value))
  })

  qb_data_as_text <- reactive({
    req(qb_pred_filtered())
    print("IN QB DATA AS TEXT")
    if (nrow(qb_pred_filtered()) == 0) {
      return("No player data available.")
    }
    output_text <- HTML(paste("Quarterback: ", rv$selected_qb))
    return(output_text)
  })

  output$qb_table <- renderTable({
    req(qb_pred_filtered())
    print("IN RENDER TABLE")
    table_data <- qb_pred_filtered() %>%
      select(
        pred_passing_yards,
        pred_passing_tds,
        pred_rushing_yards,
        pred_fantasy_points
      )
    table_data <- as.data.frame(table_data)
    return(table_data)
  })

  output$qb_info <- renderUI({
    print("IN RENDER UI")
    qb_data_as_text()
  })

}
