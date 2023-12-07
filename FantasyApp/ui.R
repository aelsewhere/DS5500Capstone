libs <- c(
  "shiny", "ggplot2", "DT", "plotly",
  "bslib", "gridlayout"
)
invisible(lapply(libs, library, character.only = TRUE))

ui <- fluidPage(
  shinyjs::useShinyjs(),
  shinyjs::inlineCSS(appCSS),
  sidebarLayout(
    sidebarPanel(
      h3("My Roster"),
      uiOutput("saved_roster_info"),
      div(
        id = "roster_form",
        selectInput(
          "myRosterQB",
          star_mand("Quarterback"),
          qb_choices,
          selected = NULL,
          multiple = FALSE,
          selectize = TRUE,
          width = NULL,
          size = NULL
        ),
        selectInput(
          "myRosterRB1",
          star_mand("Running Back"),
          rb_choices,
          selected = NULL,
          multiple = FALSE,
          selectize = TRUE,
          width = NULL,
          size = NULL
        ),
        selectInput(
          "myRosterRB2",
          "Running Back",
          rb_choices,
          selected = NULL,
          multiple = FALSE,
          selectize = TRUE,
          width = NULL,
          size = NULL
        ),
        selectInput(
          "myRosterWR1",
          star_mand("Wide Receiver"),
          wr_choices,
          selected = NULL,
          multiple = FALSE,
          selectize = TRUE,
          width = NULL,
          size = NULL
        ),
        selectInput(
          "myRosterWR2",
          "Wide Receiver",
          wr_choices,
          selected = NULL,
          multiple = FALSE,
          selectize = TRUE,
          width = NULL,
          size = NULL
        ),
        selectInput(
          "myRosterTE",
          star_mand("Tight End"),
          te_choices,
          selected = NULL,
          multiple = FALSE,
          selectize = TRUE,
          width = NULL,
          size = NULL
        ),
        selectInput(
          "myRosterFlex",
          "Flex",
          flex_choices,
          selected = NULL,
          multiple = FALSE,
          selectize = TRUE,
          width = NULL,
          size = NULL
        ),
        selectInput(
          "myRosterDST",
          "Defense/Special Team",
          dst_choices,
          selected = NULL,
          multiple = FALSE,
          selectize = TRUE,
          width = NULL,
          size = NULL
        ),
        selectInput(
          "myRosterK",
          star_mand("Kicker"),
          k_choices,
          selected = NULL,
          multiple = FALSE,
          selectize = TRUE,
          width = NULL,
          size = NULL
        ),
        actionButton("submit", star_mand("Submit"), class = "btn-primary")
      ),
      shinyjs::hidden(
        div(
          id = "roster_submit_msg",
          h3("Roster Saved."),
          actionLink("resubmit_", "Resubmit Roster.")
        )
      ),
      card_footer()
    ),
    mainPanel(
      grid_card_text(
        area = "header",
        content = "Fantasy for Everyone",
        alignment = "start",
        is_title = TRUE
      ),
      grid_card(
        area = "area3",
        card_body(
          tabsetPanel(
            nav_panel(
              title = "Home",
              h3("My Team"),
              uiOutput("qb_info"),
              tableOutput("qb_table")
            ),
            nav_panel(
              title = "Upcoming Games",
              h3("Upcoming Games"),
              dataTableOutput("schedule")
            ),
            nav_panel(
              title = "Predictions"
            ),
            navbarMenu(
              title = "Learn",
              tabPanel("About the Game"),
              tabPanel("About the App"),
              tabPanel("About the Creators")
            )

          )
        )
      )
    )
  )
)
