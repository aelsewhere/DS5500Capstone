libs <- c(
  "shiny", "ggplot2", "DT", "plotly",
  "bslib", "gridlayout", "fmsb"
)
invisible(lapply(libs, library, character.only = TRUE))

ui <- fluidPage(
  shinyjs::useShinyjs(),
  shinyjs::inlineCSS(appCSS),
  tags$head(
    tags$script(
      HTML('
        Shiny.addCustomMessageHandler("updateSelectize",
          function(message) {
            $("#" + message.inputId).selectize()[0].selectize.clear();
            $("#" + message.inputId).selectize()[0].selectize.addOption(
              $.map(message.choices, function(value) {
                return { value: value, text: value };
              })
            );
            $("#" + message.inputId).selectize()[0].selectize.addItem(message.selected);
          }
        );
      ')
    )
  ),
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
              h3("Fantasy Football"),
              htmlOutput("intro_description"),
              br(),
              htmlOutput("problem_description"),
              br(),
              h3("Making Fantasy Football for Everyone"),
              htmlOutput("short_app_description"),
              br(),
              htmlOutput("outro_description")
            ),
            nav_panel(
              title = "Upcoming Games",
              h3("Upcoming Games"),
              dataTableOutput("schedule")
            ),
            nav_panel(title = "Roster Predictions",
              h3("My Team"),
              uiOutput("qb_info"),
              dataTableOutput("qb_table"),
              uiOutput("rb1_info"),
              dataTableOutput("rb1_table"),
              uiOutput("rb2_info"),
              dataTableOutput("rb2_table"),
              uiOutput("wr1_info"),
              dataTableOutput("wr1_table"),
              uiOutput("wr2_info"),
              dataTableOutput("wr2_table"),
              uiOutput("te_info"),
              dataTableOutput("te_table")
            ),
            nav_panel(title = "Interactive Models",
              plotlyOutput("radar_chart")
            ),
            nav_panel(
              title = "Data",
              h3("Quarterback Predicted Stats"),
              dataTableOutput("qb_pred_table"),
              br(),
              h3("Wide Reciever Predicted Stats"),
              dataTableOutput("wr_pred_table"),
              br(),
              h3("Tight End Predicted Stats"),
              dataTableOutput("te_pred_table"),
              br(),
              h3("Running Back Predicted Stats"),
              dataTableOutput("rb_pred_table")
            ),
            navbarMenu(
              title = "Learn",
              tabPanel("About the Game",
                h3("An Introduction to American Football"),
                htmlOutput("football_intro"),
                br(),
                htmlOutput("explaining_plays"),
                h3("Scoring in Football"),
                htmlOutput("explaining_scoring"),
                h3("Defensive Strategies"),
                htmlOutput("explaining_defense")
              ),
              tabPanel("About the App"),
              tabPanel("About the Creators")
            )
          )
        )
      )
    )
  )
)
