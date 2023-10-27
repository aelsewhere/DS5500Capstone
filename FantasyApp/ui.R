library(shiny)
library(ggplot2)
library(DT)
library(plotly)
library(bslib)
library(gridlayout)

ui <- grid_page(
  shinyjs::useShinyjs(),
  shinyjs::inlineCSS(appCSS),
  layout = c(
    "header  header header",
    "sidebar area3  area3 ",
    "sidebar area3  area3 ",
    "sidebar area3  area3 "
  ),
  row_sizes = c(
    "45px",
    "1fr",
    "1fr",
    "1fr"
  ),
  col_sizes = c(
    "210px",
    "1.47fr",
    "0.53fr"
  ),
  gap_size = "1rem",
  grid_card(
    area = "sidebar",
    h3("My Roster"),
    div(
      id = "form",
      textInput("myRosterQB", star_mand("Quarterback"), ""),
      textInput("myRosterRB1", star_mand("Running Back"), ""),
      textInput("myRosterRB2", "Running Back", ""),
      textInput("myRosterWR1", star_mand("Wide Receiver"), ""),
      textInput("myRosterWR2", "Wide Receiver", ""),
      textInput("myRosterTE", star_mand("Tight End"), ""),
      textInput("myRosterFlex", "Flex", ""),
      textInput("myRosterDST", "Defense/Special Team", ""),
      textInput("myRosterK", star_mand("Kicker"), ""),
      actionButton("submit", star_mand("Submit"), class = "btn-primary")
    ),
    shinyjs::hidden(
      div(
        id = "roster_submit_mssg",
        h3("Roster Saved."),
        actionLink("resubmit_", "Resubmit Roster.")
      )
    ),
    card_footer()
  ),
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
        nav_panel(title = "Upcoming Games"),
        nav_panel(title = "Predictions"),
        nav_panel(title = "Player Statistics"),
        navbarMenu(
          title = "Learn",
          tabPanel("About the Game"),
          tabPanel("About the App"),
          tabPanel("About the Creators")
        ),
        nav_panel(
          title = "Data",
          h3("Quarterback Predictions"),
          tableOutput("qb_pred_table"),
          br(),
          h3("Wide Reciever Predictions"),
          tableOutput("wr_pred_table"),
          br(),
          h3("Tight End Predictions"),
          tableOutput("te_pred_table"),
          br(),
          h3("Running Back Predictions"),
          tableOutput("rb_pred_table")
        )
      )
    )
  )
)
