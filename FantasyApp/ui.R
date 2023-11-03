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
      actionButton("submit", star_mand("Submit"), class = "btn-primary"),
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
        nav_panel(title = "Home"),
        nav_panel(title = "Upcoming Games"),
        nav_panel(
          title = "Data",
          h3("Quarterback Predictions"),
          dataTableOutput("qb_pred_table"),
          br(),
          h3("Wide Reciever Predictions"),
          dataTableOutput("wr_pred_table"),
          br(),
          h3("Tight End Predictions"),
          dataTableOutput("te_pred_table"),
          br(),
          h3("Running Back Predictions"),
          dataTableOutput("rb_pred_table")
        ),
        nav_panel(title = "Predictions"),
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
