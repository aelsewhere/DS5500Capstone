library(shiny)
library(ggplot2)
library(DT)
library(plotly)
library(bslib)
library(gridlayout)

ui <- grid_page(
  layout = c(
    "header  header header",
    "sidebar area3  area3 ",
    "sidebar area3  area3 ",
    "sidebar area3  area3 "
  ),
  row_sizes = c("45px",
                "1fr",
                "1fr",
                "1fr"),
  col_sizes = c("210px",
                "1.47fr",
                "0.53fr"),
  gap_size = "1rem",
  grid_card(
    area = "sidebar",
    h3("My Roster"),
    card_body(
      textInput(
        inputId = "myRosterQB",
        label = "Quarterback",
        value = ""
      ),
      textInput(
        inputId = "myRosterRB1",
        label = "Running Back",
        value = ""
      ),
      textInput(
        inputId = "myRosterRB2",
        label = "Running Back",
        value = ""
      ),
      textInput(
        inputId = "myRosterWR1",
        label = "Wide Receiver",
        value = ""
      ),
      textInput(
        inputId = "myRosterWR2",
        label = "Wide Receiver",
        value = ""
      ),
      textInput(
        inputId = "myRosterTE",
        label = "Tight End",
        value = ""
      ),
      textInput(
        inputId = "myRosterFlex",
        label = "Flex",
        value = ""
      ),
      textInput(
        inputId = "myRosterDST",
        label = "Defense/Special Team",
        value = ""
      ),
      textInput(
        inputId = "myRosterK",
        label = "Kicker",
        value = ""
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
  grid_card(area = "area3",
            card_body(
              tabsetPanel(
                nav_panel(title = "Upcoming Games"),
                nav_panel(title = "Predictions"),
                nav_panel(title = "Player Statistics"),
                nav_panel(title = "Learn"),
                nav_panel(title = "Data")
              )
            ))
)
