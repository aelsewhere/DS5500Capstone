library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # Application title
  titlePanel("Fantasy for Everyone"),
  
  navbarPage("My Application",
    tabPanel("Link 1"),
    tabPanel("Link 2")
  ),
  
  
  sidebarLayout(
    position = "left",
    sidebarPanel(
      "Sidebar Panel",
      p(
        "Paragraph of text. Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum"
      ),
      p(" This is a new paragraph. Next is a line break."),
      br(),
      p(
        "Next paragraph. Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum"
      )
    ),
    mainPanel(
      p(
        "Paragraph of text. Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum"
      ),
      
      imageOutput("example_roster")
    )
  ),
  
  # fluidRow(
  #   column(3,
  #          h3("Buttons"),
  #          actionButton("action", "Action"), br(), br(),
  #          submitButton("Submit")
  #          ),
  #   column(3,
  #          h3("Single Checkbox"),
  #          checkboxInput("checkbox", "Choice A", value=TRUE)
  #          ),
  #   column(3,
  #          checkboxGroupInput("checkGroup",
  #                             h3("Checkbox group"),
  #                             choices = list("Choice 1" = 1,
  #                                            "Choice 2" = 2,
  #                                            "Choice 3" = 3),
  #                             selected = 1)
  #          )
  #   ),
  # fluidRow(
  #   column(3, dateRangeInput("dates", h3("Date Range"))),
  #   column(3, fileInput("file", h3("File Input"))),
  #   column(3,
  #          h3("Help Text"),
  #          helpText("Note: help text isn't a true widget,",
  #                   "But it provides an easy way to add text",
  #                   "to accompany widgets.")
  #          ),
  #   column(3,
  #          numericInput("num",
  #                       h3("Numeric Input"),
  #                       value=1)
  #          )
  #   ),
  #
  # fluidRow(
  #   column(3,
  #          radioButtons("radio",
  #                       h3("Radio Buttons"),
  #                       choices = list("Choice 1" = 1,
  #                                      "Choice 2" = 2,
  #                                      "Choice 3" = 3),
  #                       selected = 1)
  #          ),
  #   column(3,
  #          selectInput("select",
  #                      h3("Select Box"),
  #                      choices = list("Choice1" = 1,
  #                                     "Choice2" = 2,
  #                                     "Choice3" = 3),
  #                      selected = 1)
  #          ),
  #   column(3,
  #          sliderInput("slider1", h3("Sliders"),
  #                      min=0,max=100,value=50),
  #          sliderInput("slider2", "",
  #                      min=0, max=100, value=50)
  #          ),
  #   column(3,
  #          textInput("text",
  #                    h3("Text Input"),
  #                    value = "Enter text....")
  #          )
  #   )
))
