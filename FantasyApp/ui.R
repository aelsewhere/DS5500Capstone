library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  # Application title
  titlePanel("Fantasy for Everyone"),
  
  navbarPage(
    "Learn the Game",
    # ---------------------------------------------------
    # Home Page -----------------------------------------
    tabPanel("Home Page",
             h2("Home Page"),
             fluidRow(
               column(5,
                      imageOutput("example_roster")),
               column(
                 10,
                 h3("Current Roster"),
                 helpText(
                   "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum"
                 )
               )
               
             )),
    
    
    #---------------------------------------------------
    # Stats --------------------------------------------
    tabPanel("Stats",
             fluidRow(column(
               5,
               fluidRow(column(12,
                               fluidRow(
                                 column(
                                   3,
                                   selectInput(
                                     "select1",
                                     label = h3("Model Selection: "),
                                     choices = list(
                                       "Model A" = 1,
                                       "Model B" = 2,
                                       "Model C" = 3
                                     ),
                                     selected = 1
                                   )
                                 ),
                                 column(
                                   3,
                                   align = "left",
                                   selectInput(
                                     "select2",
                                     label = h3("selection: "),
                                     choices = list(
                                       "Choice 1" = 1,
                                       "Choice 2" = 2,
                                       "Choice 3" = 3
                                     ),
                                     selected = 1
                                   )
                                 )
                               )))
             ))),
    
    
    #---------------------------------------------------
    # Upcoming -----------------------------------------
    tabPanel("Upcoming",
             h2("Future Games")),
    
    
    #----------------------------------------------------
    # Information----------------------------------------
    tabPanel("Information",
             p(
               "Including an informative tab in some way"
             ),
             br(),
             p(
               "possibly model brakdowns & variable exploration"
             )),
    
    
    # ---------------------------------------------------
    # Data -----------------------------------------------
    tabPanel(
      "Data",
      h2("Data Table"),
      DT::dataTableOutput("fantasy_points")
    )
  )
))
