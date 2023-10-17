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
             h2(
               "Home Page"
             ),
             fluidRow(column(10,
                             imageOutput("example_roster")),
                      column(3,
                             h3(
                               "Current Roster"
                             ),
                             helpText("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum"))
               
             )),
    
    
    #---------------------------------------------------
    # Stats --------------------------------------------
    tabPanel("Stats"),
    
    
    #---------------------------------------------------
    # Upcoming -----------------------------------------
    tabPanel("Upcoming",
             h2(
               "Future Games"
             )),
    
    
    #----------------------------------------------------
    # Information----------------------------------------
    tabPanel("Information"),
    
    
    # ---------------------------------------------------
    # Data -----------------------------------------------
    tabPanel("Data",
             h2(
               "Data Table"
             ),
             DT::dataTableOutput("fantasy_points"))
  )
  
  
))
