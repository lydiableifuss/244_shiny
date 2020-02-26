# Load in necessary packages

library(shiny)
library(tidyverse)
library(shinythemes)
library(shinydashboard)

# Read in our data


# User interface

ui <- dashboardPage(
  dashboardHeader(title = "Recharge for Resilience"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Project Information", tabName = "homepage"),
      menuItem("Groundwater Basins", tabName = "basins"),
      menuItem("Recharge Suitability", tabName = "suitability"),
      menuItem("Explore Benefits and Fesibility", tabName = "multibenefit"),
      menuItem("Learn More!", tabName = "referencepage")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "homepage",
        fluidRow(
          box(title = "Project Overview Goes Here")
        )
      ),
      tabItem(
        tabName = "basins",
        fluidRow(
          box(title = "Central Valley Groundwater Basins",
              selectInput("gw_basin",
                          "Choose a groundwater basin to explore further:",
                          choices = c("Madera", "Kern", "Chowchilla", "Kaweah"),
                          selected = NULL))
        )
      ),
      tabItem(
        tabName = "suitability",
        fluidRow(
          box(title = "Recharge Suitability Rankings")
        )
      ),
      tabItem(
        tabName = "multibenefit",
        fluidRow(
          box(title = "Additional Considerations",
              checkboxGroupInput("consideration_select",
                                 label = h3("Choose recharge considerations to visualize"),
                                 choices = c("Conveyance", "GDEs", "Dry Domestic Wells", "EnviroScreen")))
        )
      ),
      tabItem(
        tabName = "referencepage",
        fluidRow(
          box(title = "Thank you to our client EDF")
        )
      )
    )
  )
)


# Server

server <- function(input, output){}



# Put them together to make our app!

shinyApp(ui = ui, server = server)
