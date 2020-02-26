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
      menuItem("Explore Recharge Suitability", tabName = "suitability_considerations"),
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
        tabName = "suitability_considerations",
        fluidRow(
          box(title = "Benefits and Feasibility Considerations",
              checkboxGroupInput("consideration_select",
                                 label = h3("Choose recharge considerations to visualize"),
                                 choices = c("Conveyance", "GDEs", "Dry Domestic Wells", "EnviroScreen")))
        )
      ),
      tabItem(
        tabName = "referencepage",
        fluidRow(
          box(title = "Data Sources")
        )
      )
    )
  )
)


# Server

server <- function(input, output){}



# Put them together to make our app!

shinyApp(ui = ui, server = server)
