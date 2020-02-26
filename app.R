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
  dashboardBody()
)


# Server

server <- function(input, output){}



# Put them together to make our app!

shinyApp(ui = ui, server = server)
