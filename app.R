# Load in necessary packages

#General Shiny 
library(shiny)
library(tidyverse)
library(shinythemes)
library(shinydashboard)
library(ggmap)
library(here)
library(janitor)
library(snakecase)

#Mapping 
library(paletteer)
library(sf)
library(tmap)
library(mapview)
library(tmaptools)
library(leaflet)
#library(rmapshaper)

#1 Map of central valley basins, when you select your basin, the fill color changes (cv.shp and sgma_basins.shp)

# Read in our data
cv_all <- read_sf(dsn = here::here("data"),
                  layer = "cv") %>% 
  st_transform(crs = 4326) %>% 
  clean_names()

sgma_basins_all <- read_sf(dsn = here::here("data"),
                           layer = "sgma_basins") %>% 
  st_transform(crs = 4326) %>% 
  clean_names() 

sgma_basins <- sgma_basins_all %>% 
  separate(basin_su_1, c("basin", "sub_basin"), sep = " - ") %>% 
  mutate(sub_basin_final = ifelse(is.na(sub_basin), basin, sub_basin)) %>% 
  mutate(sub_basin_final = to_upper_camel_case(sub_basin_final)) 




# User interface

ui <- dashboardPage(
  dashboardHeader(title = "Recharge for Resilience"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Project Information", 
               tabName = "homepage", 
               icon = icon("tint")),
      menuItem("Groundwater Basins", 
               tabName = "basins", 
               icon = icon("map-marker-alt")),
      menuItem("Explore Recharge Suitability", 
               tabName = "suitability_considerations", 
               icon = icon("search-plus")),
      menuItem("Learn More!", 
               tabName = "referencepage", 
               icon = icon("book-open"))
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
                          label = ("Choose a groundwater basin to explore further:"),
                          choices = c(unique(sgma_basins$sub_basin_final)),
                          selected = NULL))
        ),
        fluidPage(
          box(title = "Map of Groundwater Basins",
          tmapOutput("ca_map")
        ),
        fluidRow(
          box(title = "Central Valley GW Basins",
              textInput("address",
                        label = ("Enter an address in the Central Valley to explore further:"),
                        value = "Address"))
        )
      )
      ),
      tabItem(
        tabName = "suitability_considerations",
        fluidRow(
          box(title = "Benefits and Feasibility Considerations",
              checkboxGroupInput("consideration_select",
                                 label = ("Choose recharge considerations to visualize"),
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

server <- function(input, output){
  
  gw_basin <- reactive({
    sgma_basins %>% 
      filter(basin_name %in% input$gw_basin)
  })
  
  output$ca_map = renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addPolygons(data = sgma_basins)
      
  }
  )
  
}



# Put them together to make our app!

shinyApp(ui = ui, server = server)
