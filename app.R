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
library(htmltools)
library(raster)
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

basin_pop_area <- read_csv(here("data", "basin_pop_area.csv"))

sgma_basins <- sgma_basins_all %>% 
  separate(basin_su_1, c("basin", "sub_basin"), sep = " - ") %>% 
  mutate(sub_basin_final = ifelse(is.na(sub_basin), basin, sub_basin)) %>% 
  mutate(sub_basin_final = to_upper_camel_case(sub_basin_final, sep_out = " ")) %>% 
  arrange(sub_basin_final) %>% 
  inner_join(basin_pop_area)

wgs84 = "+proj=longlat +datum=WGS84 +ellps=WGS84 +no_defs" # Just have this ready to copy/paste

max_score_raster <- raster::raster(here::here("data", "Max_final_score_LU.tif"))

max_score_reproj = projectRaster(max_score_raster, crs = wgs84, method = "bilinear")


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
                          selected = NULL)),
            box(title = "Central Valley GW Basins",
                textInput("address",
                          label = ("Enter an address in the Central Valley to explore further:"),
                          value = "Address"))  
        ),
        fluidPage(
          box(title = "Map of Groundwater Basins",
          tmapOutput("ca_map", height = 425, width = 425),
          status = "info",
          width = 8
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
  
  basin_filter <- reactive({
    
    sgma_basins %>% 
      filter(sub_basin_final == input$gw_basin)
    
  })
  
  
  basin_labels <- reactive({
    
    sprintf(
    "%s, Area: %g acres, Population: %g, DWR Priority: %s",
    basin_filter()$sub_basin_final, basin_filter()$area_acres, basin_filter()$population, basin_filter()$priority %>% 
      lapply(htmltools::HTML)
  )
  })
  
  basin_map <- reactive({
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addPolygons(data = sgma_basins,
                  label = ~sub_basin_final,
                  color = "black",
                  weight = 0.5,
                  fillOpacity = 0.1
                          ) %>% 
      addPolygons(data = basin_filter(),
                  color = "blue",
                  weight = 0.5,
                  fillOpacity = 0.8,
                  label = basin_labels(),
                  labelOptions = labelOptions(direction = 'bottom',
                                              offset=c(0,15)))
 })
  
  
  output$ca_map = renderLeaflet({
    basin_map()
  })
  
  #################
  #Second Map!
  
  max_score_filter <- reactive({
    
    sgma_basins %>% 
      filter(sub_basin_final == input$gw_basin)
    
  })
}



# Put them together to make our app!

shinyApp(ui = ui, server = server)
