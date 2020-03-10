# Load in necessary packages
# push again

#General Shiny 
library(shiny)
library(tidyverse)
library(shinythemes)
library(shinydashboard)
library(ggmap)
library(here)
library(janitor)
library(snakecase)
library(RColorBrewer)

#Mapping 
library(paletteer)
library(sf)
library(tmap)
library(mapview)
library(tmaptools)
library(leaflet)
library(htmltools)
library(raster)
library(tiler)
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
  full_join(basin_pop_area) %>% 
  dplyr::select(-sub_basin)

wgs84 = "+proj=longlat +datum=WGS84 +ellps=WGS84 +no_defs" # Just have this ready to copy/paste

max_score_raster <- raster::raster(here::here("data", "Max_final_score_LU.tif"))

max_score_reproj = projectRaster(max_score_raster, crs = wgs84, method = "bilinear")

zipcodes <- read_sf(dsn = here::here("data"),
                           layer = "ZCTA2010") %>% 
  st_transform(crs = 4326) %>% 
  clean_names() %>% 
  dplyr::select(zcta)


# User interface

ui <- navbarPage("Recharge for Resilience",
                 #themeSelector(),
                 theme = shinytheme("paper"),
                 tabPanel("Project Information",
                          h1("Welcome to our shiny app"),
                          p("hopefully it functions by next weeek")
                 ),
                 tabPanel("Groundwater Basins", 
                          sidebarLayout(
                            sidebarPanel("Use your zipcode to identify a groundwater basin!",
                                         textInput("zip_code",
                                                   label = ("Enter an zipcode in the Central Valley to identify its subbasin:"),
                                                   value = "e.g. 93638"),
                                         "Select your groundwater basin to see its location and statistics!",
                                         selectInput("gw_basin",
                                                     label = ("Choose a groundwater basin to see its location and statistics:"),
                                                     choices = c(unique(sgma_basins$sub_basin_final)),
                                                     selected = NULL)
                            ),
                            mainPanel(tmapOutput("ca_map"),
                                      HTML("<br><br><br>"),
                                      tableOutput("basin_table")
                            )
                          )
                 ),
                 tabPanel("Benefits and Feasibility",
                          sidebarLayout(
                            sidebarPanel("Select datasets to visualize in your basin",
                                         checkboxGroupInput("consideration_select",
                                                            label = ("Choose recharge considerations to visualize"),
                                                            choices = c("Conveyance", "GDEs", "Dry Domestic Wells", "EnviroScreen")),
                            ),
                            mainPanel(leafletOutput("max_map")
                            )
                          )),
                 tabPanel("Learn More",
                          h1("Bren School Masters Group Project"),
                          p("Are we done with this yet")),
                 tabPanel("Data Sources",
                          h1("from lots of places"),
                          p("publically available, state agencies or research institutions"))
)




# Server

server <- function(input, output){
  
  ################################################
  # First map!
  
  # Filtering for basins based on dropdown menu
  
  basin_filter <- reactive({
    
    sgma_basins %>% 
      filter(sub_basin_final == input$gw_basin) 
    
  })
  
  # Filtering for zip code based on user entry
  
  zipcode_filter <- reactive({
    
    zipcodes %>% 
      filter(zcta == input$zip_code)
    
  })
  
  
  # Making the reactive map with basin and zip code selections
  
  #basin_labels <- reactive({
    
    #sprintf(
    #"%s, Area: %g acres, Population: %g, DWR Priority: %s",
    #basin_filter()$sub_basin_final, basin_filter()$area_acres, basin_filter()$population, basin_filter()$priority %>% 
      #lapply(htmltools::HTML)
  #)
  #})
  
  basin_map <- reactive({
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addPolygons(data = sgma_basins,
                  label = ~sub_basin_final,
                  labelOptions = labelOptions(direction = 'bottom',
                                              offset=c(0,15)),
                  color = "black",
                  weight = 0.5,
                  fillOpacity = 0.1
                          ) %>% 
      addPolygons(data = basin_filter(),
                  color = "blue",
                  weight = 0.5,
                  fillOpacity = 0.8,
                  label = ~sub_basin_final,
                  labelOptions = labelOptions(direction = 'bottom',
                                              offset=c(0,15))
                  ) %>% 
      addPolygons(data = zipcode_filter(),
                  color = "red",
                  weight = 0.4)
    
 })
  
  
  output$ca_map = renderLeaflet({
    basin_map()
  })
  

  
  ###################################################
  # Table with basin stats!
  
  # table_info <- reactive({
  #   
  #   sgma_basins %>% 
  #     dplyr::filter(sub_basin_final == input$gw_basin) %>% 
  #     dplyr::select(sub_basin_final, area_sq_mi, population, priority)
  #     
  # }) 
  
  output$basin_table <- renderTable({
    
    data.frame(basin_name = c(input$gw_basin), basin_area = c(basin_filter()$area_sq_mi), population = c(basin_filter()$population), DWR_priority = c(basin_filter()$priority))
    
  })
  
  
  ####################################################
  #Second Map!
  
  max_score_filter <- reactive({
    raster::mask(max_score_raster, basin_filter())
    })
  # 'mask' is not working, need to find a new method of clipping raster to selected basin 

  
  max_score_map <- reactive({
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addRasterImage(max_score_filter()) 
  })
  
  output$max_map <- renderLeaflet({
    max_score_map()
  })
  
}



# Put them together to make our app!

shinyApp(ui = ui, server = server)

