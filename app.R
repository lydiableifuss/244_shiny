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
library(shinyjs)
library(DT)
library(visNetwork)
library(rintrojs)
library(stringr)
library(png)
library(shinyWidgets)

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

drywells <- read_sf(here("data",
                         "drywells_cv.shp")) %>% 
  st_transform(crs = 4326)


# User interface

ui <- navbarPage(
  
  header = tagList(
  useShinydashboard()
),

"Recharge for Resilience",
                 #themeSelector(),
                 theme = shinytheme("paper"),
                 tabPanel("Project Information",
                          icon = icon("home"),
                          h1("Welcome to our shiny app",
                             style = "font-size:40px",
                             align = "center"),
                          shiny::HTML("
                          <p> California has an increasingly scarce and unreliable surface water supply. As the climate changes, droughts are expected to become more frequent and extreme, precipitation is expected to fall as rain rather than snow in shorter, more intense periods, and reliance on the Sierra snowpack for storage will become less tenable. Strategically planning for water storage, including the protection and augmentation of groundwater resources, can help make farms, cities, and ecosystems more resilient to less predictable future water availability. <br> <br>

                            The Sustainable Groundwater Management Act of 2014 (SGMA) adds regulatory structure to the goal of protecting and augmenting groundwater supplies by requiring a regionalized approach to groundwater management throughout the state. Many Groundwater Sustainability Agencies (GSAs) have identified managed aquifer recharge (MAR) as a tool they will use to comply with SGMA during  the 20-year implementation period, beginning in 2020. <br> <br>
                            
                            Currently, groundwater managers lack the tools and information necessary to identify ideal locations to invest in groundwater recharge projects that are able to achieve multiple benefits. The spatial visualization in this app allows users to see how physical surface and subsurface conditions along with a surficial nitrogen balance inform areas that are better and worse for implementing recharge in a groundwater basin. In addition, users are able to overlay the location of other points of interest, including: domestic wells that have run dry, potential groundwater dependent ecosystems, listed contamination cleanup sites, and water conveyance infrastructure. <br> <br>
                            
                            This tool makes information regarding multi-benefit groundwater recharge at a regional level available to groundwater management entities, allowing Groundwater Sustainability Agencies to meet compliance requirements while realizing other locally relevant benefits. The tool incorporates publicly available information from research institutions and state agencies, eliminating some costs associated with using a recharge siting tool and ensuring transferability across the Central Valley to basins with a varying degree of local data. <br> <br>
                            
                            The Sustainable Groundwater Management Act presents an opportunity to reinvision California’s approach to water management by including a more comprehensive consideration of the interconnected benefits associated with groundwater recharge and storage. The information available in this tool will facilitate the realization of a suite of benefits to be gained through the implementation of groundwater recharge projects in California and will help the state move forward in achieving a sustainable and resilient water future.
                            "),
                          img(src="image.jpg", height="100%",width="100%",style = 'position: absolute; opacity: 0.2;'
                          ),
                          tags$hr()
                 ),
                 tabPanel("Groundwater Basins", 
                          icon = icon("tint"),
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
                          icon = icon("swatchbook"),
                          sidebarLayout(
                            sidebarPanel("Select datasets to visualize in your basin",
                                         checkboxGroupInput("consideration_select",
                                                            label = ("Choose recharge considerations to visualize"),
                                                            choices = c("Conveyance", "GDEs", "Dry Domestic Wells", "EnviroScreen"))
                            ),
                            mainPanel(leafletOutput("max_map")
                            )
                          )),
                 tabPanel("Learn More",
                          icon = icon("envelop"),
                          h1("Bren School Masters Group Project"),
                          p("Are we done with this yet")),
                 tabPanel("Data Sources",
                          icon = icon("server"),
                          shiny::HTML("<h3><b> References: </b></h1>
                                      <p> [1] Soil Agricultural Groundwater Banking Index. Available at: https://casoilresource.lawr.ucdavis.edu/sagbi/ <br><br>
                                      [2] Depth to Groundwater. https://gis.water.ca.gov/app/gicima/ <br><br>
                                      [3] Corcoran Clay Depth: https://water.usgs.gov/GIS/metadata/usgswrd/XML/pp1766_corcoran_clay_depth_feet.xml <br><br>
                                      [4] Corcoran Clay Thickness: https://water.usgs.gov/GIS/metadata/usgswrd/XML/pp1766_corcoran_clay_thickness_feet.xml <br><br>
                                      [5] National Hydrography Dataset: https://www.usgs.gov/core-science-systems/ngp/national-hydrography/nhdplus-high-resolution <br><br>
                                      [6] Natural Communities Commonly Associated With Groundwater: https://gis.water.ca.gov/app/NCDatasetViewer/ <br><br>
                                      [7] GeoTracker: https://geotracker.waterboards.ca.gov/map/?CMD=runreport&myaddress=Sacramento <br><br>
                                      [8] California Household Water Shortage Data: https://mydrywatersupply.water.ca.gov/report/publicpage <br><br>
                                      [9] CalEnviroScreen: https://oehha.maps.arcgis.com/apps/webappviewer/index.html?id=4560cfbce7c745c299b2d0cbb07044f5 <br><br>
                                      [10] California Zip Codes: https://earthworks.stanford.edu/catalog/stanford-dc841dq9031"))
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
  
  
  output$basin_table <- renderTable({
    
    table_df <- data.frame(basin_name = c(input$gw_basin), basin_area = c(basin_filter()$area_sq_mi), population = c(basin_filter()$population), DWR_priority = c(basin_filter()$priority))
    
    `colnames<-`(table_df, c("Basin Name", "Area (sq. mi.)", "Population", "DWR Priority"))
    
  })
  
  
  ####################################################
  #Second Map!
  
  basin_select <- reactive({ 
   
    sgma_basins %>% 
      dplyr::filter(sub_basin_final == input$gw_basin)
    
    })
  
   max_score_filter <- reactive({
    
    raster_mask <- raster::mask(max_score_reproj, basin_select())
    
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

