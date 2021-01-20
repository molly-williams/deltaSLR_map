# Code adapted in part from the COVID-19 mapper app: https://shiny.rstudio.com/gallery/covid19-tracker.html,
# and the SuperZIP map: https://shiny.rstudio.com/gallery/superzip-example.html, 

library(shiny)
library(leaflet)
library(RColorBrewer)
library(raster)
library(tidyverse)
library(rgeos)
library(sf)
require(stringr)
library(htmlwidgets)

###################### Data import and wrangling ###########################

# Note: these polygons were last updated 19 Nov 2020 and were copied in Jan '21 from G:ArcGIS\Projects\CCVA_Flood\DeltaAdapts_Mapping_KG\DeltaAdapts_FloodMap_InputData\201120_FloodMap_Inputs\Deterministic111920



# Import individual deterministic polygons

M0_det <- read_sf("3_ShinyData/Deterministic/BaseDet.shp") %>% 
  select(NAME, fldfight) %>% 
  mutate(Probability = NA) %>% 
  mutate(scenario = "Deterministic: 0' SLR") %>% 
  st_transform(4326)

M1 <- read_sf("3_ShinyData/Deterministic/HlfDet.shp") %>% 
  select(NAME, fldfight) %>%
  mutate(Probability = NA) %>% 
  mutate(scenario = "Deterministic: 2030, 0.5' SLR") %>%
  st_transform(4326)

M2 <- read_sf("3_ShinyData/Deterministic/OneDet.shp") %>% 
  select(NAME, fldfight) %>% 
  mutate(Probability = NA) %>% 
  mutate(scenario = "Deterministic: 2050, 1' SLR") %>% 
  st_transform(4326)

M3 <- read_sf("3_ShinyData/Deterministic/TwoDet.shp") %>% 
  select(NAME, fldfight) %>%
  mutate(Probability = NA) %>% 
  mutate(scenario = "Deterministic: 2050, 2' SLR") %>%
  st_transform(4326)

M4 <- read_sf("3_ShinyData/Deterministic/3_5Det.shp") %>% 
  select(NAME, fldfight) %>%
  mutate(Probability = NA) %>% 
  mutate(scenario = "Deterministic: 2050+, 3.5' SLR") %>%
  st_transform(4326)


### Probabilistic polygons 
## Import separate shapefiles and combine 10, 50, 100 and 200 yr exposure polygons for each scenario
## Then clip each so that they don't overlap (overlap causes shading to become opaque and colors to be misleading)

# M0 - existing conditions
M0_prob_10 <- read_sf("3_ShinyData/Probabilistic/M010yrpoly.shp") %>% 
  select(NAME) %>%
  mutate(fldfight = NA) %>% # create a floodfight column so that it can rbind with deterministic polygons
  mutate(Probability = 10) %>%  # specify flood exposure probability for legend distinction
  mutate(scenario = "Probabilistic: Existing conditions") %>% # Match to scenario in dropdown menu
  st_transform(4326) # standardize projection

M0_prob_50 <- read_sf("3_ShinyData/Probabilistic/M050yrpoly.shp") %>% 
  select(NAME) %>%
  mutate(fldfight = NA) %>% 
  mutate(Probability = 50) %>% 
  mutate(scenario = "Probabilistic: Existing conditions") %>%
  st_transform(4326) %>% 
  filter(!NAME %in% M0_prob_10$NAME)  # clip 


M0_prob_100 <- read_sf("3_ShinyData/Probabilistic/M0100yrpoly.shp") %>% 
  select(NAME) %>%
  mutate(fldfight = NA) %>% 
  mutate(Probability = 100) %>% 
  mutate(scenario = "Probabilistic: Existing conditions") %>%
  st_transform(4326) %>% 
  filter(!NAME %in% M0_prob_10$NAME) %>% # this is very inelegant... could use a better solution to clip out previous polys
  filter(!NAME %in% M0_prob_50$NAME) 


M0_prob_200 <- read_sf("3_ShinyData/Probabilistic/M0200yrpoly.shp") %>% 
  select(NAME) %>%
  mutate(fldfight = NA) %>%
  mutate(Probability = 200) %>% 
  mutate(scenario = "Probabilistic: Existing conditions") %>%
  st_transform(4326) %>% 
  filter(!NAME %in% M0_prob_10$NAME) %>% 
  filter(!NAME %in% M0_prob_50$NAME) %>% 
  filter(!NAME %in% M0_prob_100$NAME) 


M0_prob <- rbind(M0_prob_10, M0_prob_50, M0_prob_100, M0_prob_200)


# M5 - 2030 conditions
M5_prob_10 <- read_sf("3_ShinyData/Probabilistic/M510yrpoly.shp") %>% 
  select(NAME) %>%
  mutate(fldfight = NA) %>% 
  mutate(Probability = 10) %>% 
  mutate(scenario = "Probabilistic: 2030 conditions") %>%
  st_transform(4326)

M5_prob_50 <- read_sf("3_ShinyData/Probabilistic/M550yrpoly.shp") %>% 
  select(NAME) %>%
  mutate(fldfight = NA) %>% 
  mutate(Probability = 50) %>% 
  mutate(scenario = "Probabilistic: 2030 conditions") %>%
  st_transform(4326) %>% 
  filter(!NAME %in% M5_prob_10$NAME) 


M5_prob_100 <- read_sf("3_ShinyData/Probabilistic/M5100yrpoly.shp") %>% 
  select(NAME) %>%
  mutate(fldfight = NA) %>% 
  mutate(Probability = 100) %>% 
  mutate(scenario = "Probabilistic: 2030 conditions") %>%
  st_transform(4326) %>% 
  filter(!NAME %in% M5_prob_10$NAME) %>% 
  filter(!NAME %in% M5_prob_50$NAME) 


M5_prob_200 <- read_sf("3_ShinyData/Probabilistic/M5200yrpoly.shp") %>% 
  select(NAME) %>%
  mutate(fldfight = NA) %>%
  mutate(Probability = 200) %>% 
  mutate(scenario = "Probabilistic: 2030 conditions") %>%
  st_transform(4326) %>% 
  filter(!NAME %in% M5_prob_10$NAME) %>% 
  filter(!NAME %in% M5_prob_50$NAME) %>% 
  filter(!NAME %in% M5_prob_100$NAME) 


M5 <- rbind(M5_prob_10, M5_prob_50, M5_prob_100, M5_prob_200)


# M6 - 2050 conditions
M6_prob_10 <- read_sf("3_ShinyData/Probabilistic/M610yrpoly.shp") %>% 
  select(NAME) %>%
  mutate(fldfight = NA) %>% 
  mutate(Probability = 10) %>% 
  mutate(scenario = "Probabilistic: 2050 conditions") %>%
  st_transform(4326)

M6_prob_50 <- read_sf("3_ShinyData/Probabilistic/M650yrpoly.shp") %>% 
  select(NAME) %>%
  mutate(fldfight = NA) %>% 
  mutate(Probability = 50) %>% 
  mutate(scenario = "Probabilistic: 2050 conditions") %>%
  st_transform(4326) %>% 
  filter(!NAME %in% M6_prob_10$NAME) 

  

M6_prob_100 <- read_sf("3_ShinyData/Probabilistic/M6100yrpoly.shp") %>% 
  select(NAME) %>%
  mutate(fldfight = NA) %>% 
  mutate(Probability = 100) %>% 
  mutate(scenario = "Probabilistic: 2050 conditions") %>%
  st_transform(4326) %>% 
  filter(!NAME %in% M6_prob_10$NAME) %>% 
  filter(!NAME %in% M6_prob_50$NAME) 


M6_prob_200 <- read_sf("3_ShinyData/Probabilistic/M6200yrpoly.shp") %>% 
  select(NAME) %>%
  mutate(fldfight = NA) %>%
  mutate(Probability = 200) %>% 
  mutate(scenario = "Probabilistic: 2050 conditions") %>%
  st_transform(4326) %>% 
  filter(!NAME %in% M6_prob_10$NAME) %>% 
  filter(!NAME %in% M6_prob_50$NAME) %>% 
  filter(!NAME %in% M6_prob_100$NAME) 


M6 <- rbind(M6_prob_10, M6_prob_50, M6_prob_100, M6_prob_200)


# M7 - 2085 conditions
M7_prob_10 <- read_sf("3_ShinyData/Probabilistic/M710yrpoly.shp") %>% 
  select(NAME) %>%
  mutate(fldfight = NA) %>% 
  mutate(Probability = 10) %>% 
  mutate(scenario = "Probabilistic: 2085 conditions") %>%
  st_transform(4326)

M7_prob_50 <- read_sf("3_ShinyData/Probabilistic/M750yrpoly.shp") %>% 
  select(NAME) %>%
  mutate(fldfight = NA) %>% 
  mutate(Probability = 50) %>% 
  mutate(scenario = "Probabilistic: 2085 conditions") %>%
  st_transform(4326) %>% 
  filter(!NAME %in% M7_prob_10$NAME) 


M7_prob_100 <- read_sf("3_ShinyData/Probabilistic/M7100yrpoly.shp") %>% 
  select(NAME) %>%
  mutate(fldfight = NA) %>% 
  mutate(Probability = 100) %>% 
  mutate(scenario = "Probabilistic: 2085 conditions") %>%
  st_transform(4326) %>% 
  filter(!NAME %in% M7_prob_10$NAME) %>% 
  filter(!NAME %in% M7_prob_50$NAME) 


M7_prob_200 <- read_sf("3_ShinyData/Probabilistic/M7200yrpoly.shp") %>% 
  select(NAME) %>%
  mutate(fldfight = NA) %>%
  mutate(Probability = 200) %>% 
  mutate(scenario = "Probabilistic: 2085 conditions") %>%
  st_transform(4326) %>% 
  filter(!NAME %in% M7_prob_10$NAME) %>% 
  filter(!NAME %in% M7_prob_50$NAME) %>% 
  filter(!NAME %in% M7_prob_100$NAME) 


M7 <- rbind(M7_prob_10, M7_prob_50, M7_prob_100, M7_prob_200)





det_prob_polys <- rbind(M0_det, M1, M2, M3, M4, M0_prob, M5, M6, M7)



#levees <- read_sf("3_ShinyData/Deterministic/leveeOutput11302020.shp") %>% 
#  st_transform(4326)


############ Import assets 
# Note: these data came from AECOM's DSC Asset Output folder on sharepoint from Dec 2020. 

#### Transportation infrastructure
airstrips <- read_sf("3_ShinyData/Assets/trans_airstrips.shp") %>% 
  select(Type=SOURCE_D_1, geometry) %>% 
  st_transform(4326)

bridges <- read_sf("3_ShinyData/Assets/trans_bridges.shp") %>% 
  select(geometry) %>% 
  mutate(Type="Bridge") %>% 
  st_transform(4326)

trans_points <- rbind(airstrips, bridges)


scenic_hwys <- read_sf("3_ShinyData/Assets/trans_scenic_hwys.shp") %>% 
  select(geometry) %>% 
  mutate(Type="Scenic Highway") %>% 
  st_transform(4326)

county_hwys <- read_sf("3_ShinyData/Assets/trans_county_highways.shp") %>% 
  select(geometry) %>% 
  mutate(Type="County Highway") %>% 
  st_transform(4326)
  
highways <- read_sf("3_ShinyData/Assets/trans_highways.shp") %>% 
  select(geometry) %>% 
  mutate(Type="Highway") %>% 
  st_transform(4326)
  
railroads <- read_sf("3_ShinyData/Assets/trans_railroads.shp") %>% 
  select(geometry) %>% 
  mutate(Type="Railroad") %>% 
  st_transform(4326)

bike_routes <- read_sf("3_ShinyData/Assets/trans_bike_routes.shp") %>% 
  select(geometry) %>% 
  mutate(Type="Bike Route") %>% 
  st_transform(4326)

trans_lines <- rbind(county_hwys, scenic_hwys) # not working????

#### Energy 

############## Import combined simplified probabilistic polygons
# Note: these polygons are outputs from the overtopping analysis script in this repository
flood_polys <- read_sf("3_ShinyData/poly_combined_simp.shp")


# Transform to lat/long
flood_polys <- st_transform(flood_polys, 4326)


# Rename columns and hydrology/probability categories +re-level
flood_polys <- flood_polys %>% 
    dplyr::select(Hydrology=hydro, SLR, Probability=Prob) %>% 
    dplyr::mutate(Hydrology = ifelse(str_detect(Hydrology, "E"), "2085", 
                                     ifelse(str_detect(Hydrology, "M"), 2050, "Historical"))) %>% 
    dplyr::mutate(Probability = factor(ifelse(Probability == 10, "10 year flood exposure",
                                      ifelse(Probability == 50, "50 year flood exposure", 
                                             ifelse(Probability == 100, "100 year flood exposure", "200 year flood exposure")
                                             )
                                      ), levels=c("10 year flood exposure", "50 year flood exposure", "100 year flood exposure", "200 year flood exposure")) 
                  )

# Convert SLR from factor to numeric to enable slider (have to coerce to character first to preserve data)
flood_polys$SLR <- as.numeric(as.character(flood_polys$SLR)) 


# Split combined shapefile into four (one for each probabilistic scenario)
flood_polys10 <- flood_polys %>% 
  filter(Probability == "10 year flood exposure")

flood_polys50 <- flood_polys %>% 
  filter(Probability == "50 year flood exposure")

flood_polys100 <- flood_polys %>% 
  filter(Probability == "100 year flood exposure")

flood_polys200 <- flood_polys %>% 
  filter(Probability == "200 year flood exposure")


# Add Delta + Suisun Marsh boundary shapefile and reproject
delta_sm <- read_sf("3_ShinyData/LD_SM_Merged.shp") %>% 
  st_transform(4326) 

# Add not-modeled polygon shapefile and reproject
not_modeled <- read_sf("3_ShinyData/NotModeled_201001.shp") %>% 
  st_transform(4326) %>% 
  mutate(Modeled= "Regions Not Modeled") # for legend
               

# Add vulnerability shapefile and color palette to match SOVI app
sovi <- read_sf("3_ShinyData/sovi_simp.shp") %>% 
  select("Vulnerability" = Vlnrblt, "Rating"=VULNBLT) %>% 
  st_transform(4326)


################### Create palettes and popup text ###############

colorData <- ifelse(sovi$Vulnerability == 1, "1: Moderate", ifelse(sovi$Vulnerability == 2, "2: High", "3: Highest"))  
sovi_pal <- colorFactor("Reds", colorData)

# Import probability category table
prob_legend <- read_csv("3_ShinyData/prob_legend.csv")


# Create color palettes
#pal <- colorFactor(c("#08519C", "#3182BD", "#6BAED6", "#BDD7E7"), ordered=TRUE, flood_polys$Probability, na.color="transparent")

nm_pal <- colorFactor(c("#969696"), ordered=TRUE, not_modeled$Modeled, na.color="transparent")


#display.brewer.all(colorblindFriendly = T)
#brewer.pal(5, "Blues") = "#EFF3FF" "#BDD7E7" "#6BAED6" "#3182BD" "#08519C"
#display.brewer.pal(5, "Blues") - look at colors


# Create text for popups
popup_text10 <- paste(
  flood_polys10$Probability, sep="") %>%
  lapply(htmltools::HTML)

popup_text50 <- paste(flood_polys50$Probability, sep="") %>%
  lapply(htmltools::HTML)

popup_text100 <- paste(flood_polys100$Probability, sep="") %>%
  lapply(htmltools::HTML)

popup_text200 <- paste(flood_polys200$Probability, sep="") %>%
  lapply(htmltools::HTML)


mb_url <- "https://api.mapbox.com/styles/v1/moowill/cki99lcza15g818phcibhxpv2/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoibW9vd2lsbCIsImEiOiJja2kzejloOHkxdzNtMnhxcTAwY3Zqa25zIn0.VCsBGYnJr6Z7A7XnD157cg"
# get link from published mapbox map through share > click third party > select "CARTO" > copy link 
# includes access token




######################################## Server logic ###########################################

function(input, output, session) {

    # Create point data for interactive location pins
#    points <- eventReactive(input$recalc, {
#        cbind(input$long, input$lat)
#    }, ignoreNULL = FALSE)


  # Render table in sidebar
  
  output$table <- renderTable(prob_legend,
                              striped=TRUE,
                              spacing="xs") 


  
  
############################ Delta Adapts Scenarios server logic ############################
  
  # Create deterministic basemap
  
  output$map2 <- renderLeaflet({
    leaflet() %>% 
      addTiles(
        urlTemplate = mb_url,
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      )  %>% 
      setView(lat=38.1, lng=-121.8, zoom=10) 
    #      addMarkers(data = points(), group="Location Pin")
    
    
  })  

  filteredData <- reactive({
    det_prob_polys[det_prob_polys$scenario == input$scenario,]
  })
  

  
  
  # Code for reactice color palettes 
  # Note: this could probably be condensed since all the deterministic have the same palette and all the probabilistic have the same palette
  observe({
    colorBy <- input$scenario
    
    if (colorBy == "Deterministic: 0' SLR") {
      colorData <- ifelse(filteredData()$fldfight == 1, "Flooding (mitigable with flood fighting)", "Flooding")
      pal <- colorFactor(c("#08519C", "#6BAED6"), colorData) 
    }
    
    else if (colorBy == "Deterministic: 2030, 0.5' SLR") {
      colorData <- ifelse(filteredData()$fldfight == 1, "Flooding (mitigable with flood fighting)", "Flooding")
      pal <- colorFactor(c("#08519C", "#6BAED6"), colorData) 
    }
    
    else if (colorBy == "Deterministic: 2050, 1' SLR") {
      colorData <- ifelse(filteredData()$fldfight == 1, "Flooding (mitigable with flood fighting)", "Flooding")
      pal <- colorFactor(c("#08519C", "#6BAED6"), colorData) 
    }
    
    else if (colorBy == "Deterministic: 2050, 2' SLR") {
      colorData <- ifelse(filteredData()$fldfight == 1, "Flooding (mitigable with flood fighting)", "Flooding")
      pal <- colorFactor(c("#08519C", "#6BAED6"), colorData) 
    }
    
    else if (colorBy == "Deterministic: 2050+, 3.5' SLR") {
      colorData <- ifelse(filteredData()$Probability == 1, "Flooding (mitigable with flood fighting)", "Flooding")
      pal <- colorFactor(c("#08519C", "#6BAED6"), colorData) 
    }
    
    else if (colorBy == "Probabilistic: Existing conditions") {
      colorData <- ifelse(filteredData()$Probability == 10, "1. Very high likelihood | 10%+ annual chance | 65%+ chance over 10 years", 
                          ifelse(filteredData()$Probability == 50, "2. High likelihood | 18-65% chance over 10 years | 2-10% annual chance",
                                 ifelse(filteredData()$Probability == 100, "3. Medium likelihood | 1-2% annual chance | 10-18% chance over 10 years", 
                                        "4. Low Likelihood | 0.5-1% annual chance | 5-10% chance over 10 years")))
      pal <- colorFactor(c("#08519C", "#3182BD", "#6BAED6", "#BDD7E7"), colorData) 
    }
    
    else if (colorBy == "Probabilistic: 2030 conditions") {
      colorData <- ifelse(filteredData()$Probability == 10, "1. Very high likelihood | 10%+ annual chance | 65%+ chance over 10 years", 
                          ifelse(filteredData()$Probability == 50, "2. High likelihood | 18-65% chance over 10 years | 2-10% annual chance",
                                 ifelse(filteredData()$Probability == 100, "3. Medium likelihood | 1-2% annual chance | 10-18% chance over 10 years", 
                                        "4. Low Likelihood | 0.5-1% annual chance | 5-10% chance over 10 years")))
      pal <- colorFactor(c("#08519C", "#3182BD", "#6BAED6", "#BDD7E7"), colorData) 
    }
    
    else if (colorBy == "Probabilistic: 2050 conditions") {
      colorData <- ifelse(filteredData()$Probability == 10, "1. Very high likelihood | 10%+ annual chance | 65%+ chance over 10 years", 
                          ifelse(filteredData()$Probability == 50, "2. High likelihood | 18-65% chance over 10 years | 2-10% annual chance",
                                 ifelse(filteredData()$Probability == 100, "3. Medium likelihood | 1-2% annual chance | 10-18% chance over 10 years", 
                                        "4. Low Likelihood | 0.5-1% annual chance | 5-10% chance over 10 years")))
      pal <- colorFactor(c("#08519C", "#3182BD", "#6BAED6", "#BDD7E7"), colorData) 
    }
    
    else { 
      colorData <- ifelse(filteredData()$Probability == 10, "1. Very high likelihood | 10%+ annual chance | 65%+ chance over 10 years", 
                          ifelse(filteredData()$Probability == 50, "2. High likelihood | 18-65% chance over 10 years | 2-10% annual chance",
                                 ifelse(filteredData()$Probability == 100, "3. Medium likelihood | 1-2% annual chance | 10-18% chance over 10 years", 
                                        "4. Low Likelihood | 0.5-1% annual chance | 5-10% chance over 10 years")))
      pal <- colorFactor(c("#08519C", "#3182BD", "#6BAED6", "#BDD7E7"), colorData) 
    }
    
    
    
    # Create reactive popup text 

      
    det_popup <- paste(
      "<span style='font-size: 120%'><strong>", filteredData()$NAME,"</strong></span><br/>", 
      "Population: ", #comma(filteredData()$pop), 
      "<br/>",
      "Area: ", #comma(filteredData()$area), 
      "<br/>", 
      "Asset value: ", #dollar(filteredData()$assets), 
      "<br/>",
      sep="") %>%
      lapply(htmltools::HTML)
    
    
    leafletProxy("map2") %>%
      clearShapes() %>% 
      

       addPolygons(data=filteredData(),
                   fillColor=pal(colorData),
                   fillOpacity=0.6,
                   stroke = T,
                   color="black",
                   weight=0.8,
                #   label=filteredData()$NAME
                   popup=det_popup # for some reason this is changing opacity of the polygons? 
                   
                   ) %>% 
     
      ## Delta boundary
      addPolygons(data=delta_sm, 
                  fill=F, 
                  stroke=T, 
                  color="black", # polygon border color
                  weight=4, # polygon border weight
                  group = "Delta + Suisun Marsh Boundary"
      ) %>% 
      
      ## Social vulnerability data
      #         addPolygons(data=sovi, 
      #                       fillColor=sovi_pal(colorData),
      #                       fillOpacity = 0.5,
      #                       stroke=T, 
      #                       color="black", # polygon border color
      #                       weight=0.8, # polygon border weight
      #                       label=paste(sovi$Rating),
      #                       group = "Social Vulnerability") %>% 
      
    
    ## Not-modeled polygons
    addPolygons(data=not_modeled, 
                fillColor="#969696",
                fillOpacity = 0.7,
                stroke=T,  
                color="black", # polygon border color
                weight=0.8, # polygon border weight
                label=paste("Not Modeled"),
                group = "Regions Not Modeled"
    ) %>% 
    
      
      # Add legends  
      addLegend("bottomright", 
                pal=nm_pal, 
                values=not_modeled$Modeled, 
                layerId="colorLegend2", 
                # group="Regions Not Modeled",
                opacity = 1,
                labFormat = labelFormat(prefix = "", 
                                        suffix = "", 
                                        between = " - ", 
                                        digits = 0)
      ) %>% 

    
    
            addLegend("bottomright", 
                      pal=pal, 
                      values=colorData, 
                      title="Flood Exposure Risk",
                      layerId="colorLegend3", 
                      opacity = 0.6,
    #                  group = "Social Vulnerability",
                      labFormat = labelFormat(prefix = "", 
                                              suffix = "", 
                                              between = " - ", 
                                              digits = 0)
          ) %>% 
      
      
      # Add point data
      addCircles(data=trans_points,
                 radius = 6,
                 color="black",
                 group = HTML('<i class="fa fa-circle"; style="font-size:120%; color:black; margin-top:3px;"></i><span style="font-size:110%; color:black;padding-left:5px;"> Transportation: Airstrips and Bridges'),
                 label = trans_points$Type
                 ) %>% 
    
    
    
    ## County boundaries 
    #           addPolygons(data=delta_counties, 
    #                       fillColor = "transparent", 
    #                       stroke=T, 
    #                       color="yellow", # polygon border color
    #                       weight=3, # polygon border weight
    #                       label=paste(delta_counties@data$NAME_UCASE),
    #                       group = "County Boundaries") %>% 
    
  
      # add layer control panel 
    addLayersControl(
      #    baseGroups = c("Basemap"),
      overlayGroups = c(HTML('<i class="fa fa-circle"; style="font-size:120%; color:black; margin-top:3px;"></i><span style="font-size:110%; color:black;padding-left:5px;"> Transportation: Airstrips and Bridges')
                        #,
                    #    "Asset Category 2",
                     #   "Asset Category 3"
                    ),
      position = "topright",
      options = layersControlOptions(collapsed = FALSE)
    )  %>% 
      htmlwidgets::onRender("
        function() {
            $('.leaflet-control-layers-overlays').prepend('<label style=\"text-align:center\">My Epic Title</label>');
        }
    ") %>%         
        hideGroup(HTML('<i class="fa fa-circle"; style="font-size:120%; color:black; margin-top:3px;"></i><span style="font-size:110%; color:black;padding-left:5px;"> Transportation: Airstrips and Bridges')
                          )
      
     
    
    
  })
  
  

############################# Scenario Explorer server logic ####################################
  
    # Create probabilistic basemap
   
   output$map <- renderLeaflet({
        leaflet() %>% 
            addTiles(
                urlTemplate = mb_url,
                attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
            )  %>% 
            setView(lat=38.1, lng=-121.8, zoom=10) %>% 
        addMapPane("ten", zIndex = 440) %>% # creates layer order for each set of polygons
        addMapPane("fifty", zIndex = 430) %>% 
        addMapPane("hun", zIndex = 420) %>% 
        addMapPane("twohun", zIndex = 410) 
      #      addMarkers(data = points(), group="Location Pin")
        

    })
    

   filteredData10 <- reactive({
     flood_polys10[flood_polys10$Hydrology == input$hydro & flood_polys10$SLR == input$SLR,]
   })
   
   filteredData50 <- reactive({
     flood_polys50[flood_polys50$Hydrology == input$hydro & flood_polys50$SLR == input$SLR,]
   })
   
   filteredData100 <- reactive({
     flood_polys100[flood_polys100$Hydrology == input$hydro & flood_polys100$SLR == input$SLR,]
   })
   
   filteredData200 <- reactive({
     flood_polys200[flood_polys200$Hydrology == input$hydro & flood_polys200$SLR == input$SLR,]
   })   

    # Draw interactive polygons
    observe({
      

          leafletProxy("map") %>%
            clearShapes() %>% 
      
                    
           addPolygons(data=filteredData10(), 
                        fillColor="#08519C", 
                        fillOpacity = 0.3, 
                       stroke=T, # add border 
                       color="black", # polygon border color
                        weight=0.3, # polygon border weight
                       label = popup_text10,  # info on hover
                       #popup = popup_text10, # info on click
                        group = HTML('<i class="fa fa-circle"; style="font-size:120%; color:#08519C; margin-top:3px;"></i><span style="font-size:110%; color:black;padding-left:5px;"> Very high likelihood | 10%+ annual chance | 65%+ chance over 10 years'),
                        options = pathOptions(pane = "ten")
              
                   
                   
               ## Display label info on mouseover         
                #,  highlight = highlightOptions(
                #     weight = 2,
                #     fillOpacity = 0,
                #     color = "black",
                #     opacity = 1.0,
                #     bringToFront = TRUE,
                #     sendToBack = TRUE),  
                   
                  
                #   label = popup_text10,
                #   labelOptions = labelOptions(
                #     style = list("font-weight" = "normal", padding = "3px 8px"),
                #     textsize = "15px",
                #     direction = "auto")
                   
                       ) %>% 
        


           addPolygons(data=filteredData50(), 
                       fillColor="#3182BD", 
                       stroke=T, 
                       fillOpacity = 0.5, 
                       color="black", # polygon border color
                       weight=0.3, # polygon border weight
                       label = popup_text50,
                       #popup = popup_text50,
                       group =  HTML('<i class="fa fa-circle"; style="font-size:120%; color:#3182BD; margin-top:3px;"></i><span style="font-size:110%; color:black;padding-left:5px;">  High likelihood | 2-10% annual chance | 18-65% chance over 10 years'),
                       options = pathOptions(pane = "fifty")
                       ) %>% 


           addPolygons(data=filteredData100(), 
                       fillColor= "#6BAED6", 
                       stroke=T, 
                       fillOpacity = 0.5, 
                       color="black", # polygon border color
                       weight=0.3, # polygon border weight
                       label = popup_text100,
                       #popup = popup_text100,
                       group = HTML('<i class="fa fa-circle"; style="font-size:120%; color:#6BAED6;margin-top:3px;"></i><span style="font-size:110%; color:black;padding-left:5px;">  Medium likelihood | 1-2% annual chance | 10-18% chance over 10 years'),
                       options = pathOptions(pane = "hun")
                       ) %>% 
        

           addPolygons(data=filteredData200(), 
                       fillColor="#BDD7E7", 
                       stroke=T, 
                       fillOpacity = 0.5, 
                       color="black", # polygon border color
                       weight=0.3, # polygon border weight
                       label = popup_text200,
                       #popup = popup_text200,
                       group = HTML('<i class="fa fa-circle"; style="font-size:120%; color:#BDD7E7;margin-top:3px;"></i><span style="font-size:110%; color:black;padding-left:5px;">  Low likelihood | 0.5-1% annual chance | 5-10% chance over 10 years'),
                       options = pathOptions(pane = "twohun")
                       ) %>% 
  
            
            # add location markers
            #addMarkers(data = points(), group="Location Pin") %>% 
            

            # add more feature shapefiles
            ## Delta boundary
        addPolygons(data=delta_sm, 
                    fill=F, 
                    stroke=T, 
                    color="black", # polygon border color
                    weight=4, # polygon border weight
                    group = "Delta + Suisun Marsh Boundary"
                    ) %>% 
        
      ## Social vulnerability data
#         addPolygons(data=sovi, 
#                       fillColor=sovi_pal(colorData),
#                       fillOpacity = 0.5,
#                       stroke=T, 
#                       color="black", # polygon border color
#                       weight=0.8, # polygon border weight
#                       label=paste(sovi$Rating),
#                       group = "Social Vulnerability") %>% 
           

        ## Not-modeled polygons
        addPolygons(data=not_modeled, 
                    fillColor="#969696",
                    fillOpacity = 0.5,
                    stroke=T,  
                    color="black", # polygon border color
                    weight=0.8, # polygon border weight
                    label=paste("Not Modeled"),
                    group = "Regions Not Modeled"
                    ) %>% 
      
          addLegend("bottomright", 
                  pal=nm_pal, 
                  values=not_modeled$Modeled, 
                  layerId="colorLegend2", 
                 # group="Regions Not Modeled",
                  opacity = 1,
                  labFormat = labelFormat(prefix = "", 
                                          suffix = "", 
                                          between = " - ", 
                                          digits = 0)
        ) %>%
        
#        addLegend("bottomright", 
#                  pal=pal, 
#                  values=flood_polys$Probability, 
 #                 title="Flood Exposure Probability",
#                  layerId="colorLegend1",
#                  opacity = 1,
#                  labFormat = labelFormat(prefix = "", 
#                                          suffix = "", 
#                                          between = " - ", 
#                                          digits = 0)
#        )# %>%
        
#        addLegend("bottomright", 
#                  pal=sovi_pal, 
#                  values=colorData, 
#                  title="Social Vulnerability",
#                  layerId="colorLegend3", 
#                  opacity = 0.5,
#                  group = "Social Vulnerability",
#                  labFormat = labelFormat(prefix = "", 
#                                          suffix = "", 
#                                          between = " - ", 
#                                          digits = 0)
 #      ) %>%
        
        
             ## County boundaries 
 #           addPolygons(data=delta_counties, 
 #                       fillColor = "transparent", 
 #                       stroke=T, 
 #                       color="yellow", # polygon border color
 #                       weight=3, # polygon border weight
 #                       label=paste(delta_counties@data$NAME_UCASE),
 #                       group = "County Boundaries") %>% 
            
            # add layer control panel 
            addLayersControl(
                #    baseGroups = c("Basemap"),
                overlayGroups = c(
                  HTML('<i class="fa fa-circle"; style="font-size:120%; color:#08519C; margin-top:3px;"></i><span style="font-size:110%; color:black;padding-left:5px;"> Very high likelihood | 10%+ annual chance | 65%+ chance over 10 years'),
                  HTML('<i class="fa fa-circle"; style="font-size:120%; color:#3182BD; margin-top:3px;"></i><span style="font-size:110%; color:black;padding-left:5px;">  High likelihood | 2-10% annual chance | 18-65% chance over 10 years'),
                  HTML('<i class="fa fa-circle"; style="font-size:120%; color:#6BAED6;margin-top:3px;"></i><span style="font-size:110%; color:black;padding-left:5px;">  Medium likelihood | 1-2% annual chance | 10-18% chance over 10 years'),
                  HTML('<i class="fa fa-circle"; style="font-size:120%; color:#BDD7E7;margin-top:3px;"></i><span style="font-size:110%; color:black;padding-left:5px;">  Low likelihood | 0.5-1% annual chance | 5-10% chance over 10 years') #,
                 #"Delta + Suisun Marsh Boundary"#,
                 #"Regions Not Modeled",
                 #"Social Vulnerability"
                 #"Location Pin"
                   ),
              position = "topright",
                options = layersControlOptions(collapsed = FALSE)
                )  
            #%>% 
#            hideGroup("Social Vulnerability")

        
    })
    ### Data explorer tab details would go here
    
}

