#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(RColorBrewer)
library(rgdal)
library(DT)
library(raster)
library(tidyverse)
library(rgeos)
library(sf)




# Import combined simplified polygons
flood_polys <- read_sf("3_ShinyData/poly_combined_simp.shp")


# Transform to lat/long
flood_polys <- st_transform(flood_polys, 4326)


# Rename columns and hydrology/probability categories +re-level
flood_polys <- flood_polys %>% 
    dplyr::select(Hydrology=hydro, SLR, Probability=Prob) %>% 
    dplyr::mutate(Hydrology = ifelse(stringr::str_detect(Hydrology, "E"), "2085", 
                                     ifelse(str_detect(Hydrology, "M"), 2050, "Historical"))) %>% 
    dplyr::mutate(Probability = factor(ifelse(Probability == 10, "<10 years",
                                      ifelse(Probability == 50, "<50 years", 
                                             ifelse(Probability == 100, "<100 years", "<200 years")
                                             )
                                      ), levels=c("<10 years", "<50 years", "<100 years", "<200 years")) 
                  )

# Convert SLR from factor to numeric to enable slider (have to coerce to character first to preserve data)
flood_polys$SLR <- as.numeric(as.character(flood_polys$SLR)) 


flood_polys10 <- flood_polys %>% 
  filter(Probability == "<10 years")

flood_polys50 <- flood_polys %>% 
  filter(Probability == "<50 years")

flood_polys100 <- flood_polys %>% 
  filter(Probability == "<100 years")

flood_polys200 <- flood_polys %>% 
  filter(Probability == "<200 years")


# Add Delta + Suisun Marsh boundary shapefile
delta_sm <- read_sf("3_ShinyData/LD_SM_Merged.shp")

## Reproject to lat/long
delta_sm <- st_transform(delta_sm, 4326) 



# Create color palette
pal <- colorFactor(c("#BDD7E7", "#6BAED6", "#3182BD", "#08519C"), ordered=TRUE, flood_polys$Probability, na.color="transparent")


#display.brewer.all(colorblindFriendly = T)
#brewer.pal(5, "Blues") = "#EFF3FF" "#BDD7E7" "#6BAED6" "#3182BD" "#08519C"
#display.brewer.pal - look at colors



# Create text for popups
popup_text10 <- paste(
  "<span style='font-size: 100%'>Flood risk probability: ", flood_polys10$Probability,"</span><br/>",
  sep="") %>%
  lapply(htmltools::HTML)

popup_text50 <- paste(
  "<span style='font-size: 100%'>Flood risk probability: ", flood_polys50$Probability,"</span><br/>",
  sep="") %>%
  lapply(htmltools::HTML)

popup_text100 <- paste(
  "<span style='font-size: 100%'>Flood risk probability: ", flood_polys100$Probability,"</span><br/>",
  sep="") %>%
  lapply(htmltools::HTML)

popup_text200 <- paste(
  "<span style='font-size: 100%'>Flood risk probability: ", flood_polys200$Probability,"</span><br/>",
  sep="") %>%
  lapply(htmltools::HTML)



function(input, output, session) {

    # Create point data for interactive location pins
#    points <- eventReactive(input$recalc, {
#        cbind(input$long, input$lat)
#    }, ignoreNULL = FALSE)
    
    
    # Create basemap
    output$map <- renderLeaflet({
        leaflet() %>% 
            addTiles(
                urlTemplate = "https://api.mapbox.com/styles/v1/moowill/cki3zbj5o4k4b19qlq98amqia/tiles/{z}/{x}/{y}?access_token=pk.eyJ1IjoibW9vd2lsbCIsImEiOiJja2kzejloOHkxdzNtMnhxcTAwY3Zqa25zIn0.VCsBGYnJr6Z7A7XnD157cg",
                attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
            )  %>% 
            setView(lat=38.1, lng=-121.8, zoom=10) %>% 
        addMapPane("ten", zIndex = 440) %>% # creates layer order for each set of polygons
        addMapPane("fifty", zIndex = 430) %>% 
        addMapPane("hun", zIndex = 420) %>% 
        addMapPane("twohun", zIndex = 410)
      
      #%>% 
      #      addMarkers(data = points(), group="Location Pin")
        

    })
    
    ## Somehow use session and leaflet proxy to create a new map everytime new inputs are selected
    
    # Draw interactive polygons
    observe({
      
      
  #    filteredData <- reactive({
  #      flood_polys[flood_polys$Hydrology == input$hydro & flood_polys$SLR == input$SLR,]
  #    })
      
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
          
        
         leafletProxy("map") %>%
            clearShapes() %>% 
           
          # addPolygons(data=filteredData(), 
          #             fillColor=~pal(Probability), 
          #             stroke=T, 
          #             fillOpacity = 0.5, 
          #             color="black", # polygon border color
          #             weight=0.8, # polygon border weight
          #             popup = popup_text,
          #             group = "Flood Extents") %>% 
           
           addPolygons(data=filteredData10(), 
                        fillColor="#BDD7E7", 
                        stroke=T, 
                        fillOpacity = 0.3, 
                        color="black", # polygon border color
                        weight=0.3, # polygon border weight
                   #     popup = popup_text10,
                        group = "10 year",
                        options = pathOptions(pane = "ten"),
                       
                   highlight = highlightOptions(
                     weight = 2,
                     fillOpacity = 0,
                     color = "black",
                     opacity = 1.0,
                     bringToFront = TRUE,
                     sendToBack = TRUE),  
                   
                   # # Add label info when mouseover
                   label = popup_text10,
                   labelOptions = labelOptions(
                     style = list("font-weight" = "normal", padding = "3px 8px"),
                     textsize = "15px",
                     direction = "auto")
                   
                       ) %>% 
           
           
           
           addPolygons(data=filteredData50(), 
                       fillColor="#6BAED6", 
                       stroke=T, 
                       fillOpacity = 1, 
                       color="black", # polygon border color
                       weight=0.3, # polygon border weight
                       popup = popup_text50,
                       group = "50 year",
                       options = pathOptions(pane = "fifty")) %>% 
           
           addPolygons(data=filteredData100(), 
                       fillColor= "#3182BD", 
                       stroke=T, 
                       fillOpacity = 1, 
                       color="black", # polygon border color
                       weight=0.3, # polygon border weight
                       popup = popup_text100,
                       group = "100 year",
                       options = pathOptions(pane = "hun")) %>% 
           
           addPolygons(data=filteredData200(), 
                       fillColor="#08519C", 
                       stroke=T, 
                       fillOpacity = 1, 
                       color="black", # polygon border color
                       weight=0.3, # polygon border weight
                       popup = popup_text200,
                       group = "200 year",
                       options = pathOptions(pane = "twohun")) %>% 
           
             addLegend("bottomright", 
                      pal=pal, 
                      values=flood_polys$Probability, 
                      title="Flood Scenario Probability",
                      layerId="colorLegend", 
                      opacity = 1,
                      labFormat = labelFormat(prefix = "", 
                                              suffix = "", 
                                              between = " - ", 
                                              digits = 0)
            ) %>%
            
            # add location markers
            #addMarkers(data = points(), group="Location Pin") %>% 
            

            # add feature shapefiles
            ## Delta boundary
        addPolygons(data=delta_sm, 
                    fill=F, 
                    stroke=T, 
                    color="black", # polygon border color
                    weight=4, # polygon border weight
                    group = "Delta + Suisun Marsh Boundary") %>% 

           
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
                 # "Flood Extents",
                  "10 year",
                  "50 year",
                  "100 year",
                  "200 year",
                  "Delta + Suisun Marsh Boundary" 
                  #"Location Pin"
#                   "County Boundaries",
#                   "Vegetation Cover"
                   ),
                options = layersControlOptions(collapsed = FALSE)
                ) # %>% 
#            hideGroup("County Boundaries")

        
    })
    ### Data explorer tab details would go here
    
}
