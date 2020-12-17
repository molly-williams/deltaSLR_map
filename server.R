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
                                      ifelse(Probability == 50, "10-50 years", 
                                             ifelse(Probability == 100, "50-100 years", "100-200 years")
                                             )
                                      ), levels=c("<10 years", "10-50 years", "50-100 years", "100-200 years")) 
                  )

# Convert SLR from factor to numeric to enable slider (have to coerce to character first to preserve data)
flood_polys$SLR <- as.numeric(as.character(flood_polys$SLR)) 


# Split combined shapefile into four (one for each probabilistic scenario)
flood_polys10 <- flood_polys %>% 
  filter(Probability == "<10 years")

flood_polys50 <- flood_polys %>% 
  filter(Probability == "10-50 years")

flood_polys100 <- flood_polys %>% 
  filter(Probability == "50-100 years")

flood_polys200 <- flood_polys %>% 
  filter(Probability == "100-200 years")

# Create color palette
pal <- colorFactor(c("#08519C", "#3182BD", "#6BAED6", "#BDD7E7"), ordered=TRUE, flood_polys$Probability, na.color="transparent")


#display.brewer.all(colorblindFriendly = T)
#brewer.pal(5, "Blues") = "#EFF3FF" "#BDD7E7" "#6BAED6" "#3182BD" "#08519C"
#display.brewer.pal - look at colors



# Add Delta + Suisun Marsh boundary shapefile and reproject
delta_sm <- read_sf("3_ShinyData/LD_SM_Merged.shp") %>% 
  st_transform(4326) 


# Add vulnerability shapefile and color palette to match SOVI app
sovi <- read_sf("3_ShinyData/sovi_simp.shp") %>% 
  select("Vulnerability" = Vlnrblt, "Rating"=VULNBLT) %>% 
  st_transform(4326)

colorData <- ifelse(sovi$Vulnerability == 1, "1: Moderate", ifelse(sovi$Vulnerability == 2, "2: High", "3: Highest"))  
sovi_pal <- colorFactor("viridis", colorData)

# Import probability category table
prob_legend <- read_csv("3_ShinyData/prob_legend.csv")



# Create text for popups
popup_text10 <- paste(
  "<span style='font-size: 100%'>Flood risk probability: ", flood_polys10$Probability,"</span><br/>",
 # "Vulnerability Rating: ", sovi$Vulnerability, # not working correctly
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




mb_url <- "https://api.mapbox.com/styles/v1/moowill/cki99lcza15g818phcibhxpv2/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoibW9vd2lsbCIsImEiOiJja2kzejloOHkxdzNtMnhxcTAwY3Zqa25zIn0.VCsBGYnJr6Z7A7XnD157cg"
# get link from published mapbox map through share > click third party > select "CARTO" > copy link 
# includes access token


function(input, output, session) {

    # Create point data for interactive location pins
#    points <- eventReactive(input$recalc, {
#        cbind(input$long, input$lat)
#    }, ignoreNULL = FALSE)


  # Render table
  
  output$table <- renderTable(prob_legend,
                              striped=TRUE,
                              spacing="xs") 


    
    # Create basemap
   
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
      
      #%>% 
      #      addMarkers(data = points(), group="Location Pin")
        

    })
    

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
                        fillColor="#08519C", 
                        stroke=T, 
                        fillOpacity = 0.3, 
                        color="black", # polygon border color
                        weight=0.3, # polygon border weight
                   #     popup = popup_text10,
                        group = "<10 year flood exposure",
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
                       fillColor="#3182BD", 
                       stroke=T, 
                       fillOpacity = 1, 
                       color="black", # polygon border color
                       weight=0.3, # polygon border weight
                       popup = popup_text50,
                       group = "10-50 year flood exposure",
                       options = pathOptions(pane = "fifty")) %>% 

           addPolygons(data=filteredData100(), 
                       fillColor= "#6BAED6", 
                       stroke=T, 
                       fillOpacity = 1, 
                       color="black", # polygon border color
                       weight=0.3, # polygon border weight
                       popup = popup_text100,
                       group = "50-100 year flood exposure",
                       options = pathOptions(pane = "hun")) %>% 

           addPolygons(data=filteredData200(), 
                       fillColor="#BDD7E7", 
                       stroke=T, 
                       fillOpacity = 1, 
                       color="black", # polygon border color
                       weight=0.3, # polygon border weight
                       popup = popup_text200,
                       group = "100-200 year flood exposure",
                       options = pathOptions(pane = "twohun")) %>% 

             addLegend("bottomright", 
                      pal=pal, 
                      values=flood_polys$Probability, 
                      title="Flood Exposure Probability",
                      layerId="colorLegend", 
                 #     group="floodLegend",
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
        

         addPolygons(data=sovi, 
                       fillColor=sovi_pal(colorData),
                       fillOpacity = 0.5,
                       stroke=T, 
                       color="black", # polygon border color
                       weight=0.8, # polygon border weight
                       label=paste(sovi$Rating),
                       group = "Social Vulnerability") %>% 
           
           addLegend("bottomright", 
                     pal=sovi_pal, 
                     values=colorData, 
                     title="Social Vulnerability",
                     layerId="colorLegend", 
                     opacity = 0.5,
                     group = "Social Vulnerability",
                     labFormat = labelFormat(prefix = "", 
                                             suffix = "", 
                                             between = " - ", 
                                             digits = 0)
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
                overlayGroups = c(
                 # "Flood Extents",
                  "<10 year flood exposure",
                  "10-50 year flood exposure",
                  "50-100 year flood exposure",
                  "100-200 year flood exposure",
                  "Delta + Suisun Marsh Boundary",
                  "Social Vulnerability"
                  #"Location Pin"
                   ),
                options = layersControlOptions(collapsed = FALSE)
                )  %>% 
            hideGroup("Social Vulnerability")

        
    })
    ### Data explorer tab details would go here
    
}
