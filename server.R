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
require(stringr)


###################### Data import and wrangling ###########################

# Import combined simplified polygons
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
pal <- colorFactor(c("#08519C", "#3182BD", "#6BAED6", "#BDD7E7"), ordered=TRUE, flood_polys$Probability, na.color="transparent")
nm_pal <- colorFactor(c("#969696"), ordered=TRUE, not_modeled$Modeled, na.color="transparent")


#display.brewer.all(colorblindFriendly = T)
#brewer.pal(5, "Blues") = "#EFF3FF" "#BDD7E7" "#6BAED6" "#3182BD" "#08519C"
#display.brewer.pal - look at colors


# Create text for popups
popup_text10 <- paste(
  #"<span style='font-size: 100%'>Flood risk probability: ", 
  flood_polys10$Probability,
  #"</span><br/>",
 # "Vulnerability Rating: ", sovi$Vulnerability, # not working correctly
  sep="") %>%
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


############################# Server logic #################################

function(input, output, session) {

    # Create point data for interactive location pins
#    points <- eventReactive(input$recalc, {
#        cbind(input$long, input$lat)
#    }, ignoreNULL = FALSE)


  # Render table in sidebar
  
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
      #      addMarkers(data = points(), group="Location Pin")
        

    })
    
#   filteredData10 <- reactive({
#     flood_polys10[flood_polys10$Hydrology == input$hydro & flood_polys10$SLR == input$SLR & flood_polys10$Probability == input$ID10,]
#   })
   
#   filteredData50 <- reactive({
#     flood_polys50[flood_polys50$Hydrology == input$hydro & flood_polys50$SLR == input$SLR & flood_polys50$Probability == input$ID50,]
#   })
   
#   filteredData100 <- reactive({
#     flood_polys100[flood_polys100$Hydrology == input$hydro & flood_polys100$SLR == input$SLR & flood_polys100$Probability == input$ID100,]
#   })
   
#   filteredData200 <- reactive({
#     flood_polys200[flood_polys200$Hydrology == input$hydro & flood_polys200$SLR == input$SLR & flood_polys200$Probability == input$ID200,]
#   })
   
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

