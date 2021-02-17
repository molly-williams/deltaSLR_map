# Code adapted in part from the COVID-19 mapper app: https://shiny.rstudio.com/gallery/covid19-tracker.html,
# and the SuperZIP map: https://shiny.rstudio.com/gallery/superzip-example.html, 

library(shiny)
library(leaflet)
library(shinyjs)
library(leaflet.extras2)
library(slickR)
library(DT)


# Load and wrangle all required components for app
source("wrangling.R")


######################################## Server logic ###########################################

function(input, output, session) {


  # Create photo slider on intro page
  
  output$slickr <- renderSlickR({
    imgs <- list.files("3_ShinyData/tutorial/", pattern=".png", full.names = TRUE)
    slickR(imgs) + 
      settings(dots=TRUE)
  })
  
  
############################ Delta Adapts Scenarios server logic ############################
  


  # Create point data for interactive location pins
  points <- eventReactive(input$recalc, {
    cbind(input$long, input$lat)
  }, ignoreNULL = FALSE)
  

      
  # Create basemap

  
  output$map2 <- renderLeaflet({
   
    leaflet() %>% 
      addTiles(
        urlTemplate = mb_url,
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      )  %>% 
      setView(lat=38.1, lng=-121.7, zoom=10) %>% 
      addMarkers(data = points(), group="Location Pin") #%>% 
 #     addEasyprint(options = easyprintOptions(title = 'Download Map',
#                                              position = 'topleft',
#                                              exportOnly = TRUE,
#                                              filename = "map"))
      
  })  
  

  
  # Filter polygons based on scenario selection

  filteredData <- reactive({
    det_prob_polys[det_prob_polys$scenario == input$scenario,]
  })
  
  
  # Filter county data based on scenario selection 
  filtered_table <- reactive({
    tab_data %>% 
      filter(Scenario == input$scenario) %>% 
      select(-Scenario) %>% 
      select(-Probability) %>% 
      select(-'Vulnerable Pop.')
  })
    
  
  # Data table output
  
  output$table <- renderTable(filtered_table(), 
                              align=c("lrrrrrr"),
                              rownames = FALSE
  )
  
  # Data table scenario tag
  
  output$text <- renderText({
    paste("People, land, and assets at risk of exposure in the Delta by county (", input$scenario, ")")
  })
  

  # Code for reactive color palettes 
  # Note: this could probably be condensed since all the deterministic have the same palette and all the probabilistic have the same palette
  observe({
    colorBy <- input$scenario
    
    if (colorBy == "Deterministic baseline: 0' SLR") {
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
       colorData <- ifelse(filteredData()$fldfight == 1, "Flooding (mitigable with flood fighting)", "Flooding")
       pal <- colorFactor(c("#08519C", "#6BAED6"), colorData) 
     }
    
    else if (colorBy == "Probabilistic: Existing conditions") {
      colorData <- ifelse(filteredData()$Probability == 10, "1. Very high likelihood | 10%+ annual chance | 65%+ chance over 10 years", 
                          ifelse(filteredData()$Probability == 50, "2. High likelihood | 18-65% chance over 10 years | 2-10% annual chance",
                                 ifelse(filteredData()$Probability == 100, "3. Medium likelihood | 1-2% annual chance | 10-18% chance over 10 years", 
                                        "4. Low Likelihood | 0.5-1% annual chance | 5-10% chance over 10 years")))
      pal <- colorFactor(c("#08306B", "#08519C", "#2171B5", "#6BAED6"), colorData) 
    }
    
    else if (colorBy == "Probabilistic: 2030 conditions") {
      colorData <- ifelse(filteredData()$Probability == 10, "1. Very high likelihood | 10%+ annual chance | 65%+ chance over 10 years", 
                          ifelse(filteredData()$Probability == 50, "2. High likelihood | 18-65% chance over 10 years | 2-10% annual chance",
                                 ifelse(filteredData()$Probability == 100, "3. Medium likelihood | 1-2% annual chance | 10-18% chance over 10 years", 
                                        "4. Low Likelihood | 0.5-1% annual chance | 5-10% chance over 10 years")))
      pal <- colorFactor(c("#08306B", "#08519C", "#2171B5", "#6BAED6"), colorData) 
    }
    
    else if (colorBy == "Probabilistic: 2050 conditions") {
      colorData <- ifelse(filteredData()$Probability == 10, "1. Very high likelihood | 10%+ annual chance | 65%+ chance over 10 years", 
                          ifelse(filteredData()$Probability == 50, "2. High likelihood | 18-65% chance over 10 years | 2-10% annual chance",
                                 ifelse(filteredData()$Probability == 100, "3. Medium likelihood | 1-2% annual chance | 10-18% chance over 10 years", 
                                        "4. Low Likelihood | 0.5-1% annual chance | 5-10% chance over 10 years")))
      pal <- colorFactor(c("#08306B", "#08519C", "#2171B5", "#6BAED6"), colorData) 
    }
    
    else { 
      colorData <- ifelse(filteredData()$Probability == 10, "1. Very high likelihood | 10%+ annual chance | 65%+ chance over 10 years", 
                          ifelse(filteredData()$Probability == 50, "2. High likelihood | 18-65% chance over 10 years | 2-10% annual chance",
                                 ifelse(filteredData()$Probability == 100, "3. Medium likelihood | 1-2% annual chance | 10-18% chance over 10 years", 
                                        "4. Low Likelihood | 0.5-1% annual chance | 5-10% chance over 10 years")))
      pal <- colorFactor(c("#08306B", "#08519C", "#2171B5", "#6BAED6"), colorData) 
    }
    
    
    
    # Create reactive popup text 
      
    det_prob_popup <- paste(
      "<span style='font-size: 120%'><strong>", filteredData()$NAME,"</strong></span><br/>", 
      "<strong>", "Area (square miles): ", "</strong>", filteredData()$area, 
      "<br/>", 
      "<strong>", "Population: ", "</strong>", format(round(as.numeric(filteredData()$total_pop), 0), nsmall=0, big.mark=","), 
      "<br/>",
      "<strong>", "% pop. in socially vulnerable areas: ", "</strong>", percent(as.numeric(filteredData()$svi_pct)), 
      "<br/>",
      sep="") %>%
      lapply(htmltools::HTML)
    
    
    leafletProxy("map2") %>%
      clearControls() %>% 
      clearShapes() %>% 

     
    
      # add location markers
      addMarkers(data = points(), group="Location Pin") %>% 
      
      ## Add flood polygons
       addPolygons(data=filteredData(),
                   fillColor=pal(colorData),
                   fillOpacity=0.8,
                   stroke = T,
                   color="black",
                   weight=0.8,
                   group = "Flood exposure risk regions",
                   popup=det_prob_popup  
                   ) %>% 
     
      ## Delta boundary
      addPolygons(data=delta_sm, 
                  fill=F, 
                  stroke=T, 
                  color="#273746", # polygon border color
                  weight=4, # polygon border weight
                  group = "Delta + Suisun Marsh Boundary"
                  ) %>% 
      

                
    ## County boundaries
      addPolygons(data=counties, 
                  fillColor = "transparent", 
                  stroke=T, 
                  color="black", # polygon border color
                  weight=4, # polygon border weight
                  label=paste(counties$NAME_UCASE),
                  group = "County Boundaries") %>% 
      
    ## Not-modeled polygons
    addPolygons(data=not_modeled, 
                fillColor="#969696",
                fillOpacity = 0.7,
                stroke=T,  
                color="black", # polygon border color
                weight=0.8, # polygon border weight
                popup=paste("Not Modeled"),
                group = "Regions Not Modeled"
                ) %>% 

      
      ## Social vulnerability data
      addPolygons(data=sovi,
                  fillColor=sovi_pal(s_colorData),
                  fillOpacity = 0.4,
                  stroke=T,
                  color="black", # polygon border color
                  weight=0.8, # polygon border weight
                  label=sovi$Rating,
                  group = "Social vulnerability by block group"
      ) %>%
      
      # Vulnerability legend - still reappearing when scenario is switched even if group is not selected?
      addLegend("bottomright",
                pal=sovi_pal,
                values=s_colorData,
                title="Social Vulnerability Rating",
                layerId="colorLegend3",
                opacity = 0.4,
                group = "Social vulnerability by block group",
                labFormat = labelFormat(prefix = "",
                                        suffix = "",
                                        between = " - ",
                                        digits = 0)
      ) %>%
      
      clearControls() %>%  # prevent the social vulnerability legend from automatically appearing when you generate a new map
    
      # Add legends  
      addLegend("bottomright", 
                pal=nm_pal, 
                values=not_modeled$Modeled, 
                layerId="colorLegend1", # layerid prevents legend from duplicating whenever you redraw the map with a new selection
                opacity = 0.7,
                labFormat = labelFormat(prefix = "", 
                                        suffix = "", 
                                        between = " - ", 
                                        digits = 0)
      ) %>% 
      
      
      
      
      # Flooding legend
      addLegend("bottomright",
                pal=pal, 
                values=colorData, 
                title="Flood Exposure Risk",
                layerId="colorLegend2", 
                opacity = 0.8,
                group = "Flood exposure risk regions",
                labFormat = labelFormat(prefix = "", 
                                        suffix = "", 
                                        between = " - ", 
                                        digits = 0)
      ) %>% 

      # Add polygon and point data for assets

      addPolygons(data=ag_polys, 
                  fillColor="#B9F3A8",
                  fillOpacity = 0.5,
                  stroke=T,  
                  color="black", # polygon border color
                  weight=0.8, # polygon border weight
                  group = ag_group,
                  label=ag_polys$Type
      ) %>% 
      
      addCircles(data=comm_points,
                 radius = 75,
                 opacity=0.8,
                 color="#CF2D2D",
                 group = comm_group,
                 label = comm_points$Type
      ) %>% 
      
      addCircles(data=critical_points,
                 radius = 75,
                 opacity=0.8,
                 color="#F44AA9",
                 group = critical_group,
                 label = critical_points$Type
      ) %>% 
      
      addCircles(data=cultural_points,
                 radius = 75,
                 opacity=0.8,
                 color="orange",
                 group = cultural_group, 
                 label = cultural_points$Type
      ) %>% 
      
      
      addCircles(data=energy_points,
                 radius = 75,
                 opacity=0.8,
                 color="purple",
                 group = energy_point_group,
                 label = energy_points$Type
      ) %>% 
      
      addCircles(data=rec_points,
                 radius = 75,
                 opacity=0.8,
                 color="#249729",
                 group = rec_point_group, 
                 label = rec_points$Type
      ) %>% 
      
      addCircles(data=trans_points,
                 radius = 75,
                 opacity=0.8,
                 color="black",
                 group = trans_point_group,
                 label = trans_points$Type
      ) %>% 
      
      addCircles(data=waste_points,
                 radius = 75,
                 opacity=0.8,
                 color="#E0CC5F",
                 group = waste_group,
                 label = waste_points$Type
      ) %>% 
      
      # Add lines data for assets

      addPolylines(data=energy_lines,
                   weight = 2,
                   color="purple",
                   group = energy_line_group,
                   label = energy_lines$Type
      ) %>% 
      
      addPolylines(data=rec_lines,
                   weight = 2,
                   color="green",
                   group = rec_line_group,
                   label = rec_lines$Type
      ) %>% 
      
      addPolylines(data=trans_lines,
                   weight = 2,
                   color="black",
                   group = trans_line_group,
                   label = trans_lines$Type
      ) %>% 

      addPolylines(data=water_conveyance,
                   weight = 2,
                   color="blue",
                   group=water_group,
                   label = water_conveyance$Type
      ) %>% 
    
      # add layer control panel 
      addLayersControl(
        overlayGroups = c(
          ag_group,
          comm_group,
          critical_group, 
          cultural_group,
          energy_point_group,
          energy_line_group,
          rec_point_group, 
          rec_line_group,
          trans_point_group,
          trans_line_group, 
          waste_group,
          water_group,
          "Flood exposure risk regions",
          "County Boundaries",
          "Social vulnerability by block group",
          "Location Pin"
        ),
        
        position = "topright",
        
        options = layersControlOptions(collapsed = TRUE)
      )  %>%     
      hideGroup(ag_group) %>% 
      hideGroup(comm_group) %>% 
      hideGroup(critical_group) %>% 
      hideGroup(cultural_group) %>%
      hideGroup(energy_point_group) %>% 
      hideGroup(energy_line_group) %>% 
      hideGroup(rec_point_group) %>% 
      hideGroup(rec_line_group) %>% 
      hideGroup(trans_point_group) %>% 
      hideGroup(trans_line_group) %>% 
      hideGroup(waste_group) %>% 
      hideGroup(water_group) %>% 
      hideGroup("Social vulnerability by block group") %>% 
      hideGroup("County Boundaries")
      
      


  }) # end observer
 
  # Add print map functionality
   
#  observeEvent(input$print, {
#    leafletProxy("map2") %>%
#      easyprintMap(sizeModes = "CurrentSize", filename = "Delta_Flood_map")
#  })
  



############################# Scenario Explorer server logic ####################################
  
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
      
      req(input$nav=="Hydrology Explorer") # map for selected parameters will appear automatically when user navigates to this tab
      
          leafletProxy("map") %>%
            clearShapes() %>% 
      
                    
           addPolygons(data=filteredData10(), 
                        fillColor="#08306B", 
                        fillOpacity = 1, 
                       stroke=T, # add border 
                       color="black", # polygon border color
                        weight=0.3, # polygon border weight
                       label = popup_text10,  # info on hover
                       #popup = popup_text10, # info on click
                        group = group_10,
                        options = pathOptions(pane = "ten")
              
                   
                   
               ## Display label info on mouseover and highlight polygon        
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
                       fillColor="#08519C", 
                       stroke=T, 
                       fillOpacity = 1, 
                       color="black", # polygon border color
                       weight=0.3, # polygon border weight
                       label = popup_text50,
                       #popup = popup_text50,
                       group =  group_50,
                       options = pathOptions(pane = "fifty")
                       ) %>% 


           addPolygons(data=filteredData100(), 
                       fillColor= "#2171B5", 
                       stroke=T, 
                       fillOpacity = 1, 
                       color="black", # polygon border color
                       weight=0.3, # polygon border weight
                       label = popup_text100,
                       #popup = popup_text100,
                       group = group_100,
                       options = pathOptions(pane = "hun")
                       ) %>% 
        

           addPolygons(data=filteredData200(), 
                       fillColor="#6BAED6", 
                       stroke=T, 
                       fillOpacity = 1, 
                       color="black", # polygon border color
                       weight=0.3, # polygon border weight
                       label = popup_text200,
                       #popup = popup_text200,
                       group = group_200,
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

        
            
            # add layer control panel 
            addLayersControl(
                #    baseGroups = c("Basemap"),
                overlayGroups = c(
                  group_10,
                  group_50,
                  group_100,
                  group_200
                 #"Delta + Suisun Marsh Boundary"#,
                 #"Regions Not Modeled",
                 #"Social Vulnerability"
                 #"Location Pin"
                   ),
              position = "topright",
                options = layersControlOptions(collapsed = FALSE)
                )  

        
    }) # end observer
    
    ### Data download tab
    
    county_data <- read_csv("3_ShinyData/county_scenario_data.csv") %>%
      select(-Probability, -"Vulnerable Pop.") %>% 
      mutate("Probability of flooding"="100 year/1% annual chance") %>% 
      rename("Population within Delta exposed to flooding"="Population") %>% 
#      rename("Total pop. in socially vulnerable areas" = "Vulnerable Pop.") %>% 
      rename("% of exposed pop. in socially vulnerable areas" = "Pop. in socially vulnerable area") %>% 
      rename("Flood exposure area within Delta (sq. miles)" = "Area (sq miles)") %>% 
      rename("Structural assets exposed ($) - agricultural, residential and commercial properties" = "Structural assets ($)*") %>% 
      rename("Infrastructure assets exposed ($) - critical facilities, water and energy utilities, comm/transportation infrastructure" = "Infrastructure assets ($)**")  %>% 
      rename("Annual economic activity at risk ($) - agriculture and commercial" = "Annual economic activity ($)***")  
    

    output$county_table <- DT::renderDataTable({
     county_data }, 
                    extensions = 'Buttons',
                    options = list(paging=FALSE,
                                   searching=TRUE,
                                   "dom" = 'T<"clear">lBfrtip',
                                   buttons=list('copy', 'csv', 'excel', 'pdf', 'print')
                                   ),
                    class="display"
                    )
      
    
    
    
    
}

