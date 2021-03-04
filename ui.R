# Code adapted in part from the COVID-19 mapper app: https://shiny.rstudio.com/gallery/covid19-tracker.html,
# and the SuperZIP map: https://shiny.rstudio.com/gallery/superzip-example.html, 

library(shiny)
library(shinyWidgets)
library(leaflet)
library(shinydashboard)
library(slickR)
library(DT)


# Delta Adapts scenario choices for dropdown:

vars <- c("No scenario selected",
          "Deterministic baseline: 0' SLR",
          "Deterministic: 2030, 0.5' SLR",
          "Deterministic: 2050, 1' SLR",
          "Deterministic: 2050, 2' SLR", 
          "Deterministic: 2050+, 3.5' SLR",
          "Probabilistic: Existing conditions",
          "Probabilistic: 2030 conditions",
          "Probabilistic: 2050 conditions",
          "Probabilistic: 2085 conditions")


# Load JS file for adding logo
jsfile <- "https://rawgit.com/rowanwins/leaflet-easyPrint/gh-pages/dist/bundle.js" 



# Add DSC logo to navigation panel
tagList(
  tags$head(tags$script(type="text/javascript", 
                        #href="https://deltacouncil.ca.gov/", target="_blank", # trying to turn the logo into a link
                        src = "logo_code.js")
            ),


navbarPage(
  title="Climate Change Flood Scenarios in the Delta",
            id="nav",


  tabPanel('Instructions',
           tags$head(
             includeCSS("styles.css")
           ),
           
           box(a(shiny::icon("reply"), "Delta Science shinyapps homepage", 
             href="https://deltascience.shinyapps.io/Home/"),
             width=12),
           
           box(title="Welcome!",
               status="primary",
               solidHeader=TRUE,
               width = 6,
                 
           tags$div(
             "This tool visualizes the",
             tags$a("Delta Adapts", href="https://deltacouncil.ca.gov/delta-plan/climate-change", target="_blank"),
             "climate change flood scenarios, and allows users to:",
             tags$br(),
             tags$br(),
             tags$ul(
             tags$li(icon("map"),
              "Select from nine",
              tags$b("deterministic"),
              "and",
              tags$b("probabilistic"),
              "scenarios to see regions that would be exposed to flooding, including regions where flooding could be avoided with",
             tags$b("flood fighting")
             ),
             tags$br(),
             
             tags$li(icon("table"),
              "Generate a table of data for people, land, and assets in the Delta at risk of",
              tags$b("flood exposure"),
              "by county, and download data in the",
              tags$b("Data Export"),
              "tab"),
             tags$br(),

             tags$li(icon("building"),
              "Add",
              tags$b("assets, resources"),
              "and",
              tags$b("social vulnerability"),
              "to the map to see overlap with regions at risk of flood exposure"),
             tags$br(),
             
             tags$li(icon("info-circle"),
              "Click a flooded area to get information about that region"),
             tags$br(),
             
             tags$li(icon("map-pin"),
              "Pin a specific location on the map using a latitude and longitude "),
             tags$br(),
             
             tags$li(icon("water"),
              "Explore additional present and future flood exposure scenarios in the",
              tags$b("Hydrology Explorer"),
              "tab")
             ), # end ul list
             tags$br(),
             tags$em("Want to learn more about how flooded regions were determined? Head to the",
             tags$b('More Info + Methods'),
             "tab."),
             tags$br(),
             tags$br(),
             
             h3("Key Terms:"),
             tags$ul(
               tags$li(tags$b("Deterministic"), "maps use a specific amount of sea level rise (SLR) (e.g. 0, 0.5’, 1’, 2’, and 3.5’) and a range of Delta water inflow possibilities (hydrology).  The maps display areas that would be exposed to flooding during a 100-year event, or an event that has a chance of occurrence of 1% in each year."),
               tags$br(),
               tags$li(tags$b("Probabilistic"), "maps use a range of sea level rise amounts based on projections at each time-period and a range of Delta water inflow possibilities (hydrology). The maps display areas according to their likelihood of flooding. This acknowledges that all areas have some risk of flood exposure."),
               tags$br(),
               tags$li(tags$b("Flood fighting"), "is a standard practice in many areas of the Delta during high water events, where flooding can be prevented when water levels exceed the top of levee by 6“ or less. Light blue regions of deterministic maps indicate areas where flooding could be avoided by implementing flood fighting activities."),
               tags$br(),
               tags$li(tags$b("Flood exposure"), "indicates areas that would be flooded according to our simulations. However, exposure does not indicate the degree to which a region and the assets within it will experience damage or loss, as it does not consider flood severity or site-specific conditions (e.g., flood-proofed buildings) that may prevent or limit or worsen impacts."),
               tags$br(),
               tags$li(tags$b("Assets and resources"), "include identified historic places, recreational areas, infrastructure (e.g. power plants and highways), and critical facilities (e.g. hospitals and schools)"),
               tags$br(),
               tags$li(tags$b("Socially vulnerable populations"), "experience heightened risk and increased sensitivity to climate change and have less capacity and fewer resources to cope with, adapt to, or recover from climate impacts. These disproportionate effects are caused by factors such as race, class, sexual orientation and identification, national origin, and income inequality.", tags$a("Explore social vulnerability indicators and learn about how the index is calculated.", href="https://deltascience.shinyapps.io/Delta_vulnerability_map/", target="_blank"))
             )
             
           ), # end div
             

           ), # end box 1
           
           box(status="primary",
               ssolidHeader = TRUE,
               width = 6,
               slickROutput("slickr"),
               tags$br(),
               tags$br()
               ) # end photo slideshow box
         
           
  ), # end instructions tab
  
  
             tabPanel("Delta Adapts Scenarios",
                    div(class="outer",
                        tags$head(
                          includeCSS("styles.css")
                        ),
                        
                        tags$head(tags$script(src = jsfile)),
                        
                        
                        leafletOutput("map2", width="100%", height="100%"),
                        
                        absolutePanel(id = "controls", 
                                      class = "panel panel-default", 
                                      fixed = TRUE,
                                      draggable = TRUE, 
                                      top = 100, 
                                      left = 30, 
                                      right = "auto", 
                                      bottom = "auto",
                                      width = 330, 
                                      height = "auto",
                                      
                                     # h4(tags$b("1. Select scenario:")),
                                      
                                      tags$br(),
                                      
                                      tags$em("Allow a moment for layers to load."),
                                     
                                      selectInput("scenario", 
                                                 # label=NULL,
                                                 label=h4(tags$b("1. Select scenario:")), 
                                                  choices=vars, 
                                                  selectize=FALSE),
                                     tags$em("SLR = sea level rise. Selecting a scenario generates map and populates table below."),
                                     
                                     tags$br(),
                                     tags$br(),
                                    
                                     h4(tags$b("2. Toggle asset, vulnerability and flood layers in control box (upper right) "), icon("layer-group")),
                                     
                                     tags$br(),

                                      
                                      h4(tags$b("3. Pin location on map:")),
                                      tags$div(
                                        tags$a("Get a lat/long from an address here.", href="https://www.latlong.net/convert-address-to-lat-long.html", target="_blank")
                                      ),
                                     

                                     splitLayout( # display lat/long boxes side by side
                                     numericInput("long", label = h5("Longitude:"), value = -121.501),
                                      numericInput("lat", label = h5("Latitude:"), value = 38.001)
                                     ),
                                     
                                     actionButton("recalc", "Show Point", icon("map-marker-alt"), 
                                                  style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                     
                                       
                                     
                                    #  tags$br(),
                                    #  tags$br(),
                                     
                                    # h4(tags$b("Download map:")),
    
                                    # actionButton("print", "Save Map") # can't get this to work...
                                      

                            
                        
                        ), #end absolute panel
                        
                        absolutePanel(id = "controls", 
                                      class = "panel panel-default", 
                                      fixed = TRUE,
                                      draggable = TRUE, 
                                      top = "auto", 
                                      left = 30, 
                                      right = "auto", 
                                      bottom = 30,
                                      width = 900, 
                                      height = "auto",
                                      
                                      h4(tags$b(textOutput('text'))),
                                     
                                      
                                      tableOutput('table'),
                                      
                                      tags$em("*Includes agricultural, residential, and commercial properties."),
                                      
                                      tags$br(),
                                      
                                      tags$em("**Includes critical facilities, water and energy utilities, and infrastructure for communications and transportation"),
                                      
                                      tags$br(),
                                      tags$em("***Includes agricultural and commercial activities."),
                                      
                                      tags$br(),
                                      tags$br(),

                                      tags$b("Values for probabilistic scenarios reflect 100-year exposure risk (medium probability).")

                                      
                        ) # end 2nd absolute panel (data table)
                        
                        ), #end div
              
              tags$div(id="cite",
                       tags$em('This map was created in support of the Delta Adapts initiative, a project of the Delta Stewardship Council (2020)')
                       )
                        
           ), # end tabpanel
           
           
           
           tabPanel("Hydrology Explorer",
                    div(class="outer",
                        
                        tags$head(
                            # Include custom CSS
                            includeCSS("styles.css")
                        ),
                        
                        leafletOutput("map", width="100%", height="100%"),
                        
                        absolutePanel(id = "controls", 
                                      class = "panel panel-default", 
                                      fixed = TRUE,
                                      draggable = TRUE, 
                                      top = 60, 
                                      left = 30, 
                                      right = "auto", 
                                      bottom = "auto",
                                      width = 350, 
                                      height = "auto",
                                      
                                   #   h3("Explore Hydrology and Sea Level Rise (SLR) Scenarios"),
                                   
                                   h4("Create your own map:"),
                                   tags$div(
                                     tags$ul(
                                       tags$li("Use this tool to visualize Delta regions at risk of flood exposure and associated likelihoods by selecting a watershed hydrology category and an amount of sea level rise"),
                                       tags$li("Toggle flood exposure polygons in the upper right-hand corner to isolate areas at high flood risk in the next 10 years, 50 years, 100 years, and 200 years."),
                                       tags$li("The polygons are inclusive, so flood exposure regions of lower flood exposure risk will include those of higher risk.")#,
                                     #  tags$li("Maybe say something here describing what a combination represents - eg. assumptions behind 2050 hydrology + 3' SLR")
                                     )
                                   ),
                                   
                                   tags$br(),
                                   
                                   
                                      radioButtons(inputId="hydro", 
                                                   label=h4("1. Select watershed hydrology:"), 
                                                   choices=c("Historical", "2050", "2085"),
                                                   selected="Historical",
                                                   ),
                                      
                                      sliderTextInput(inputId="SLR",
                                                  label = h4("2. Select sea level rise (feet):"),
                                                  choices = c(0:7, 10),
                                                  grid=TRUE
                                                  ),
                                   tags$em("Note: no data exist for 8' and 9' of SLR."),
                                   
                                   tags$br(),
                                   tags$br()
                                   
                                      
                        )
                        
                    ),
                    
                    tags$div(id="cite",
                             tags$em('This map was created in support of the Delta Adapts initiative, a project of the Delta Stewardship Council (2020)')
                    )
           ), # end hydrology explorer tab panel
           
  
  tabPanel("Data Export",
           DT::dataTableOutput("county_table")
  ),

          
           tabPanel('More Info + Methods',
                    tags$head(
                      includeCSS("styles.css")
                    ),
                    
                    h3("Learn more + contact us"),
                    
                    tags$ul(
                      tags$li("Visit the",
                              tags$a("Delta Adapts website",
                                     href="https://deltacouncil.ca.gov/delta-plan/climate-change",
                                     target="_blank"),
                              "to learn more about the initiative."),
                      tags$li("Read more about the flood exposure data, flood hazard scenarios, and flood hazard mapping processes in the",
                              tags$a("Flood Hazard Assessment Technical Memorandum.", 
                                     href="https://deltacouncil.ca.gov/pdf/delta-plan/2021-01-15-delta-adapts-flood-hazard-assessment.pdf", 
                                     target="_blank")),
                      tags$li( "Read more about Delta assets and economic impact and exposure analysis in the",
                               tags$a("Economics Technical Memorandum.",
                                      href="https://deltacouncil.ca.gov/pdf/delta-plan/2021-01-15-delta-adapts-economic-assessment.pdf",
                                      target="_blank")),
                      tags$li("Check out the app code on",
                              tags$a("Github.",
                                     href="https://github.com/molly-williams/deltaSLR_map",
                                     target="_blank")),
                      tags$li("Contact the Delta Adapts team at",
                              tags$a("climatechange@deltacouncil.ca.gov", href="mailto:climatechange@deltacouncil.ca.gov"),
                              " at the Delta Stewardship Council with any questions.")
                    ), # end bullets 
                    
                 
                    
                    h3("Project Background"),
                    tags$div(
                        "The maps generated on this app are a product of",
                        tags$a("Delta Adapts: Creating a Climate Resilient Future,", href="https://deltacouncil.ca.gov/delta-plan/climate-change", target="_blank"),
                        "an initiative of the Delta Stewardship Council. The first of its kind, the project assesses vulnerability of the Delta’s 
                        people, places, ecosystems, infrastructure, assets and resources to varied effects from future climate conditions. The draft 
                        vulnerability assessment was released in early 2021 and can be accessed",
                        tags$a("here.",
                               href="https://deltacouncil.ca.gov/pdf/delta-plan/2021-01-15-delta-adapts-public-draft-vulnerability-assessment.pdf",
                               target="_blank"),
                        
                        tags$br(),
                        tags$br(),
                        
                        "Delta water levels are influenced by tides, weather, and streamflow from rivers and tributaries. Climate change is expected to increase both
                        average sea level and the timing and amount of water flowing into the Delta from rivers and tributaries. These changes will combine to increase water levels throughout the Delta.",

                        tags$br(),
                        tags$br(),
                        
                        "The Delta is a complex system and climate change will not affect all areas in the same ways. These maps serve as tools to visualize the flood
                        exposure risk of regions within the Delta based on hydrology and sea level rise.  These maps also include other factors which influence water 
                        levels including daily tides and storm surge."

                        
                           ), # end div
                    

                    tags$br(),
                    

                    h3("Analysis Approach"),
                    tags$div(
                        "The analysis behind the mapping tools is designed to explore the ways in which climate change – in the form of higher sea levels and larger streamflows – will increase
                        exposure to flooding and how those climate-related changes will interact with storm surge, tides, and existing flood risk throughout the Delta.",
                    
                        tags$br(),
                        tags$br(),
                        
                        "Through simulations using global climate models, the flood hazard exposure analysis estimates annual peak water levels that will occur under future climate scenarios throughout the Delta. 
                        The peak water level analysis evaluates the effects of sea level rise and changes in the timing, magnitude, and distribution of peak inflows to the Delta. Results are used in the flood 
                        hazard mapping to inform the asset, resources, and economic exposure assessments. ",
                        
                        tags$br(),
                        tags$br(),
                        
                        "Both coastal and riverine flood hazards were analyzed to estimate peak water levels projected to occur through the end of the century. These water levels are used to map potential flood extents, 
                        which indicates areas of the Delta that are likely to be exposed to flooding. Exposure indicates areas that would be flooded according to our simulations. However, exposure does not indicate the
                        degree to which a region and the assets within it will experience damage or loss, as it does not consider flood severity or site-specific conditions (e.g., flood-proofed buildings) that may prevent or limit or worsen impacts. "
                        ), # end div
                    
                    tags$br(),
                    
                    h3("Mapping Approach"),
                    tags$div(
                        "Delta Adapts evaluates likelihood of flood hazards from projections of sea level rise and changes in watershed hydrology. This analysis provides information about the likelihood of a flood event 
                        occurring that would overtop levees at each location throughout the Delta simulated from two million iterations.",
                      
                      tags$br(),
                      tags$br(),
                      
                      "These simulations consider a range of flood events that could occur in each year (including wet, average, and dry years). This method of flood exploration and mapping provides 
                      important information about flood likelihood and the different ways in which sea level rise, tides, storm surge, and river inflows during a storm event can combine to cause high water 
                      levels in different areas of the Delta. This tool shows which islands would be overtopped and flooded at five levels of frequency or likelihood:",
                    
                      tags$ul(
                        tags$li(tags$b("Less than a 10-year event (very high likelihood):"), "an event that has a 10 percent chance of occurrence in each year and would have a 65 percent chance of occurring at least once over a 10-year period. This level of flood frequency would be acceptable only for wetland, riparian, or subtidal open water habitat."),
                        tags$li(tags$b("Between a 10-year and 50-year event (high likelihood):"), "an event that has between a 2 and 10 percent chance of occurrence in each year and would have an 18 to 65 percent chance of occurring at least once over a 10-year period. This level of flood frequency would likely be too high to support agricultural investment and too low to support wetland development."),
                        tags$li(tags$b("Between a 50-year and 100-year event (medium likelihood):"), "an event that has between a 1 and 2 percent chance of occurrence in each year and would have a 10 to 18 percent chance of occurring at least once over a 10-year period. This level of flood frequency is likely acceptable for agricultural investment of non-permanent crops. "),
                        tags$li(tags$b("Between a 100-year and 200-year event (low likelihood):"), "an event that has between a 0.5 and 1 percent chance of occurrence in each year and would have a 5 to 10 percent chance of occurring at least once over a 10-year period. This level of flood frequency is likely acceptable for existing rural population protection and investment in permanent crops. ")
                      ), # end bullets
                      
                      tags$br(),
                      
                      "The flood hazard mapping compares modeled local peak water levels throughout the Delta (for each of the likelihood levels described above) to adjacent levee crest or shoreline surface elevations and projects floodwaters landward. Topography is represented using Delta-wide elevation data gathered in 2017 and released in 2019 by USGS and DWR. The Delta 
                      Adapts team conducted extensive stakeholder review with Delta levee engineers and local city and county staff familiar with recent and ongoing levee improvement projects to ensure the levee dataset accurately represents conditions on the ground. Levee elevations used for future conditions flood hazard assessments (2030, 2050, and 2085) do not include 
                      potential future subsidence, slumping, or degradation or levees nor do they include potential future investments that could improve levees.",
                      
                      tags$br(),
                      tags$br(),
                      
                      "The flood hazard maps identify islands and tracts that are exposed to flooding by overtopping of levees for each future planning horizon. Other modes of levee failure such as seepage and stability are not evaluated; thus, the likelihood of flooding shown in these maps may under-estimate the likelihood of levee failure across all failure modes. Changes 
                      to local water levels and flood risk caused by unreclaimed island flooding (i.e., islands that flood and permanently become open water) are also not considered in these maps. Unreclaimed islands could lower flood water levels in adjacent channels by providing additional storage but could also increase flood risk by creating open water conditions that 
                      increase wind wave and other erosive processes. The flood hazard modeling also does not account for overbank flows and floodplain storage that may be especially important along the San Joaquin River as it enters the Delta.",
                      
                      tags$br()
                      
                      ), # end div
                    
                    tags$br(),
                    
                    h3("Mapping Scenarios"),
                    
                    h4("Deterministic"),
                    tags$div(
                      "Deterministic scenarios use specific sea level rise amounts and identify regions that would be flooded at the 100-year/1% annual chance water level. Water levels for the deterministic scenarios were estimated using Monte Carlo simulations with 300,000 iterations.",
                      tags$br(),
                      tags$br(),
                      
                      "In addition to sea level rise amount, the deterministic scenarios require selection of an expected watershed hydrology that will be representative of conditions at a specific planning horizon. The existing and 2030 scenarios use historical watershed hydrology, while 
                      2050 scenarios are based on expected conditions at mid-century (2035-2064) under RCP 8.5 (a climate scenario that is representative of the current emissions trajectory). A single post-2050 planning horizon (2050+) was selected to illustrate the nature of flood hazards 
                      that may occur at end-of-century (2070-2099) under RCP 8.5, as substantial flooding beyond 2050 may alter hydrodynamics in a way that cannot be captured by the model.",
                      tags$br(),
                      tags$br(),
                      
                      "Selection of sea level rise amounts to incorporate into the models were informed by the Ocean Protection Council’s",
                      tags$a("2018 Sea Level Rise Guidance document:",
                             href="https://opc.ca.gov/webmaster/ftp/pdf/agenda_items/20180314/Item3_Exhibit-A_OPC_SLR_Guidance-rd3.pdf",
                             target="_blank"
                             ),
                      
                      tags$ul(
                        tags$li("6 inches is nearly equal to the median projection for 2030 and was selected to be representative of plausible 2030 conditions"),
                        tags$li("12 inches is roughly equal to the median projection for 2050 and was selected to be representative of plausible 2050 conditions (or extreme 2030 conditions) "),
                        tags$li("24 inches is nearly equal to the upper range projection for 2050 and was selected to be representative of extreme 2050 conditions (or plausible 2070 conditions) "),
                        tags$li("42 inches may occur before 2100 and could occur as early as 2070 and was selected to be representative of post-2050 conditions. In addition, 3.5 feet (42 inches) is recommended for planning by the state’s recently adopted",
                          tags$a("Principles for Aligned State Action",
                                 href="https://documents.coastal.ca.gov/reports/2020/5/w6g/w6g-5-2020-exhibits.pdf",
                                 target="_blank"),
                          "and Ocean Protection Council’s",
                        tags$a("2020 Strategic Plan.",
                               href="http://www.opc.ca.gov/webmaster/ftp/pdf/agenda_items/20200226/OPC-2020-2025-Strategic-Plan-FINAL-20200228.pdf",
                               target="_blank")
                        ),
                      ), # end bullets
                      

                      ), # end div
                    
                    h4("Probabilistic"),
                    tags$div(
                      "Probabilistic scenarios combine potential conditions across a range of possible climate outcomes, providing planners and managers with information that more readily fits 
                      into a risk-based decision-making framework. The probabilistic scenarios show regions with a low, medium, high, or very high probability of occurrence at each planning horizon,
                      as described above. Each is associated with a range of inflow and sea level rise scenarios based on probability thresholds and the adjacent levee crest elevation.",
                      tags$br(),
                      tags$br(),
                      
                      "Water levels for the probabilistic scenarios were estimated using Monte Carlo simulations with 2,000,000 iterations. So many iterations are required for the probabilistic scenarios 
                      due to the addition of a probability distribution for sea level rise at each planning horizon as opposed to a constant sea level rise value. As a result, many more iterations are required 
                      to examine the full range of potential sea level rise and inflow conditions."
                      ),
                    
                    tags$br(),
                    tags$br()
                    
                    
           ), # end methods tab
           
           conditionalPanel("false", icon("crosshair"))
) # end navbarpage
) # end taglist


