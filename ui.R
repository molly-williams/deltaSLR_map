# Code adapted in part from the COVID-19 mapper app: https://shiny.rstudio.com/gallery/covid19-tracker.html,
# and the SuperZIP map: https://shiny.rstudio.com/gallery/superzip-example.html, 


library(shiny)
library(shinyWidgets)
library(leaflet)




inline=function(x) {
    tags$div(style="display:inline-block;", x)
}



navbarPage("Climate Change Flood Scenarios in the Delta", id="nav",
           
           tabPanel("Probabilistic map",
                    div(class="outer",
                        
                        tags$head(
                            # Include custom CSS
                            includeCSS("styles.css"),
                            includeScript("gomap.js")
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
                                   
                                   h4("Instructions:"),
                                   tags$div(
                                     tags$ul(
                                       tags$li("Use this tool to visualize Delta regions at risk of flood exposure and associated likelihoods by selecting a watershed hydrology category and an amount of sea level rise"),
                                       tags$li("Toggle flood exposure polygons in the upper right-hand corner to isolate areas at high flood risk in the next 10 years, 50 years, 100 years, and 200 years."),
                                       tags$li("The polygons are inclusive, so flood exposure regions of lower flood exposure risk will include those of higher risk."),
                                       tags$li("Maybe say something here describing what a combination represents - eg. assumptions behind 2050 hydrology + 3' SLR")
                                     )
                                   ),
                                   
                                   tags$br(),
                                   
                                   
                                      radioButtons(inputId="hydro", 
                                                   label=h4("1. Select watershed hydrology:"), 
                                                   choices=c("Historical", "2050", "2085")
                                                   ),
                                      
                                      sliderTextInput(inputId="SLR",
                                                  label = h4("2. Select sea level rise (feet):"),
                                                  choices = c(0:7, 10),
                                                  grid=TRUE
                                                  ),
                                   tags$em("Note: no data exist for 8' and 9' of SLR."),
                                   
                                   tags$br(),
                                   tags$br()
                                   
                                   #, 
                                   # checkboxGroupButtons(
                                   #   inputId = "ID10",
                                   #   label = h4("Toggle flood exposure risk layers:"),
                                   #   choices = "10 year flood exposure",
                                   #   selected = "10 year flood exposure",
                                   #   checkIcon = list(
                                   #     yes = tags$i(class = "fa fa-check-square",
                                   #                  style = "color: #08519C"),
                                   #     no = tags$i(class = "fa fa-square-o",
                                   #                 style = "color: #08519C"))
                                   # ),
                                   #
                                   # tags$div(
                                   #   tags$ul(
                                   #     tags$li(
                                   #       tags$b("Very high likelihood")
                                   #     ),
                                   #     tags$li("Annual Chance: 10%"),
                                   #     tags$li("Chance over 10 years: >65%")
                                   #   )
                                   # ),
                                   #
                                   # checkboxGroupButtons(
                                   #   inputId = "ID50",
                                   #   choices = c("50 year flood exposure"),
                                   #   selected = c("50 year flood exposure"),
                                   #   checkIcon = list(
                                   #     yes = tags$i(class = "fa fa-check-square",
                                   #                  style = "color: #3182BD"),
                                   #     no = tags$i(class = "fa fa-square-o",
                                   #                 style = "color: #3182BD"))
                                   # ),
                                   #
                                   # tags$div(
                                   #   tags$ul(
                                   #     tags$li(
                                   #       tags$b("High likelihood")
                                   #     ),
                                   #     tags$li("Annual Chance: 2-10%"),
                                   #     tags$li("Chance over 10 years: 18-65%")
                                   #   )
                                   # ),
                                   #
                                   # checkboxGroupButtons(
                                   #   inputId = "ID100",
                                   #   choices = c("100 year flood exposure"),
                                   #   selected = c("100 year flood exposure"),
                                   #   checkIcon = list(
                                   #     yes = tags$i(class = "fa fa-check-square",
                                   #                  style = "color: #6BAED6"),
                                   #     no = tags$i(class = "fa fa-square-o",
                                   #                 style = "color: #6BAED6"))
                                   # ),
                                   #
                                   # tags$div(
                                   #   tags$ul(
                                   #     tags$li(
                                   #       tags$b("Medium likelihood")
                                   #     ),
                                   #     tags$li("Annual Chance: 1-2%"),
                                   #     tags$li("Chance over 10 years: 10-18%")
                                   #   )
                                   # ),
                                   #
                                   # checkboxGroupButtons(
                                   #   inputId = "ID200",
                                   #   choices = c("200 year flood exposure"),
                                   #   selected = c("200 year flood exposure"),
                                   #   checkIcon = list(
                                   #     yes = tags$i(class = "fa fa-check-square",
                                   #                  style = "color: #BDD7E7"),
                                   #     no = tags$i(class = "fa fa-square-o",
                                   #                 style = "color: #BDD7E7"))
                                   # ),
                                   #
                                   # tags$div(
                                   #   tags$ul(
                                   #     tags$li(
                                   #       tags$b("Low likelihood")
                                   #     ),
                                   #     tags$li("Annual Chance: 0.5-1%"),
                                   #     tags$li("Chance over 10 years: 5-10%")
                                   #   )
                                   # )
                                   #
                                   #
                                   #
                                   
                                   
                                   #  uiOutput("selected_var"), # reactive text
                                   
                                   #    tags$br(),
                                   #    tags$br(),
                                   
                                   #    h4("Pin Location on Map:"),
                                   #    inline(numericInput("long", label = h5("Longitude:"), value = -121.50001)),
                                   #    inline(numericInput("lat", label = h5("Latitude:"), value = 38.00001)),
                                   
                                   #    actionButton("recalc", "Show point", width="40%"),
                                   
                                   #  tags$br(),
                                   #  tags$br(),
                                   
                                   #  tags$div(
                                   #    tags$a("Get a lat/long from an address here.", href="https://www.latlong.net/convert-address-to-lat-long.html", target="_blank")
                                   #  ),
                                   
                                   #    tableOutput('table')
                                      
                        )
                        
                    ),
                    
                    tags$div(id="cite",
                             tags$em('This map was created in support of the Delta Adapts initiative, a project of the Delta Stewardship Council (2020)')
                    )
           ),
           

          
           tabPanel('Methodology and Resources',
                    h3("Background"),
                    tags$div(
                        "This tool was developed as part of",
                        tags$a("Delta Adapts: Creating a Climate Resilient Future,", href="https://deltacouncil.ca.gov/delta-plan/climate-change", target="_blank"),
                        "an initiative of the Delta Stewardship Council.",
                        
                        tags$br(),
                        tags$br(),
                        
                  #      "more intro text here",
                        
                        tags$br(),
                        tags$br(),
                        
                        "The results of the vulnerability assessment are slated to be published in early 2021."
                    ),
                    
                    #         img(src="vulnerability_model.jpg", height="100%", width = "100%"),
                    
                    tags$br(),
                    
                    h3("Methodology"),
                    
                    h4("Analysis Approach"),
                    tags$div(
                #        "methodology text here"
                    ),
                    tags$br(),
                    
                    h4("Mapping Approach"),
                    tags$div(
                #        "more methodology text here"
                    ),
                    
                    tags$br(),
                    
                   
                    
                    h3("Resources"),

                    tags$br(),
                    
                    h3("Contact"),
                    tags$div(
                        "Please contact ",
                        tags$a("Andrew/Cory?", href="mailto:avery.livengood@deltacouncil.ca.gov"),
                        " at the Delta Stewardship Council with any questions."
                    ),
                    
                    tags$br(),
                    tags$br()
                    
           ),
           
           conditionalPanel("false", icon("crosshair"))
)


