# Delta Adapts Flood Analysis

Welcome! This repository contains code that creates and visualizes polygons for flooded regions of the Delta under future climate scenarios, and is a project of the Delta Stewardship Council's [Delta Adapts](https://deltacouncil.ca.gov/delta-plan/climate-change) initiative. 

The code is organized in three sections: 
- 1_WSEs: Estimate water surface elevations (WSE) for future climate scenarios (driven by user specified boundary conditions for sea level rise and riverine inflows), and create cumulative distribution functions (CDFs) based on WSEs.
- 2_Overtop_analysis: Create polygons of flooded areas under different deterministic and probabilistic scenarios
- 3_ShinyData: Load flood polygons, assets, and vulnerability data into an interactive Shiny app where users can visualize flood exposure risk under future climate and hydrological scenarios.


## 1_WSEs: Create WSE data and CDFs

Use the “createWSE.R” analysis script to create WSE data for each scenario. This script allows the user to emulate Monte Carlo simulations using three previously developed boundary conditions (or “hydrologies” - historical, midcentury, and end of century) and sea level rise scenarios from 0-10 feet (whole foot increments). Also allows user to establish flow caps on any of the tributaries (flow caps are proxies for upstream channel capacity allowing the user to cap the maximum inflow to the Delta). 

1. Optional: add a flow cap for any of the five Delta tributaries (San Joaquin, Sacramento, Mokelumne, Calaveras, Cosumnes rivers) 
2. Identify scenarios you want to explore (ex “Hist_0_SACcapped”) (Note: there are 165 possible scenarios to explore: 3 hydrologies, 11 SLR levels, and 5 rivers to cap) 
3. Set output location for WSE csv files. 
4. Run loop. Each scenario will run 429 nodes (these are unique locations throughout the Delta) and output a single .csv file (can take hours to run one scenario). 

After running createWSE.R, use the “createCDF.R” analysis script (in the same Delta Flooding repo) to create CDFs from WSE data. 
1. Identify scenarios for which you have saved WSE .csv files.  
2. Call folder where they are stored within the loop and set output location for CDF csvs. 
3. Run loop.  


## 2_OvertopAnalysis: Create flood polygons

Use the “OvertopAnalysis.Rmd” script to use WSE CDFs from 1_WSEs to create polygons of flooded areas under different deterministic and probabilistic scenarios. Requires CDFs, and polygons for elevation regions and levees (included in repository under 2_OvertopAnalysis/data). Note: this code is well-commented and should be navigable for potential users. 

1. Import a CDF file as the WSE file. Filter probabilistic scenarios (each column represents an array of data across nodes for a probability/year increment) of interest. (year x probability is equal to one: 0.1 = 10 year, 0.02 = 50 year,  0.01 = 100 year, 0.005 = 200 year) 
2. Call levee shapefile (2_OvertopAnalysis/data/Levees) 
3. Work through code to produce deterministic and/or probabilistic shapefiles.  
4. Output files will be stored as shapefiles in the working directory/output/FloodPoly (Note: the script does not overwrite existing files with the same name. The files must be deleted or moved if you are updating the polygons.) 


## 3_ShinyData: Build Shiny app

The code required to run the Shiny app is in two scripts in the main directory: server.R (server logic) and ui.R (user interface logic). Opening either of these files will allow the user to “Run App” and launch the shiny app. Server.R will automatically run “wrangling.R”, which contains code for manipulating the tabular and spatial data used in the shiny app. 

1. If updates are made to input data in the previous two sections, polygons exported to   2_OvertopAnalysis/output/FloodPoly can be imported into the Shiny App by recreating 3_ShinyData/poly_combined.shp in OvertopAnalysis.Rmd, or by dragging individual polygons for each scenario into 3_ShinyData/Deterministic or 3_ShinyData/Probabilistic, which are then imported into the app. 
2. Additional shapefiles for assets can be added to 3_ShinyData/Assets (wrangling.R and server.R will need to be updated to reflect additions). 


Please contact [climatechange@deltacouncil.ca.gov](mailto:climatechange@deltacouncil.ca.gov) with any questions.


### Other information

A step 0 may be added in the future that includes Python code for running the Monte Carlo simulations which are then incorporated into the 1_WSEs scripts. 


This Shiny app was inspired by the [SuperZIP Explorer](https://shiny.rstudio.com/gallery/superzip-example.html) and the [COVID-19 tracker](https://shiny.rstudio.com/gallery/covid19-tracker.html).

