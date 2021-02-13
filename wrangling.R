library(shiny)
library(leaflet)
library(RColorBrewer)
library(raster)
library(tidyverse)
library(rgeos)
library(sf)
require(stringr)
library(rmapshaper)
library(scales)
library(shinyalert)
library(rhandsontable)





###################### Data import and wrangling ###########################


# Import tabular data for responsive table with population and assets at risk of exposure
# Accessed from Delta Adapts sharepoint (Data and Modeling > Flooding Analysis) Jan 28 2021

tab_data <- read_csv("3_ShinyData/county_scenario_data.csv")


# remove decimal points and add comma to population columns

tab_data$Population <- format(round(as.numeric(tab_data$Population), 0), nsmall=0, big.mark=",")  



##### Import polygons


# County shapefiles

counties <- read_sf("3_ShinyData/LDSMcounties.shp") %>% 
  st_transform(4326) #reproject to lat/long



#### Import deterministic and probabilistic Deta Adapts polygons
## Note: these polygons were last updated 19 Nov 2020 and were copied in Jan '21 from G:ArcGIS\Projects\CCVA_Flood\DeltaAdapts_Mapping_KG\DeltaAdapts_FloodMap_InputData\201120_FloodMap_Inputs\Deterministic111920


# Import individual deterministic polygons

M0_det <- read_sf("3_ShinyData/Deterministic/BaseDet.shp") %>% 
  select(NAME, fldfight) %>% 
  mutate(Probability = NA) %>% 
  mutate(scenario = "Deterministic baseline: 0' SLR") %>% 
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



# Join polygons for all 9 scenarios 

det_prob_polys <- rbind(M0_det, M1, M2, M3, M4, M0_prob, M5, M6, M7)


# Join polygons with tabular data by island for population, asset value ($) and area (square miles)
# From Delta Adapts sharepoint page, updated ~Jan 26 2021
# https://deltacouncil.sharepoint.com/sites/DeltaAdaptsProject/Shared%20Documents/Forms/AllItems.aspx?csf=1&web=1&e=SCeQzl&cid=3e2f8e57%2Dbb67%2D44b1%2D840e%2D594ef83cb5fc&RootFolder=%2Fsites%2FDeltaAdaptsProject%2FShared%20Documents%2FData%20and%20Modeling%2FFlooding%20Analysis&FolderCTID=0x01200090DD54C0503D78468E7D9D752120A071


island_data <- read_csv("3_ShinyData/island_data.csv") 

det_prob_polys <- merge(det_prob_polys, island_data, all=TRUE) %>% 
  drop_na("scenario")


############ Import assets 
# Note: these data came from AECOM's DSC Asset Output folder on sharepoint from Dec 2020. 


# Agriculture
# ag_delta <- read_sf("3_ShinyData/Assets/ag_delta.shp") %>%
#  select(Type=Subclass, geometry) %>%
#  filter(Type !="Water") %>%
#  filter(Type != "Urban") %>%
#  filter(Type != "Riparian") %>%
#  filter(Type != "Semi-agricultural/ROW") %>%
#  filter(Type != "Upland Herbaceous") %>%
#  filter(Type != "Wet herbaceous/sub irrigated pasture") %>%
#  filter(Type != "Summer Fallow") %>%
#  st_transform(4326) %>%
#  st_zm()
# 
# ag_suisun <- read_sf("3_ShinyData/Assets/ag_suisun.shp") %>%
#  select(Type=Crop2016, geometry) %>%
#  filter(Type !="Idle") %>%
#  filter(Type != "Urban") %>%
#  filter(Type != "Managed Wetland") %>%
#  st_transform(4326) %>%
#  st_zm()
# 
# # combine and simplify to reduce filesize
# ag_polys <- rbind(ag_delta, ag_suisun) %>%
#  rmapshaper::ms_simplify() %>%
#  st_as_sf() %>%

#st_write(ag_polys, dsn = "3_ShinyData/Assets/ag_combined_simp.shp", layer = "", driver = "ESRI Shapefile" )


# ^^ Ran all of this then exported the sf object to a new simplified shapefile and deleted the other files in order to save space in the directory

ag_polys <- read_sf("3_ShinyData/Assets/ag_combined_simp.shp")


# Communications
cell_towers <- read_sf("3_ShinyData/Assets/cell_towers.shp") %>% 
  select(geometry) %>%
  mutate(Type = "Cell Tower") %>% 
  st_transform(4326) %>% 
  st_zm()

comm_facilities <- read_sf("3_ShinyData/Assets/comm_facilities.shp") %>% 
  select(geometry) %>%
  mutate(Type = "Communications Facility") %>% 
  st_transform(4326) %>% 
  st_zm()

data_centers <- read_sf("3_ShinyData/Assets/data_centers.shp") %>% 
  select(geometry) %>%
  mutate(Type = "Data Center") %>% 
  st_transform(4326) %>% 
  st_zm()

comm_points <- rbind(cell_towers, comm_facilities, data_centers)


# Cultural resources

legacy_towns <- read_sf("3_ShinyData/Assets/legacy_towns.shp") %>% 
  select(Type = FacType, geometry) %>%
  st_transform(4326) %>% 
  st_zm()

historic_places <- read_sf("3_ShinyData/Assets/historic_places.shp") %>% 
  select(geometry) %>%
  mutate(Type = "Historic Place") %>% 
  st_transform(4326) %>% 
  st_zm()

historic_landmarks <- read_sf("3_ShinyData/Assets/historic_landmarks.shp") %>% 
  select(Type = FacType, geometry) %>%
  st_transform(4326) %>% 
  st_zm()

cultural_points <- rbind(legacy_towns, historic_places, historic_landmarks)


# Critical facilities
police <- read_sf("3_ShinyData/Assets/police.shp") %>% 
  select(geometry) %>%
  mutate(Type = "Police station") %>% 
  st_transform(4326) %>% 
  st_zm()

hospitals <- read_sf("3_ShinyData/Assets/hospitals.shp") %>% 
  select(geometry) %>%
  mutate(Type = "Hospital") %>% 
  st_transform(4326) %>% 
  st_zm()

fire_stations <- read_sf("3_ShinyData/Assets/fire_stations.shp") %>% 
  select(geometry) %>%
  mutate(Type = "Fire station") %>% 
  st_transform(4326) %>% 
  st_zm()

wastewater_plants <- read_sf("3_ShinyData/Assets/wastewater_tp.shp") %>% 
  select(geometry) %>%
  mutate(Type = "Wastewater Treatment Plant") %>% 
  st_transform(4326) %>% 
  st_zm()

public_schools <- read_sf("3_ShinyData/Assets/public_schools.shp") %>% 
  select(geometry) %>%
  mutate(Type = "Public School") %>% 
  st_transform(4326) %>% 
  st_zm()

private_schools <- read_sf("3_ShinyData/Assets/private_schools.shp") %>% 
  select(geometry) %>%
  mutate(Type = "Private School") %>% 
  st_transform(4326) %>% 
  st_zm()

critical_points <- rbind(police, hospitals, fire_stations, wastewater_plants, public_schools, private_schools)


# Energy

active_wells <- read_sf("3_ShinyData/Assets/energy_active_wells.shp") %>% 
  select(geometry) %>%
  mutate(Type = "Active Oil Well") %>% 
  st_transform(4326) %>% 
  st_zm()

gas_storage <- read_sf("3_ShinyData/Assets/energy_gas_storage.shp") %>% 
  select(geometry) %>%
  mutate(Type = "Gas Storage Facility") %>% 
  st_transform(4326) %>% 
  st_zm()

natural_gas <- read_sf("3_ShinyData/Assets/energy_natural_gas.shp") %>% 
  select(geometry) %>%
  mutate(Type = "Natural Gas Station") %>% 
  st_transform(4326) %>% 
  st_zm()

power_plants <- read_sf("3_ShinyData/Assets/energy_power_plants.shp") %>% 
  select(geometry) %>%
  mutate(Type = "Power Plant") %>% 
  st_transform(4326) %>% 
  st_zm()

substations <- read_sf("3_ShinyData/Assets/energy_substations.shp") %>% 
  select(geometry) %>%
  mutate(Type = "Substation") %>% 
  st_transform(4326) %>% 
  st_zm()

#transmission_towers <- read_sf("3_ShinyData/Assets/energy_transmission_line_towers.shp") %>% 
#  select(geometry) %>%
#  mutate(Type = "Transmission Tower") %>% 
#  st_transform(4326)
# not using since they follow the transmission lines

energy_points <- rbind(active_wells, gas_storage, natural_gas, power_plants, substations)


oil_pipelines <- read_sf("3_ShinyData/Assets/energy_oil_pipelines.shp") %>% 
  select(geometry) %>%
  mutate(Type = "Oil Pipeline") %>% 
  st_transform(4326) %>% 
  st_zm()

natgas_pipeline <- read_sf("3_ShinyData/Assets/energy_natural_gas_pipelines.shp") %>% 
  select(geometry) %>%
  mutate(Type = "Natural Gas Pipeline") %>% 
  st_transform(4326) %>% 
  st_zm()

transmission_lines <- read_sf("3_ShinyData/Assets/energy_trans_lines.shp") %>% 
  select(geometry) %>%
  mutate(Type = "Transmission Lines") %>% 
  st_transform(4326) %>% 
  st_zm()

energy_lines <- rbind(oil_pipelines, natgas_pipeline, transmission_lines)


# Recreation

campgrounds <- read_sf("3_ShinyData/Assets/campgrounds.shp") %>% 
  select(geometry) %>% 
  mutate(Type = "Campground") %>% 
  st_transform(4326) %>% 
  st_zm()

state_parks <- read_sf("3_ShinyData/Assets/stateparks.shp") %>% 
  select(geometry) %>%
  mutate(Type = "State Park") %>% 
  st_transform(4326) %>% 
  st_zm()

county_parks <- read_sf("3_ShinyData/Assets/county_parks.shp") %>% 
  select(geometry) %>%
  mutate(Type = "Regional Park") %>% 
  st_transform(4326) %>% 
  st_zm()

city_parks <- read_sf("3_ShinyData/Assets/city_parks.shp") %>% 
  select(geometry) %>%
  mutate(Type = "City Park") %>% 
  st_transform(4326) %>% 
  st_zm()

rec_points <- rbind(campgrounds, state_parks, county_parks, city_parks)

rec_lines <- read_sf("3_ShinyData/Assets/trails.shp") %>% 
  select(geometry) %>%
  mutate(Type = "Trails") %>% 
  st_transform(4326) %>% 
  st_zm()


# Transportation infrastructure
airstrips <- read_sf("3_ShinyData/Assets/trans_airstrips.shp") %>% 
  select(Type=SOURCE_D_1, geometry) %>% 
  st_transform(4326) %>% 
  st_zm()

bridges <- read_sf("3_ShinyData/Assets/trans_bridges.shp") %>% 
  select(geometry) %>% 
  mutate(Type="Bridge") %>% 
  st_transform(4326) %>% 
  st_zm()

trans_points <- rbind(airstrips, bridges)


scenic_hwys <- read_sf("3_ShinyData/Assets/trans_scenic_hwys.shp") %>% 
  select(geometry) %>% 
  mutate(Type="Scenic Highway") %>% 
  st_transform(4326) %>% 
  st_zm()

county_hwys <- read_sf("3_ShinyData/Assets/trans_county_highways.shp") %>% 
  select(geometry) %>% 
  mutate(Type="County Highway") %>% 
  st_transform(4326) %>% 
  st_zm()

highways <- read_sf("3_ShinyData/Assets/trans_highways.shp") %>% 
  select(geometry) %>% 
  mutate(Type="Highway") %>% 
  st_transform(4326) %>% 
  st_zm()

railroads <- read_sf("3_ShinyData/Assets/trans_railroads.shp") %>% 
  select(geometry) %>% 
  mutate(Type="Railroad") %>% 
  st_transform(4326) %>% 
  st_zm()

bike_routes <- read_sf("3_ShinyData/Assets/trans_bike_routes.shp") %>% 
  select(geometry) %>% 
  mutate(Type="Bike Route") %>% 
  st_transform(4326) %>% 
  st_zm()

trans_lines <- rbind(county_hwys, scenic_hwys, highways, railroads, bike_routes)


# Waste facilities 

solid_waste <- read_sf("3_ShinyData/Assets/solid_waste.shp") %>% 
  select(geometry) %>%
  mutate(Type = "Solid Waste Facility") %>% 
  st_transform(4326) %>% 
  st_zm()


haz_waste <- read_sf("3_ShinyData/Assets/haz_waste.shp") %>% 
  select(geometry) %>%
  mutate(Type = "Hazardous Waste Facility") %>% 
  st_transform(4326) %>% 
  st_zm() # removes third coordinate to match solid_waste


contam_sites <-  read_sf("3_ShinyData/Assets/contaminated_sites.shp") %>% 
  select(geometry) %>%
  mutate(Type = "Contaminated Site") %>% 
  st_transform(4326) %>% 
  st_zm() # removes third coordinate


waste_points <- rbind(solid_waste, haz_waste, contam_sites) 


# Water conveyance

water_conveyance <- read_sf("3_ShinyData/Assets/water_conveyance.shp") %>% 
  select(geometry) %>%
  mutate(Type = "Water Conveyance") %>% 
  st_transform(4326) %>% 
  st_zm()




#### Assign html tags to asset groups (for cleaner code and easier changes)

ag_group <- HTML('<i class="fa fa-square"; style="font-size:120%; color:#B9F3A8; margin-top:3px;"></i><span style="font-size:100%; color:black;padding-left:5px;"> Agriculture')
comm_group <- HTML('<i class="fa fa-circle"; style="font-size:120%; color:#CF2D2D; margin-top:3px;"></i><span style="font-size:100%; color:black;padding-left:5px;"> Communications facilities')
critical_group <- HTML('<i class="fa fa-circle"; style="font-size:120%; color:#F44AA9; margin-top:3px;"></i><span style="font-size:100%; color:black;padding-left:5px;"> Critical fac.: police/fire stations, hospitals, schools, water treatment')
cultural_group <- HTML('<i class="fa fa-circle"; style="font-size:120%; color:orange; margin-top:3px;"></i><span style="font-size:100%; color:black;padding-left:5px;"> Cultural resources: legacy towns, historic places, landmarks')
energy_point_group <- HTML('<i class="fa fa-circle"; style="font-size:120%; color:purple; margin-top:3px;"></i><span style="font-size:100%; color:black;padding-left:5px;"> Energy: power plants, gas storage, oil wells, stations')
energy_line_group <- HTML('<i class="fas fa-minus"; style="font-size:120%; color:purple; margin-top:3px;"></i><span style="font-size:100%; color:black;padding-left:5px;"> Energy: oil and natural gas pipelines, transmission lines')
rec_point_group <- HTML('<i class="fa fa-circle"; style="font-size:120%; color:#249729; margin-top:3px;"></i><span style="font-size:100%; color:black;padding-left:5px;"> Recreation: campgrounds and parks')
rec_line_group <- HTML('<i class="fas fa-minus"; style="font-size:120%; color:#249729; margin-top:3px;"></i><span style="font-size:100%; color:black;padding-left:5px;"> Recreation: trails')
trans_point_group <- HTML('<i class="fa fa-circle"; style="font-size:120%; color:black; margin-top:3px;"></i><span style="font-size:100%; color:black;padding-left:5px;"> Transportation: airstrips and bridges')
trans_line_group <- HTML('<i class="fas fa-minus"; style="font-size:120%; color:black; margin-top:3px;"></i><span style="font-size:100%; color:black;padding-left:5px;"> Transportation: highways, railroads and bike routes')
waste_group <- HTML('<i class="fa fa-circle"; style="font-size:120%; color:#E0CC5F; margin-top:3px;"></i><span style="font-size:100%; color:black;padding-left:5px;"> Waste sites: solid, hazardous, and contaminated sites ')
water_group <- HTML('<i class="fas fa-minus"; style="font-size:120%; color:blue; margin-top:3px;"></i><span style="font-size:100%; color:black;padding-left:5px;"> Water conveyance')



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



#### Assign html tags to flood poly groups based on probability

group_10 <- HTML('<i class="fa fa-circle"; style="font-size:120%; color:#08306B; margin-top:3px;"></i><span style="font-size:110%; color:black;padding-left:5px;"> Very high likelihood | 10%+ annual chance | 65%+ chance over 10 years')
group_50 <- HTML('<i class="fa fa-circle"; style="font-size:120%; color:#08519C; margin-top:3px;"></i><span style="font-size:110%; color:black;padding-left:5px;">  High likelihood | 2-10% annual chance | 18-65% chance over 10 years')
group_100 <- HTML('<i class="fa fa-circle"; style="font-size:120%; color:#2171B5;margin-top:3px;"></i><span style="font-size:110%; color:black;padding-left:5px;">  Medium likelihood | 1-2% annual chance | 10-18% chance over 10 years')
group_200 <- HTML('<i class="fa fa-circle"; style="font-size:120%; color:#6BAED6;margin-top:3px;"></i><span style="font-size:110%; color:black;padding-left:5px;">  Low likelihood | 0.5-1% annual chance | 5-10% chance over 10 years')



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
# clip to Delta SM boundary?

################### Create palettes and popup text ###############

s_colorData <- ifelse(sovi$Vulnerability == 1, "1: Moderate", ifelse(sovi$Vulnerability == 2, "2: High", "3: Highest"))  
sovi_pal <- colorFactor("Reds", s_colorData)


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
# Updating the map won't change the link

