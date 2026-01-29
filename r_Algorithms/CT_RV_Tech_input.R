#CT Tech -> GHG Algorithm



  #list of modes

#should introduce overrides to all functions

setwd("C:/Users/simva/OneDrive/Documents/Project Preston/")

#Country_ISO = "ESP" 
#mode = "EV"
#vehicle_count = 1000
#Project_share = 0.8
#allocated = 1000000
#project_name = "Tyota" 
#project_currency = "SEK" 
#reporting_currency = "GBP"


#unique(Road_Proj$Car_type)
#list of modes: "EV"          "Mild Hybrid" "Full Hybrid" "PHEV"  

ct_tech_input_road <- function(Country_ISO, mode,vehicle_count, Project_share, 
                               signed, allocated, project_name, project_currency, 
                               reporting_currency)
{

  
  #currency conversion
  load("r_Other_Code/currency_convert.RData") 
  exchange_stuff <- currency_convert(currency = project_currency, outcurrency = reporting_currency)
  exchange_date <-   exchange_stuff[[2]]
  exchange_rate <-   as.numeric(exchange_stuff[[1]])
  exchange_rate_usd <- currency_convert(project_currency, "USD")[[1]]  
  
  
  
  mode_rep <- mode
  if(mode == "Electric Vehicle (EV/BEV)") { mode <- "EV"}
  if(mode == "Full Hybrid (HEV)") { mode <- "Mild Hybrid"}
  if(mode == "Mild Hybrid (MHEV)") { mode <- "Full Hybrid"}
  if(mode == "Plug-in Hybrid Electric Vehicle (PHEV)") { mode <- "PHEV"}
  
  
  
  
  
  
  
  #if we use costdb, project share needs to be 1, but they might still have provided the number
  Project_share_reporting <- Project_share
  unit_cost <- NA
  #USD_vehicle
  if (is.na(vehicle_count)) {
    cost_db <- readRDS("r_data/cost_db.rds")
    unit_cost <- cost_db$unit_cost[cost_db$Use_of_proceed == "Clean Transport - Cars" 
                                   & cost_db$ISO3 == Country_ISO 
                                   & cost_db$Technology == mode]
    vehicle_count <- (allocated*exchange_rate_usd) / unit_cost
    Project_share <- 1
  }
  

  
  
  #PROJECT: ROAD VEHICLE SECTION
    Road_Proj <- readRDS("r_data/CT_Road_Proj.rds")
    Road_Proj <- Road_Proj[Road_Proj$ISO3 == Country_ISO,]
    Proj_ef <- as.numeric(Road_Proj[Road_Proj$Car_type == mode,"Dist_EF"])
    
    Road_BL <- readRDS("r_data/CT_Road_bl.rds")
    BL_ef <- as.numeric(Road_BL[Road_BL$ISO3 == Country_ISO,"Dist_ef_pkm"])
    min_pct <- as.numeric(Road_BL[Road_BL$ISO3 == Country_ISO,"Total_min"])
    max_pct <- as.numeric(Road_BL[Road_BL$ISO3 == Country_ISO,"Total_max"])
    Source <- Road_BL[Road_BL$ISO3 == Country_ISO,"Source"]

    Mileage_data <- readRDS("r_data/CT_mileage.rds")
    Mileage <- as.numeric(Mileage_data[Mileage_data$ISO3 == Country_ISO, c("Mileage")])
    
    occupancy <- as.numeric(Mileage_data[Mileage_data$ISO3 == Country_ISO, c("Occupancy")])
    
    
    Proj_co2_footprint_t = (Proj_ef * Mileage * occupancy * vehicle_count)/1000
    BL_co2_footprint_t = (BL_ef * Mileage * occupancy * vehicle_count)/1000
    avoided_ghg_t <- BL_co2_footprint_t - Proj_co2_footprint_t
  
    avoided_ghg_t_min <- (BL_co2_footprint_t * min_pct)-Proj_co2_footprint_t
    avoided_ghg_t_max <- (BL_co2_footprint_t * max_pct)-Proj_co2_footprint_t
    
    Fin_avoidedGHG_perM <- (avoided_ghg_t * Project_share)/((allocated * exchange_rate)/1000000)
    
    pkm = Mileage * vehicle_count
    
    
    load("r_Other_Code/GHG_to_DALY_func.RData")
    Fin_dalys_averted <- GHG_to_DALY_func(tGHG=(avoided_ghg_t * Project_share))
    Fin_dalys_averted_perM <- Fin_dalys_averted/((allocated * exchange_rate)/1000000)

    #Output section
    #get real country name
    ISO_Countries <- readRDS("r_data/ISO_Countries.rds")
    
    Country_name <- as.character(ISO_Countries[ISO_Countries$ISO3 == Country_ISO, "Country"])
    

    
    output <-data.frame(
      Use_of_proceed= "Clean Transportation - Personal Passenger Vehicles",
      Project_name = project_name,
      Country = Country_name, 
      Mode = mode_rep,
      passenger_km = pkm,
      Number_of_vehicles = vehicle_count,
      signed = signed * exchange_rate,
      allocated = allocated * exchange_rate,
      Project_share = Project_share_reporting,
      Fin_GHG_footprint = Proj_co2_footprint_t * Project_share,
      Fin_GHG_Avoidance = avoided_ghg_t * Project_share,
      Fin_avoidedGHG_perM = Fin_avoidedGHG_perM,
      Fin_DALYs = Fin_dalys_averted,
      Fin_DALYs_perM =  Fin_dalys_averted_perM,
      Footprint_total = Proj_co2_footprint_t,
      GHG_Avoidance_total = avoided_ghg_t,
      Fin_GHG_Avoidance_min = avoided_ghg_t_min * Project_share,
      Fin_GHG_Avoidance_max = avoided_ghg_t_max * Project_share,
      GHG_Avoidance_total_min = avoided_ghg_t_min,
      GHG_Avoidance_total_max = avoided_ghg_t_max,
      exchange_date = exchange_date,
      exchange_rate = exchange_rate,
      project_currency = project_currency,
      reporting_currency = reporting_currency,
      Category = c("Green"),
      allocated_orgcur = allocated,
      allocated_usd = allocated * exchange_rate_usd,
      Country_ISO = Country_ISO
      #assumption table additions
      ,
      unit_cost,
      Proj_ef,
      BL_ef,
      Mileage,
      occupancy
      
          )
    output
  }




#ct_tech_input_road(project_name="Toyota Hybrids", Country_ISO="AFG", allocated=1000000, mode="EV", Project_share=0.5, vehicle_count=1000, project_currency="SEK", reporting_currency="GBP")
save(ct_tech_input_road, file = "r_Algorithms/ct_tech_input_road.RData")


