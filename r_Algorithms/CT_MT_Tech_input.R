#pkm =
#Country_ISO = "URY"
#City = "Munchen"
#mode ="Electric Train"
#Project_share = 0.044
#allocated = 1923376	
#project_name = "Project 66"
#project_currency = "EUR"
#reporting_currency = "EUR"



ct_tech_input_mass_urban <- function(pkm, Country_ISO, City = NA, mode, Project_share, 
                                     signed, allocated, project_name, project_currency, 
                                     reporting_currency) {
  
  
  #currency conversion
  load("r_Other_Code/currency_convert.RData") 
  exchange_stuff <- currency_convert(currency = project_currency, outcurrency = reporting_currency)
  exchange_date <-   exchange_stuff[[2]]
  exchange_rate <-   as.numeric(exchange_stuff[[1]])
  exchange_rate_usd <- currency_convert(project_currency, "USD")[[1]]  
  
  
  
  
  #if we use costdb, project share needs to be 1, but they might still have provided the number
  Project_share_reporting <- Project_share
  unit_cost <- NA
  if (is.na(pkm)) {
    # Look up replacement value from costdb
    cost_db <- readRDS("r_data/cost_db.rds")
    unit_cost <- cost_db$unit_cost[cost_db$Use_of_proceed == "Clean Transport - Mass Transit"
                                   & cost_db$ISO3 == Country_ISO 
                                   & cost_db$Technology == mode]
    pkm <- (allocated*exchange_rate_usd) / unit_cost
    Project_share <- 1
  }
  
    
  CT_Mass_Proj_energyeff <- readRDS("r_data/CT_Mass_Proj_energyeff.rds")
  Grid_EFs <- readRDS("r_data/Grid_EFs.rds")
  
  Grid_EF <- Grid_EFs[Grid_EFs$ISO3 == Country_ISO,]
  Grid_EF <- as.numeric(Grid_EF[Grid_EF$Type == "Firm", "EF"])
  kwh_pkm <- as.numeric(CT_Mass_Proj_energyeff[CT_Mass_Proj_energyeff$Mode == mode, c("kwh_pkm")])
  
  proj_ef_kgco2_pkm <- (kwh_pkm * Grid_EF)/1000
  #consider introducing project-side error margins
  
  
  #Baseline EF
  #CT_Intercity_Passenger.rds
  Urban_Passenger_Citylevel <- readRDS("r_data/CT_Urban_Passenger_City_Data.rds")
  available_cities <- as.vector(Urban_Passenger_Citylevel[Urban_Passenger_Citylevel$ISO3 == Country_ISO, ][,"Geo"])
  
  datalevel <- ifelse(City %in% available_cities$Geo, "City", "Country")
  
  if (datalevel %in% c("City")) {
    bl_row <- Urban_Passenger_Citylevel[Urban_Passenger_Citylevel$ISO3 == Country_ISO, ]
    Source = bl_row$Source
    min_pct = bl_row$min_pct
    max_pct = bl_row$max_pct

  }
  if (datalevel %in% c("Country")) {
    Urban_Passenger_Countrylevel <- readRDS("r_data/CT_Urban_Passenger_Country_Data.rds")
    bl_row <- Urban_Passenger_Countrylevel[Urban_Passenger_Countrylevel$ISO3 == Country_ISO, ]
    Source = bl_row$Source
    min_pct = bl_row$Min_PCT
    max_pct = bl_row$Max_PCT
  }
  bl_ef_kgco2_pkm <- as.numeric(bl_row[, c("Weighted_EF")])
  
  
  Proj_footprint_t <- (proj_ef_kgco2_pkm * pkm)/1000
  BL_footprint_t <- (bl_ef_kgco2_pkm * pkm)/1000
  ghg_avoided_t <- BL_footprint_t-Proj_footprint_t
  ghg_avoided_t_min <- (BL_footprint_t * min_pct)-Proj_footprint_t
  ghg_avoided_t_max <- (BL_footprint_t * max_pct)-Proj_footprint_t
  
  Fin_avoidedGHG_perM <- (ghg_avoided_t * Project_share)/((allocated * exchange_rate)/1000000)
  
  
  
  load("r_Other_Code/GHG_to_DALY_func.RData")
  Fin_dalys_averted <- GHG_to_DALY_func(tGHG=(ghg_avoided_t * Project_share))
  Fin_dalys_averted_perM <- Fin_dalys_averted/((allocated * exchange_rate)/1000000)
  
  
  #Output section
  #get real country name
  ISO_Countries <- readRDS("r_data/ISO_Countries.rds")
  
  Country_name <- as.character(ISO_Countries[ISO_Countries$ISO3 == Country_ISO, "Country"])
  
  
  output <-data.frame(
    Use_of_proceed="Clean Transportation - Mass Transit",
    Project_name = project_name,
    Country = Country_name,
    City = City,
    Mode = mode,
    passenger_km = pkm,
    signed = signed * exchange_rate,
    allocated = allocated * exchange_rate,
    Project_share = Project_share_reporting,
    Fin_GHG_footprint = Proj_footprint_t * Project_share,
    Fin_GHG_Avoidance = ghg_avoided_t * Project_share,
    Fin_avoidedGHG_perM = Fin_avoidedGHG_perM,
    Fin_DALYs = Fin_dalys_averted,
    Fin_DALYs_perM =  Fin_dalys_averted_perM,
    Footprint_total = Proj_footprint_t,
    GHG_Avoidance_total = ghg_avoided_t,
    Fin_GHG_Avoidance_min = ghg_avoided_t_min * Project_share,
    Fin_GHG_Avoidance_max = ghg_avoided_t_max * Project_share,
    GHG_Avoidance_total_min = ghg_avoided_t_min,
    GHG_Avoidance_total_max = ghg_avoided_t_max,
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
    proj_ef_kgco2_pkm,
    bl_ef_kgco2_pkm
    
    
  )
  output
  #train and bus
  # "Electric Bus", "Electric Train", "HighspeedElectric"
  #    Hybrid Bus
  
}

#ct_tech_input_mass_urban(project_name="LocalBus", pkm=1000, allocated=1000000, Project_share = 0.5, Country_ISO="JOR", City = "Amman", mode="Electric Bus", project_currency="SEK", reporting_currency="GBP")

save(ct_tech_input_mass_urban, file = "r_Algorithms/ct_tech_input_mass_urban.RData")
