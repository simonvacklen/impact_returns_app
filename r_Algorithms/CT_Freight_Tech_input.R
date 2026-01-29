#only train freight for now

ct_tech_input_intercity_freight <- function(project_name, tkm, Country_ISO, mode, 
                                            Project_share, signed, allocated, 
                                            project_currency, reporting_currency)
{
  
  #currency conversion
  load("r_Other_Code/currency_convert.RData") 
  exchange_stuff <- currency_convert(currency = project_currency, outcurrency = reporting_currency)
  exchange_date <-   exchange_stuff[[2]]
  exchange_rate <-   as.numeric(exchange_stuff[[1]])
  exchange_rate_usd <- currency_convert(project_currency, "USD")[[1]]  
  
  
  #if we use costdb, project share needs to be 1, but they might still have provided the number
  Project_share_reporting <- Project_share
  
  unit_cost <- NA
  #estimate based on cost
  if (is.na(tkm)) {
    # Look up replacement value from costdb
    cost_db <- readRDS("r_data/cost_db.rds")
    unit_cost <- cost_db$unit_cost[cost_db$Use_of_proceed == "Clean Transport - Freight" 
                                   & cost_db$ISO3 == Country_ISO 
                                   & cost_db$Technology == mode]
    tkm <- (allocated*exchange_rate_usd) / unit_cost
    Project_share <- 1
  }
  

  #BASELINE
  #rail, cars share
  Intercity_Freight <- readRDS("r_data/CT_Intercity_Freight.rds")
  bl_row <- Intercity_Freight[Intercity_Freight$ISO3 == Country_ISO, ]
  Source_bl = bl_row$Source
  min_pct = as.numeric(bl_row$Min_PCT)
  max_pct = as.numeric(bl_row$Max_PCT)
  bl_ef = as.numeric(bl_row$Weighted_EF)
  
  baseline_footprint_kg <- bl_ef * tkm
  
  #Project
  CT_Freight_Proj_energyeff <- readRDS("r_data/CT_Freight_Proj_energyeff.rds")
  kwh_tkm <- as.numeric(CT_Freight_Proj_energyeff$kwh_tkm)
  Source_Proj <- as.character(CT_Freight_Proj_energyeff$Source)
  
  Grid_EFs <- readRDS("r_data/Grid_EFs.rds")
  Grid_EF <- Grid_EFs[Grid_EFs$ISO3 == Country_ISO,]
  Grid_EF <- as.numeric(Grid_EF[Grid_EF$Type == "Firm", "EF"])/1000
  
  kgco2_tkm <- kwh_tkm * Grid_EF
  

  project_footprint <- kgco2_tkm * tkm
  
  ghg_avoided_t <- (baseline_footprint_kg - project_footprint)/1000
  GHG_Avoidance_t_min <- ((baseline_footprint_kg * min_pct) - project_footprint)/1000
  GHG_Avoidance_t_max <- ((baseline_footprint_kg * max_pct) - project_footprint)/1000
  
  Fin_avoidedGHG_perM <- (ghg_avoided_t * Project_share)/((allocated * exchange_rate)/1000000)
  
  load("r_Other_Code/GHG_to_DALY_func.RData")
  Fin_dalys_averted <- GHG_to_DALY_func(tGHG=(ghg_avoided_t * Project_share))
  Fin_dalys_averted_perM <- Fin_dalys_averted/((allocated * exchange_rate)/1000000)
  
  
  
  #not utilised yet is the min max of the project
  
  ###OUtput Section
  ISO_Countries <- readRDS("r_data/ISO_Countries.rds")
  Country_name <- as.character(ISO_Countries[ISO_Countries$ISO3 == Country_ISO, "Country"])
  
  
  output <-data.frame(
    Use_of_proceed="Clean Transportation - Freight",
    Project_name = project_name,
    Country = Country_name,
    Mode = mode,
    tonnekm = tkm,
    signed = signed * exchange_rate,
    allocated = allocated * exchange_rate,
    Project_share = Project_share_reporting,
    Fin_GHG_footprint = project_footprint * Project_share,
    Fin_GHG_Avoidance = ghg_avoided_t * Project_share,
    Fin_avoidedGHG_perM = Fin_avoidedGHG_perM,
    Fin_DALYs = Fin_dalys_averted,
    Fin_DALYs_perM =  Fin_dalys_averted_perM,
    Footprint_total = project_footprint,
    GHG_Avoidance_total = ghg_avoided_t,
    Fin_GHG_Avoidance_min = GHG_Avoidance_t_min * Project_share,
    Fin_GHG_Avoidance_max = GHG_Avoidance_t_max * Project_share,
    GHG_Avoidance_total_min = GHG_Avoidance_t_min,
    GHG_Avoidance_total_max = GHG_Avoidance_t_max,
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
    kgco2_tkm,    
    bl_ef

    
  )
  output
  #"Electric Train"
}

#ct_tech_input_intercity_freight(project_name="freightcorp", tkm=1000000, Country_ISO="GBR", mode="Electric Train",Project_share=0.5, allocated=1000000,  project_currency="GBP", reporting_currency="SEK")
save(ct_tech_input_intercity_freight, file = "r_Algorithms/ct_tech_input_intercity_freight.RData")


