#WM Tech -> GHG Algorithm
setwd("C:/Users/simva/OneDrive/Documents/Project Preston/")

#mwh = 1000
#mw= NA
#Country_ISO <- "GBR"
#tonnage <- NA
#project_currency = "SEK"
#reporting_currency = "GBP"
#Project_share <- 1
#allocated = 1000000



wm_tech_input <- function(mwh=NA,mw=NA, tonnage=NA, Country_ISO, signed, allocated, 
                          Project_share, project_name, project_currency, reporting_currency)
{
  

  #currency conversion
  load("r_Other_Code/currency_convert.RData") 
  exchange_stuff <- currency_convert(currency = project_currency, outcurrency = reporting_currency)
  exchange_date <-   exchange_stuff[[2]]
  exchange_rate <-   as.numeric(exchange_stuff[[1]])
  exchange_rate_usd <- currency_convert(project_currency, "USD")[[1]]  
  
  
  load("r_Algorithms/re_capacity_generation_conv.RData")
  
  #mwh is estimated from cost here - but missing tonnage is sorted in the uop function, based on mwh
  #WTE is a technology but treated like a UoP so looks a bit different
  #if (is.na(inputs$`Share of total project financing`)) {
  
  
  
  #if we use costdb, project share needs to be 1, but they might still have provided the number
  Project_share_reporting <- Project_share
  
  unit_cost <- NA
  if(all(
    is.na(mw),
    is.na(mwh),
    is.na(tonnage)
  )
  ) {  
    
    # Look up replacement value from costdb
    cost_db <- readRDS("r_data/cost_db.rds")
    unit_cost <- cost_db$unit_cost[cost_db$ISO3 == Country_ISO 
                                   & cost_db$Technology == "Waste-to-energy"] 
    mwh <- allocated / unit_cost
    Project_share <- 1
  }
  
  capacity_factor <- NA
  ## If MW is missing to capacity/generation conversion here
  if (is.na(mw)) {
      mw  <- capacity_generation_conv(iso3=Country_ISO, 
                                     technology="Waste-to-energy", 
                                     mw=mw,
                                     mwh=mwh)
      capacity_factors_ds <- readRDS("r_data/RE_Capacity_Factors.rds")
      capacity_factor <- capacity_factors_ds[capacity_factors_ds$ISO3 == Country_ISO  
                                             & capacity_factors_ds$Technology == "Waste-to-energy", ]$capacity_factor
      
  }
  ## If MWh is missing to capacity/generation conversion here
  if (is.na(mwh)) {
    mwh <- capacity_generation_conv(iso3=Country_ISO, 
                                    technology="Waste-to-energy", 
                                    mw=mw,
                                    mwh=mwh)
    capacity_factors_ds <- readRDS("r_data/RE_Capacity_Factors.rds")
    capacity_factor <- capacity_factors_ds[capacity_factors_ds$ISO3 == Country_ISO  
                                           & capacity_factors_ds$Technology == "Waste-to-energy", ]$capacity_factor
    
  }
  
  

  
  ###Getting baseline waste EF
  WM_Data <- readRDS("r_data/wm_weight_ef.rds")
  WM_Data <- WM_Data[WM_Data$ISO3 == Country_ISO, ]

  ###Getting baseline grid emission factor
  Grid_EFs <- readRDS("r_data/Grid_EFs.rds")
  Grid_EF <- Grid_EFs[Grid_EFs$ISO3 == Country_ISO, ]
  Grid_EF <- as.numeric(Grid_EF[Grid_EF$Type == c("Firm"), "EF"])
  
  #project emissions: if there is tonnes use that, otherwise use MWh
  if (is.na(tonnage)) {
    Project_EF_waste <- NA
    re_project_ef_data <-readRDS("r_data/RE_proj_EFs.rds")
    Project_EF <- as.numeric(re_project_ef_data[re_project_ef_data$Technology == "Waste-to-energy", 2])
    proj_tco2 <- mwh * (Project_EF/1000)
    proj_tco2_low <- proj_tco2 * 0.9
    proj_tco2_high <- proj_tco2 * 1.1
  } else {
    Project_EF <- NA
    Project_EF_waste <- (as.numeric(WM_Data[,c("Incineration_EF")]))
    proj_tco2 <- tonnage * (Project_EF_waste/1000)
    proj_tco2_low <- proj_tco2 * as.numeric(WM_Data[,c("Low_incineration")])
    proj_tco2_high <- proj_tco2 * as.numeric(WM_Data[,c("High_incineration")])
  }
  

  #baseline waste emissions 
  if (is.na(tonnage)) {
  tonnage <- (mwh*1000)/as.numeric(WM_Data[,c("kWh_tMSW")]) 
  baseline_ef_waste <- (as.numeric(WM_Data[,c("WM_BL_EF")]))
  baseline_waste_tco2 <- tonnage * (baseline_ef_waste/1000)
  } else {  
    baseline_ef_waste <- (as.numeric(WM_Data[,c("WM_BL_EF")]))
    baseline_waste_tco2 <- tonnage * (baseline_ef_waste/1000)
  }
  
  
  
  
  baseline_waste_tco2_low <- baseline_waste_tco2 * as.numeric(WM_Data[,c("WM_BL_Low")])
  baseline_waste_tco2_high <- baseline_waste_tco2 * as.numeric(WM_Data[,c("WM_BL_High")])
  
  #baseline electricity emissions  

  if (is.na(tonnage)) {
    baseline_elec_kwh = mwh/1000  
  } else {
    baseline_elec_kwh <- tonnage * as.numeric(WM_Data[,c("kWh_tMSW")])
  }
    
    baseline_elec_kwh_low <- baseline_elec_kwh * as.numeric(WM_Data[,c("kWh_tMSW_Low")])
    baseline_elec_kwh_high <- baseline_elec_kwh * as.numeric(WM_Data[,c("kWh_tMSW_High")])

  baseline_elec_tco2 <- (baseline_elec_kwh * Grid_EF)/1000000
  baseline_elec_tco2_low <- (baseline_elec_kwh_low * Grid_EF)/1000000
  baseline_elec_tco2_high <- (baseline_elec_kwh_high * Grid_EF)/1000000
  
  #avoided ghg
  avoided_tco2 <- (baseline_elec_tco2 + baseline_waste_tco2) - proj_tco2
  avoided_tco2_low <- (baseline_elec_tco2_low + baseline_waste_tco2_low) - proj_tco2_high
  avoided_tco2_high <- (baseline_elec_tco2_high + baseline_waste_tco2_high) - proj_tco2_low

  Fin_avoidedGHG_perM <- (avoided_tco2 * Project_share)/((allocated * exchange_rate)/1000000)
  
  load("r_Other_Code/GHG_to_DALY_func.RData")
  Fin_dalys_averted <- GHG_to_DALY_func(tGHG=(avoided_tco2 * Project_share))
  Fin_dalys_averted_perM <- Fin_dalys_averted/((allocated * exchange_rate)/1000000)
  
  
  #Output section
  #get real country name
  ISO_Countries <- readRDS("r_data/ISO_Countries.rds")
  Country_name <- as.character(ISO_Countries[ISO_Countries$ISO3 == Country_ISO, "Country"])
  

  

  
  output <-data.frame(Use_of_proceed= "Waste Management and Resource Efficiency",
                      Project_name = project_name,
                      Country = Country_name, 
                      Technology = "Waste-to-Energy",
                      Tonnes_treated = tonnage,
                      Generation_MWh = baseline_elec_kwh/1000,
                      Capacity_MW = mw,
                      signed = signed * exchange_rate,
                      allocated = allocated * exchange_rate,
                      Project_share = Project_share_reporting,
                      Fin_GHG_footprint = proj_tco2 * Project_share,
                      Fin_GHG_Avoidance = avoided_tco2 * Project_share,
                      Fin_avoidedGHG_perM = Fin_avoidedGHG_perM,
                      Fin_DALYs = Fin_dalys_averted,
                      Fin_DALYs_perM =  Fin_dalys_averted_perM,
                      Footprint_total = proj_tco2,
                      GHG_Avoidance_total = avoided_tco2,
                      Fin_GHG_Avoidance_min = avoided_tco2_low * Project_share,
                      Fin_GHG_Avoidance_max = avoided_tco2_high * Project_share,
                      GHG_Avoidance_total_min = avoided_tco2_low,
                      GHG_Avoidance_total_max = avoided_tco2_high,
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
                      capacity_factor,
                      Project_EF,
                      Grid_EF,
                      Project_EF_waste,
                      baseline_ef_waste
                      
  )
  output
  } 




#wm_tech_input(mwh=1000, mw,tonnage=1, Country_ISO="GBR", Project_share=0.5, project_name = "myproj", allocated = 1000, project_currency="USD", reporting_currency="SEK")
save(wm_tech_input, file = "r_Algorithms/wm_tech_input.RData")