#GB Tech -> GHG Algorithm
setwd("C:/Users/simva/OneDrive/Documents/Project Preston/")

#Country_ISO <- "AUT"
#Type <- "Office"
#M2 <- 21277
#Cert = "LEED Gold"
#Cert = NULL
#project_currency = "EUR"
#reporting_currency = "EUR"
#Project_share = 0.94
#allocated = 87468309

gb_tech_input <- function(M2, Country_ISO, Type, Cert, epc, Project_share, 
                          signed, allocated, project_name, project_currency, reporting_currency,
                          water_int=NA)
{
  
  #currency conversion
  load("r_Other_Code/currency_convert.RData") 
  exchange_stuff <- currency_convert(currency = project_currency, outcurrency = reporting_currency)
  exchange_date <-   exchange_stuff[[2]]
  exchange_rate <-   as.numeric(exchange_stuff[[1]])
  exchange_rate_usd <- currency_convert(project_currency, "USD")[[1]]  
  
  #if we use costdb, project share needs to be 1, but they might still have provided the number
  Project_share_reporting <- Project_share
  
  #if unknown type, this is the default
  if (is.na(Type)) { Type <- "Mixed Residential/Office" }
 
  
  ## translating names in data intake to names in report
  type_rep <- Type
  if(Type == "Distribution Warehouse (Cold)") {Type <- "Distribution Warehouse Cold"}
  if(Type == "Distribution Warehouse (Warm)") {Type <- "Distribution Warehouse Warm"}
  
   
  # Look up replacement value from costdb
  unit_cost <- NA
  if (is.na(M2)) {
    cost_db <- readRDS("r_data/cost_db.rds")
    unit_cost <- cost_db$unit_cost[cost_db$Use_of_proceed == "Green Buildings"
                                   & cost_db$ISO3 == Country_ISO 
                                   & cost_db$Technology == Type]
    M2 <- (allocated*exchange_rate_usd)/unit_cost
    Project_share <- 1
  } 
  
  
  

###Getting baseline EUI
BL_EUI_data <- readRDS("r_data/gb_bl_eui.rds")
BL_EUI_row <- BL_EUI_data[BL_EUI_data$ISO3 == Country_ISO, ]
BL_EUI <- as.numeric(BL_EUI_row[BL_EUI_row$BuildingType_Name == Type, c("EUI")])
BL_EUI_min <- as.numeric(BL_EUI_row[BL_EUI_row$BuildingType_Name == Type, c("Low_margin")]) * BL_EUI
BL_EUI_max <- as.numeric(BL_EUI_row[BL_EUI_row$BuildingType_Name == Type, c("High_margin")]) *BL_EUI


PR_EUI_data <- readRDS("r_data/gb_proj_diff.rds")

#prefer EPC if it exists - but will check later if it exists for that country
if (!is.na(epc)) {
  epc_name <- paste0("EPC ",epc)
  PR_EUI <- PR_EUI_data[PR_EUI_data$ISO3==Country_ISO,]
  PR_EUI <- PR_EUI[PR_EUI$BuildingType_Name==Type,] 
  PR_EUI <- PR_EUI[PR_EUI$Certification==epc_name,] 
} else { PR_EUI<-data.frame() }

### in case EPC is added, but doesnt exist for the country
if (nrow(PR_EUI)==0 && is.na(Cert)) {
  Cert <- "Other"
  PR_EUI <- PR_EUI_data[PR_EUI_data$Certification==Cert,]
}


#if no epc, but cert iis provided
if  (!is.na(Cert) && is.na(epc)) {
  PR_EUI <- PR_EUI_data[PR_EUI_data$Certification==Cert,]
}


#if neither is provided
if (is.na(Cert) && is.na(epc)) {
  Cert <- "Other"
  PR_EUI <- PR_EUI_data[PR_EUI_data$Certification==Cert,]
}





Diff_from_avg <- PR_EUI[, c("Diff_from_avg")]
proj_min_pct <- PR_EUI[, c("Min")]
proj_max_pct <- PR_EUI[, c("Max")]

Proj_EUI <- as.numeric(BL_EUI * Diff_from_avg)
Proj_EUI_min <- as.numeric(BL_EUI * proj_min_pct)
Proj_EUI_max <- as.numeric(BL_EUI * proj_max_pct)

#getting building EF
GB_EF <- readRDS("r_data/building_EF.rds")
GB_EF_row <- GB_EF[GB_EF$ISO3==Country_ISO,]
GB_EF <-     as.numeric(GB_EF_row[, c("Building_EF")])
GB_EF_min <- as.numeric(GB_EF_row[,c("Min_EF_pct")]) * GB_EF
GB_EF_max <- as.numeric(GB_EF_row[,c("Max_EF_pct")]) * GB_EF


#enegy savings calculation
proj_energy <- M2 * Proj_EUI
proj_energy_min <- M2 * Proj_EUI_min
proj_energy_max <- M2 * Proj_EUI_max

baseline_energy <- M2*BL_EUI
baseline_energy_min <- M2*BL_EUI_min
baseline_energy_max <- M2*BL_EUI_max

energy_avoided <- baseline_energy - proj_energy
energy_avoided_min <- baseline_energy_min - proj_energy_max
energy_avoided_max <- baseline_energy_max - proj_energy_min


#avoidance calculation
#GB EF using residential for all - diff captured by EUI
#no proper error bars for ef either - stems from power mix and 
proj_footprint <- proj_energy * GB_EF
proj_footprint_min <- proj_energy_min * GB_EF_min
proj_footprint_max <- proj_energy_max * GB_EF_max

baseline_footprint <- baseline_energy * GB_EF
baseline_footprint_min <- baseline_energy_min * GB_EF_min
baseline_footprint_max <- baseline_energy_max * GB_EF_max


co2avoided <- baseline_footprint - proj_footprint
co2avoided_min <- baseline_footprint_min - proj_footprint_max
co2avoided_max <- baseline_footprint_max - proj_footprint_min

Fin_avoidedGHG_perM <- (co2avoided * Project_share)/((allocated * exchange_rate)/1000000)

load("r_Other_Code/GHG_to_DALY_func.RData")
Fin_dalys_averted <- GHG_to_DALY_func(tGHG=(co2avoided * Project_share))
Fin_dalys_averted_perM <- Fin_dalys_averted/((allocated * exchange_rate)/1000000)


#Output section
#get real country name
ISO_Countries <- readRDS("r_data/ISO_Countries.rds")
Country_name <- as.character(ISO_Countries[ISO_Countries$ISO3 == Country_ISO, "Country"])


# Remove all variables for safety and clarity

  output <-data.frame(Use_of_proceed= "Green Buildings",
                      Project_name = project_name,
                      Country = Country_name, 
                      BuildingType = type_rep,
                      M2 = M2,
                      Proj_EUI = Proj_EUI,
                      water_int = water_int,
                      Cert = Cert,
                      epc = epc,
                      signed = signed * exchange_rate,
                      allocated = allocated * exchange_rate,
                      Project_share = Project_share_reporting,
                      Fin_GHG_footprint = proj_footprint * Project_share,
                      Fin_GHG_Avoidance = co2avoided * Project_share,
                      Fin_avoidedGHG_perM = Fin_avoidedGHG_perM,
                      Fin_DALYs = Fin_dalys_averted,
                      Fin_DALYs_perM =  Fin_dalys_averted_perM,
                      Footprint_total = proj_footprint,
                      GHG_Avoidance_total = co2avoided,
                      Fin_GHG_Avoidance_min = co2avoided_min * Project_share,
                      Fin_GHG_Avoidance_max = co2avoided_max * Project_share,
                      GHG_Avoidance_total_min = co2avoided_min,
                      GHG_Avoidance_total_max = co2avoided_max,
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
                      BL_EUI,
                      GB_EF
                      
  )
output

#input help info
#"RSF" - "Residential Single Family"
#"OFF" - "Office"   
#"RMF" - "Residential Multi Family" 
#"DWC" -  "Distribution Warehouse Cold"
#"DWW" - "Distribution Warehouse Warm"
#"HEC" - "Healthcare"    
#"HOT" - "Hotel"
#"LEI" - "Leisure/Lodging"    
#"RHS" - "Retail High Street" 
#"RSM" -  "Shopping Center"  
#"RWB" - "Retail Warehouse"  

} 

#gb_tech_input(1000, "GBR", "OFF", "LEED Gold", Project_share=0.5, allocated=1000, project_name="muuuh", project_currency="SEK", reporting_currency="GBP")

save(gb_tech_input, file = "r_Algorithms/gb_tech_input.RData")
