#RE Tech -> GHG Algorithm
#project_name = "the project"
#Country_ISO <- "BEL"
#Technology <- "Offshore Wind"
#MWh <- 591000
#allocated = 1000000
#Project_share = 0.8
#project_currency = "SEK"
#reporting_currency = "USD"
re_tech_input <- function(MWh, MW, Country_ISO, Technology, Project_share, 
                          signed, allocated, project_name, project_currency, reporting_currency)
{


  #currency conversion
  load("r_Other_Code/currency_convert.RData") 
  exchange_stuff <- currency_convert(currency = project_currency, outcurrency = reporting_currency)
  exchange_date <-   exchange_stuff[[2]]
  exchange_rate <-   as.numeric(exchange_stuff[[1]])
  exchange_rate_usd <- currency_convert(project_currency, "USD")[[1]]  

  
  #if they didnt supply project share- whatever MW/MWH they provided is useless - so we use CostDB-generated 
  #MW instead of data intake MW
  if (is.na(Project_share)) {
    MW <- NA
    MWh <- NA
  }
  
  
    
  
  ## If MW is missing but generation is not, we estimate it based on MW
  if (is.na(MW) & !is.na(MWh)) {
    load("r_Algorithms/re_capacity_generation_conv.RData")
    MW <- capacity_generation_conv(iso3=Country_ISO, 
                                   technology=Technology, 
                                   mw=MW,
                                   mwh=MWh)
  }
  
  
  #if we use costdb, project share needs to be 1 for calculations, but they might still have 
  #provided the number and we can report this
  Project_share_reporting <- Project_share

    #if MW is missing and there is no MWh either, we estimate based on costdb
  
  unit_cost <- NA
  capacity_factor <- NA
  if (is.na(MW)) {
    # Look up replacement value from costdb
    cost_db <- readRDS("r_data/cost_db.rds")
    unit_cost <- cost_db$unit_cost[cost_db$Use_of_proceed == "Renewable Energy" 
                                   & cost_db$ISO3 == Country_ISO 
                                   & cost_db$Technology == Technology] 
    MW <- allocated/((unit_cost*1000)/exchange_rate_usd)
    Project_share <- 1
    capacity_factors_ds <- readRDS("r_data/RE_Capacity_Factors.rds")
    capacity_factor <- capacity_factors_ds[capacity_factors_ds$ISO3 == Country_ISO  
                                           & capacity_factors_ds$Technology == Technology, ]$capacity_factor
  } 

  #if MW is generated from CostDB, or its provided, this is where we get MWh,
  if (is.na(MWh)) {
    load("r_Algorithms/re_capacity_generation_conv.RData")
    MWh <- capacity_generation_conv(iso3=Country_ISO, 
                                    technology=Technology, 
                                    mw=MW,
                                    mwh=MWh)
    
    capacity_factors_ds <- readRDS("r_data/RE_Capacity_Factors.rds")
    capacity_factor <- capacity_factors_ds[capacity_factors_ds$ISO3 == Country_ISO  
                                           & capacity_factors_ds$Technology == Technology, ]$capacity_factor

  }
  
  
  
  
  
  
###Getting baseline grid emission factor
Grid_EFs <- readRDS("r_data/Grid_EFs.rds")
tech_cat_data <- readRDS("r_data/tech_cat.rds")

#Get category of the input tech
firm_int <- as.character(tech_cat_data[tech_cat_data$Technology == Technology, 2])

#Get grid emission factor
Grid_EF_row <- Grid_EFs[Grid_EFs$ISO3 == Country_ISO, ]
Grid_EF <- as.numeric(Grid_EF_row[Grid_EF_row$Type == firm_int, "EF"])
Grid_min_pct <- as.numeric(Grid_EF_row[Grid_EF_row$Type == firm_int, "Min_PCT"])
Grid_max_pct <- as.numeric(Grid_EF_row[Grid_EF_row$Type == firm_int, "Max_PCT"])


##getting project EF
re_project_ef_data <-readRDS("r_data/RE_proj_EFs.rds")
Project_EF <- as.numeric(re_project_ef_data[re_project_ef_data$Technology == Technology, 2])

kWh = MWh*1000
#avoidance calculation
footprint_tonnes <- (Project_EF*kWh)/1000000
baseline_footprint_tonnes <- (Grid_EF*kWh)/1000000

avoidance_tonnes <- baseline_footprint_tonnes - footprint_tonnes
avoidance_tonnes_max <- ((baseline_footprint_tonnes*Grid_max_pct) - footprint_tonnes)
avoidance_tonnes_min <- ((baseline_footprint_tonnes*Grid_min_pct) - footprint_tonnes)

Fin_avoidedGHG_perM <- (avoidance_tonnes * Project_share)/((allocated * exchange_rate)/1000000)


load("r_Other_Code/GHG_to_DALY_func.RData")
Fin_dalys_averted <- GHG_to_DALY_func(tGHG=(avoidance_tonnes * Project_share))
Fin_dalys_averted_perM <- Fin_dalys_averted/((allocated * exchange_rate)/1000000)

#Output section
#get real country name
ISO_Countries <- readRDS("r_data/ISO_Countries.rds")

Country_name <- as.character(ISO_Countries[ISO_Countries$ISO3 == Country_ISO, "Country"])



output <-data.frame(Use_of_proceed= "Renewable Energy",
                    Project_name = project_name,
                    Country = Country_name, 
                    Technology = Technology,
                    Generation_MWh = MWh,
                    Capacity_MW = MW,
                    signed = signed * exchange_rate,
                    allocated = allocated * exchange_rate,
                    Project_share = Project_share_reporting,
                    Fin_GHG_footprint = footprint_tonnes * Project_share,
                    Fin_GHG_Avoidance = avoidance_tonnes * Project_share,
                    Fin_avoidedGHG_perM = Fin_avoidedGHG_perM,
                    Fin_DALYs = Fin_dalys_averted,
                    Fin_DALYs_perM =  Fin_dalys_averted_perM,
                    Fin_GHG_Avoidance_min = avoidance_tonnes_min * Project_share,
                    Fin_GHG_Avoidance_max = avoidance_tonnes_max * Project_share,
                    Footprint_total = footprint_tonnes,
                    GHG_Avoidance_total = avoidance_tonnes,
                    GHG_Avoidance_total_min = avoidance_tonnes_min,
                    GHG_Avoidance_total_max = avoidance_tonnes_max,
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
                    firm_int,
                    unit_cost,
                    capacity_factor,
                    Project_EF,
                    Grid_EF
                    
                    )
output
}

#re_tech_input(project_name = "MyProj", allocated = 1000000, MWh=1000, Country_ISO="AFG", Project_share = 0.8, Technology="Onshore Wind", project_currency="SEK", reporting_currency="GBP")
save(re_tech_input, file = "r_Algorithms/re_tech_input.RData")
