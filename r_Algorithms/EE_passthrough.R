
#project_name = "the project"
#Country_ISO <- "SWE"
#Technology <- "LEDs"
#proj_elec_MWh_saved <- 591000
#tghg_avoided <- NA
#allocated = 1000000
#Project_share = 0.8
#project_currency = "SEK"
#reporting_currency = "USD"

ee_passthrough <- function(proj_elec_MWh_saved, proj_tghg_footprint, proj_tghg_avoided, Country_ISO, Technology, 
                          Project_share, signed, allocated, project_name, 
                          project_currency, reporting_currency)
{


  #currency conversion
  load("r_Other_Code/currency_convert.RData") 
  exchange_stuff <- currency_convert(currency = project_currency, outcurrency = reporting_currency)
  exchange_date <-   exchange_stuff[[2]]
  exchange_rate <-   as.numeric(exchange_stuff[[1]])
  exchange_rate_usd <- currency_convert(project_currency, "USD")[[1]]  

  
  ### If electricity savings provided and not avoidance, its calculated
  if (!is.na(proj_elec_MWh_saved) && is.na(proj_tghg_avoided)) {
  
###Getting baseline grid emission factor
Grid_EFs <- readRDS("r_data/Grid_EFs.rds")
Grid_EF_row <- Grid_EFs[Grid_EFs$ISO3 == Country_ISO, ]
Grid_EF <- as.numeric(Grid_EF_row[Grid_EF_row$Type == "Firm", "EF"])
#Grid_min_pct <- as.numeric(Grid_EF_row[Grid_EF_row$Type == firm_int, "Min_PCT"])
#Grid_max_pct <- as.numeric(Grid_EF_row[Grid_EF_row$Type == firm_int, "Max_PCT"])

kWh = proj_elec_MWh_saved*1000
#avoidance calculation
proj_tghg_avoided <- (Grid_EF*kWh)/1000000
  } else {
    
    Grid_EF <- NA
    
  }

Fin_avoidedGHG_perM <- (proj_tghg_avoided * Project_share)/((allocated * exchange_rate)/1000000)


load("r_Other_Code/GHG_to_DALY_func.RData")
Fin_dalys_averted <- GHG_to_DALY_func(tGHG=(proj_tghg_avoided * Project_share))
Fin_dalys_averted_perM <- Fin_dalys_averted/((allocated * exchange_rate)/1000000)

#Output section
#get real country name
ISO_Countries <- readRDS("r_data/ISO_Countries.rds")
Country_name <- as.character(ISO_Countries[ISO_Countries$ISO3 == Country_ISO, "Country"])



output <-data.frame(Use_of_proceed= "Energy Efficiency",
                    Project_name = project_name,
                    Country = Country_name, 
                    Technology = Technology,
                    proj_elec_MWh_saved = proj_elec_MWh_saved,  
                    signed = signed * exchange_rate,
                    allocated = allocated * exchange_rate,
                    Project_share = Project_share,
                    Fin_GHG_footprint = proj_tghg_footprint * Project_share,
                    Fin_GHG_Avoidance = proj_tghg_avoided * Project_share,
                    Fin_avoidedGHG_perM = Fin_avoidedGHG_perM,
                    Fin_DALYs = Fin_dalys_averted,
                    Fin_DALYs_perM =  Fin_dalys_averted_perM,
                    Footprint_total = proj_tghg_footprint,
                    GHG_Avoidance_total = proj_tghg_avoided,
                    exchange_date = exchange_date,
                    exchange_rate = exchange_rate,
                    project_currency = project_currency,
                    reporting_currency = reporting_currency,
                    Category = c("Green"),
                    allocated_org = allocated,
                    allocated_usd = allocated * exchange_rate_usd,
                    Country_ISO = Country_ISO
                    #assumption table additions
                    ,Grid_EF
                    
                    )
output
}

#ee_passthrough(project_name = "MyProj", proj_elec_MWh_saved = 1000000,
#               allocated = 10000000,
#               proj_tghg_footprint=1000, proj_tghg_avoided = 10000,
#              Country_ISO="AFG", Project_share = 0.8, Technology="Onshore Wind", 
#              project_currency="USD", reporting_currency="USD")

save(ee_passthrough, file = "r_Algorithms/ee_passthrough.RData")
