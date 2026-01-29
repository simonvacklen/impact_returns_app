#default biome? average?



setwd("C:/Users/simva/OneDrive/Documents/Project Preston/")

#Country_ISO <- "SWE"
#ha = 1
#Project_share=0.6
#allocated =10000000
#project_name = "ForestPreservation"
#biome = "Coastal systems"
#project_currency = "SEK"
#reporting_currency = "SEK"

lu_tech_input <- function(ha,biome, Country_ISO, Project_share, signed, allocated, 
                          project_name, project_currency, reporting_currency)
{
  
  
  
  #currency conversion
  load("r_Other_Code/currency_convert.RData") 
  exchange_stuff <- currency_convert(currency = project_currency, outcurrency = reporting_currency)
  exchange_date <-   exchange_stuff[[2]]
  exchange_rate <-   as.numeric(exchange_stuff[[1]])
  exchange_rate_usd <- currency_convert(project_currency, "USD")[[1]]  
  exchange_rate_rep_usd <- currency_convert("USD", reporting_currency)[[1]]
  
  
  #convert from 2020 to 2024 PPP
  CPI <- readRDS("r_data/BD_CPI.rds")
  CPI_adj <- CPI[CPI$ISO3 == Country_ISO, "CPI_Adjustment"]
  
  

  bd_data <- readRDS("r_data/BD_ES.rds")
  ecosystem_value_PPP <- as.numeric(bd_data[bd_data$Biome == biome, c("Esval_Int2020_ha_year")]) * CPI_adj
#  ecosystem_value_PPP_min <- ecosystem_value_PPP * 0.75
#  ecosystem_value_PPP_max <- ecosystem_value_PPP * 1.25
  
    #max min stuff is a placeholder
  es_footprint_PPP <- ecosystem_value_PPP * ha
#  es_footprint_PPP_min <- ecosystem_value_PPP_min * ha
#  es_footprint_PPP_max <- ecosystem_value_PPP_max * ha
    

  BD_degr <- readRDS("r_data/BD_degr.rds")
  degradation_rate <- as.numeric(BD_degr[BD_degr$Biome == biome, c("Value")])
  
  additional_annual_es_PPP <- es_footprint_PPP * degradation_rate


  #add ghg   
    #maybe split by restoration and preservation?
  
  
  #convert to LYs - no need to involve project share in financed, because its based on the allocated
  load("r_Other_Code/preston_function.RData")
  Fin_dalys_averted <- preston_function(ISO3 = Country_ISO, amount_PPP = additional_annual_es_PPP)
  Fin_dalys_averted_perM <- Fin_dalys_averted/(allocated/1000000 * exchange_rate)
  
  
  #get real country name
  ISO_Countries <- readRDS("r_data/ISO_Countries.rds")
  Country_name <- as.character(ISO_Countries[ISO_Countries$ISO3 == Country_ISO, "Country"])
  

  # Remove all variables for safety and clarity
  output <-data.frame(Use_of_proceed= "Living Natural Resources and Land Use",
                      Project_name = project_name,
                      Country = Country_name, 
                      Preserved_ha = ha,
                      biome = biome,
                      biome_value = ecosystem_value_PPP * exchange_rate_rep_usd,
                      signed = signed * exchange_rate,
                      allocated = allocated * exchange_rate,
                      Project_share = Project_share,
                      Fin_ES_footprint = es_footprint_PPP * exchange_rate_rep_usd * Project_share,
                      Fin_add_ES = additional_annual_es_PPP * exchange_rate_rep_usd * Project_share,
                      Fin_add_ES_perCUR = (additional_annual_es_PPP * exchange_rate_rep_usd * Project_share)/(allocated * exchange_rate),
                      Fin_DALYs = Fin_dalys_averted,
                      Fin_DALYs_perM =  Fin_dalys_averted_perM,
                      add_ES = es_footprint_PPP * exchange_rate_rep_usd,
                      exchange_date = exchange_date,
                      exchange_rate = exchange_rate,
                      exchange_rate_usd = exchange_rate_usd,
                      project_currency = project_currency,
                      reporting_currency = reporting_currency,
                      Category = c("Biodiversity"),
                      allocated_org = allocated,
                      allocated_usd = allocated * exchange_rate_usd,
                      Country_ISO = Country_ISO
                      #assumption additional
                      ,degradation_rate
                      ,ecosystem_value_PPP
                      
  )
  output
  
  
  #input help info
  #"Marine"                                
  #"Coastal Systems"                        
  #"Inland Wetlands"
  #"Tropical and subtropical forests"   -ghg   
  #"Temperate forest and woodland" -ghg
  
} 

#lu_tech_input(ha = 1000, biome="Marine", Country_ISO="TUR", Project_share = 0.5, allocated=1000000, project_name="Forestsavers", project_currency="SEK", reporting_currency="GBP")

save(lu_tech_input, file = "r_Algorithms/lu_tech_input.RData")
