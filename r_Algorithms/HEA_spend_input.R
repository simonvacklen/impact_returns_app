#HEA Spend -> GHG Algorithm

#Country_ISO <- "GBR"
#project_currency <- "SEK" 
#allocated <- 1000
#reporting_currency = "USD"


hea_spend_input <- function(project_name, signed, allocated, Country_ISO,Project_share, 
                            project_currency, reporting_currency, tenor, target_group="NA", 
                            no_of_patients=NA)
  {
  
  #currency conversion
  load("r_Other_Code/currency_convert.RData") 
  exchange_stuff <- currency_convert(currency = project_currency, outcurrency = reporting_currency)
  exchange_date <-   exchange_stuff[[2]]
  exchange_rate <-   as.numeric(exchange_stuff[[1]])
  exchange_rate_usd <- currency_convert(project_currency, "USD")[[1]]  
  
  usd_amount <- allocated * exchange_rate_usd

  
  #daly ratio is like one-off "purchase", so to get yearly we need to divide it, 
  #if its unknown or a credit facility we 
  #make an assumption of 5 years, its a good horizon, beyond that we can assume 
  #that underlying data has changed
 
  ##input is months, but need years to annualise impact
   if (is.na(tenor)) {tenor <- 5 *12}
  if (tenor>=5 * 12) {tenor <- 5 *12}
  #just for output reporting
  tenor_rep <- tenor
  #from here on, tenor is in years
  tenor <- tenor/12

  
  
  
  ##cost per daly
  health_data <- readRDS("r_data/HEA.rds")
  costperdaly <- health_data[health_data$ISO3 == Country_ISO, c("CostPerDaly")]
  costperdaly_low <- health_data[health_data$ISO3 == Country_ISO, c("MINCostPerDaly")]
  costperdaly_high <- health_data[health_data$ISO3 == Country_ISO, c("MAXCostPerDaly")]
  
  
  dalys <- as.numeric(usd_amount/costperdaly)/tenor
  dalys_high <- as.numeric(usd_amount/costperdaly_low)/tenor
  dalys_low <- as.numeric(usd_amount/costperdaly_high)/tenor
  
  Fin_dalys_averted_perM <- dalys/((allocated * exchange_rate)/1000000)
  
  #Output section
  #get real country name
  ISO_Countries <- readRDS("r_data/ISO_Countries.rds")
  Country_name <- as.character(ISO_Countries[ISO_Countries$ISO3 == Country_ISO, "Country"])
  
  
  output <-data.frame(Use_of_proceed= "Access to Essential Services: Healthcare",
                      Project_name = project_name,
                      Country = Country_name, 
                      target_group = target_group, 
                      no_of_patients = no_of_patients,
                      signed = signed * exchange_rate,
                      allocated = allocated * exchange_rate,
                      Project_share = Project_share,
                      Fin_DALYs = dalys,
                      Fin_dalys_averted_perM,
                      Fin_DALYs_low = dalys_low,
                      Fin_DALYs_high = dalys_high,
                      DALYs = dalys/Project_share,
                      DALYs_low = dalys_low/Project_share,
                      DALYs_high = dalys_high/Project_share,
                      exchange_date = exchange_date,
                      exchange_rate_to_usd = exchange_rate_usd,
                      exchange_rate = exchange_rate,
                      project_currency = project_currency,
                      reporting_currency = reporting_currency,
                      Category = c("Social"),
                      allocated_usd = allocated * exchange_rate_usd,
                      allocated_orgcur = allocated,
                      Country_ISO = Country_ISO
                      
                      ### assumption table
                      ,costperdaly,
                      tenor = tenor_rep
                      
  )
  

  output
}

#hea_spend_input(project_name="Heartspital", allocated=1000000, Country_ISO="CAF",Project_share=0.6 , project_currency="USD", reporting_currency="USD", tenor=3)

save(hea_spend_input, file = "r_Algorithms/hea_spend_input.RData")