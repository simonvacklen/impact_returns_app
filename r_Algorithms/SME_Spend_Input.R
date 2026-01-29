#SME Spend -> GHG Algorithm

#add min max stuff

#Country_ISO <- "AFG"
#Industry <- "Agriculture, hunting, forestry" 
#allocated <- 1000000
#Target_group= "Women"
#bl_pct_override = NA
#project_currency="SEK"
#reporting_currency="GBP"
#Project_share = 1


## could add a discoutn % for some developing countries and whatnot..

#consider including currency conversion - but this would require some kind of exchange rate API
sme_spend_input <- function(project_name, signed, allocated, Country_ISO, Project_share, 
                            Industry, Target_group, 
                            bl_pct_override, project_currency, 
                            reporting_currency, no_of_loans=NA)
  

{
  #setwd("C:/Users/simva/OneDrive/Documents/Project Preston/")

  #currency conversion
  load("r_Other_Code/currency_convert.RData") 
  exchange_stuff <- currency_convert(currency = project_currency, outcurrency = reporting_currency)
  exchange_date <-   exchange_stuff[[2]]
  exchange_rate <-   as.numeric(exchange_stuff[[1]])
  exchange_rate_proj_usd <- currency_convert("USD", project_currency)[[1]]
  exchange_rate_rep_usd <- currency_convert("USD", reporting_currency)[[1]]
  
  
  
  
  
  #if we use costdb, project share needs to be 1, but they might still have provided the number
  Project_share_reporting <- Project_share
  if (is.na(Project_share)) {Project_share <- 1 }  
  
  
  
  ##T2 Multipliers
  SME_IO <- readRDS("r_data/SME_IO.rds")
  multiplier <- as.numeric(SME_IO[SME_IO$ISO3 == Country_ISO, Industry])
#  multiplier_low <- multiplier * as.numeric(SME_IO[SME_IO$ISO3 == Country_ISO, c("Error_low")])
#  multiplier_high <- multiplier * as.numeric(SME_IO[SME_IO$ISO3 == Country_ISO, c("Error_high")])


  ## Assett-turnover ratios
  SME_ATR <- readRDS("r_data/SME_ATR.rds")
  atr <- as.numeric(SME_ATR[SME_ATR$Industry == Industry, c("AT_Ratio")])
#  atr_low <- atr * as.numeric(SME_ATR[SME_ATR$Industry == Industry, c("AT_Ratio_Low")])
#  atr_high <- atr * as.numeric(SME_ATR[SME_ATR$Industry == Industry, c("AT_Ratio_High")])
  
  #Financial Inclusion Proxy
  SME_FinIncl<- readRDS("r_data/SME_FinIncl.rds")
  Fin_Incl <- SME_FinIncl[SME_FinIncl$Country_ISO == Country_ISO,]
  #add error margins here?
  #https://www.worldbank.org/en/topic/smefinance
  #maybe do share of SMEs + share of financed companies that are SMES (WB data..)

#  Target_group = "Women"
  
  ##COnverting real name to data name
  target_group_cats <- data.frame(
    Category = c("Total", "FEMALE", "MALE", "RURAL", "URBAN", "Primary_Only", "Poorest40pct"),
    Description = c("General", "Women", "Men", "Rural", "Urban", "Primary School only", "Poorest 40%")
  )

  if (is.na(Target_group)){
    Target_group= "General"
  } 


  Fin_Incl_data <- SME_FinIncl[SME_FinIncl$Country_ISO == Country_ISO,]
  Target_group_dname <- target_group_cats[target_group_cats$Description==Target_group, "Category"]
  Fin_Incl <- as.numeric(Fin_Incl_data[Fin_Incl_data$Sub_Cat == Target_group_dname, c("Banked_share")])

  if (!is.na(bl_pct_override)){
    Fin_Incl <- bl_pct_override
  } 

  #Convert to GDP PPP
  currencylist <- readRDS("r_data/currency_list.rds")
  LCU <- currencylist[currencylist$ISO3 == Country_ISO, ]$CurrencyCode
  lcu_exchangerate <- as.numeric(currency_convert(currency = project_currency, outcurrency = LCU)[1])
  
  #then convert it to int$ - for preston function
  PPP_data <- readRDS("r_data/PPP_data.rds")
  ppp_converter <- as.numeric(PPP_data[PPP_data$ISO3 == Country_ISO, ]$PPP_factor)

  
    
  project_GDP <- allocated * atr * multiplier
 # project_GDP_min <- allocated * atr_low * multiplier_low
#  project_GDP_max <- allocated * atr_high * multiplier_high
  project_GDP_ppp <- (project_GDP * lcu_exchangerate)/ ppp_converter
  
  
  
  baseline_GDP <- project_GDP * Fin_Incl
#  baseline_GDP_min <- project_GDP_min * Fin_Incl
#  baseline_GDP_max <- project_GDP_max * Fin_Incl
  baseline_GDP_ppp <- (baseline_GDP * lcu_exchangerate)/ ppp_converter
  
  
  additional_gdp_ppp <- project_GDP_ppp - baseline_GDP_ppp
#  additional_gdp_min <- pmax(project_GDP_min- baseline_GDP_max, 0.1*project_GDP_min)
#  additional_gdp_max <- project_GDP_max - baseline_GDP_min

  

  
  #convert to LYs - no need to involve project share in financed, because its based on the allocated
  load("r_Other_Code/preston_function.RData")
  Fin_dalys_averted <- preston_function(ISO3 = Country_ISO, amount_PPP = additional_gdp_ppp)
  Fin_dalys_averted_perM <- Fin_dalys_averted/((allocated  * exchange_rate)/1000000)
  
  
  
  
  #Output section
  #get real country name
  ISO_Countries <- readRDS("r_data/ISO_Countries.rds")
  Country_name <- as.character(ISO_Countries[ISO_Countries$ISO3 == Country_ISO, "Country"])
  
  
  output <- data.frame(Use_of_proceed= "Access to Essential Services: Financial Services",
                      Project_name = project_name,
                      project_type = "SME Finance",
                      Country = Country_name, 
                      Industry = Industry,
                      Target_group = Target_group,
                      no_of_loans = no_of_loans,
                      signed = signed * exchange_rate,
                      allocated = allocated * exchange_rate,
                      Project_share = Project_share_reporting,
                      Fin_Supported_GDP = project_GDP_ppp * exchange_rate_rep_usd,
                      Fin_Additional_GDP = additional_gdp_ppp * exchange_rate_rep_usd,
                      Fin_Additional_GDP_perCUR = (additional_gdp_ppp * exchange_rate_rep_usd)/(allocated * exchange_rate),
                      Fin_DALYs = Fin_dalys_averted,
                      Fin_DALYs_perM =  Fin_dalys_averted_perM,
#                      Fin_Additional_GDP_min = additional_gdp_min,
#                      Fin_Additional_GDP_max = additional_gdp_max,
                      exchange_date = exchange_date,
                      exchange_rate = exchange_rate,
                      project_currency = project_currency,
                      reporting_currency = reporting_currency,
                      exchange_rate_proj_usd = exchange_rate_proj_usd,
                      Category = c("Social"),
                      allocated_orgcur = allocated,
                      allocated_usd = allocated * exchange_rate_proj_usd,
                      Country_ISO = Country_ISO,
                      lcu_exchangerate
                      #assumption table additions
                      ,
                      multiplier,
                      atr,
                      Fin_Incl
                      
  )
  

  output
}


#sme_spend_input(project_name= "SME Loan 2", allocated=1000000,signed = 2000000, Project_share=0.5, Country_ISO = "AFG", Industry= "Agriculture, hunting, forestry" , Target_group="Women", bl_pct_override=NA, project_currency="SEK", reporting_currency="GBP", no_of_loans=1000)


save(sme_spend_input, file = "r_Algorithms/sme_spend_input.RData")