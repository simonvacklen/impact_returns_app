
#AH -> Monetising Algorithm

#add min max stuff
#even though filename is tech_input this is actually spend input
#note in function inputs that some of the inputs will be included as "hidden", which means they 
#might not have an data intake counterpart, they always need to have a default input here
#if affordable rent is specified, its in the same currency as the project

#outline:
#median income * 80% = eligible income (USA only i suppose dodgy source TBH), some UK too, point is, its one of the
#highest so its a conservative estimate
#50% vs 30% is project/baseline
#30% affordability  we have solid resources - 50% is w considered severely costburdened, 40% in the EU -  we use the
#lowest according to conservative principle



#setwd("C:/Users/simva/OneDrive/Documents/Project Preston/")
#Country_ISO <- "SWE"
#signed = 2000000
#no_of_homes <- 10
#allocated <- 1000000
#affordable_rent_monthly = 800
#project_currency="SEK"
#reporting_currency="GBP"
#Project_share = 0.5
#housing_type = NA

#these are "hidden inputs" - can put in data intake
#eligibility_crit = 0.8
#affordability_crit = 0.3
#cost_burdened_def = 0.5
#m2_per_house = 100

ah_spend_input <- function(project_name, signed, 
                           allocated, housing_type, 
                           Country_ISO, Project_share, no_of_homes,
                           target_group="NA",
                           project_currency, reporting_currency,
                           #hidden, with default values in function
                           eligibility_crit, 
                           affordability_crit,
                           affordable_rent_monthly, 
                           cost_burdened_def, 
                           m2_per_house)

{

  #defaults - see notes for methodology above
  if(is.na(eligibility_crit)) {eligibility_crit = 0.8}
  if(is.na(affordability_crit)) {affordability_crit = 0.3}
  if(is.na(cost_burdened_def)) {cost_burdened_def = 0.4}
  #the 120 is from EDGE, medium flat
  if(is.na(m2_per_house)) {m2_per_house = 120}
  
  
  #currency conversion
  load("r_Other_Code/currency_convert.RData") 
  exchange_stuff <- currency_convert(currency = project_currency, outcurrency = reporting_currency)
  exchange_date <-   exchange_stuff[[2]]
  exchange_rate <-   as.numeric(exchange_stuff[[1]])
  exchange_rate_usd <- currency_convert(project_currency, "USD")[[1]]  
  exchange_rate_rep_usd <- currency_convert("USD", reporting_currency)[[1]]

  
  
  
  
  
  #if unknown type, this is the default
  if (is.na(housing_type)) { housing_type <- "Mixed Residential" }
  
  #if we use costdb, project share needs to be 1, but they might still have provided the number
  Project_share_reporting <- Project_share
  # Look up replacement value from costdb

  unit_cost <- NA
    if (is.na(no_of_homes)) {
    cost_db <- readRDS("r_data/cost_db.rds")
    unit_cost <- cost_db$unit_cost[cost_db$Use_of_proceed == "Affordable Housing"
                                   & cost_db$ISO3 == Country_ISO 
                                   & cost_db$Technology == housing_type]
    no_of_homes <-  (allocated*exchange_rate_usd)/ (unit_cost*m2_per_house)
    Project_share <- 1
    }
  
  
  
  
  
  
  
 #Median Income 
  household_income <- readRDS("r_data/household_income.rds")
  household_inc_2013_ppp <- household_income[household_income$ISO3==Country_ISO, ]$Median_household_income
  
  AH_CPI <- readRDS("r_data/AH_CPI.rds")
  CPI_adj <- AH_CPI[AH_CPI$ISO3==Country_ISO, ]$CPI_Adjustment

  
  #all of these calcs are done in dataset currency, int$
  household_inc_2023_ppp <- household_inc_2013_ppp * CPI_adj

  #Eligeble income
  elig_income_PPP <- household_inc_2023_ppp * eligibility_crit
  
  #baseline rent
  baseline_rent_PPP <- elig_income_PPP * cost_burdened_def
  

  
    #affordable project rent - if they provide its project currency
    if (is.na(affordable_rent_monthly))
    {
      affordable_rent_PPP <- elig_income_PPP * affordability_crit
    } else {
      PPP_data <- readRDS("r_data/PPP_data.rds")
      PPP_conv <- PPP_data[PPP_data$ISO3==Country_ISO, ]$PPP_factor
      
      currencylist <- readRDS("r_data/currency_list.rds")
      lcu <- currencylist[currencylist$ISO3 == Country_ISO, ]$CurrencyCode
      exchange_rate_lcu_repcur <- currency_convert(currency = project_currency, outcurrency = lcu)[[1]] 
      affordable_rent_PPP <- ((affordable_rent_monthly * exchange_rate_lcu_repcur)/PPP_conv) * 12      
    }

CSP_individual_PPP <-   baseline_rent_PPP - affordable_rent_PPP


#making 
Fin_CSP_total_PPP <- CSP_individual_PPP * no_of_homes * Project_share
Fin_CSP_perCUR <- (Fin_CSP_total_PPP * exchange_rate_rep_usd)/(allocated * exchange_rate)


CSP_total_PPP <- CSP_individual_PPP * no_of_homes
load("r_Other_Code/preston_function.RData")
Fin_dalys_averted <- preston_function(ISO3 = Country_ISO, amount_PPP = CSP_total_PPP)
Fin_dalys_averted_perM <- Fin_dalys_averted/((allocated  * exchange_rate)/1000000)



#Output section
#get real country name
ISO_Countries <- readRDS("r_data/ISO_Countries.rds")
Country_name <- as.character(ISO_Countries[ISO_Countries$ISO3 == Country_ISO, "Country"])


# Remove all variables for safety and clarity

output <-data.frame(Use_of_proceed= "Affordable Housing",
                    Project_name = project_name,
                    Country = Country_name,
                    target_group,
                    Housing_type = housing_type,
                    no_of_homes,
                    signed = signed * exchange_rate,
                    allocated = allocated * exchange_rate,
                    Project_share = Project_share_reporting,
                    CSP_individual = CSP_individual_PPP * exchange_rate_usd,
                    Fin_CSP_total = Fin_CSP_total_PPP * exchange_rate_usd,
                    Fin_CSP_perCUR = Fin_CSP_perCUR,
                    Fin_DALYs = Fin_dalys_averted,
                    Fin_DALYs_perM =  Fin_dalys_averted_perM,
                    Project_CSP_total = CSP_individual_PPP * exchange_rate_usd * no_of_homes,
                    median_household_income = household_inc_2023_ppp * exchange_rate_usd,
                    eligable_income = elig_income_PPP  * exchange_rate_usd,
                    affordable_rent  = affordable_rent_PPP  * exchange_rate_usd,
                    baseline_rent = baseline_rent_PPP  * exchange_rate_usd,
                    exchange_date = exchange_date,
                    exchange_rate = exchange_rate,
                    project_currency = project_currency,
                    reporting_currency = reporting_currency,
                    Category = c("Social"),
                    allocated_usd = allocated * exchange_rate_usd,
                    Country_ISO = Country_ISO,
                    allocated_orgcur = allocated
                    ### assumption table
                    ,unit_cost
                    ,household_inc_2023_ppp
                    ,affordable_rent_PPP
                    ,baseline_rent_PPP
)
output




}
 
save(ah_spend_input, file = "r_Algorithms/ah_spend_input.RData") 

#temp<- ah_spend_input(project_name= "test", 
#               allocated = 1000, 
 #              housing_type = "Renting",
  #             Country_ISO = "SWE", 
   #            Project_share = 0.5, 
    #           no_of_homes = 10,
     #           project_currency = "GBP", 
      #         reporting_currency = "SEK",
                           #hidden, with default values
       #                    eligibility_crit = 0.8, 
        #                   affordability_crit = 0.3,
         #                  affordable_rent_monthly = NULL, 
          #                 cost_burdened_def = 0.4)
  
#View(temp)