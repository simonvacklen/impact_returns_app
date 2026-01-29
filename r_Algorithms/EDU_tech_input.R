#EDU Spend -> GHG Algorithm

#Country_ISO <- "AFG"
#project_name = "test"
#signed <- 1000000
#allocated <- 1000000
#target_group = "the stupid"
##level="Primary"
#Project_share = 1
#student_years = 4202
#project_currency = "SEK"
#reporting_currency = "GBP"

edu_spend_input <- function(project_name, signed, allocated, Country_ISO,Project_share,
                            student_years, level="Any level", 
                            project_currency, reporting_currency, target_group="NA")
  
  #also inflation
  
{
  
  #currency conversion
  load("r_Other_Code/currency_convert.RData") 
  exchange_stuff <- currency_convert(currency = project_currency, outcurrency = reporting_currency)
  exchange_date <-   exchange_stuff[[2]]
  exchange_rate <-   as.numeric(exchange_stuff[[1]])
  exchange_rate_usd <- currency_convert(project_currency, "USD")[[1]]  
  exchange_rate_rep_usd <- currency_convert("USD", reporting_currency)[[1]]
  
  

  
  
  #if we use costdb, project share needs to be 1, but they might still have provided the number
  Project_share_reporting <- Project_share
  
  
  
  if (level == "Any level") {
    level_reporting <- "Mixed"
  }  else {
    level_reporting <- level
  }
  
  
  unit_cost <- NA
  if (is.na(student_years)) {
    # Look up replacement value from costdb
    #if we dont know school level, we use secondary cost, its in the middle
    cost_db <- readRDS("r_data/cost_db.rds")
    unit_cost <- cost_db$unit_cost[cost_db$Use_of_proceed == "Education" 
                                   & cost_db$ISO3 == Country_ISO
                                   & cost_db$Technology == level] 
    student_years<- (allocated*exchange_rate_usd) / unit_cost
    Project_share <- 1
  }
  
  
  
  #Bseline calcs
  enrollment <- readRDS("r_data/enrollment.rds")
  
  
  if (level == "Any level")
  {
    enr_level <- "Secondary"
  } else {
    enr_level <- level
  }
  
  
  enrollment_rate <- enrollment[enrollment$School_level == enr_level 
                                & enrollment$ISO3 == Country_ISO, ]$enrollment
  
  student_years_tot <- student_years
  student_years <- student_years * (1-enrollment_rate)
  
  
  
  ##Return on education
  edu_data <- readRDS("r_data/EDU_RoE.rds")
  edu_data_row <- edu_data[edu_data$ISO3 == Country_ISO, ]
  return_on_education <- as.numeric(edu_data_row[, level])
  
  #stnddev3 <- as.numeric(edu_data_row[, "Margin_95"])
  #return_on_education_low <- return_on_education - stnddev3
  #return_on_education_high <- return_on_education + stnddev3
  
  
  
  #Median Income
  median_income_ppp <- readRDS("r_data/median_income.rds")
  median_income_ppp <- as.numeric(median_income_ppp[median_income_ppp$ISO3 == Country_ISO, "Median_Income_2025_PPP"])
  
  
  
  #calcs need both ppp and LCU?
  annual_income_increase_ppp <- median_income_ppp *  return_on_education
  
  
  
  #NPV function
  calculate_npv <- function(annual_cash_flow, years, discount_rate) {
    # Initialize NPV
    npv <- 0
    # Calculate the present value of each annual cash flow and add it to NPV
    for (t in 1:years) {
      npv <- npv + annual_cash_flow / (1 + discount_rate) ^ t
    }
    return(npv)
  }
  
  
  # pers_lifetime_income_npv_ppp_high <- calculate_npv(annual_income_increase_ppp, 40, 0.02)
  pers_lifetime_income_npv_ppp <- calculate_npv(annual_income_increase_ppp, 40, 0.03)
  #  pers_lifetime_income_npv_ppp_low <- calculate_npv(annual_income_increase_ppp, 40, 0.04)
  
  total_lifetime_income_npv_ppp <- pers_lifetime_income_npv_ppp * student_years
  #  total_lifetime_income_npv_ppp_high <- pers_lifetime_income_npv_high * student_years
  #  total_lifetime_income_npv_ppp_low <- pers_lifetime_income_npv_low * student_years
  
  ##this is repcur
  Fin_lifetime_income_perCUR_repcur <- (total_lifetime_income_npv_ppp * exchange_rate_rep_usd * Project_share)/(allocated * exchange_rate)
  
  
  load("r_Other_Code/preston_function.RData")
  Fin_dalys_averted <- preston_function(ISO3 = Country_ISO, amount_PPP = total_lifetime_income_npv_ppp  * Project_share)
  Fin_dalys_averted_perM <- Fin_dalys_averted/((allocated * exchange_rate)/1000000)
  
  
  #Output section
  ISO_Countries <- readRDS("r_data/ISO_Countries.rds")
  Country_name <- as.character(ISO_Countries[ISO_Countries$ISO3 == Country_ISO, "Country"])
  
  
  #project and local currency should be the same most time but mgiht not
  output <- data.frame(Use_of_proceed= "Access to Essential Services: Education",
                       Project_name = project_name,
                       Country = Country_name, 
                       target_group = target_group,
                       Level = level_reporting,
                       fin_student_years = student_years_tot,
                       signed = signed * exchange_rate,
                       allocated = allocated  * exchange_rate,
                       Project_share = Project_share_reporting,
                       add_student_years = student_years,
                       return_on_education = return_on_education,
                       fin_lt_inc = total_lifetime_income_npv_ppp * Project_share * exchange_rate_rep_usd,
                       fin_lt_inc_perCUR = Fin_lifetime_income_perCUR_repcur,
                       Fin_DALYs = Fin_dalys_averted,
                       Fin_DALYs_perM =  Fin_dalys_averted_perM,
                       #                      fin_lt_inc_low = total_lifetime_income_npv_low * Project_share * exchange_rate_usd,
                       #                     fin_lt_inc_high = total_lifetime_income_npv_high * Project_share * exchange_rate_usd,
                       total_lt_inc = total_lifetime_income_npv_ppp * exchange_rate_usd,
                       #                    total_lt_inc_low = total_lifetime_income_npv_low * exchange_rate_usd,
                       #                   total_lt_inc_high = total_lifetime_income_npv_high * exchange_rate_usd,
                       exchange_date = exchange_date,
                       exchange_rate = exchange_rate,
                       exchange_rate_usd = exchange_rate_usd,
                       project_currency = project_currency,
                       reporting_currency = reporting_currency,
                       Category = c("Social"),
                       allocated_usd = allocated * exchange_rate_usd,
                       allocated_orgcur = allocated,
                       Country_ISO = Country_ISO
                       
                       ## assumption table additions
                       ,unit_cost 
                       ,return_on_education
                       ,enrollment_rate
                       ,median_income_ppp
  )
  
  
  output
  #Primary_Returns Secondary_Returns Tertiary_Returns
}

#edu_spend_input(project_name="Heartspital", allocated=1000000, Country_ISO="NOR",Project_share=0.6, student_years = 100, level="Secondary", project_currency="SEK", reporting_currency="GBP")

save(edu_spend_input, file = "r_Algorithms/edu_spend_input.RData")