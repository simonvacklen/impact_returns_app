
oth_passthrough <- function(use_of_proceed,
                            Country_ISO, project_type, proj_tghg_footprint=NA, proj_tghg_avoided=NA,
                            metric1name=NA, metric1val=NA, metric2name=NA, metric2val=NA,
                          Project_share, signed, allocated, project_name, 
                          project_currency, reporting_currency)
{

  
  proj_tghg_footprint <-  suppressWarnings(as.numeric(proj_tghg_footprint))
  proj_tghg_avoided <-  suppressWarnings(as.numeric(proj_tghg_avoided))
  metric1val <-  suppressWarnings(as.numeric(metric1val))
  metric2val <-  suppressWarnings(as.numeric(metric2val))

  #currency conversion
  load("r_Other_Code/currency_convert.RData") 
  exchange_stuff <- currency_convert(currency = project_currency, outcurrency = reporting_currency)
  exchange_date <-   exchange_stuff[[2]]
  exchange_rate <-   as.numeric(exchange_stuff[[1]])

Fin_avoidedGHG_perM <- (proj_tghg_avoided * Project_share)/((allocated * exchange_rate)/1000000)


load("r_Other_Code/GHG_to_DALY_func.RData")
Fin_dalys_averted <- GHG_to_DALY_func(tGHG=(proj_tghg_avoided * Project_share))
Fin_dalys_averted_perM <- Fin_dalys_averted/((allocated * exchange_rate)/1000000)

#Output section
#get real country name
ISO_Countries <- readRDS("r_data/ISO_Countries.rds")
Country_name <- as.character(ISO_Countries[ISO_Countries$ISO3 == Country_ISO, "Country"])



output <- data.frame(Use_of_proceed= "Additional Use of Proceeds",
                    Use_of_proceed2= use_of_proceed,
                    Project_name = project_name,
                    Country = Country_name, 
                    project_type = project_type,
                    signed = signed * exchange_rate,
                    allocated = allocated * exchange_rate,
                    Project_share = Project_share,
                    Fin_GHG_footprint = proj_tghg_footprint * Project_share,
                    Fin_GHG_Avoidance = proj_tghg_avoided * Project_share,
                    Fin_avoidedGHG_perM = Fin_avoidedGHG_perM,
                    metric1name = metric1name, 
                    metric1val = metric1val, 
                    metric2name = metric2name, 
                    metric2val = metric2val,
                    Fin_DALYs = Fin_dalys_averted,
                    Fin_DALYs_perM =  Fin_dalys_averted_perM,
                    Footprint_total = proj_tghg_footprint,
                    GHG_Avoidance_total = proj_tghg_avoided,
                    exchange_date = exchange_date,
                    exchange_rate = exchange_rate,
                    project_currency = project_currency,
                    reporting_currency = reporting_currency,
                    Category = "Mixed",
                    allocated_orgcur = allocated,
                    Country_ISO = Country_ISO
                    #assumption table additions - none, no methodology
                    
                    
                    )
output
}

save(oth_passthrough, file = "r_Algorithms/oth_passthrough.RData")



