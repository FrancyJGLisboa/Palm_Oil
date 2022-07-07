#Packages####
library(decisionSupport)
library(readr)
library(random)

#help_function####
#The function can help you to check piece of codes separately

make_variables <- function(est,n=1)
{x <- random(rho=est,n=n)
for(i in colnames(x))assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)}
make_variables(estimate_read_csv(paste("palm_input.csv",sep="")))

#overview_input_data####
input_data <- read.csv("palm_input.csv")
View(input_data)


#build the model function####

po_decision_function <- function(x, varnames){
  
  #Define each variable as vectors of 25 values corresponding to 25 years of simulation
  establishment_cost_n_tcl <- rep(0, n_years)
  estab_cost_tcl <- rep(0, n_years)
  mainte_cost <- rep(0, n_years)
  land_tax_var <- rep(0, n_years)
  harvest_cost <- rep(0, n_years)
  transport_cost <- rep(0, n_years)
  cert_cost <- rep(0, n_years)
  tot_po_yield_non_tcl <- rep(0, n_years)
  total_po_benefit_TCL <- rep(0, n_years)
  FFB_price <- rep(0, n_years)
  po_yield <- rep(0, n_years)
  
  
  
  #Simulate the chance for risk events to occur during the simulation period
  po_risk <- chance_event(fire_risk, value_if = 1, n = n_years)
   
  # pre-calculate common random draws for two scenario model runs ####
  
  #Calculate palm oil system costs (minimum land conversion)####
  
  #establishment cost
  establishment_cost_n_tcl[1] <- establishment_cost_non_tcl
  establishment_cost_n_tcl[2:25] <- 0
  
  #tax
  land_tax_var[1] <- 0
  land_tax_var[2:3] <- vv(land_tax, var_CV, 2)
  land_tax_var[4:25] <- vv(land_tax, var_CV, 22)
  
  #harvesting cost
  harvest_cost[1:3] <- 0
  harvest_cost[4:25] <- vv(harvesting_cost, var_CV, 22)
  
  #transportation cost -> to bring FFB to market
  transport_cost[1:3] <- 0
  transport_cost[4:25] <- vv(transportation_cost, var_CV, 22)
  
  #maintenance cost (including fertilizers and labour)
  mainte_cost[1] <- 0
  mainte_cost[2:4] <- vv(maintenance_cost, var_CV, 3) 
  mainte_cost[5:25] <- vv(maintenance_cost, var_CV, 21)
  
  #RSPO certification cost -> to minimize risk FFBs are not accepted in market
  cert_cost <- vv(certification_cost, var_CV, n_years)
  
  envrionmental_cost_n_tcl <- vv(environmental_cost_non_tcl, var_CV, n_years)
  
  #Calculate palm oil system costs with Tree Cover Loss
  #establishment cost
  estab_cost_tcl[1] <- establishment_cost_tcl
  estab_cost_tcl[2:25] <- 0
  
  
  environment_cost_tcl <- vv(environmental_cost_tcl, var_CV, n_years)
  
  #FFB Price
  FFB_price[1] <- 0
  FFB_price[2:4] <- vv(FFB_price_immature, var_CV, 3) 
  FFB_price[5:25] <- vv(FFB_price_mature, var_CV, 21)
  
  
  
  
  
  
  # calculate palm oil yield####
  po_yield <- gompertz_yield(max_harvest = max_palm_harvest, 
                             time_to_first_yield_estimate = immature_palm_est, 
                             time_to_second_yield_estimate = mature_palm_est,
                             first_yield_estimate_percent = immature_palm_yield_est,
                             second_yield_estimate_percent = mature_palm_yield_est,
                             n_years = n_years, 
                             var_CV = 0, 
                             no_yield_before_first_estimate = TRUE)
  
  tot_po_yield_non_tcl <- po_yield * tot_of_po_area_non_tcl
  #tot_po_yield_non_tcl <- tot_po_yield_non_tcl * (1 - po_risk * tot_of_po_area_non_tcl)
  tot_po_benefit_non_tcl <- tot_po_yield_non_tcl * FFB_price
  
  tot_po_yield_tcl <- po_yield * tot_of_po_area_tcl
  #tot_po_yield_tcl <- tot_po_yield_tcl * (1 - po_risk * tot_of_po_area_tcl)
  
  
  
  
  
  
  #tot_po_benefit_tcl_immature <- tot_po_yield_tcl * FFB_price
  tot_po_benefit_tcl <- tot_po_yield_tcl * FFB_price
  
  # Calculate NPVs palm oil plantation non Tree Cover Area and Tree Cover Loss
  total_po_benefit_non_TCL <- tot_po_benefit_non_tcl - establishment_cost_n_tcl - mainte_cost - transport_cost - harvest_cost - cert_cost - envrionmental_cost_n_tcl
  
  total_po_benefit_TCL <- tot_po_benefit_tcl -  estab_cost_tcl - mainte_cost - transport_cost - harvest_cost - cert_cost - environment_cost_tcl
  
  #NPV of palm oil plantation non Tree Cover Loss
  NPV_non_TCL <- discount(total_po_benefit_non_TCL, discount_rate = discount_rate, 
                          calculate_NPV = TRUE ) 
  
  #NPV of palm oil plantation Tree Cover Loss
  NPV_TCL <- discount(total_po_benefit_TCL, 
                      discount_rate = discount_rate, 
                      calculate_NPV = TRUE)
  
  #Benefit of choosing agroforestry over maize monoculture
  tradeoff_benefit <- NPV_non_TCL - NPV_TCL
  
  #final NPV of the decision to choose non Tree Cover Loss or Tree Cover Loss
  NPV_tradeoff <- discount(tradeoff_benefit, 
                           discount_rate = discount_rate, 
                           calculate_NPV = TRUE )
  
  #in the return list, one could indicate any outcome that they wish to see from the model
  return(list(trade_off = NPV_tradeoff, 
              Non_TCL_NPV = NPV_non_TCL, 
              TCL_NPV = NPV_TCL))
}
#runMCSimulation####

mcSimulation_results <- decisionSupport::mcSimulation(
  estimate = decisionSupport::estimate_read_csv("palm_input.csv"),
  model_function = po_decision_function,
  numberOfModelRuns = 1000,
  functionSyntax = "plainNames"
)




#plot_result####

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = c("Non_TCL_NPV", "TCL_NPV"),
                                    method = 'smooth_simple_overlay', 
                                    base_size = 7)

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = c("Non_TCL_NPV",
                                             "TCL_NPV"),
                                    method = 'boxplot')

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = "trade_off",
                                    method = 'boxplot_density')








