library(decisionSupport)
library(readr)
library(readxl)
library(ggplot2)
library(plotly)

# The function can help you to check piece of codes separately
make_variables <- function(est,n=1)
{x <- random(rho=est,n=n)
for(i in colnames(x))assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)}
make_variables(estimate_read_csv(paste("C:/Users/erlan/Documents/Data/Example Code/Sample/palm_input_new.csv",sep="")))



po_decision_function <- function(x, varnames){
  
  #Define each variable as vectors of 25 values corresponding to 25 years of simulation
  establishment_cost_n_tcl <- rep(0, n_years)
  estab_cost_tcl <- rep(0, n_years)
  mainte_cost <- rep(0, n_years)
  mainte_cost_im <- rep(0, n_years)
  land_tax_var <- rep(0, n_years)
  harvest_cost <- rep(0, n_years)
  transport_cost <- rep(0, n_years)
  cert_cost <- rep(0, n_years)
  tot_po_yield_non_tcl <- rep(0, n_years)
  total_po_benefit_TCL <- rep(0, n_years)
  FFB_price <- rep(0, n_years)
  po_yield <- rep(0, n_years)
  envrionmental_cost_n_tcl <- rep(0, n_years)
  environment_cost_tcl <- rep(0, n_years)
  total_po_benefit_TCL <- rep(0, n_years)
  tot_po_yield_non_tcl <- rep(0, n_years)
  
  #Simulate the chance for risk events to occur during the simulation period
  # po_risk <- chance_event(fire_risk, value_if = 1, n = n_years)
  
  # pre-calculate common random draws for two scenario model runs ####
  
  #Calculate palm oil system costs with non Tree Cover Loss (minimum land conversion)
  #establishment cost
  establishment_cost_n_tcl[1] <- establishment_cost_non_tcl
  establishment_cost_n_tcl[2:25] <- 0
  
  #tax
  land_tax_var[1] <- 0
  land_tax_var[2:3] <- vv(land_tax, var_land, 2)
  land_tax_var[4:25] <- vv(land_tax, var_land, 22)
  
  #harvesting cost
  harvest_cost[1:3] <- 0
  harvest_cost[4:25] <- vv(harvesting_cost, var_harvest, 22)
  
  #transportation cost -> to bring FFB to market
  transport_cost[1:3] <- 0
  transport_cost[4:25] <- vv(transportation_cost, var_transport, 22)
  
  #maintenance cost (including fertilizers and labour)
  mainte_cost[1:7] <- 0
  mainte_cost[8:25] <- vv(maintenance_cost, var_maintenance, 18)
  
  mainte_cost_im[1] <- 0
  mainte_cost_im[2:7] <- vv(maintenance_cost_im, var_maintenance, 6)
  mainte_cost_im[8:25] <- 0
  
  #RSPO certification cost -> to minimize risk FFBs are not accepted in market
  cert_cost[1] <- certification_cost
  cert_cost[2:25] <- 0
  
  
  #Calculate palm oil system costs with Tree Cover Loss
  #establishment cost
  estab_cost_tcl[1] <- establishment_cost_tcl
  estab_cost_tcl[2:25] <- 0
  
  #establishment cost
  environment_cost_tcl[1:25] <- vv(environmental_cost_tcl, var_CV, 25)
  
  #FFB Price
  FFB_price[1] <- 0
  FFB_price[2:7] <- vv(FFB_price_immature, var_FFB, 6) 
  FFB_price[8:25] <- vv(FFB_price_mature, var_FFB, 18)
  
  # benefits of palm oil
  po_yield <- gompertz_yield(max_harvest = max_palm_harvest, 
                             time_to_first_yield_estimate = immature_palm_est, 
                             time_to_second_yield_estimate = mature_palm_est,
                             first_yield_estimate_percent = immature_palm_yield_est,
                             second_yield_estimate_percent = mature_palm_yield_est,
                             n_years = n_years, 
                             var_CV = 0, 
                             no_yield_before_first_estimate = TRUE)
  
  tot_po_yield_non_tcl <- po_yield
  tot_po_benefit_non_tcl <- tot_po_yield_non_tcl * FFB_price
  
  #
  tot_po_yield_tcl <- po_yield
  tot_po_benefit_tcl <- tot_po_yield_tcl * FFB_price
  
  # Calculate NPVs palm oil plantation non Tree Cover Area and Tree Cover Loss
  total_po_benefit_non_TCL <- tot_po_benefit_non_tcl - establishment_cost_n_tcl - mainte_cost - mainte_cost_im- transport_cost - harvest_cost - cert_cost
  
  total_po_benefit_TCL <- tot_po_benefit_tcl -  estab_cost_tcl - mainte_cost - mainte_cost_im - transport_cost - harvest_cost - cert_cost - environment_cost_tcl
  
  
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


mcSimulation_results <- decisionSupport::mcSimulation(
  estimate = decisionSupport::estimate_read_csv("C:/Users/erlan/Documents/Data/Example Code/Sample/palm_input_new.csv"),
  model_function = po_decision_function,
  numberOfModelRuns = 1000,
  functionSyntax = "plainNames"
)


decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = c("Non_TCL_NPV", "TCL_NPV"),
                                    method = 'smooth_simple_overlay', 
                                    base_size = 10)


decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = c("Non_TCL_NPV",
                                             "TCL_NPV"),
                                    method = 'boxplot')

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = "trade_off",
                                    method = 'boxplot_density')


#MCall <- read.table(file = "MCResults/mcSimulationResults.csv", 
#header = TRUE, sep  = ",")

#multi_EVPI(MCall, "trade_off", write_table = TRUE)

mcSimulation_table <- data.frame(mcSimulation_results$x, mcSimulation_results$y[1:3])

evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "trade_off")

plot_evpi(evpi, decision_vars = "trade_off")
