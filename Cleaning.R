################################################################################
##### Cleaning IEA energy price data  ##########################################
#       14 June 2022
#       Ebba Mark
#       ebba.mark@nuffield.ox.ac.uk
################################################################################
rm(list = ls())
library(tidyverse)
library(tidyr)
library(zoo)
library(here)
library(plm)

standardise <- function(df) {
  df_clean <- df %>%
  mutate(Country= replace(Country, Country == "", NA)) %>% 
  mutate(Time= replace(Time, Time == "", NA)) %>%
  fill(Country, .direction = "down") %>%
  fill(Time, .direction = "down") %>% 
  setNames(tolower(names(.)))
  return(df_clean)
}

################################################################################
# Annual: CPI, PPI, CPI-energy
y_pi <- read.csv("Annual_Energy_price_indices.csv", header = TRUE, skip = 1) %>%
  standardise()
# %>% make.pbalanced()

# Quarterly: CPI, PPI, CPI-energy
q_pi <- read.csv("Quarterly_Energy_price_indices.csv", header = TRUE, skip = 1) %>%
  standardise() %>% 
  mutate(time = as.yearqtr(time, format = "Q%q-%Y"))

writexl::write_xlsx(y_pi, here("clean_yr_nrg_price_indices.xlsx"))
writexl::write_xlsx(q_pi, here("clean_qtr_nrg_price_indices.xlsx"))

################################################################################
# Annual: Real and Nominal End-use price indices
y_rnp <- read.csv("Annual_Indices of Real and Nominal End-Use Energy Prices.csv", header = TRUE, skip = 1) %>%
  standardise() 

# Quarterly: Real and Nominal End-use price indices
q_rnp <- read.csv("Quarterly_Indices of Real and Nominal End-Use Energy Prices.csv", header = TRUE, skip = 1) %>% 
  standardise() %>% 
  mutate(time = as.yearqtr(time, format = "Q%q-%Y"))

writexl::write_xlsx(y_rnp, here("clean_yr_real_nom_eup.xlsx"))
writexl::write_xlsx(q_rnp, here("clean_qtr_real_nom_eup.xlsx"))

################################################################################
# Annual: Wholesale and retail price indices
y_wrp <- read.csv("Annual_Wholesale and Retail Price Indices for Energy Products.csv", header = TRUE, skip = 1) %>% 
  standardise() 

# Quarterly: Wholesale and retail price indices
q_wrp <- read.csv("Quarterly_Wholesale and Retail Price Indices for Energy Products.csv", header = TRUE, skip = 1) %>% 
  standardise() %>% 
  mutate(time = as.yearqtr(time, format = "Q%q-%Y"))

writexl::write_xlsx(y_wrp, here("clean_yr_wholesale_retail_indices.xlsx"))
writexl::write_xlsx(q_wrp, here("clean_qtr_wholesale_retail_indices.xlsx"))

################################################################################
# Annual: End use energy prices and taxes
y_eupt <- read.csv("Annual_end use energy prices and taxes in USD.csv", header = TRUE, skip = 1) %>%
  standardise()

# Quarterly: End use energy prices and taxes
q_eupt <- read.csv("Quarterly_end use energy prices and taxes in USD.csv", header = TRUE, skip = 1) %>%
  standardise() %>% 
  mutate(time = as.yearqtr(time, format = "Q%q-%Y"))

q_eupt[,5:10] <- as.data.frame(lapply(q_eupt[,5:10], as.numeric))

writexl::write_xlsx(y_eupt, here("clean_yr_end_use_price_taxes.xlsx"))
writexl::write_xlsx(q_eupt, here("clean_qtr_end_use_price_taxes.xlsx"))

################################################################################
