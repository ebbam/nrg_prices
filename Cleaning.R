################################################################################
### Cleaning producer and consumer energy price index data from the IEA ########
#       14 June 2022
#       Ebba Mark
#       ebba.mark@nuffield.ox.ac.uk
################################################################################

library(tidyverse)
library(tidyr)
library(zoo)
library(here)

# Annual data
y_nrg <- read.csv("Annual_Energy_price_indices.csv", header = TRUE, skip = 1)

y_nrg <- y_nrg %>%
  mutate(Country= replace(Country, Country == "", NA)) %>% 
  fill(Country, .direction = "down") %>%
  setNames(c("country", "time", "us_ex_rate", "ppp", "ppi", "cpi", "cpi_nrg"))

# Quarterly data
q_nrg <- read.csv("Quarterly_Energy_price_indices.csv", header = TRUE, skip = 1)

q_nrg <- q_nrg %>%
  mutate(Country= replace(Country, Country == "", NA)) %>% 
  mutate(Time = as.yearqtr(Time, format = "Q%q-%Y")) %>% 
  fill(Country, .direction = "down") %>%
  setNames(c("country", "time", "us_ex_rate", "ppi", "cpi", "cpi_nrg"))

writexl::write_xlsx(y_nrg, here("clean_yr_nrg_price_indices.xlsx"))
writexl::write_xlsx(q_nrg, here("clean_qtr_nrg_price_indices.xlsx"))
