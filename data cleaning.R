#Packages used
library(tsibble)
library(tidyverse)
library(dplyr)


## Data wrangling

raw_data <- read.csv('raw_data_ECU.csv')

data <- raw_data %>% 
  mutate(real_gdp = log(rgdpo),
         openness = trade.openness,
         invest = inv_share,
         hcapital = hci,
         ln_lp = log(l_share)) %>% 
  dplyr::select(Year, real_gdp, openness, invest, hcapital, ln_lp, fin_dev)

data <- as_tsibble(data, index = Year)
data %>% summary()

#data observation
data %>% autoplot(real_gdp)
data %>% autoplot(openness)
data %>% autoplot(invest)
data %>% autoplot(hcapital)
data %>% autoplot(ln_lp)
data %>% autoplot(fin_dev)


