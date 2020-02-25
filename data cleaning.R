#Packages used
library(tsibble)
library(tidyverse)
library(dplyr)
library(hexView)
library(nlWaldTest)
library(broom)
library(dynlm)
library(kableExtra)
library(zoo)
library(lmtest)
library(urca)
library(tseries)
library(CADFtest)
library(strucchange)
library(vars)



## Data wrangling
raw_data <- read.csv('raw_data_ECU.csv')

data <- raw_data %>% 
  mutate(real_gdp = log(rgdpo),
         openness = trade.openness,
         invest = inv_share,
         hcapital = hci,
         ln_lp = log(l_share)) %>% 
  dplyr::select(Year, real_gdp, openness, invest, hcapital, ln_lp, fin_dev)

data <- as_tsibble(data, index = Year) #%>% column_to_rownames(var = 'Year')
data %>% summary()

#data observation
data %>% 
  gather(key = Var, value  = value,
         real_gdp, openness, invest, hcapital, ln_lp, fin_dev) %>% 
  ggplot(aes(x = Year, y = value)) +
  geom_line() +
  facet_wrap(~ Var, scales = "free_y", nrow = 3) +
  theme_minimal() + 
  scale_color_brewer(palette = "Dark2") + 
  theme(legend.position = "bottom", legend.title = element_blank()) +
  ylab("")
  

#unit root test for each series to indicate the level of each series
#real gdp
ggtsdisplay(data$real_gdp) # it has a trend, need to check if deterministic or stochastic

dcmp <- data %>%
  dplyr::select(Year, real_gdp) %>% 
  model(STL(real_gdp))
components(dcmp)


#VAR Lag order selection criteria 
VARselect(data, type = "both")

var1 <- VAR(data, p=1, type="const")
serial.test(var1, lags.pt=10, type="PT.asymptotic")
var2 <- VAR(data, p=2, type="const")
serial.test(var2, lags.pt=10, type="PT.asymptotic")
