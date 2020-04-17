###--------Packages used
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
library(lubridate)
library(readxl)
library(xtable)
library(uroot)

## read PWT data
raw_data <- read.csv('raw_data_ECU.csv')

#variables used 
data <- raw_data %>% 
  mutate(year = ymd(paste(Year,'0101')),
         logreal_gdp = log(rgdpo/pop),
         trade_open = trade.openness,
         inv = inv_share,
         hcapital = hci) %>% 
  dplyr::select(year, logreal_gdp, trade_open, inv, hcapital, fin_dev)

#labour productivity data from TED
cbt_data <- read_xlsx('TED_1_APR20191.xlsx', sheet = 3, skip = 3) %>% 
  filter(COUNTRY == 'Ecuador',
         INDICATOR == 'Output per Employed Person') %>% 
  gather(key = 'year', value = 'Value', c(-1:-5)) %>% 
  mutate(year = ymd(paste(year,'0101')), la_productivity = Value) %>%
  dplyr::select(year, la_productivity) %>% slice(1:65)

#joining both datasets
full_data <- full_join(data, cbt_data, by = 'year') %>% 
  mutate(log_lapro = log(la_productivity)) %>% 
  dplyr::select(-7)

#read new created data set
full_data <- read_csv('full_ecudata.csv')

#data summary statistics
full_data[2:7] %>% 
  pastecs::stat.desc(basic = T) %>% 
  xtable()

#data observation
full_data %>% 
  gather(key = Var, value  = value,
         logreal_gdp, trade_open, inv, hcapital, fin_dev, log_lapro) %>% 
  ggplot(aes(x = year, y = value)) +
  geom_line() +
  facet_wrap(~ Var, scales = "free_y", nrow = 3) +
  theme_minimal() + 
  scale_color_brewer(palette = "Dark2") + 
  theme(legend.position = "bottom", legend.title = element_blank(), 
        axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  ylab("") +
  scale_x_date(date_labels = "%Y", date_breaks = '2 years')
  
##unit root test for each series to indicate the level of each series-----
#transform data into time series
rgdp <- ts(full_data$logreal_gdp, start = 1950, end = 2014)
lprod <- ts(full_data$log_lapro, start = 1950, end = 2014)
invst <- ts(full_data$inv, start = 1950, end = 2014)
hcap <- ts(full_data$hcapital, start = 1950, end = 2014)
findev <- ts(full_data$fin_dev, start = 1950, end = 2014)
trade <- ts(full_data$trade_open, start = 1950, end = 2014)

#plots of univariate series
plot(rgdp)
plot(lprod)
plot(invst)
plot(hcap)
plot(findev)
plot(trade)

#detrend time series with possible DT
gdp_dt <- ts(residuals(lm(rgdp~c(1:length(rgdp)))), start = 1950, end = 2014)
lprod_dt <- ts(residuals(lm(lprod~c(1:length(lprod)))), start = 1950, end = 2014)
hcap_dt <- ts(residuals(lm(hcap~c(1:length(hcap)))), start = 1950, end = 2014)
plot(gdp_dt, type = 'l')
plot(lprod_dt, type = 'l')
plot(hcap_dt, type = 'l')
#findev, invst and trade do not presetn DT and all variables must contain ST
#because they did not produce a stationary series

trend <- seq_along(rgdp)
fit <- lm(rgdp~trend)
summary(fit)
plot(trend, rgdp, ylim=c(7.6, 9.5), type = 'l')
lines(predict(fit), col='red', type = 'l')

##------ADF unit root test of univariate timeseries in levels----
rgdp.urt <- ur.df(rgdp, type = 'trend', lags = 10) #trend and intercept
summary(rgdp.urt) #reject if tcalc < tcrit
# -1.4333 > -3.45 and we fail to reject the null that gdp has unit root

lprod.urt <- ur.df(lprod, type = 'trend', selectlags = 'AIC') #trend and intercept
summary(lprod.urt)
# -2.1256 > -3.45 and we fail to reject the null that lprod has unit root

invst.urt <- ur.df(invst, type = 'drift', selectlags = 'AIC') #intercept
summary(invst.urt)
# -1.4879 > -2.89 and we fail to reject the null that invst has a unit root

hcap.urt <- ur.df(hcap, type = 'trend', selectlags = 'AIC') #trend and intercept
summary(hcap.urt)
# -2.3268 > -3.45 and we fail to reject the null that hcap has unit root

findev.urt <- ur.df(findev, type = 'drift', selectlags = 'AIC') #intercept
summary(findev.urt)
#-2.8321 < -2.89 and we reject the null that findev has unit root

trade.urt <- ur.df(trade, type = 'drift', selectlags = 'AIC') #intercept
summary(trade.urt)
#-1.8249 > -2.89 and we fail to rejeect the null that trade has unit root

#when tested for a unit root in variables we were unable to reject the unit root
#hypothesis. This suggests that the variables has AT LEAST ONE unit root. Varaibles are not I(0)


##------ADF unit root test of univariate timeseries in first difference----
#Difference time series
drgdp <- diff(rgdp)
dlprod <- diff(lprod)
dinvst <- diff(invst)
dhcap <- diff(hcap) #we may not use this for our model since it is not WN
dtrade <- diff(trade)
dfindev <- diff(findev)

drgdp.urt <- ur.df(drgdp, type = 'drift', selectlags = 'AIC') #intercept
summary(drgdp.urt) #reject if tcalc < tcrit
# -3.5035 < -2.89 and we reject the null that gdp has unit root

dlprod.urt <- ur.df(dlprod, type = 'drift', selectlags = 'AIC') #intercept
summary(dlprod.urt)
# -4.0761 < -2.89 and reject the null that lprod has unit root

dinvst.urt <- ur.df(dinvst, type = 'drift', selectlags = 'AIC') #intercept
summary(dinvst.urt)
# -8.3568 < -2.89 and we reject the null that invst has a unit root

dhcap.urt <- ur.df(dhcap, type = 'drift', selectlags = 'AIC') #intercept
summary(dhcap.urt)
# -1.8381 > -3.45 and we fail to reject the null that hcap has unit root

trade.urt <- ur.df(dtrade, type = 'drift', selectlags = 'AIC') #intercept
summary(trade.urt)
#-4.3405 < -2.89 and we reject the null that trade has unit root

dfindev.urt <- ur.df(dfindev, type = 'drift', selectlags = 'AIC')#intercept
summary(dfindev.urt)
#-5.4496 <-2.89 and we reject the null that trade has unit root

#when tested for a unit root in DIFFERENCE we reject the unit root hypothesis except for dhcap
#This suggests that the Varaibles are I(1) and hcap is at least I(2)


## KPSS test for unit root in levels of variables -----
rgdp.kpss <- ur.kpss(rgdp, type = 'tau', use.lag = 6) #trend and intercept
summary(rgdp.kpss) #reject if tcalc > tcrit
# 0.0927 < 0.146 and we fail to reject the null that gdp has NO unit root
#c:no unit root

lprod.kpss <- ur.kpss(lprod, type = 'tau', use.lag = 6) #trend and intercept
summary(lprod.kpss)
# 0.2189 > 0.146 and we reject the null that lprod has NO unit root
#c: unit root

invst.kpss <- ur.kpss(invst, type = 'mu', use.lag = 6) #intercept
summary(invst.kpss)
# 0.7867 > 0.463 and we reject the null that invst has NO unit root
# c: unit root

hcap.kpss <- ur.kpss(hcap, type = 'tau', use.lag = 6) #trend and intercept
summary(hcap.kpss)
# 0.2326 > 0.146 and we reject the null that hcap has NO unit root
#c: unit root

findev.kpss <- ur.kpss(findev, type = 'mu', use.lag = 5) #intercept
summary(findev.kpss)
#0.5632 > 0.463 and we reject the null that findev has NO unit root
#c: unit root

trade.kpss <- ur.kpss(trade, type = 'mu', use.lag = 6) #intercept
summary(trade.kpss)
#0.6907 > 0.463 and we reject the null that trade has NO unit root
#c: unit root


## KPSS test for unit root in DIFFERENCE of variables -----
drgdp.kpss <- ur.kpss(drgdp, type = 'mu', use.lag = 5) #intercept
summary(drgdp.kpss) #reject if tcalc > tcrit
# 0.1263 < 0.463 and we fail to reject the null that gdp has NO unit root
#c: no unit root

dlprod.kpss <- ur.kpss(dlprod, type = 'mu', use.lag = 5) #intercept
summary(dlprod.kpss)
# 0.3371 < 0.463 and we fail to reject the null that lprod has NO unit root
#c: no unit root

dinvst.kpss <- ur.kpss(dinvst, type = 'mu', use.lag = 12) #intercept
summary(dinvst.kpss)
# 0.2295 < 0.463 and we fail to reject the null that invst has NO unit root
#conclusion: no unit root in difference

dhcap.kpss <- ur.kpss(dhcap, type = 'mu', use.lag = 5) #intercept
summary(dhcap.kpss)
# 0.3858 < 0.463 and we fail to reject the null that hcap has NO unit root
#c: no unit root

dfindev.kpss <- ur.kpss(dfindev, type = 'mu', use.lag = 1) #intercept
summary(dfindev.kpss)
#0.0453 < 0.463 and we fail reject the null that findev has NO unit root
#c: no unit root

dtrade.kpss <- ur.kpss(dtrade, type = 'mu', use.lag = 3) #intercept
summary(dtrade.kpss)
#0.1628 < 0.463 and we fail to reject the null that trade has NO unit root
#c: no unit root







data(Raotbl3)
attach(Raotbl3)
lc <- ts(lc, start=c (1966, 4), end=c(1991, 2), frequency=4)
lc.ct <- ur.df(lc, lags = 3, type = 'trend') #non-zero constant with trend
summary(lc.ct)
ar_lc <- arima(lc, order = c(3,1,0))
res_lc <- residuals(ar_lc)
plot(lc)
lc.co <- ur.df(lc, lags = 3, type = 'drift') #intercept w/o trend
summary(lc.co)
lc2 <- diff(lc)
lc2.ct <- ur.df(lc2, type = 'drift', lags = 3)
summary(lc2.ct)



## PP test for unit root in levels of variables -----
rgdp.ppt <- ur.pp(rgdp, type = 'Z-tau', model = 'trend', use.lag = 5) #trend and intercept
summary(rgdp.ppt) #reject if tcalc < tcrit
# -1.4747 > -3.45 and we fail to reject the null that gdp has unit root

lprod.ppt <- ur.pp(lprod, type = 'Z-tau', model = 'trend', lags = 'short') #trend and intercept
summary(lprod.ppt)
# -1.8199 > -3.45 and we fail to reject the null that lprod has unit root

invst.ppt <- ur.pp(invst, type = 'Z-tau', model = 'constant', lags = 'short') #intercept
summary(invst.ppt)
# -1.7999 > -2.89 and we fail to reject the null that invst has a unit root

hcap.ppt <- ur.pp(hcap, type = 'Z-tau', model = 'trend', lags = 'short') #trend and intercept
summary(hcap.ppt)
# -2.3222 > -3.45 and we fail to reject the null that hcap has unit root

findev.ppt <- ur.pp(findev, type = 'Z-tau', model = 'constant', lags = 'short') #intercept
summary(findev.ppt)
#-2.2719 > -2.89 and we fail to reject the null that findev has unit root

trade.ppt <- ur.pp(trade, type = 'Z-tau', model = 'constant', lags = 'short') #intercept
summary(trade.ppt)
#-1.8513 > -2.89 and we fail to reject the null of unit root


## PP test for unit root in DIFFERENCE of variables -----
drgdp.ppt <- ur.pp(drgdp, type = 'Z-tau', model = 'constant', use.lag = 5) #trend and intercept
summary(drgdp.ppt) #reject if tcalc < tcrit
# -5.6995 < -2.90 and we reject the null that drgdp has unit root

dlprod.ppt <- ur.pp(dlprod, type = 'Z-tau', model = 'constant', lags = 'short') #trend and intercept
summary(dlprod.ppt)
# -7.2118 < -2.90 and we reject the null that dlprod has unit root

dinvst.ppt <- ur.pp(dinvst, type = 'Z-tau', model = 'constant', lags = 'short') #intercept
summary(dinvst.ppt)
# -11.8926 < -2.89 and we reject the null that dinvst has a unit root

dhcap.ppt <- ur.pp(dhcap, type = 'Z-tau', model = 'constant', use.lag = 2) #trend and intercept
summary(dhcap.ppt)
# -1.8828 > -2.89 and we fail to reject the null that dhcap has unit root

dfindev.ppt <- ur.pp(dfindev, type = 'Z-tau', model = 'constant', use.lag = 7) #intercept
summary(dfindev.ppt)
#-5.0139 < -2.89 and we reject the null that findev has unit root

dtrade.ppt <- ur.pp(dtrade, type = 'Z-tau', model = 'constant', lags = 'short') #intercept
summary(dtrade.ppt)
#-6.4569 < -2.89 and we reject the null of unit root


#create a huxtable with results
hux(
  Series = c('LY', 'Openness', 'I', 'HCI', 'Findev', 'Lnlp', 'DLY', 'DOpenness', 'DI', 'DHCI', 'DFindev', 'DLnlp'), 
  H0  = c('gamma = 0', 'gamma = 0', 'gamma = 0', 'gamma = 0', 'gamma = 0', 'gamma = 0', 'gamma = 0', 'gamma = 0', 'gamma = 0', 'gamma = 0', 'gamma = 0', 'gamma = 0'),
  H1 = c('gamma < 0', 'gamma < 0', 'gamma < 0', 'gamma < 0', 'gamma < 0', 'gamma < 0', 'gamma = 0', 'gamma < 0', 'gamma < 0', 'gamma < 0', 'gamma < 0', 'gamma < 0'),
  DR = c('', '', '', '', '', '', '', '', '', '', '', ''),
  tcal = c('', '', '', '', '', '', '', '', '', '', '', ''),
  tcrit = c('', '', '', '', '', '', '', '', '', '', '', ''),
  add_colnames = TRUE
)                               %>%
  set_right_padding(20)           %>%
  set_left_padding(20)            %>% 
  set_bold(1, 1:6, TRUE)          %>% 
  set_bottom_border(1, 1:6, 1)    %>%
  set_align(1:6, 2, 'right')      %>%
  set_number_format(1)            %>% 
  xtable()
