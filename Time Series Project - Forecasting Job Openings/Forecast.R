library(TSA)
library(astsa)
library(forecast)
library(dplyr)
library(ggplot2)

# DATA PREPARATION

d = read.csv('https://dxl-datasets.s3.amazonaws.com/data/industrial_data.csv')%>%
  filter(date >= '2010-01-01')

## create time series object
open = ts(d$opening, start=c(2010, 01), frequency=12)
layoff = ts(d$layoffs, start=c(2010, 01), frequency=12)
spend = ts(d$spend, start=c(2010, 01), frequency=12)
employees = ts(d$employees, start=c(2010, 01), frequency=12)

## split to train and test data
open_train = window(open, start=c(2010, 1), end=c(2020,12))
layoff_train = window(layoff, start=c(2010, 1), end=c(2020,12))
spend_train = window(spend, start=c(2010, 1), end=c(2020, 12))
employees_train = window(employees, start=c(2010, 1), end=c(2020, 12))

open_test = window(open, start=2021)
layoff_test = window(layoff, start=2021)
spend_test = window(spend, start=2021)
employees_test = window(employees, start=2021)

plot(open)
plot(layoff)
plot(spend)
plot(employees)

#MODEL FITTING

## Linear Regression
m_lin =tslm(open_train ~ poly(trend, 2) + season)

round(summary(m_lin)$coefficients, 1)

f_lin = forecast(m_lin, h=12)
autoplot(open_train) +
  autolayer(fitted(m_lin))

autoplot(open_test) +
  autolayer(f_lin, lwd=2) + 
  autolayer(open_test, lwd=2)

accuracy(f_lin$mean, open_test)


## STL

m_stl = stl(open_train, s.window = 'periodic')

plot(m_stl)

f_stl = forecast(m_stl, h=12)
stl_fit = m_stl$time.series[, 1] + m_stl$time.series[, 2]
autoplot(open_train) + 
  autolayer(stl_fit)

autoplot(open_test) + 
  autolayer(f_stl, lwd=2) + 
  autolayer(open_test, lwd=2)

accuracy(f_stl$mean, open_test)


## ETS

m_ets = ets(open_train, model='ZZZ', lambda = 0)

plot(m_ets)

f_ets = forecast(m_ets, h=12)
autoplot(open_train) +
  autolayer(m_ets$fitted)

autoplot(open_test) + 
  autolayer(f_ets, lwd=2) + 
  autolayer(open_test, lwd=2)

accuracy(f_ets$mean, open_test)


## ARIMA

plot(open_train)

### transformation

BoxCox.ar(open_train, method = 'b')  

#choose to use log
plot(log(open_train))
plot(diff(log(open_train)))

# pdq (3,1,3) or (1,1,4)?  PDQ (1,0,0)
acf2(diff(log(open_train), lag=12))
eacf(diff(log(open_train), lag=12))

# after overfitting, chose (3,1,3)(1,0,0)
m_arima = Arima(open_train, 
                order=c(3, 1, 3), seasonal=c(1, 0, 0), lambda = 0)

f_arima = forecast(m_arima, h=12)
autoplot(open_train) + 
  autolayer(fitted(m_arima))

autoplot(open_test) + 
  autolayer(f_arima, lwd=2) + 
  autolayer(open_test, lwd=2)

accuracy(f_arima$mean, open_test)


#ARIMAX

employees_lag = c(stats::lag(employees_train)[-1], employees_train[132])
month = factor(season(open_train), levels=c('January', 'February', 'March', 'April', 'May','June', 'July', 'August', 'September', 'October', 'November', 'December'))
x_vars = model.matrix( ~ employees_lag +  month)[ , -1]
m_arimax = Arima(open_train, xreg=x_vars, order=c(3, 1, 3), seasonal=c(1, 0, 0), method="ML", lambda = 0)
new_x_vars = cbind(employees_lag = c(employees_train[132], stats::lag(employees_test)[-12]), 
                   monthFebruary = c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                   monthMarch = c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                   monthApril = c( 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0),
                   monthMay = c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0 ),
                   monthJune = c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0 ),
                   monthJuly = c( 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
                   monthAugust = c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0 ),
                   monthSeptember = c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0 ),
                   monthOctober = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0 ),
                   monthNovember = c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0),
                   monthDecember = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 )
)
f_arimax = forecast(m_arimax, h=12, xreg=new_x_vars)

autoplot(open_train) + 
  autolayer(fitted(m_arimax))

autoplot(open_test) + 
  autolayer(f_arimax, lwd=2) +
  autolayer(open_test, lwd=2)

accuracy(f_arimax$mean, open_test)


##Vector Autoregression

m_var = VAR(cbind(open_train, employees_train, layoff_train, spend_train), 
            p=4, 
            season = 12)

round(m_var$varresult$open_train$coefficients, 1)

f_all_var = predict(m_var, n.ahead=12)
fanchart(f_all_var)
f_var = ts(f_all_var$fcs$open_train[, 1:3], start=c(2021, 1), frequency=12)

var_fits = ts(fitted(m_var)[,1], start=c(2010,1), frequency=12)
autoplot(open_train) + 
  autolayer(var_fits)

accuracy(f_var[, 1], open_test)


##Neural Network Autoregression

m_nnet = nnetar(open_train, p=10, P=1, size=3, lamba=0, decay = 0.05)

f_nnet = forecast::forecast(m_nnet, h=12)
autoplot(open_train) + 
  autolayer(fitted(m_nnet))

autoplot(open_test, col='black') + 
  autolayer(f_nnet, col='red')

accuracy(f_nnet$mean, open_test)


##LM
m_lin = tslm(open_train ~ poly(trend,2) + season)
f_lin = forecast(m_lin, h=12)
res = m_lin$residuals
acf2(res)
eacf(res)
plot(res)
arima_for_resids = Arima(res,order = c(1,1,5)) 
summary(arima_for_resids)
f_resids = forecast(arima_for_resids,h=12)$mean
f_lin_ts = f_lin$mean + f_resids
accuracy(f_lin_ts, open_test)

#Forecast Results
autoplot(open_test) + 
  autolayer(f_lin$mean) +
  autolayer(f_stl$mean) +
  autolayer(f_ets$mean) + 
  autolayer(f_arima$mean) + 
  autolayer(f_arimax$mean) + 
  autolayer(f_var[, 1]) + 
  autolayer(f_nnet$mean) 


# Model Performance

acc = rbind(
  data.frame(accuracy(f_lin$mean, open_test), row.names='Linear'),
  data.frame(accuracy(f_stl$mean, open_test), row.names='STL'),
  data.frame(accuracy(f_arima$mean, open_test), row.names='ARIMA'),
  data.frame(accuracy(f_arimax$mean, open_test), row.names='ARIMAX'),
  data.frame(accuracy(f_var[, 1], open_test), row.names='VAR'),
  data.frame(accuracy(f_nnet$mean, open_test), row.names='NNET'),
  data.frame(accuracy(f_ets$mean, open_test), row.names='ETS')
)
acc$Model = rownames(acc)

ggplot(acc, aes(x=reorder(Model, RMSE), y=RMSE)) + 
  geom_col() + 
  coord_flip() + 
  labs(x='')

## decide to pick arimax, ets, lm, nnet, vector regression to combine
## Combining Forecasts / Averaging
f_avg = ts(c(f_arimax$mean + f_ets$mean + f_lin$mean*2 + f_nnet$mean + f_var[,1]*1/2)/5, 
           start=c(2021, 1), frequency=12)
autoplot(open_test) + 
  autolayer(f_avg)

accuracy(f_avg, open_test)


##Plot the final performance
data.frame(
  Model = c(rep('Truth', 12), rep('Linear', 12), rep('STL', 12), rep('ETS', 12), rep('ARIMA', 12), rep('ARIMAX', 12), rep('VAR', 12), rep('NNET', 12), rep('AVERAGE', 12)),
  Time = rep(time(open_test), 9),
  Forecast = c(
    as.numeric(open_test), f_lin$mean, f_stl$mean, f_ets$mean, f_arima$mean, f_arimax$mean, f_var[, 1], f_nnet$mean, f_avg)
) %>%
  ggplot(aes(x=Time, y=Forecast, col=Model)) + 
  geom_line(size=1) + 
  scale_color_manual(values = c("Truth" = "black", "Linear" = "grey70", "STL" = "grey70", "ETS" = "grey70", "ARIMA" = "grey70", "ARIMAX" = "grey70", "VAR" = "grey70", "NNET" = "grey70", "AVERAGE" = "red")) +
  labs(color = "Model") +
  theme(legend.position = "bottom")


#OUTCOME FPRECAST for 2022
#use arimax, ets, lm, nnet, vector regression to combine
#lm
m_lin =tslm(open ~ poly(trend, 2) + season)
f_lin_22 = forecast(m_lin, h=12)

#ets
m_ets = ets(open, model='ZZZ', lambda = 0)
f_ets_22 = forecast(m_ets, h=12)

#arimax
employees_lag = c(stats::lag(employees)[-1], employees[144])
month = factor(season(open), levels=c('January', 'February', 'March', 'April', 'May','June', 'July', 'August', 'September', 'October', 'November', 'December'))
x_vars = model.matrix( ~ employees_lag +  month)[ , -1]
m_arimax = Arima(open, xreg=x_vars, order=c(3, 1, 3), seasonal=c(1, 0, 0), method="ML", lambda = 0)
new_x_vars = cbind(employees_lag = c(employees[144], stats::lag(employees)[-12]), 
                   monthFebruary = c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                   monthMarch = c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                   monthApril = c( 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0),
                   monthMay = c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0 ),
                   monthJune = c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0 ),
                   monthJuly = c( 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
                   monthAugust = c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0 ),
                   monthSeptember = c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0 ),
                   monthOctober = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0 ),
                   monthNovember = c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0),
                   monthDecember = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 )
)
f_arimax_22 = forecast(m_arimax, h = 12, xreg = new_x_vars[1:12,])

#nnet
m_nnet = nnetar(open, p=10, P=1, size=3, lamba=0, decay = 0.05)

f_nnet_22 = forecast::forecast(m_nnet, h=12)

#vector regression
m_var = VAR(cbind(open, employees, layoff, spend), 
            p=4, 
            season = 12)

round(m_var$varresult$open$coefficients, 1)

f_all_var = predict(m_var, n.ahead=12)
f_var_22 = ts(f_all_var$fcs$open[, 1:3], start=c(2022, 01), frequency=12)

#AVERAGE
f_avg_22 = ts(c(f_arimax_22$mean + f_ets_22$mean + f_lin_22$mean*2 + f_nnet_22$mean + f_var_22[,1]*1/2)/5, 
           start=c(2022, 1), frequency=12)

#OUTPUT FORECAST FILE
write.csv(f_avg_22, file = 'forecast_22.csv')



