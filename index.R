# AUTHOR: RAHUL CHADA

# PACKAGES

library(tidyverse)
library(rmarkdown)
library(knitr)
library(dplyr)
library(DT)
library(htmlTable)
library(lubridate)
library(tsibble)
library(ggplot2)
library(ggpubr)
library(plotly)
library(scales)
library(forecast)
library(zoo)
library(magrittr)
library(feasts)
library(tseries)
library(gapminder)
library(janitor)
library(gt)
library(patchwork)
library(kableExtra)
library(knitr)
library(data.table)
library(corrplot)
library(feasts)
library(fable)
library(psych)
library(tidyverse)
library(fable.prophet)
library(prophet)
library(lemon)
library(gridExtra)

# INTRODUCTION

price_data <- read.csv("DCOILWTICO.csv")
colnames(price_data) <- c('Date','Price')
#kable(head(price_data), align=rep('c', 2))

price_data$Date <- ymd(price_data$Date)
price_data <- price_data %>% filter(Date >= '2012-01-01')

head(price_data[-1,]) %>%
  kbl(align=rep('c', 2), caption = "Sample of the dataset") %>%
  kable_styling()

price_data$Price[price_data$Price == "."] <- NA # Replacing special character with NA
price_data <- price_data %>% drop_na(Price) # Removing rows with NA values
price_data$Price <- as.numeric(price_data$Price)

# Monthly data
price_data_monthly <- price_data %>%
  mutate(Date = yearmonth(Date)) %>%
  group_by(Date) %>%
  summarise(Price = mean(Price)) %>%
  as_tsibble(index = Date)

price_monthly_test <- price_data_monthly %>%
  filter(Date>=yearmonth('2020-01-01'))

price_monthly_train = price_data_monthly %>% 
  filter(Date<yearmonth('2020-01-01'))

head(price_monthly_train) %>%
  kbl(align=rep('c', 2), caption = "Sample of the average monthly crude oil prices") %>%
  kable_styling()

# EDA

oil_price_plot <- price_monthly_train %>%
  ggplot() +
  geom_line(aes(Date, Price)) +
  theme_bw() +
  xlab("Date") +
  ylab("Crude Oil Price") +
  ggtitle("Crude Oil Monthly Average Prices"
          ,subtitle = "Price Trend from 2012 to 2019 (in USD per barrel)"
  )

oil_price_plot

hist <- price_monthly_train %>%
  ggplot() +
  geom_histogram(aes(Price), bins = 50, fill="black", colour="black", alpha = 0.25) +
  theme_bw() +
  xlim(0, 150) +
  ylim(0,12) +
  xlab("Price") +
  ylab("Count") +
  ggtitle("Histogram of Crude Oil Prices")

hist

dens <- price_monthly_train %>%
  ggplot() +
  geom_density(aes(Price)) +
  theme_bw() +
  xlab("Price") +
  ylab("Density") +
  ggtitle("Density Plot of Crude Oil Prices")

dens

boxplot <- price_monthly_train %>%
  ggplot() +
  geom_boxplot(aes("", Price)) +
  theme_bw()  +
  ggtitle("Boxplot of Crude Oil Prices")

boxplot

summary_stats <- round(t(describe(price_monthly_train$Price)),2)

summary_stats <- cbind('Summary Statistic' = rownames(summary_stats), summary_stats)
row.names(summary_stats) <- NULL
colnames(summary_stats) <- c('Summary Statistic','Value')
#summary_stats$Value <- round(summary_stats$Value, digit = 2 )
summary_stats[-c(1,12),]  %>%
  kbl(align=rep('c', 2), caption = "Summary Statistics") %>%
  kable_styling()

price_ma_data <- price_monthly_train %>%
  arrange(Date) %>%
  mutate(
    value_ma_6 = rollapply(Price, 6, FUN = mean, align = "right", fill = NA),
    value_ma_9 = rollapply(Price, 9, FUN = mean, align = "right", fill = NA)
  )

price_ma_data_pivot <- price_ma_data %>%
  set_colnames(c("Date", "Monthly_Price", "6th Order", "9th Order")) %>%
  pivot_longer(-c('Date', 'Monthly_Price'), names_to = "MA_Order", values_to = "Price")

#price_ma_data_pivot

price_ma_data_pivot %>%
  ggplot() +
  geom_line(aes(Date, Monthly_Price), size = 1, alpha = 1.0) +
  geom_line(aes(Date, Price,color = MA_Order), size = 1, alpha =0.9) +
  theme_bw() +
  xlab("Date") +
  ylab("Price") +
  ggtitle("Moving Average of Crude Oil Prices")


### Seasonality

price_data_decomp <- price_monthly_train %>%
  arrange(Date) %>%
  mutate(
    ma_6_right = rollapply(Price, 6, FUN = mean, align = "right", fill = NA)
  ) %>%
  mutate(resid = Price - ma_6_right) %>%
  select(Date, Price, ma_6_right, resid)

price_monthly_train %>%
  model(
    classical_decomposition(Price)
  ) %>%
  components() %>%
  autoplot()


price_data_decomp %>%
  drop_na() %>%
  gg_lag(resid, geom = "point", lags = 1:12, ) +
  geom_smooth(aes(color=NULL),method='lm',color='red',se=F) +
  labs(title="Lag Plot of the residuals to test seasonality")

# ARIMA

par(mfrow = c(1, 2))
acf(price_monthly_train)
pacf(price_monthly_train)


price_data_roll <- price_monthly_train %>%
  mutate(
    price_mean = zoo::rollmean(
      Price, 
      k = 12, 
      fill = NA),
    price_sd = zoo::rollapply(
      Price, 
      FUN = sd, 
      width = 12, 
      fill = NA)
  )

price_data_rollmean <- price_data_roll %>%
  ggplot() +
  geom_line(aes(Date, Price)) +
  geom_line(aes(Date, price_mean),color='blue') +
  theme_bw() +
  ggtitle("Oil Prices Mean over Time (12 month rolling window)") +
  ylab("Price (in USD)") +
  xlab("Date")

price_data_rollsd <- price_data_roll %>%
  ggplot() +
  geom_line(aes(Date, price_sd)) +
  geom_smooth(aes(Date,price_sd),method='lm',se=F)+
  theme_bw() +
  ggtitle("Oil Prices Standard Deviation over Time (12 month rolling window)") +
  ylab("Price (in USD)") +
  xlab("Date")

#par(mfrow = c(1, 2))

grid.arrange(price_data_rollmean, price_data_rollsd, ncol=2)


price_trans_train <- price_monthly_train %>%
  mutate(price_log = log1p(Price))
# %>% mutate(price_boxcox = forecast::BoxCox(Price, lambda = "auto"))

price_trans_df <- price_trans_train %>%
  mutate(
    price_log_mean = zoo::rollmean(
      price_log, 
      k = 12, 
      fill = NA),
    price_log_sd = zoo::rollapply(
      price_log, 
      FUN = sd, 
      width = 12, 
      fill = NA)
  ) %>%
  mutate(log_price_diff = price_log - lag(price_log)) %>%
  mutate(
    price_log_diff_mean = zoo::rollmean(
      log_price_diff, 
      k = 12, 
      fill = NA),
    price_log_diff_sd = zoo::rollapply(
      log_price_diff, 
      FUN = sd, 
      width = 12, 
      fill = NA)
  )

price_log_roll_mean_plot <- price_trans_df %>%
  ggplot() +
  geom_line(aes(Date, price_log)) +
  geom_line(aes(Date, price_log_mean),color='blue') +
  theme_bw() +
  ggtitle("Log Transformed", subtitle = '12 month rolling mean') +
  ylab("Log-Price") +
  xlab("Date")

price_log_roll_sd_plot <- price_trans_df %>%
  ggplot() +
  geom_line(aes(Date, price_log_sd)) +
  geom_smooth(aes(Date,price_log_sd),method='lm',se=F)+
  theme_bw() +
  ggtitle("Log Transformed SD", subtitle = '12 month rolling SD') +
  ylab("SD Log-Price") +
  xlab("Date")

price_log_diff_mean_plot <- price_trans_df %>%
  drop_na() %>%
  ggplot() +
  geom_line(aes(Date, log_price_diff)) +
  geom_line(aes(Date, price_log_diff_mean),color='blue') +
  theme_bw() +
  ggtitle("First-Difference, Log", subtitle = "12 month rolling mean") +
  ylab("Value") +
  xlab("Date")

price_log_diff_sd_plot <- price_trans_df %>%
  ggplot()+
  geom_line(aes(Date,price_log_diff_sd))+
  geom_smooth(aes(Date,price_log_diff_sd),method='lm',se=F)+
  theme_bw() +
  ggtitle("First-Difference Log SD", subtitle = '12 month rolling SD') +
  ylab("Value") +
  xlab("Date")

grid.arrange(price_log_roll_mean_plot, price_log_roll_sd_plot, price_log_diff_mean_plot, price_log_diff_sd_plot, ncol=2, nrow = 2)

# Raw price - Non-stationary
raw_value_kpss = price_trans_df %>% 
  features(Price, unitroot_kpss)

# Log price - Non-stationary
log_value_kpss = price_trans_df %>%
  features(price_log, unitroot_kpss)

# Log,difference price  - Stationary
log_diff_value_kpss = price_trans_df %>%
  features(log_price_diff, unitroot_kpss)

kpss_testing <- bind_rows(raw_value_kpss, log_value_kpss, log_diff_value_kpss)
kpss_testing <- kpss_testing %>% 
  add_column(type = c('Original Dataset', 'Log Transformed', 'Log Differenced')) %>%
  relocate(type, .before = kpss_stat) %>%
  rename("Timeseries" = 'type', "KPSS Stat Value" = "kpss_stat", 'p-value' = 'kpss_pvalue')

head(kpss_testing,3) %>%
  kbl(align=rep('c', 3), caption = "KPSS test for stationarity") %>%
  kable_styling()

acf = price_trans_df %>%
  drop_na() %>%
  ACF(log_price_diff,lag_max=10) %>%
  autoplot()

pacf =  price_trans_df %>%
  drop_na() %>%
  fill_gaps() %>%
  PACF(log_price_diff) %>%
  autoplot()

acf + pacf

arima_models = price_trans_df %>%
  select(Date, price_log) %>%
  drop_na() %>%
  model(
    Model1 = ARIMA(price_log~pdq(0,1,1)+PDQ(0,0,0)),
    Model2 = ARIMA(price_log~pdq(0,1,0)+PDQ(0,0,0)),
    Model3 = ARIMA(price_log~pdq(1,1,0)+PDQ(0,0,0)),
    Model4 = ARIMA(price_log~pdq(1,1,1)+PDQ(0,0,0)),
    Model5 = ARIMA(price_log~pdq(2,1,0)+PDQ(0,0,0)),
    Model6 = ARIMA(price_log~pdq(0,2,1)+PDQ(0,0,0)),
    Model7 = ARIMA(price_log~pdq(0,1,2)+PDQ(0,0,0)),
    Model8 = ARIMA(price_log, approximation = F)
  )

# best_model = price_trans_df %>%
#   select(Date, price_log) %>%
#   drop_na() %>%
#   model(
#     ARIMA(price_log,approximation=F,
#     )
#   )

models_bic <- arima_models %>%
  glance() %>%
  arrange(BIC)

head(models_bic[1:6],8) %>%
  kbl(align=rep('c', 9), caption = "Best ARIMA models sorted by BIC values") %>%
  kable_styling()


best_model = price_trans_df %>%
  select(Date, price_log) %>%
  drop_na() %>%
  model(
    ARIMA(price_log~pdq(1,1,0)+PDQ(0,0,0))
  )

best_model %>%
  gg_tsresiduals()

ljung_lag3 <- best_model %>%
  augment() %>%
  features(.innov, ljung_box, lag = 3, dof = 1)

ljung_lag8 <- best_model %>%
  augment() %>%
  features(.innov, ljung_box, lag = 8, dof = 1)

Lag <- c('Lag 3', 'Lag 8')
ljung_box_results <- rbind(ljung_lag3, ljung_lag8)
ljung_box_results <- cbind(Lag, ljung_box_results)

head(ljung_box_results) %>%
  kbl(align=rep('c', 3), caption = "Ljung-Box Test on the Best ARIMA Model") %>%
  kable_styling()

# PROPHET

prophet_test <- price_monthly_test %>%
  rename(ds = Date, y = Price)

prophet_train = price_monthly_train %>%
  rename(ds = Date, y = Price)

orig_model = prophet(prophet_train, weekly.seasonality=FALSE, daily.seasonality = FALSE) # Train Model

orig_future = make_future_dataframe(orig_model,periods = 36, freq = 'month') # Create future dataframe for predictions

orig_forecast = predict(orig_model,orig_future) # Get forecast

plot(orig_model, fcst = orig_forecast) +
  add_changepoints_to_plot(orig_model) +
  ylab("Crude Oil Price (in $ per barrel)") +
  xlab("Date") +
  theme_bw() +
  ggtitle("Prophet Model on the Crude Oil Prices"
          ,subtitle = "Default Changepoints and Forecast for 36 periods"
  )

prophet_plot_components(orig_model,orig_forecast)

model = prophet(prophet_train,changepoint.prior.scale=0.15,n.changepoints = 25,changepoint.range=0.9,
                yearly.seasonality = FALSE)
#changepoints = c('2014-05-01','2016-01-01','2020-01-01','2021-06-01','2022-06-01', '2022-07-01'))

forecast = predict(model,orig_future)

plot(model,forecast)+
  add_changepoints_to_plot(model)+
  theme_bw()+
  xlab("Date")+
  ylab("Crude-Oil Price")


prophet_plot_components(model,forecast)

prophet_train$cap = 150
prophet_train$floor = 0

log_future = orig_future %>%
  mutate(cap = 150, floor = 0)

log_model = prophet(prophet_train, growth = 'logistic',changepoint.prior.scale=0.5,n.changepoints = 25,changepoint.range=0.9, weekly.seasonality = FALSE, yearly.seasonality = FALSE, daily.seasonality = FALSE) # Logistic Growth Model

log_forecast = predict(log_model,log_future) # Get forecast

plot(log_model,log_forecast) +
  add_changepoints_to_plot(log_model)+
  ylab("Crude Oil Price (in $ per barrel)") +
  xlab("Date") +
  theme_bw() +
  ggtitle("Prophet Model on the Crude Oil Prices"
          ,subtitle = "Forecast for 36 months from 2020-02 to 2023-12"
  )


price_monthly_train_cv <- price_monthly_train |>
  stretch_tsibble(.init = 36, .step = 6)

#best_arima <- function(x, h){forecast(Arima(log(x), order=c(1,1,0)), h=h)}
#e <- tsCV(price_monthly_train, best_arima, h=12)

#Fit the same model with a rolling window of length 30
#e <- tsCV(price_monthly_train, best_arima, h=12, window=1, initial = 1)

naive_model <- price_monthly_train_cv %>%
  model(Naive = NAIVE(Price)) %>%
  forecast(h = 12) %>%
  group_by(.id) %>%
  mutate(h = row_number()) %>%
  ungroup() %>%
  as_fable(response = "Price", distribution = Price)

accuracy_naive <- naive_model %>%
  accuracy(price_monthly_train, by = c("h", ".model"))
# accuracy(price_monthly_train, by = c("h", ".model")) |>
# ggplot(aes(x = h, y = MASE)) +
# geom_point()

best_arima <- price_monthly_train_cv %>%
  model(
    Arima = ARIMA(log(Price),approximation=F)
  ) %>%
  forecast(h = 12) %>%
  group_by(.id) %>%
  mutate(h = row_number()) %>%
  ungroup() %>%
  as_fable(response = "Price", distribution = Price)

accuracy_arima <- best_arima %>%
  accuracy(price_monthly_train, by = c("h", ".model"))

# Best Prophet Model
#best_prophet <- prophet(prophet_train, changepoint.prior.scale=0.15, n.changepoints = 25, changepoint.range=0.9, weekly.seasonality = FALSE, yearly.seasonality = FALSE, daily.seasonality = FALSE)
#prophet_cv <- cross_validation(best_prophet, initial = 3*365, period = 180, horizon = 365, units = 'days')
# prophet_cv <- cross_validation(best_prophet, initial = 3*365, period = 180, horizon = 180, units = 'days')
# metrics1 = performance_metrics(prophet_cv)

best_prophet <- price_monthly_train_cv %>% 
  # select(ds,y) %>%
  model(
    Prophet = fable.prophet::prophet(Price ~ growth("linear", n_changepoints = 25, changepoint_range = 0.9,
                                                    changepoint_prior_scale = 0.15))
  ) %>%
  forecast(h = 12) %>%
  group_by(.id) %>%
  mutate(h = row_number()) %>%
  ungroup() %>%
  as_fable(response = "Price", distribution = Price)

accuracy_prophet <- best_prophet %>%
  accuracy(price_monthly_train, by = c("h", ".model"))

accuracy_comparison <- accuracy_naive %>% 
  bind_rows(accuracy_arima) %>% 
  bind_rows(accuracy_prophet)

accuracy_comparison <- accuracy_comparison %>%
  rename(Horizon = h, Model = .model)

accuracy_comparison %>%
  ggplot()+
  geom_line(aes(Horizon,RMSE, color = Model)) +
  theme_bw() +
  xlab("Horizon (in Months)") +
  ylab("RMSE") +
  ggtitle("Model Performance Comparison Across Horizon"
          ,subtitle = "Comparing Naive, Best Arima, Best Prophet Model"
  ) +
  xlim(0, 13)


# FORECAST

best_model = price_monthly_train %>%
  model(Naive = NAIVE(Price))

best_model %>%
  forecast(h=38) %>%
  autoplot(
    price_monthly_train %>%
      select(Date,Price) %>%
      bind_rows(
        price_monthly_test %>%
          as_tsibble()
      )
  ) +
  geom_vline(aes(xintercept = ymd("2020-01-01")), color = "red", linetype = "dashed") +
  ggtitle("3-year Forecast vs Actual of crude-oil prices", subtitle = 'Naive Forecast')


predicted <- best_model %>%
  forecast(h=38) %>%
  as_tsibble() %>%
  select (Date, .mean) %>%
  rename(predicted = .mean)

perf_naive <- bind_cols(predicted, price_monthly_test$Price) %>%
  rename(actual = ...3)

out_rmse <- sqrt(mean((perf_naive$actual - perf_naive$predicted)^2,na.rm=T))
out_mape <- mean(abs((perf_naive$actual-perf_naive$predicted)/perf_naive$actual)) * 100

perf_metrics_test <- data.frame(out_rmse,out_mape)
colnames(perf_metrics_test) <- c('RMSE', 'MAPE')

head(perf_metrics_test) %>%
  kbl(align=rep('c', 2), caption = "Performance Metrics on the Test Dataset") %>%
  kable_styling()
