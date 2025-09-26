library(tidyverse)
library(KFAS)
library(zoo)

### function for feauture extraction and data cleaning ###
# convery to quarterly ts object
gdp_df <- real_economy_data %>%
  filter(id == "GDPC1") %>% 
  mutate(date = as.Date(date), 
         gdp = as.numeric(value)) %>% 
  arrange(date)

first_year <- as.integer(year(min(gdp_df$date)))
first_quarter <- as.integer(quarter(min(gdp_df$date)))
gdp_ts <- ts(gdp_df$gdp, start = c(first_year, first_quarter), frequency = 4)

# -------- 1. Specify state-space model ----------------#
# local linear trend (level + slope), no seasonal
n <- length(gdp_ts)

# Observation equation matrices
Z <- matrix(c(1, 0), nrow = 1)
H <- matrix(NA)

# state transition
Tmat <- matrix(c(1, 1, 0, 1), 2, 2)
R <- diag(2)

Q <- matrix(NA, 2, 2)

# Build SSModel manually
ssm_manual <- SSModel(gdp_ts ~ SSMtrend(1, Q = 0.01 )+ 
                        SSMcycle(period = 20, Q = 0.01), H = 0.01)

# estimate parameters (H, Q elements)
fit_manual <- fitSSM(ssm_manual, 
                     inits = c(log(0.1), log(0.01), log(0.01)),
                     method = "BFGS")

fitted_model <- fit_manual$model

fit_manual$model$Q   # state noise variances
fit_manual$model$H   # observation variance


# run kalman filter 
out <- KFS(fitted_model, smoothing = c("state", "signal"))

# forecast h quarters
h <- 12
fc <- predict(fitted_model, n.ahead = h, interval = "prediction", level = 0.9)

# observed
obs_df <- data.frame(
  date = as.Date(as.yearqtr(time(gdp_ts))),
  gdp = as.numeric(gdp_ts),
  type = "observed"
)

# forecast
last_time <- time(gdp_ts)[length(gdp_ts)]
fc_time <- seq(from = last_time + 1/4, by = 1/4, length.out = h)
fc_dates <- as.Date(as.yearqtr(fc_time))

fc_df <- data.frame(
  date  = fc_dates,
  gdp   = fc[,"fit"],
  lower = fc[,"lwr"],
  upper = fc[,"upr"],
  type = "forecast"
)

combined <- bind_rows(obs_df, fc_df) %>% arrange(date)

#---------- 6. Compute growth rates ----------
  combined <- combined %>%
  mutate(
    qoq = (gdp / lag(gdp) - 1),
    qoq_saar =  ((gdp / lag(gdp))^4 - 1),
    yoy = (gdp / lag(gdp,4)) - 1
  )

p <- ggplot() +
  geom_line(data = obs_df, aes(x = date, y = gdp), color = "#1f77b4", size = 1.1) +
  geom_line(data = fc_df, aes(x = date, y = gdp), color = "#ff7f0e", size = 1.1, linetype = "dashed") +
  geom_vline(xintercept = max(obs_df$date), color = "black", linetype = "dotted") +
  labs(title = "GDP Forecast with KFAS (Local Linear Trend)",
       subtitle = paste0("Forecast horizon: ", h, " quarters"),
       x = "Date", y = "Real GDP (chained 2017 dollars)",
       caption = "Source: FRED/BEA GDPC1 | Model: KFAS local linear trend") +
  theme_minimal(base_size = 13) +
  scale_x_date(date_labels = "%Y", date_breaks = "4 years") +
  scale_y_continuous(labels = scales::comma)

print(p)
