# REQUIREMENTS & OBJECTIVE

# DATA 
# - Macroeconomic indicators
# - time series by construct

# TIME SERIES ANALYSIS
# - Forecasting univariate : ARIMA, ETS, GARCH, PROPHET, ARIMAX,
#                            NNETAR, OLS, state space model
# - Multivariate: VAR, SVAR, State Space model

# OUTPUT
# - Rmarkdown report, rendered to pdf or html files

# FILE STRUCTURE 
# - data_fetch.R -> various functions to fetch series
# - utils.R -> Misc/Helper functions
# - univariate_analysis : - arima.R
#                         - ols.R
#                         - machine_learning.R
#                         - state_space.R
#                         - compare_methods.R -> select top 3 best models
# - multivariate_analysis : - var.R
#                           - svar.R
#                           - maybe more models
#                           - compare_methods.R -> select top 3 models
# - main.rmd 

# Data fetch section
# - BPS API 
# - BI API
# - fredr
# - WDI
# - world bank
# - quandl

library(tidyverse)
library(lubridate)
library(data.table)
library(httr2)
library(fredr)
library(cli)

# downloaded series
## Real Economy:
# GDP Growth 
# Nominal GDP - GDP
# Real GDP - GDPC1
# Industrial production and it's composition
# Industrial Production:
# - TOTAL INDPRO
# - manufacturing IPMAN
# - durable manufacturing IPDMAN
# - nondurable manufacturing IPNMAN
# - mining IPMINE
# - utilities IPUTIL
# Retail Sales :
# - Retail Trade RSXFS
# - Retail Trade and Food Services RSAFS
# - Food services and drinking places RSFSDP
# - Furniture and Home Furnishings Stores RSFHFS
# - Clothing and Clothing Accessory Stores RSCCAS
# - Retail Trade and Food Services, Excluding Motor Vehicle and Parts Dealers RSFSXMV
# - Nonstore Retailers RSNSR
# - Gasoline Stations RSGASS
# - Grocery Stores RSGCS
# - Motor Vehicle and Parts Dealers RSMVPD
# PMI

## labour market
# Unemployment rate UNRATE
# nonfarm payrolls PAYEMS
# labor force participation rate 25-54 yrs LNS11300060
# job openings and vacancies rate: 
# - job Openings: Total Nonfarm JTSJOL
# - job Openings: Construction JTS2300JOL
# - Job Openings: Manufacturing JTS3000JOL
# - Job Openings: Total Private JTS1000JOL
# - Job Openings: Professional and Business Services (JTS540099JOL)
# - Job Openings: Information (JTU5100JOL)
# - Job Openings: Leisure and Hospitality (JTS7000JOL)
# - Job Openings: Health Care and Social Assistance (JTS6200JOL)
# - job Openings: Health Care and Social Assistance (JTS6200JOL)
# - Job Openings: Retail Trade (JTS4400JOL)
# - Job Openings: Government (JTS9000JOL)
# - Job Openings: Finance and Insurance (JTU5200JOL)
# - Job Openings: Transportation, Warehousing, and Utilities (JTU480099JOL)
# - Job Openings: State and Local (JTS9200JOL)
# Average Weekly Hours of Production Workers — AWHMAN 
# verage Hourly Earnings — CES0500000003

## Inflation
# CPI
# Core CPI & sticky inflation - (CORESTICKM159SFRBATL)
# PCE inflation 
# durable and non durable series

## Monetary policy & financial markets
# Federal fund rate
# treasury yields (10Y)
# Equity indicies
# credit spreads

## External sector
# Trade balance
# USD index

## Expectations
# consumer sentiment
# market-implied inflation expectations 


# search function look up
api_search_request <- function(..., search_text, search_tag, limit = 100){
  params <- list(
    ...,
    search_text = search_text,
    tag_names = search_tag,
    limit = limit,
    file_type = "json"
  )
  
  req_json <- request("https://api.stlouisfed.org/fred/series") |> 
    req_url_path_append("search") |> 
    req_url_query(!!!params,`api_key` = Sys.getenv("FRED_API_KEY")) |> 
    req_perform() |> 
    resp_body_json(simplifyVector = TRUE)
  
  req_body <- tibble(req_json$seriess)
  
  return(req_body)
}

# series info function (name extraction)
api_series_info_request <- function(..., series) {
  params <- list(
    ...,
    series_id = series,
    file_type = "json"
  )
  
  req_json <- request("https://api.stlouisfed.org/fred/series") %>% 
    req_url_query(!!!params, `api_key` = Sys.getenv("FRED_API_KEY")) %>% 
    req_perform() %>% 
    resp_body_json(simplifyVector = TRUE)
  
  return(req_json)
}

## pull data function
api_pull_data_request <- function(..., series, start, end, sort = "desc") {
  if (!is.character(series)) {
    stop("Ensure the series name is in character")
  }
  
  params <- list(
    ...,
    series_id = series,
    observation_start = start,
    observation_end = end,
    sort_order = sort,
    file_type = "json"
  )
  
  req_json <- request("https://api.stlouisfed.org/fred/series") |> 
    req_url_path_append("observations") |> 
    req_url_query(!!!params, `api_key` = Sys.getenv("FRED_API_KEY")) |> 
    req_perform() |> 
    resp_body_json(simplifyVector = TRUE)
  
  # use series info function
  series_info <- api_series_info_request(series = series)$seriess

  # pull useful information
  series_title <- series_info$title
  filter_series_info <- series_info[ ,c("id", "title", "frequency", "units", "units_short", "seasonal_adjustment", "last_updated", "notes")]
  
  req_json_tibble <- tibble(req_json$observations) 
 # colnames(req_json_tibble)[colnames(req_json_tibble) == "value"] <- series_title
  req_json_tibble <- req_json_tibble %>%
    mutate(across(where(is.character) & !c(value), ~ as.Date(.x, format = "%Y-%m-%d"))) %>% 
    bind_cols(filter_series_info)
  
  return(req_json_tibble)
}


# check frequency
check_frequency <- function(data) {

  date_vec <-sort(data$date)
  
  if(is.Date(date_vec)){
    
    diff_date <-  floor(as.numeric(mean(diff(date_vec))))

    if (diff_date == 30) {
      
      data["frequency"] <- "monthly"
      
    } else if (diff_date == 90) {
      
      data["frequency"] <- "quarterly"
      
    } else {
      
      data["frequency"] <- "annual"
    }
  }
  return(data)
}

# function to check for available series
update_fun_text <- function(series_data) {
  cli_h1("Newer time-series release is/are available to download.")
  series_update <- series_data %>% filter(need_to_update_series == TRUE)
  series_update_id <- series_update$id
  for (i in series_update_id) {
    row_data <- series_update[series_update$id == i, ]
    series_id <- row_data$id
    series_title <- row_data$title
    series_queried_date <- row_data$queried_end_date
    series_latest_date <- row_data$last_updated
    cli_h3(paste0("Series ID: ", series_id))
    cli_li(paste0("Title: ", series_title))
    cli_li(paste0("Queried Date: ", series_queried_date))
    cli_li(paste0("Latest/recent available series: ", series_latest_date))
  }
}

# function to pull various series based on macroeconomic indicator theme
pull_data_fred <- function(series_vector, start_date, end_date) {
  
  # container list
  series_container <- list()
  
  cli_h1("Downloading time-series data from FRED")
  
  for (i in seq_along(series_vector)) {
    
    # print download series
    cli_li(paste0(series_vector[i], " start: ", start_date," end: ", end_date))
    
    # add try catch
    series_data <- tryCatch({
      
      data <- api_pull_data_request(series = series_vector[i], start = start_date, end = end_date, sort = "desc") 
      
    }, error = function(e) {
      message("Error for series ", series_vector[i], ". Please recheck parameters or connection.")
      print(e)
      data <- data.frame(realtime_start = NA,
                         realtime_end = NA,
                         date = NA,
                         value = NA,
                         id = NA,
                         title = NA,
                         frequency = NA,
                         units = NA,
                         units_short = NA,
                         seasonal_adjustment = NA,
                         last_updated = NA,
                         notes = NA)
    })

    # change the i with the series ID
    series_container[[series_vector[i]]] <- data
    
  }
  # check for recent data updates
  # if newer version of the series exist then inform the user
  # delete and add recent series
  full_data <- bind_rows(series_container)
  check_full_data <- full_data %>% 
    group_by(across(!c("value", "date"))) %>%
    summarise(n = n()) %>% 
    select(-n) %>% 
    mutate(queried_end_date = ymd(end_date),
           last_updated = date(ymd_hms(last_updated)),
           need_to_update_series = if_else(as.numeric(queried_end_date - last_updated) > 0, FALSE, TRUE))
  
  if(any(check_full_data$need_to_update_series)) {
    update_fun_text(check_full_data)
  } else {
    cli_alert_success("All of the queried series are up to date, Awesome work!")
  }
    
  return(full_data)
}

# Download Real economy time series data
real_economy_id <- c("GDP", "GDPC1",
                     "INDPRO", "IPMAN", "IPDMAN", "IPNMAN", "IPMINE", "IPUTIL",
                     "RSXFS", "RSAFS", "RSMVPD")

real_economy_data <- pull_data_fred(series_vector = real_economy_id, start_date = "2000-01-30", end_date = "2025-09-22")

# labour market 
labour_market_id <- c("UNRATE", "PAYEMS", "LNS11300060", "JTSJOL", "AWHMAN", "CES0500000003")

labour_market_data <- pull_data_fred(series_vector = labour_market_id, start_date = "2000-01-30", end_date = "2025-09-22")


