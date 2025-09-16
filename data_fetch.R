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
library(data.table)
library(httr2)
library(fredr)

# downloaded series
## Real Economy:
# GDP Growth 
# Nominal GDP
# Real GDP
# Industrial production and it's composition
# Retail Sales
# PMI

## labour market
# Unemployment rate
# nonfarm payrolls
# labor force participation rate
# job openings and vacancies rate

## Inflation
# CPI
# Core CPI
# PCE inflation
# durable and non durable series
# sticky inflation

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
  
  req_json_tibble <- tibble(req_json$observations)
  colnames(req_json_tibble)[colnames(req_json_tibble) == "value"] <- series
  
  return(req_json_tibble)
  
}

test <- api_pull_data_request(series = "NGDPRNSAXDCUSQ", start = "2010-01-01", end = "2024-12-01", sort = "desc")






