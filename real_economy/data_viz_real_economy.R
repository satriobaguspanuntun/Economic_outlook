library(tidyverse)
library(ggplot2)
library(patchwork)

### function for real economy series charts ###

# Economist-style theme
theme_economist_custom <- function(base_size = 14, base_family = "serif") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      plot.title = element_text(face = "bold", size = rel(1.0), hjust = 0, margin = margin(b = 5)),
      plot.subtitle = element_text(size = rel(1.0), color = "gray30", hjust = 0, margin = margin(b = 10)),
      plot.caption = element_text(size = rel(0.8), color = "gray40", hjust = 0),
      axis.title = element_text(size = rel(1.0)),
      axis.text = element_text(size = rel(0.9), color = "black"),
      axis.line = element_line(color = "black", linewidth = 0.4),
      panel.grid.major.y = element_line(color = "gray85"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.title = element_blank(),
      legend.position = c(.2,.85),
      legend.text = element_text(size = rel(0.9))
    )
}

# GDP chart 
gdp_line_chart <- function(data, type = c("qoq_annualised", "qoq_growth", "yoy_growth")) {
  
  data_graph <- data %>% 
    filter(id %in% c("GDP", "GDPC1")) %>% 
    select(id, title, date, value, frequency, units, last_updated) %>% 
    group_by(id) %>% 
    arrange(date) %>% 
    mutate(
      value = as.numeric(value),
      qoq_growth = (value / lag(value) - 1),                 # % QoQ
      qoq_annualised = ((value / lag(value))^4 - 1),          # % SAAR
      yoy_growth = (value / lag(value, 4) - 1),
      series = ifelse(id == "GDP", "Nominal GDP", "Real GDP")
    ) 
    
  if(type == "qoq_annualised") {
    
    data_graph %>% 
      drop_na() %>% 
      ggplot(aes(x = date, y = qoq_annualised, colour = series)) +
      geom_line(size = 1.2) +
      scale_color_manual(values = c("Nominal GDP" = "#003f5c", "Real GDP" = "#bc5090")) +
      scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
      scale_y_continuous(labels = scales::percent, breaks = function(x) pretty(x, n = 10)) +
      labs(
        title = "US Nominal vs Real GDP",
        subtitle = "QoQ Annualised growth rate",
        x = NULL,
        y = "Percent",
        caption = "Source: FRED, BEA"
      ) +
      theme_economist_custom()
    
  } else if (type == "qoq_growth") {
    
    data_graph %>% 
      drop_na() %>% 
      ggplot(aes(x = date, y = qoq_growth, colour = series)) +
      geom_line(size = 1.2) +
      scale_color_manual(values = c("Nominal GDP" = "#003f5c", "Real GDP" = "#bc5090")) +
      scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
      scale_y_continuous(labels = scales::percent, breaks = function(x) pretty(x, n = 10)) +
      labs(
        title = "US Nominal vs Real GDP",
        subtitle = "QoQ Growth rate",
        x = NULL,
        y = "Percent",
        caption = "Source: FRED, BEA"
      ) +
      theme_economist_custom()
    
  } else if (type == "yoy_growth") {
    
    data_graph %>% 
      drop_na() %>% 
      ggplot(aes(x = date, y = yoy_growth, colour = series)) +
      geom_line(size = 1.2) +
      scale_color_manual(values = c("Nominal GDP" = "#003f5c", "Real GDP" = "#bc5090")) +
      scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
      scale_y_continuous(labels = scales::percent, breaks = function(x) pretty(x, n = 10)) +
      labs(
        title = "US Nominal vs Real GDP",
        subtitle = "YoY Growth rate",
        x = NULL,
        y = "Percent",
        caption = "Source: FRED, BEA"
      ) +
      theme_economist_custom()
    
  } else {
     
    data_graph %>% 
      drop_na() %>% 
      ggplot(aes(x = date, y = value, colour = series)) +
      geom_line(size = 1.2) +
      scale_color_manual(values = c("Nominal GDP" = "#003f5c", "Real GDP" = "#bc5090")) +
      scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
      scale_y_continuous(labels = scales::comma, breaks = function(x) pretty(x, n = 10)) +
      labs(
        title = "US Nominal vs Real GDP",
        subtitle = "Seasonally adjusted annual rate, billions of chained 2017 dollars",
        x = NULL,
        y = "Billions of Dollars",
        caption = "Source: FRED, BEA"
      ) +
      theme_economist_custom()
  }
  
}

# GDP component driver













