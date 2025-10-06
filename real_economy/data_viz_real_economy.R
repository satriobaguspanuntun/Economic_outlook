library(tidyverse)
library(ggplot2)
library(lubridate)
library(gt)
library(gtExtras)

options(scipen = 999)
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
      legend.position = c(0.12,.85),
      legend.text = element_text(size = rel(0.9))
    )
}

# GDP chart 
gdp_line_chart <- function(data, cutoff, type = c("qoq_annualised", "qoq_growth", "yoy_growth")) {
  
  data_graph <- data %>% 
    filter(id %in% c("GDP", "GDPC1") & date >= cutoff) %>% 
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
# stacked bar chart
gdp_component_chart <- function(data1, data2, cutoff) {
  
  chart_data <- data1 %>%
    filter(id %in% c("PCECC96", "GPDIC1", "NETEXC", "GCEC1", "A960RX1Q020SBEA")) %>% 
    select(date, value, id, title, frequency, units) %>% 
    filter(date >= cutoff) %>% 
    mutate(date = as.Date(date),
           value = as.numeric(value),
           component = case_when(id == "PCECC96" ~ "Consumption",
                                 id == "GPDIC1" ~ "Investment", 
                                 id == "NETEXC" ~ "NetXM",
                                 id == "GCEC1" ~ "Government",
                                 id == "A960RX1Q020SBEA" ~ "Residual")) %>% 
    group_by(id) %>% 
    arrange(date) %>% 
    mutate(
      qoq_growth = (abs(value) / abs(lag(value)) - 1),                 # % QoQ
      qoq_annualised = ((abs(value) / abs(lag(value)))^4 - 1),          # % SAAR
      yoy_growth = (abs(value) / abs(lag(value, 4)) - 1)
    ) 
  
  # stacked bar chart
  sb_comp <- chart_data %>% 
    ggplot(aes(x = date, y = value, fill = component)) +
    geom_area(position = "stack", alpha = 1, linewidth = 0.2, color = "white") +
    scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
    scale_y_continuous(labels = scales::comma, breaks = function(x) pretty(x, n = 10)) +
    scale_fill_brewer(palette = "Set2") +
    labs(
      title  = "GDP Components over Time",
      subtitle = "Consumption, Investment, Government & Net Exports",
      x      = NULL,
      y      = "GDP (billions $)",
      fill   = "Component",
      caption = "Source: FRED, BEA"
    ) +
    theme_economist_custom()
  
  # QOQ chart
  qoq_comp <- chart_data %>% 
    drop_na() %>% 
    filter(!component %in% c("Residual", "NetXM") & date >= cutoff) %>% 
    ggplot(aes(x = date, y = yoy_growth, colour = component)) +
    geom_line(size = 1.2) +
    scale_x_date(date_labels = "%Y", date_breaks = "1 years") +
    scale_y_continuous(labels = scales::percent, breaks = function(x) pretty(x, n = 10)) +
    labs(
      title  = "YoY GDP Growth Components",
      subtitle = "Consumption, Investment, Government",
      x      = NULL,
      y      = "Percentage",
      fill   = "Component",
      caption = "Source: FRED, BEA"
    ) +
    theme_economist_custom()
  
  # stacked contribution
  real_gdp <- data2 %>% 
    filter(id == "GDPC1" & date >= cutoff) %>% 
    select(date, value, id, title, frequency, units) %>% 
    mutate(date = as.Date(date),
           value_gdp = as.numeric(value)) %>% 
    select(date, value_gdp)
  
  share <- chart_data %>%
    left_join(real_gdp, by = join_by(date)) %>% 
    arrange(date) %>% 
    group_by(id, component) %>% 
    mutate(share_lag = lag(value/value_gdp, 4))
  
  contrib <- share %>% 
    ungroup() %>% 
    mutate(contribution = yoy_growth * share_lag) %>% 
    select(date, component, contribution)
  
  contrib_comp <- contrib %>% 
    drop_na() %>% 
    ggplot(aes(x = date, y = contribution, fill = component)) +
    geom_col(position = "identity") +
    scale_x_date(date_labels = "%Y", date_breaks = "1 years") +
    scale_y_continuous(labels = scales::percent, breaks = function(x) pretty(x, n = 10)) +
    scale_fill_brewer(palette = "Set1") +
    labs(title = "Contributions to Real GDP Growth (YoY)",
         y = "Contribution to GDP Growth",
         x = NULL, 
         caption = "Source: FRED, BEA") +
    theme_economist_custom()
  
  return(list("stack_area_comp" = sb_comp, "qoq_line_comp" = qoq_comp, "contrib_comp" = contrib_comp))
}
qoq_gdp_line <- gdp_line_chart(data = real_economy_data, cutoff = "2018-01-01", type = "qoq_annualised")
qoq_gdp_line
yoy_gdp_line <- gdp_line_chart(data = real_economy_data, cutoff = "2018-01-01", type = "yoy_growth")
yoy_gdp_line
chart_gdp_comp <- gdp_component_chart(data1 = gdp_components, data2 = real_economy_data, cutoff = "2018-01-01")
chart_gdp_comp$contrib_comp

## GDP table
gdp_table_func <- function(data, cutoff, type = c("summary", "growth", "default")) {
  
  data_table <- real_economy_data %>% 
    filter(id %in% c("GDP", "GDPC1") & date >= cutoff) %>% 
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
  
  min_gdp_date <- unique(min(data_table$date))
  max_gdp_date <- unique(max(data_table$date))
  
  # summary stats table
  if (type == "summary") {
    gdp_freq <- unique(data_table$frequency)
    gdp_units <- unique(data_table$units)
    
    gdp_sum <- data_table %>% 
      ungroup() %>% 
      select(date, series, value) %>% 
      pivot_wider(names_from = series, values_from = value) %>% 
      arrange(date) %>% 
      summarise(
        across(`Nominal GDP`:`Real GDP`,
               list(
                 Mean = ~mean(., na.rm = TRUE),
                 Median = ~median(., na.rm = TRUE),
                 Min = ~min(., na.rm = TRUE),
                 Max =  ~max(., na.ram = TRUE),
                 SD = ~sd(., na.rm = TRUE),
                 N = ~sum(!is.na(.), na.rm = TRUE)
               ))
      ) %>% 
      pivot_longer(cols = everything(),
                   names_to = c("Series", "Statistics"),
                   names_sep = "_",
                   values_to = "Value") %>% 
      pivot_wider(names_from = Series, values_from = Value)
    
    # Render with gt
    gdp_stats_gt <- gdp_sum %>%
      gt(rowname_col = "Statistics") %>%
      tab_header(
        title = md("**Descriptive Statistics of U.S. GDP**"),
        subtitle = "Nominal vs Real GDP"
      ) %>%
      fmt_number(
        columns = c(`Nominal GDP`, `Real GDP`),
        decimals = 2,
        suffixing = TRUE
      ) %>%
      cols_label(
        `Nominal GDP` = "Nominal GDP (Billions $)",
        `Real GDP`    = "Real GDP (Billions, Chained $2017)"
      ) %>%
      # ---- TABLE OPTIONS ----
    tab_options(
      table.width = pct(80),
      table.font.names = c("Lato", "Helvetica", "Arial", "sans-serif"),
      table.font.size = 14,
      heading.title.font.size = 16, 
      heading.align = "left",
      heading.subtitle.font.size = 14,
      column_labels.font.weight = "bold",
      data_row.padding = px(6),
      row.striping.background_color = "rgba(245,245,245,0.6)",
      table.border.top.color = "transparent",
      table.border.bottom.color = "#444444",
      heading.border.bottom.color = "#444444"
    ) %>%
      
      # ---- TITLE STYLING ----
    tab_style(
      style = list(
        cell_text(weight = "bold", size = "large", color = "#1a1a1a")
      ),
      locations = cells_title(groups = "title")
    ) %>%
      
      # ---- SUBTITLE STYLING ----
    tab_style(
      style = list(
        cell_text(color = "#555555", style = "italic")
      ),
      locations = cells_title(groups = "subtitle")
    ) %>%
      
      # ---- ROW STRIPING ----
    opt_row_striping() %>%
      
      # ---- FOOTNOTE ----
    tab_source_note(
      source_note = md("**Source:** Bureau of Economic Analysis (BEA), calculations by author")
    )
    
    gdp_stats_gt
    
  } else if (type == "growth") {
    # growth table
    gdp_growth <- data_table %>% 
      ungroup() %>% 
      select(date, series, qoq_growth, qoq_annualised, yoy_growth) %>% 
      pivot_longer(cols = qoq_growth:yoy_growth,
                   names_to = "growth_var",
                   values_to = "growth_val") %>% 
      pivot_wider(names_from = growth_var, values_from = growth_val) %>% 
      filter(date >= floor_date(max_gdp_date - 90 * 8, unit = "month")) %>% 
      arrange(desc(date)) %>% 
      # mutate(across(qoq_growth:yoy_growth, ~paste0(round(.x * 100, 2), "%"))) %>% 
      mutate(date = paste0(year(date), " Q", quarter(date))) %>% 
      pivot_longer(cols = qoq_growth:yoy_growth, names_to = "growth_var", values_to = "value") %>% 
      pivot_wider(names_from = date, values_from = value) %>% 
      mutate(growth_var = case_when(growth_var == "qoq_growth" ~ paste0("QoQ Growth"),
                                    growth_var == "qoq_annualised" ~ paste0("QoQ Annualised"),
                                    growth_var == "yoy_growth" ~ paste0("YoY Growth")))
    
    gdp_growth_gt <- gdp_growth %>%
      group_by(series) %>%
      gt(rowname_col = "growth_var") %>% 
      tab_header(title = md("**Growth Rate Table of U.S. GDP**"),
                 subtitle = "Nominal vs Real GDP") %>% 
      fmt_percent(columns = where(is.numeric), decimals = 2) %>% 
      data_color(
        columns = where(is.numeric),
        colors = scales::col_numeric(
          palette = c("white", "#1f77b4"),  # white to blue
          domain = NULL  # auto-scales to data range
        )
      ) %>% 
    tab_options(
      table.width = pct(100),
      table.font.names = c("Lato", "Helvetica", "Arial", "sans-serif"),
      table.font.size = 14,
      heading.title.font.size = 16, 
      heading.align = "left",
      heading.subtitle.font.size = 14,
      column_labels.font.weight = "bold",
      data_row.padding = px(6),
      table.border.top.color = "transparent",
      table.border.bottom.color = "#444444",
      heading.border.bottom.color = "#444444",
      row.striping.background_color = "rgba(245,245,245,0.6)"
    ) %>%
    tab_style(
      style = list(
        cell_text(weight = "bold", size = "large", color = "#1a1a1a")
      ),
      locations = cells_title(groups = "title")
    ) %>%
    tab_style(
      style = list(
        cell_text(color = "#555555", style = "italic")
      ),
      locations = cells_title(groups = "subtitle")
    ) %>%
    tab_style(
      style = cell_text(color = "black", weight = "bold"),
      locations = cells_row_groups()
    ) %>% 
    opt_row_striping() %>%
    tab_source_note(
      source_note = md("**Source:** Bureau of Economic Analysis (BEA), calculations by author")
    )
    
    gdp_growth_gt
  }
  
}

gdp_table_func(data = real_economy_id, cutoff = "2015-01-01", type = "growth")

# GDP contribution table
gdp_contrib_table <- function(data1, data2, cutoff) {
  
  
  
  
  
  
}








## Industrial production
# IP table

## Retail trade
# retail trade chart
# Retail table



