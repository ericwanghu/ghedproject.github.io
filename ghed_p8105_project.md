P8105 Final Project
================

``` r
library(tidyverse)
library(readxl)

knitr::opts_chunk$set(
  fig.width = 6,
  fig.asp = .6,
  out.width = "90%"
)
theme_set(theme_bw() + theme(legend.position = "bottom"))
options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

scale_coulour_discrete = scale_color_viridis_d
scale_fill_discrete = scale_fill_viridis_d
```

## Data Import

``` r
ghed_df <- 
  read_excel("data/GHED_data.xlsx")
```
