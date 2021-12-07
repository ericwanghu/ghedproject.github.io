P8105 Final Project
================

## Data Import

``` r
ghed_df <- 
  read_excel("data/GHED_data.XLSX")
```

### Safiya’s section

**Indicators** Primary health care - phc\_usd\_pc  
Infectious disease - dis1\_usd2019  
Noncommunicable disease - dis4\_usd2019  
Preventive care - hc6\_usd2019  
Curative care - hc1\_usd2019  
GDP - gdp\_pc\_usd  
CHE - che\_pc\_usd

**Primary Health Care Spending**

Primary health care spending by income level (USD per capita)

``` r
PHC_df <-
  ghed_df %>% 
    janitor::clean_names() %>% 
    filter(year == 2019) %>% 
    drop_na(phc_usd_pc) %>% 
    group_by(income_group) %>% 
    summarize(
      n_countries = n(),
      avg_phc = mean(phc_usd_pc))

PHC_df %>% 
  ggplot(aes(x = fct_relevel(as.factor(income_group), c("Low", "Low-Mid", "Up-Mid", "Hi")), y = avg_phc,  fill = income_group)) +
  geom_col() +
  labs(
    title = "Primary Health Care Spending",
    x = "Income level",
    y = "Average PHC spending (per capita USD)"
  ) + 
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.position = "none")
```

<img src="ghed_p8105_project_files/figure-gfm/unnamed-chunk-2-1.png" width="90%" />

Primary health care spending by income level (as as proportion of per
capita GDP)

``` r
PHC_df <-
  ghed_df %>% 
    janitor::clean_names() %>% 
    filter(year == 2019) %>% 
    drop_na(phc_usd_pc, gdp_pc_usd) %>% 
    group_by(income_group) %>% 
    summarize(
      n_countries = n(),
      avg_phc = mean(phc_usd_pc / gdp_pc_usd))

PHC_df %>% 
  ggplot(aes(x = fct_relevel(as.factor(income_group), c("Low", "Low-Mid", "Up-Mid", "Hi")), y = avg_phc,  fill = income_group)) +
  geom_col() +
  labs(
    title = "Primary Health Care Spending",
    x = "Income level",
    y = "Average PHC spending (as proportion of \n per capita GDP)"
  ) + 
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.position = "none")
```

<img src="ghed_p8105_project_files/figure-gfm/unnamed-chunk-3-1.png" width="90%" />

Primary health care spending by income level (as a proportion of per
capita health expenditure)

``` r
PHC_df <-
  ghed_df %>% 
    janitor::clean_names() %>% 
    filter(year == 2019) %>% 
    drop_na(phc_usd_pc, gdp_pc_usd) %>% 
    group_by(income_group) %>% 
    summarize(
      n_countries = n(),
      avg_phc = mean(phc_usd_pc / che_pc_usd))

PHC_df %>% 
  ggplot(aes(x = fct_relevel(as.factor(income_group), c("Low", "Low-Mid", "Up-Mid", "Hi")), y = avg_phc,  fill = income_group)) +
  geom_col() +
  labs(
    title = "Primary Health Care Spending",
    x = "Income level",
    y = "Average PHC spending (as proportion of \n per capita health expenditure)"
  ) + 
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.position = "none")
```

<img src="ghed_p8105_project_files/figure-gfm/unnamed-chunk-4-1.png" width="90%" />

**Preventive vs. Curative Spending**

Primary health care spending by income level (USD per capita)

``` r
prev_df <-
  ghed_df %>% 
    janitor::clean_names() %>% 
    filter(year == 2019) %>% 
    drop_na(hc6_usd) %>% 
    group_by(income_group) %>% 
    summarize(
      n_countries = n(),
      `Preventive` = mean(hc6_usd2019))

cur_df <-
  ghed_df %>% 
    janitor::clean_names() %>% 
    filter(year == 2019) %>% 
    drop_na(hc1_usd) %>% 
    group_by(income_group) %>% 
    summarize(
      n_countries = n(),
      `Curative` = mean(hc1_usd2019))

prev_cur_df <-
  left_join(prev_df, cur_df) %>% 
  pivot_longer(
    `Preventive`:`Curative`,
    names_to = "Type of Spending",
    values_to = "avg_spend"
  )
```

    ## Joining, by = c("income_group", "n_countries")

``` r
prev_cur_df %>% 
  ggplot(aes(x = fct_relevel(as.factor(income_group), c("Low", "Low-Mid", "Up-Mid", "Hi")), y = avg_spend, fill = `Type of Spending`)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Preventive vs. Curative Spending",
    x = "Income level",
    y = "Average spending \n (million USD)"
  ) + 
  theme(axis.text.x = element_text(angle = 90))
```

<img src="ghed_p8105_project_files/figure-gfm/unnamed-chunk-5-1.png" width="90%" />

Primary health care spending by income level (as as proportion of GDP)

``` r
prev_df <-
  ghed_df %>% 
    janitor::clean_names() %>% 
    filter(year == 2019) %>% 
    drop_na(hc6_usd, gdp_pc_usd) %>% 
    group_by(income_group) %>% 
    summarize(
      n_countries = n(),
      `Preventive` = mean(hc6_usd2019 / gdp_usd2019))

cur_df <-
  ghed_df %>% 
    janitor::clean_names() %>% 
    filter(year == 2019) %>% 
    drop_na(hc1_usd, gdp_pc_usd) %>% 
    group_by(income_group) %>% 
    summarize(
      n_countries = n(),
      `Curative` = mean(hc1_usd2019 / gdp_usd2019))

prev_cur_df <-
  left_join(prev_df, cur_df) %>% 
  pivot_longer(
    `Preventive`:`Curative`,
    names_to = "Type of Spending",
    values_to = "avg_spend"
  )
```

    ## Joining, by = c("income_group", "n_countries")

``` r
prev_cur_df %>%
  ggplot(aes(x = fct_relevel(as.factor(income_group), c("Low", "Low-Mid", "Up-Mid", "Hi")), y = avg_spend, fill = `Type of Spending`)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Preventive vs. Curative Spending",
    x = "Income level",
    y = "Average spending (as proportion \n of GDP)"
  ) + 
  theme(axis.text.x = element_text(angle = 90))
```

<img src="ghed_p8105_project_files/figure-gfm/unnamed-chunk-6-1.png" width="90%" />

Primary health care spending by income level (as proportion of health
expenditure)

``` r
prev_df <-
  ghed_df %>% 
    janitor::clean_names() %>% 
    filter(year == 2019) %>% 
    drop_na(hc6_usd, gdp_pc_usd) %>% 
    group_by(income_group) %>% 
    summarize(
      n_countries = n(),
      `Preventive` = mean(hc6_usd2019 / che_usd2019))

cur_df <-
  ghed_df %>% 
    janitor::clean_names() %>% 
    filter(year == 2019) %>% 
    drop_na(hc1_usd, gdp_pc_usd) %>% 
    group_by(income_group) %>% 
    summarize(
      n_countries = n(),
      `Curative` = mean(hc1_usd2019 / che_usd2019))

prev_cur_df <-
  left_join(prev_df, cur_df) %>% 
  pivot_longer(
    `Preventive`:`Curative`,
    names_to = "Type of Spending",
    values_to = "avg_spend"
  )
```

    ## Joining, by = c("income_group", "n_countries")

``` r
prev_cur_df %>%
  ggplot(aes(x = fct_relevel(as.factor(income_group), c("Low", "Low-Mid", "Up-Mid", "Hi")), y = avg_spend, fill = `Type of Spending`)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Preventive vs. Curative Spending",
    x = "Income level",
    y = "Average spending (as proportion \n of total health expenditure)"
  ) + 
  theme(axis.text.x = element_text(angle = 90))
```

<img src="ghed_p8105_project_files/figure-gfm/unnamed-chunk-7-1.png" width="90%" />

**Infectious vs. Non-communicable Diseases**

### Ragyie’s section

**Indicators for analysis**

-   Curative care, in million constant (2019) US$ —&gt; hc1\_usd2019
-   Preventive care, in million constant (2019) US$ —&gt; hc6\_usd2019
-   Infectious and Parasitic Diseases, in million constant (2019) US$
    —&gt; dis1\_usd2019
-   Noncommunicable Diseases (NCDs), in million constant (2019) US$
    —&gt; dis4\_usd2019
-   Primary Health Care (PHC) Expenditure per Capita in US$ —&gt;
    phc\_usd\_pc
-   GDP - gdp\_pc\_usd  
-   CHE - che\_pc\_usd
-   che\_usd2019
-   gdp\_usd2019

**Expenditure category analysis: WHO regions**

Will compare and plot the average in 2019 of spending in categories of
interest for each WHO region

**Primary health care**:

Average primary health care expenditure for each WHO region in million
constant USD$

``` r
phc_ghed = 
  ghed_df %>% 
  janitor::clean_names() %>% 
  filter(year == 2019) %>% 
  drop_na(phc_usd_pc) %>% 
  group_by(region_who) %>% 
  summarize(
    n_countries = n(),
    avg_primary = mean(phc_usd_pc)
  )
```

Barplot: Primary health care expenditure in million constant USD$

``` r
phc_ghed %>%   
  ggplot(aes(x = reorder(region_who, avg_primary), y = avg_primary, fill = region_who)) + 
  geom_bar(stat = "Identity") + 
  labs(
    title = "Primary health care spending by WHO region", 
    x = "WHO region", 
    y = "Average PHC spending in million constant USD$"
  ) + 
  theme(legend.position = "none")
```

<img src="ghed_p8105_project_files/figure-gfm/avg_phc_plot-1.png" width="90%" />

Average primary health care expenditure for each WHO region with
spending as a percent of GDP

To see how much different WHO regions are spending with regards to what
resources they actually have

``` r
phc_gdp_ghed =
  ghed_df %>% 
  janitor::clean_names() %>% 
  filter(year == 2019) %>% 
  drop_na(phc_usd_pc, gdp_pc_usd) %>% 
  group_by(region_who) %>% 
  summarize(
    n_countries = n(),
    avg_primary_gdp = mean(phc_usd_pc / gdp_pc_usd)
  )
```

Barplot: Primary health care expenditure with spending as a percent of
GDP

``` r
phc_gdp_ghed %>%   
  ggplot(aes(x = reorder(region_who, avg_primary_gdp), y = avg_primary_gdp, fill = region_who)) + 
  geom_bar(stat = "Identity") + 
  labs(
    title = "Primary health care spending by WHO region", 
    x = "WHO region", 
    y = "Average PHC spending per capita GDP"
  ) + 
  theme(legend.position = "none")
```

<img src="ghed_p8105_project_files/figure-gfm/phc_gdp_plot-1.png" width="90%" />

Average primary health care expenditure for each WHO region with
spending as a percentage of current health expenditure

To investigate how much of health spending goes toward each category for
each WHO region

``` r
phc_che_ghed =
  ghed_df %>% 
  janitor::clean_names() %>% 
  filter(year == 2019) %>% 
  drop_na(phc_usd_pc, che_pc_usd) %>% 
  group_by(region_who) %>% 
  summarize(
    n_countries = n(),
    avg_primary_che = mean(phc_usd_pc / che_pc_usd)
  )
```

Barplot: Primary health care expenditure with spending as a percent of
current health expenditure

``` r
phc_che_ghed %>%   
  ggplot(aes(x = reorder(region_who, avg_primary_che), y = avg_primary_che, fill = region_who)) + 
  geom_bar(stat = "Identity") + 
  labs(
    title = "Primary health care spending by WHO region", 
    x = "WHO region", 
    y = "Average PHC spending per capita health expenditure"
  ) + 
  theme(legend.position = "none")
```

<img src="ghed_p8105_project_files/figure-gfm/phc_che_plot-1.png" width="90%" />

**Curative care vs. preventative care**

Comparing average curative expenditure and average preventative
expenditure for each WHO region

Average curative and preventative health care expenditure for each WHO
region in million constant USD$

``` r
cp_df = 
  ghed_df %>% 
  janitor::clean_names() %>% 
  filter(year == 2019) %>% 
  drop_na(hc1_usd2019, hc6_usd2019) %>% 
  group_by(region_who) %>% 
  summarize(
    n_countries = n(),
    `Curative` = mean(hc1_usd2019),
    `Preventative` = mean(hc6_usd2019)
  ) %>% 
  pivot_longer(
    `Curative`:`Preventative`,
    names_to = "spending_type", 
    values_to = "average_spending"
  )
```

Barplot: Average curative vs. preventative health care expenditure for
each WHO region in million constant USD$

``` r
cp_df %>% 
  ggplot(aes(x = reorder(region_who, average_spending), y = average_spending, fill = spending_type)) + 
  geom_bar(stat = "Identity", position = "Dodge") + 
  labs(
    title = "Average curative vs. preventative spending by WHO region", 
    x = "WHO region", 
    y = "Average spending in million constant USD$"
  ) 
```

<img src="ghed_p8105_project_files/figure-gfm/avg_cp_plot-1.png" width="90%" />

Average curative vs. preventative expenditure for each WHO region with
spending as a percent of GDP

``` r
cp_gdp_df = 
  ghed_df %>% 
  janitor::clean_names() %>% 
  filter(year == 2019) %>% 
  drop_na(hc1_usd2019, hc6_usd2019, gdp_usd2019) %>% 
  group_by(region_who) %>% 
  summarize(
    n_countries = n(),
    `Curative` = mean(hc1_usd2019 / gdp_usd2019),
    `Preventative` = mean(hc6_usd2019 / gdp_usd2019)
  ) %>% 
  pivot_longer(
    `Curative`:`Preventative`,
    names_to = "spending_type", 
    values_to = "average_spending"
  )
```

Barplot: Curative vs. preventative expenditure with spending as a
percent of GDP

``` r
cp_gdp_df %>% 
  ggplot(aes(x = reorder(region_who, average_spending), y = average_spending, fill = spending_type)) + 
  geom_bar(stat = "Identity", position = "Dodge") + 
  labs(
    title = "Average curative vs. preventative spending by WHO region", 
    x = "WHO region", 
    y = "Average spending per capita GDP"
  ) 
```

<img src="ghed_p8105_project_files/figure-gfm/cp_gdp_plot-1.png" width="90%" />

Average curative vs. preventative expenditure for each WHO region with
spending as a percentage of current health expenditure

``` r
cp_che_df = 
  ghed_df %>% 
  janitor::clean_names() %>% 
  filter(year == 2019) %>% 
  drop_na(hc1_usd2019, hc6_usd2019, che_usd2019) %>% 
  group_by(region_who) %>% 
  summarize(
    n_countries = n(),
    `Curative` = mean(hc1_usd2019 / che_usd2019),
    `Preventative` = mean(hc6_usd2019 / che_usd2019)
  ) %>% 
  pivot_longer(
    `Curative`:`Preventative`,
    names_to = "spending_type", 
    values_to = "average_spending"
  )
```

Barplot: Curative vs. preventative expenditure with spending as a
percent of current health expenditure

``` r
cp_che_df %>% 
  ggplot(aes(x = reorder(region_who, average_spending), y = average_spending, fill = spending_type)) + 
  geom_bar(stat = "Identity", position = "Dodge") + 
  labs(
    title = "Average curative vs. preventative spending by WHO region", 
    x = "WHO region", 
    y = "Average spending per capita health expenditure"
  ) 
```

<img src="ghed_p8105_project_files/figure-gfm/cp_che_plot-1.png" width="90%" />

### Other people’s sections
