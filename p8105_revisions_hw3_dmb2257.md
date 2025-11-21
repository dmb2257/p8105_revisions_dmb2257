p8105_revisions_hw3_dmb2257
================
Diane Benites
2025-11-21

# Problem 1

This loads necessary packages and the dataset.

``` r
library(p8105.datasets)
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.2
    ## ✔ ggplot2   3.5.2     ✔ tibble    3.3.0
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.1.0     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
data("instacart")
```

Description of the Instacart Dataset: There are 1384617 observations of
15 variables. Each column represents one of the variables in the
dataset. Each row represents one product of an order. This data provides
information on when instacart orders in the sample were placed, which
products were ordered, the frequency of orders, and the aisles
associated with the orders.

``` r
items_ordered_df =
instacart|>
  janitor::clean_names()|>
  group_by(aisle_id, aisle)|>
  summarize(
    item_order_count=n())|>
  arrange(desc(item_order_count))
```

    ## `summarise()` has grouped output by 'aisle_id'. You can override using the
    ## `.groups` argument.

There are 134 aisles. The most items are ordered from aisle 83: fresh
vegetables (150609 items ordered), aisle 24: fresh fruits (150473 items
ordered), and aisle 123: packaged vegatables (78493 items ordered).

This makes a plot of the number of items ordered. The aisles are sorted
by the number of items ordered. The names of the aisles are included on
the x axis.

``` r
items_ordered_df|>
  filter(item_order_count >= 10000)|>
  ggplot(
    aes(fct_reorder(aisle, item_order_count), item_order_count))+
  geom_col()+
  labs(
    x = "Aisle Name",
    y = "Number of Items Ordered"
  )+
  theme(axis.text.x = element_text(angle = 70, hjust =1))
```

![](p8105_revisions_hw3_dmb2257_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

This creates a table of the 3 most popular items in the aisles “baking
ingredients”, “dog food care”, “packaged vegetables fruits”, and how
many of each item was ordered.

``` r
instacart|>
  filter(aisle %in% c("baking ingredients", "dog food care", "packaged vegetables fruits"))|>
  group_by(aisle, product_name)|>
  summarize(
    product_count = n()
  )|>
  arrange(desc(product_count))|>
  filter(min_rank(desc(product_count))< 4)|>
  knitr::kable()
```

    ## `summarise()` has grouped output by 'aisle'. You can override using the
    ## `.groups` argument.

| aisle | product_name | product_count |
|:---|:---|---:|
| packaged vegetables fruits | Organic Baby Spinach | 9784 |
| packaged vegetables fruits | Organic Raspberries | 5546 |
| packaged vegetables fruits | Organic Blueberries | 4966 |
| baking ingredients | Light Brown Sugar | 499 |
| baking ingredients | Pure Baking Soda | 387 |
| baking ingredients | Cane Sugar | 336 |
| dog food care | Snack Sticks Chicken & Rice Recipe Dog Treats | 30 |
| dog food care | Organix Chicken & Brown Rice Recipe | 28 |
| dog food care | Small Dog Biscuits | 26 |

This makes a table showing the mean hour of the day at which pank lady
apples and coffee ice cream were ordered.

``` r
instacart |>
  mutate(
    order_dow = 
      case_match(
        order_dow,
        0 ~ "Sunday",
        1 ~ "Monday",
        2 ~ "Tuesday",
        3 ~ "Wednesday",
        4 ~ "Thursday",
        5 ~ "Friday",
        6 ~ "Saturday"),
    order_dow = as.factor(order_dow))|>
  group_by(product_name, order_dow)|>
  summarize(
    mean_hour = mean(order_hour_of_day, na.rm = TRUE)
  )|>
  filter(
    product_name %in% c("Pink Lady Apples", "Coffee Ice Cream"))|>
  pivot_wider(
    names_from = order_dow,
    values_from = mean_hour)|>
  knitr::kable()
```

    ## `summarise()` has grouped output by 'product_name'. You can override using the
    ## `.groups` argument.

| product_name | Friday | Monday | Saturday | Sunday | Thursday | Tuesday | Wednesday |
|:---|---:|---:|---:|---:|---:|---:|---:|
| Coffee Ice Cream | 12.26316 | 14.31579 | 13.83333 | 13.77419 | 15.21739 | 15.38095 | 15.31818 |
| Pink Lady Apples | 12.78431 | 11.36000 | 11.93750 | 13.44118 | 11.55172 | 11.70213 | 14.25000 |

# Problem 2

This imports the datasets. It cleans the second dataset to create a
month variable instead of the detailed date. It also removed missing
price data points.

``` r
zipcodes_tidy_df = 
    read_csv("p8105_revisions_hw3_dmb2257_files/Zip Codes.csv", na = c("NA", ".", ""))|>
  janitor::clean_names()
```

    ## Rows: 322 Columns: 7
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (4): County, County Code, File Date, Neighborhood
    ## dbl (3): State FIPS, County FIPS, ZipCode
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
zori_df = 
  read_csv("p8105_revisions_hw3_dmb2257_files/Zip_zori_uc_sfrcondomfr_sm_month_NYC.csv")|>
  pivot_longer(
    -(RegionID:CountyName),
    names_to = "dates",
    values_to = "price"
  )|>
  janitor::clean_names()|>
  rename(zip_code = region_name) |>
  mutate(
    dates = as_date(dates),
    zip_code = as.numeric(zip_code)
  )|>
  drop_na(price)
```

    ## Rows: 149 Columns: 125
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (6): RegionType, StateName, State, City, Metro, CountyName
    ## dbl (119): RegionID, SizeRank, RegionName, 2015-01-31, 2015-02-28, 2015-03-3...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

This code chunk summarizes the number of months each zip code was
observed.

``` r
zip_count=
zori_df|>
    mutate(
    month = floor_date(dates, unit = "month")
  )|>
  select(-dates)|>
  group_by(zip_code)|>
  summarize(
    zip_obs = n_distinct(month)
  )

zip_count|>
  filter(zip_obs == 116)
```

    ## # A tibble: 48 × 2
    ##    zip_code zip_obs
    ##       <dbl>   <int>
    ##  1    10001     116
    ##  2    10002     116
    ##  3    10003     116
    ##  4    10005     116
    ##  5    10010     116
    ##  6    10012     116
    ##  7    10013     116
    ##  8    10014     116
    ##  9    10017     116
    ## 10    10018     116
    ## # ℹ 38 more rows

``` r
zip_count |>
  filter(zip_obs < 10)
```

    ## # A tibble: 26 × 2
    ##    zip_code zip_obs
    ##       <dbl>   <int>
    ##  1    10044       9
    ##  2    10162       2
    ##  3    10303       2
    ##  4    10308       3
    ##  5    10453       1
    ##  6    10455       3
    ##  7    10456       4
    ##  8    10459       2
    ##  9    10460       2
    ## 10    10470       1
    ## # ℹ 16 more rows

There were 48 zip codes observed 116 times. There were 26 zip codes
observed fewer than 10 times.

``` r
average_price_df =
zori_df|>
  mutate(
    year = floor_date(dates, unit = "year"))|>
  rename(
    borough = county_name)|>
  separate(year, into=c("year", "month", "day"))|>
  select(-month, -day, -dates)


average_price_df|>
  group_by(borough, year)|>
  summarize(
    avg_price = mean(price)
  )|>
  pivot_wider(
    names_from = borough,
    values_from = avg_price
  )|>
  knitr::kable()
```

    ## `summarise()` has grouped output by 'borough'. You can override using the
    ## `.groups` argument.

| year | Bronx County | Kings County | New York County | Queens County | Richmond County |
|:-----|-------------:|-------------:|----------------:|--------------:|----------------:|
| 2015 |     1759.595 |     2492.928 |        3022.042 |      2214.707 |              NA |
| 2016 |     1520.194 |     2520.357 |        3038.818 |      2271.955 |              NA |
| 2017 |     1543.599 |     2545.828 |        3133.848 |      2263.303 |              NA |
| 2018 |     1639.430 |     2547.291 |        3183.703 |      2291.918 |              NA |
| 2019 |     1705.589 |     2630.504 |        3310.408 |      2387.816 |              NA |
| 2020 |     1811.443 |     2555.051 |        3106.517 |      2315.632 |        1977.608 |
| 2021 |     1857.777 |     2549.890 |        3136.632 |      2210.787 |        2045.430 |
| 2022 |     2054.267 |     2868.199 |        3778.375 |      2406.038 |        2147.436 |
| 2023 |     2285.459 |     3015.184 |        3932.610 |      2561.615 |        2332.934 |
| 2024 |     2496.896 |     3126.803 |        4078.440 |      2694.022 |        2536.442 |

Among all boroughs, the average rental price increases each year, with
the exception of an decrease in average price in 2020 in Kings, NY
County and Queens and in 2021 in Kings and Queens. These differences may
be attributed to changes in rent the covid-19 pandemic.

``` r
average_price_df|>
  group_by(zip_code, borough, year)|>
  summarize(price)|>
  ggplot(aes(x= year, y = price, color = borough)) +
  geom_point()
```

    ## Warning: Returning more (or less) than 1 row per `summarise()` group was deprecated in
    ## dplyr 1.1.0.
    ## ℹ Please use `reframe()` instead.
    ## ℹ When switching from `summarise()` to `reframe()`, remember that `reframe()`
    ##   always returns an ungrouped data frame and adjust accordingly.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## `summarise()` has grouped output by 'zip_code', 'borough', 'year'. You can
    ## override using the `.groups` argument.

![](p8105_revisions_hw3_dmb2257_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

# Problem 3
