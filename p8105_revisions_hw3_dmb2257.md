p8105_revisions_hw3_dmb2257
================
Diane Benites
2025-11-21

# Problem 1

``` r
#This loads necessary packages and the dataset. 

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
library(patchwork)
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

``` r
# This makes a plot of the number of items ordered. The aisles are sorted by the number of items ordered. The names of the aisles are included on the x axis. 
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

``` r
#This creates a table of the 3 most popular items in the aisles "baking ingredients", "dog food care", "packaged vegetables fruits", and how many of each item was ordered.

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

``` r
#This makes a table showing the mean hour of the day at which pank lady apples and coffee ice cream were ordered.

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

``` r
#This imports the datasets. It cleans the second dataset to format the date and zip code variables. It also removed missing price data points. 

data_path <- 
"C:/Users/dmben/OneDrive/Desktop/data_science_1/p8105_revisions_dmb2257"


zipcodes_tidy_df = 
    read_csv(file.path(data_path, "Zip Codes.csv"))|>
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
  read_csv(file.path(data_path, "Zip_zori_uc_sfrcondomfr_sm_month_NYC.csv"))|>
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
  drop_na(price)|>
  rename(
    borough = county_name)|>
  mutate(
   borough = 
      case_match(
        borough,
        "Kings County" ~"Brooklyn",
        "Bronx County" ~ "Bronx",
        "New York County" ~ "Manhattan",
        "Queens County" ~ "Queens",
        "Richmond County" ~ "Staten Island"))
```

    ## Rows: 149 Columns: 125
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (6): RegionType, StateName, State, City, Metro, CountyName
    ## dbl (119): RegionID, SizeRank, RegionName, 2015-01-31, 2015-02-28, 2015-03-3...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# This code chunk summarizes the number of months each zip code was observed. 
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
  separate(year, into=c("year", "month", "day"))|>
  select(-day, -dates)

#This creates a table of average rental price each year by borough

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

| year |    Bronx | Brooklyn | Manhattan |   Queens | Staten Island |
|:-----|---------:|---------:|----------:|---------:|--------------:|
| 2015 | 1759.595 | 2492.928 |  3022.042 | 2214.707 |            NA |
| 2016 | 1520.194 | 2520.357 |  3038.818 | 2271.955 |            NA |
| 2017 | 1543.599 | 2545.828 |  3133.848 | 2263.303 |            NA |
| 2018 | 1639.430 | 2547.291 |  3183.703 | 2291.918 |            NA |
| 2019 | 1705.589 | 2630.504 |  3310.408 | 2387.816 |            NA |
| 2020 | 1811.443 | 2555.051 |  3106.517 | 2315.632 |      1977.608 |
| 2021 | 1857.777 | 2549.890 |  3136.632 | 2210.787 |      2045.430 |
| 2022 | 2054.267 | 2868.199 |  3778.375 | 2406.038 |      2147.436 |
| 2023 | 2285.459 | 3015.184 |  3932.610 | 2561.615 |      2332.934 |
| 2024 | 2496.896 | 3126.803 |  4078.440 | 2694.022 |      2536.442 |

Among all boroughs, the average rental price increases each year, with
the exception of an decrease in average price in 2020 in Brooklyn,
Manhattan and Queens and in 2021 in Brooklyn and Queens. These
differences may be attributed to changes in rent the covid-19 pandemic.

``` r
ggp_plot1=
average_price_df|>
  group_by(zip_code, borough, year)|>
  summarize(average_price = mean(price))|>
  ggplot(aes(x= year, y = average_price, group = zip_code, color = borough)) +
  geom_point()+
  geom_line()+
  labs(title = "Average Rental Prices Within Zip Codes by Year",
       x = "Year",
       y = "Average Price")+
    theme(axis.text.x = element_text(angle = 70, hjust =1))
```

    ## `summarise()` has grouped output by 'zip_code', 'borough'. You can override
    ## using the `.groups` argument.

``` r
ggp_plot1
```

![](p8105_revisions_hw3_dmb2257_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
ggp_plot2 =
zori_df|>
  mutate(
    year = floor_date(dates, unit = "month"))|>
  separate(year, into=c("year", "month", "day"))|>
  select(-day, -dates)|>
  filter(year == "2023")|>
  group_by(zip_code, borough, month)|>
  summarize(average_price = mean(price))|>
  ggplot(aes(x= month, y = average_price, group = zip_code, color = borough)) +
  geom_point()+
  geom_line()+
  labs(title = "Average Rental Price Within Zip Codes by Months in 2023",
       x = "Month",
       y = "Average Price")
```

    ## `summarise()` has grouped output by 'zip_code', 'borough'. You can override
    ## using the `.groups` argument.

``` r
ggp_plot2
```

![](p8105_revisions_hw3_dmb2257_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
ggp_combined = ggp_plot1 + ggp_plot2

ggsave("problem2_combined_plot.jpg", plot = ggp_combined, path = "results_hw2", height = 10, width = 30)
```

# Problem 3

``` r
#The loads the accelerometer dataset and mutates the seqn variable to be a character.
accel_df = 
  read_csv(file = "./nhanes_accel (1).csv")|>
  janitor::clean_names()|>
  drop_na()|>
  mutate(seqn = as.character(seqn))
```

    ## Rows: 250 Columns: 1441
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (1441): SEQN, min1, min2, min3, min4, min5, min6, min7, min8, min9, min1...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# This loads the demographic dataset. It omits the rows without values and sets the appropriate labels for the sex and education variables. 

covar_df = 
  read_csv(file = "./nhanes_covar.csv", col_names = c("SEQN", "sex", "age", "bmi", "education"))|>
  drop_na(SEQN)|>
  filter(!(SEQN == "SEQN"))|>
  janitor::clean_names()|>
  mutate(
    sex =
      case_match(
        sex,
        "1" ~ "male",
        "2" ~ "female"),
    education =
      case_match(
        education,
        "1" ~ "less than high school",
        "2" ~ "high school equivalent",
        "3" ~ "more than high school"),
    education = fct_relevel(education, "less than high school", "high school equivalent", "more than high school")
    )
```

    ## Rows: 255 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (5): SEQN, sex, age, bmi, education
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# This then joins both datasets and filters the merged dataframe to omit participants younger than 21 years old and with missing demographic information. 
merged_df = 
  left_join(accel_df, covar_df, by = "seqn")|>
  filter(age>=21)|>
  drop_na(sex, age, bmi, education)|>
  pivot_longer(
    min1:min1440,
    names_to = "minutes",
    values_to = "activity"
  )|>
  mutate(minutes = str_remove(minutes, "min"))
```

``` r
#This produces a reader friendly table of men and women in each education category. 
merged_df|>
  pivot_wider(
    names_from = minutes,
    values_from = activity
  )|>
  group_by(sex, education)|>
  summarize(n = n())|>
  pivot_wider(names_from = sex, values_from = n)|>
  knitr::kable()
```

    ## `summarise()` has grouped output by 'sex'. You can override using the `.groups`
    ## argument.

| education              | female | male |
|:-----------------------|-------:|-----:|
| less than high school  |     28 |   27 |
| high school equivalent |     23 |   35 |
| more than high school  |     59 |   56 |

``` r
#This plots the age distributions for men and women in each education category. 
sex_age=
merged_df|>
select(seqn, sex, age, education)|>
mutate(age = as.numeric(age))

sex_age|>
  group_by(seqn)|>
  ggplot()+
  geom_boxplot(aes(sex, age, fill = education))+
  labs(
    title = "Age Distribution by Sex and Education",
    x = "Sex",
    y = "Age (years)"
  )
```

![](p8105_revisions_hw3_dmb2257_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

The greatest number of men and women occur in the more than high school
education level. There is a greater frequency of females in this
category compared to men. The lowest frequency of females are in the
high school equivalent category. The lowest frequency of males are in
the less than high school category.

The greatest median age of females occurs for those with a high school
equivalent education level. The greatest median age of males occurs for
those with less than high school education level. The median age of
females with a high school equivalent is greater than the median age of
males with a high school equivalent. However for more than high school
education, the median age for males is greater than the median age of
females.

``` r
# This then plots the total activities against age, with pink for females and blue for males, and 3 separate panels for education. There is also a trend line to illustrate differences. 

merged_df|>
  mutate(age = as.numeric(age))|>  
  group_by(seqn, age, education, sex)|>
  summarize(total_activity = sum(activity))|>
  ggplot(aes(x = age, y = total_activity, color = sex))+
  geom_point()+
  geom_smooth(se = FALSE)+
  facet_grid(~education)+
  labs(
    title = "Total Activity by Age and Education",
    x = "Age (years)",
    y = "Total Activity"
  )
```

    ## `summarise()` has grouped output by 'seqn', 'age', 'education'. You can
    ## override using the `.groups` argument.
    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](p8105_revisions_hw3_dmb2257_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->
The steepest smooth curve occurs among those with less than high school
education. Based on the smooth curve, the total activity generally
decreases as age increases among males and females with less than high
school education. Based on the smooth curves for high school equivalent
and more than high school, females have greater total activity than
males.

``` r
#This code chunk plots accelerometer data by the 24 hour activity time courses for each education level, and uses color to indicate sex.

merged_df|>
  group_by(seqn)|>
  mutate(minutes = as.numeric(minutes))|>
  ggplot(aes(x =minutes, y = activity, group = seqn))+
  geom_line(aes(color = sex))+
  geom_smooth(aes(group = sex), se = FALSE)+
  facet_grid(~education)+
  theme(axis.text.x = element_text(angle = 60, hjust =1))
```

    ## `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'

![](p8105_revisions_hw3_dmb2257_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->
Based on this graph, the greatest activity occurs for females among
those with more than high school between minutes 1000 and 1500 of the
day. The greatest activity for males occurs among those with more than
high school between minutes 250 and 750 during the day. Also, the peak
activity occurs among those with more than a high school education. The
trend line shows similar patterns in activity throughout the day for all
education levels, the lowest activity occurs during the beginning and
end minutes of the day.
