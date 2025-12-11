p8105_revisions_hw2_dmb2257
================
Diane Benites
2025-11-30

# Homework 2 Revisions

## Loads Necessary Packages

``` r
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
library(janitor)
```

    ## 
    ## Attaching package: 'janitor'
    ## 
    ## The following objects are masked from 'package:stats':
    ## 
    ##     chisq.test, fisher.test

``` r
library(readxl)
```

## Problem 1

``` r
#This code chunk loads the unemployment dataframe  and cleans the names. It selects the needed columns and mutates the months to the full name. 

unemployment_df = 
  read_csv(file = "./unemployment.csv")|>
  janitor::clean_names() |>
  
  select(year, jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec) |>
  
  pivot_longer(
    jan:dec, 
    names_to = "month",
    values_to = "percentages") |>
    
  mutate(
    month = 
      case_match(
       month,
       "jan" ~ "January",
       "feb" ~ "February",
       "mar" ~ "March",
       "apr" ~ "April",
       "may" ~ "May",
       "jun" ~ "June",
       "jul" ~ "July",
       "aug" ~ "August",
       "sep" ~ "September",
       "oct" ~ "October",
       "nov" ~ "November",
       "dec" ~ "December"),
     month = as.factor(month))
```

    ## Rows: 68 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (13): Year, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
#This code chunk imports the polsmonth data. It separates the date into year, month and date and renames the numeric value of the month to the full month name. It also selects all variables except for day, prez_gop and prez_dem. It also filters the data to only include the points that were yes fo dem or gop.
polsmonth_df = 
  read_csv(file = "./pols-month.csv")|>

  janitor::clean_names() |>
  
  separate(mon, into = c("year", "month", "day")) |>

  mutate(
    month = 
      case_match(
       month,
       "01" ~ "January",
       "02" ~ "February",
       "03" ~ "March",
       "04" ~ "April",
       "05" ~ "May",
       "06" ~ "June",
       "07" ~ "July",
       "08" ~ "August",
       "09" ~ "September",
       "10" ~ "October",
       "11" ~ "November",
       "12" ~ "December"),
     month = as.factor(month)) |>
    select(year, month, prez_gop, prez_dem, gov_gop, gov_dem, sen_gop, sen_dem, rep_gop, rep_dem) |>
    
  pivot_longer(
    prez_gop:prez_dem,
    names_to = "president") |>
    
  mutate(
    president = case_match(
      president,
      "prez_gop" ~ "gop",
      "prez_dem" ~ "dem")) |>
    
  filter(value == 1)
```

    ## Rows: 822 Columns: 9
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl  (8): prez_gop, gov_gop, sen_gop, rep_gop, prez_dem, gov_dem, sen_dem, r...
    ## date (1): mon
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
#The loads the dataset snp and separates the sate into month, day and year. It mutates the numeric month to the full month name.

snp_df = 
  read_csv(file = "./snp.csv")|>
janitor::clean_names() |>

separate(date, into = c("month", "day", "year")) |>

  mutate(
    month = 
      case_match(
       month,
       "01" ~ "January",
       "02" ~ "February",
       "03" ~ "March",
       "04" ~ "April",
       "05" ~ "May",
       "06" ~ "June",
       "07" ~ "July",
       "08" ~ "August",
       "09" ~ "September",
       "10" ~ "October",
       "11" ~ "November",
       "12" ~ "December"),
     month = as.factor(month)) |>
    relocate(year, month, close)|>
    select(-day)
```

    ## Rows: 787 Columns: 2
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): date
    ## dbl (1): close
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

The polsmonth dataset contains observations regarding the political
parties of politicians. The “presidents” variable identifies the
president during that given time as democrat or republican, demonstrated
by value 1. The unemployment dataset contains observations regarding the
percentage of unemployment during a given month each year. There were
originally 13 variables, one for year and one for each of the 12 months.
The months variables and values were consolidated into one month column
and one percentages column, respectively. The snp dataset contains
observations with 2 variables, the date of the observation and the
closing S&P value at the given date. The date separated into month,
date, and year, and the numeric values for month were renamed to the
full month name.

## Problem 2

``` r
# This imports the Mr. trashwheel data sheet and cleans the names. It omits rows that do not include dumpster specific data, computes the appropriate value for homes_powered, and rounds the number of sports balls to the nearest integer. It also creates a new variable "trashwheel" with the value "mr" to identify this trashwheel when I combine the datasets.

mr_trashwheel_df <-
  read_excel("./202409 Trash Wheel Collection Data.xlsx", range = "Mr. Trash Wheel!A2:N655")|>
  janitor::clean_names()|>
  drop_na(dumpster)|>
  mutate(homes_powered = weight_tons*500/30,
        sports_balls = round(sports_balls),
        sports_balls = as.integer(sports_balls),
         year = as.numeric(year),
        trashwheel = "mr")
```

``` r
# This imports and cleans the names in the Professor Trash Wheel data sheet.It omits rows that do not include dumpster specific data. It also creates a new variable "trashwheel" with the value "professor" to identify this trashwheel when I combine the datasets.
prof_trashwheel_df <-
  read_excel("./202409 Trash Wheel Collection Data.xlsx", range = "Professor Trash Wheel!A2:M123")|>
  janitor::clean_names()|>
  drop_na(dumpster)|>
  mutate(trashwheel = "professor")|>
  mutate(weight_tons = as.numeric(weight_tons))
```

``` r
# This imports and cleans the names in the Gwynnda Trash Wheeldata sheet. It omits rows that do not include dumpster specific data. It also creates a new variable "trashwheel" with the value "gwynnda" to identify this trashwheel when I combine the datasets.
gwynnda_trashwheel_df <-
  read_excel("./202409 Trash Wheel Collection Data.xlsx", range ="Gwynnda Trash Wheel!A2:L266")|>
  janitor::clean_names()|>
  drop_na(dumpster)|>
  mutate(trashwheel = "gwynnda")
```

``` r
# This combines the datasets into one tidy dataframe. 
tidy_trashwheel_df = 
  bind_rows(mr_trashwheel_df, prof_trashwheel_df, gwynnda_trashwheel_df)
```

``` r
# This code chunk computes the total weight of trash collected by each trash wheel by computing the sum of weight_tons. It also computes the sum of cigarette butts collected after filtering for June 2022. 
tidy_trashwheel_df|>
  group_by(trashwheel)|>
  drop_na(weight_tons)|>
  summarize(
    total_trash = sum(weight_tons, na.rm = FALSE))
```

    ## # A tibble: 3 × 2
    ##   trashwheel total_trash
    ##   <chr>            <dbl>
    ## 1 gwynnda           798.
    ## 2 mr               2091.
    ## 3 professor         247.

``` r
tidy_trashwheel_df|>
  filter(month == "June")|>
  filter(year == "2022")|>
  group_by(trashwheel)|>
  summarize(cigs = sum(cigarette_butts))
```

    ## # A tibble: 3 × 2
    ##   trashwheel  cigs
    ##   <chr>      <dbl>
    ## 1 gwynnda    18120
    ## 2 mr         22500
    ## 3 professor  11600

The resulting dataset contains 1033 observations of 15 variables. Key
variables include the trash wheel variable, which identifies the
trashwheel. Another key variable is the date the trash was collected and
the weight in tons of the trash collected. The homes powered variable
was computed using the weight in tons variable. The weight of trash
collected was multiplied by 500 kilowatts of electricity divided by 30
kilowatts of electricity used per home, to determine the number of homes
powered based on the weight of trash collected by the dumpster.The total
weight of trash collected by Professor Trash Wheel was 247 tons. In June
2022, 18120 total cigarette butts were collected by Gwynnda Trash Wheel.

## Problem 3

``` r
#This imports and cleans the names to snakecase in the zip codes and zori data sets. It creates the variable "borough" for both of the data sets. It renames the county names to a consistent format under the borough variable.The zori dataset is cleaned to condense the dates and rental prices into two columns, and drop missing rental prices.

zipcodes_tidy_df = 
    read_csv(file = "./Zip Codes.csv")|>
  janitor::clean_names()|>
  rename(borough = county)|>
  mutate(
    borough = 
      case_match(
        borough,
        "Kings" ~"Brooklyn",
        "Bronx" ~ "Bronx",
        "New York" ~ "Manhattan",
        "Queens" ~ "Queens",
        "Richmond" ~ "Staten Island"))
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
  read_csv(file =  "./Zip_zori_uc_sfrcondomfr_sm_month_NYC.csv")|>
  janitor::clean_names()|>
  select(-state_name)|>
  pivot_longer(
    x2015_01_31:x2024_08_31,
    names_to = "date",
    values_to = "rent_price")|>
  drop_na(rent_price)|>
    rename(borough = county_name)|>
    mutate(
    borough = 
      case_match(
        borough,
        "Kings County" ~"Brooklyn",
        "Bronx County" ~ "Bronx",
        "New York County" ~ "Manhattan",
        "Queens County" ~ "Queens",
        "Richmond County" ~ "Staten Island"))|>
  rename("zip_code" = region_name)|>
  mutate(date = str_remove(date, "x"))
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
#This code chunk creates a single dataset with all the information contained in these files. 
tidy_ny_df =
  left_join(zori_df, zipcodes_tidy_df)|>
  arrange(zip_code, rent_price)
```

    ## Joining with `by = join_by(zip_code, borough)`

``` r
tidy_ny_df|>
  summarize(n_zip = n_distinct(zip_code),
            n_neighborhood = n_distinct(neighborhood))
```

    ## # A tibble: 1 × 2
    ##   n_zip n_neighborhood
    ##   <int>          <int>
    ## 1   149             43

``` r
zipcodes_tidy_df|>
    summarize(n_zip = n_distinct(zip_code),
            n_neighborhood = n_distinct(neighborhood))
```

    ## # A tibble: 1 × 2
    ##   n_zip n_neighborhood
    ##   <int>          <int>
    ## 1   320             43

A tidy dataset was created by merging the zori and zipcode data sets by
zip code. The dataset is arranged by ascending zip code and price. There
are 10450 observations of 15 variables. There are 149 unique zip codes
included in the dataset.There are 43 unique neighborhoods. There are 171
zip codes that appear in the ZIP code dataset but not in the zillow
rental price dataset.

``` r
dates_df = 
tidy_ny_df|>
  filter(str_starts(date, "2020_01|2021_01"))|>
  pivot_wider(
    names_from = date,
    values_from = rent_price
  )|>
  rename(jan_2020 = "2020_01_31")|>
  rename(jan_2021 = "2021_01_31")|>
  mutate(rent_drop = jan_2020-jan_2021)|>
  arrange(desc(rent_drop))|>
  select(zip_code, jan_2020, jan_2021, rent_drop, borough, neighborhood)

table_data=
  head(dates_df, 10)

table_data|>
  knitr::kable()
```

| zip_code | jan_2020 | jan_2021 | rent_drop | borough   | neighborhood                  |
|---------:|---------:|---------:|----------:|:----------|:------------------------------|
|    10007 | 6334.211 | 5421.614 |  912.5966 | Manhattan | Lower Manhattan               |
|    10069 | 4623.042 | 3874.918 |  748.1245 | Manhattan | NA                            |
|    10009 | 3406.442 | 2692.187 |  714.2550 | Manhattan | Lower East Side               |
|    10016 | 3731.135 | 3019.431 |  711.7045 | Manhattan | Gramercy Park and Murray Hill |
|    10001 | 4108.098 | 3397.648 |  710.4499 | Manhattan | Chelsea and Clinton           |
|    10002 | 3645.416 | 2935.113 |  710.3028 | Manhattan | Lower East Side               |
|    10004 | 3149.658 | 2443.697 |  705.9608 | Manhattan | Lower Manhattan               |
|    10038 | 3573.201 | 2875.616 |  697.5853 | Manhattan | Lower Manhattan               |
|    10012 | 3628.566 | 2942.344 |  686.2218 | Manhattan | Greenwich Village and Soho    |
|    10010 | 3697.284 | 3012.353 |  684.9304 | Manhattan | Gramercy Park and Murray Hill |

The 10 largest drops in price from January 2020 to January 2021 were all
observed in zip codes from the borough, Manhattan. The greatest price
drop was \$912.59 in the zip code 10007. Of the 10 zip codes with the
largest price drop, Lower Manhattan was the most common neighborhood.
