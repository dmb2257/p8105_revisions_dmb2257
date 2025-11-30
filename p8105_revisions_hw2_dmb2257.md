p8105_revisions_hw2_dmb2257
================
Diane Benites
2025-11-30

## Loading Necessary Packages and Sets Data Path

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

data_path <- 
"C:/Users/dmben/OneDrive/Desktop/data_science_1/p8105_revisions_dmb2257"
```

## Problem 1

This code chunk loads the unemployment dataframe and cleans the names.
It selects the needed columns and mutates the months to the full name.

``` r
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

This code chunk imports the polsmonth data. It separates the date into
year, month and date and renames the numeric value of the month to the
full month name. It also selects all variables except for day, prez_gop
and prez_dem. It also filters the data to only include the points that
were yes fo dem or gop.

``` r
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

The loads the dataset snp and separates the sate into month, day and
year. It mutates the numeric month to the full month name.

``` r
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

This imports the mr. trashwheel data sheet and cleans the names to
snakecase.

``` r
mr_trashwheel_df <-
  read_excel("./202409 Trash Wheel Collection Data.xlsx", range = "Mr. Trash Wheel!A2:N655")|>
  janitor::clean_names()|>
  drop_na(dumpster)|>
  mutate(sports_balls = as.integer(sports_balls))
```

This imports and cleans the names to snakecase in the Prof Trash Wheel
data sheet.

``` r
prof_trashwheel_df <-
  read_excel("./202409 Trash Wheel Collection Data.xlsx", range = "Professor Trash Wheel!A2:M123")|>
  janitor::clean_names()|>
  drop_na(dumpster)
```

This imports and cleans the names to snakecase in the Gwynnda data
sheet.

``` r
gwynnda_trashwheel_df <-
  read_excel("./202409 Trash Wheel Collection Data.xlsx", range = "Gwynnda Trash Wheel!A2:L266")|>
  janitor::clean_names()|>
  drop_na(dumpster)
```

This combines the datasets

``` r
tidy_trashwheel_df = 
  left_join(mr_trashwheel_df, prof_trashwheel_df, by = "dumpster")

tidy_trashwheel_df2 =
  left_join(tidy_trashwheel_df, gwynnda_trashwheel_df, by = "dumpster")
```

## Problem 3

This imports and cleans the names to snakecase in the zip codes and zori
data sets

``` r
zipcodes_tidy_df = 
    read_csv(file = "./Zip Codes.csv")|>
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
  read_csv(file =  "./Zip_zori_uc_sfrcondomfr_sm_month_NYC.csv")|>
  janitor::clean_names()
```

    ## Rows: 149 Columns: 125
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (6): RegionType, StateName, State, City, Metro, CountyName
    ## dbl (119): RegionID, SizeRank, RegionName, 2015-01-31, 2015-02-28, 2015-03-3...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
