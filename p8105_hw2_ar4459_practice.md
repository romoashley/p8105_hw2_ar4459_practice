Homework 2 Practice
================
ASHLEY ROMO
2023-09-29

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.3     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(readxl)
```

## Problem 1

``` r
month_df = 
  tibble(
    month_num = 1:12,
    month_abb = month.abb,
    month = month.name
  )

pols = 
  read_csv("hw2_data/pols-month.csv") |>
  separate(mon, into = c("year", "month_num", "day"), convert = TRUE) |>
  mutate(
    president = recode(prez_gop, "0" = "dem", "1" = "gop", "2" = "gop")) |>
  left_join(x = _, y = month_df) |> 
  select(year, month, everything(), -day, -starts_with("prez")) 
```

    ## Rows: 822 Columns: 9
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl  (8): prez_gop, gov_gop, sen_gop, rep_gop, prez_dem, gov_dem, sen_dem, r...
    ## date (1): mon
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Joining with `by = join_by(month_num)`

``` r
snp = 
  read_csv("hw2_data/snp.csv") |>
  separate(date, into = c("month", "day", "year"), convert = TRUE) |>
  arrange(year, month) |>
  mutate(month = month.name[month]) |>
  select(year, month, close) 
```

    ## Rows: 787 Columns: 2
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): date
    ## dbl (1): close
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

Finally, we tidy the `unemployment` data so that it can be merged with
the `pols` and `snp` datasets.

``` r
unemployment = 
  read_csv("hw2_data/unemployment.csv") |>
  rename(year = Year) |>
  pivot_longer(
    Jan:Dec, 
    names_to = "month_abb",
    values_to = "unemployment"
  ) |> 
  left_join(x = _, y = month_df) |> 
  select(year, month, unemployment)
```

    ## Rows: 68 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (13): Year, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Joining with `by = join_by(month_abb)`

Now we merge the three datasets!

``` r
data_538 = 
  left_join(pols, snp) |>
  left_join(x = _, y = unemployment)
```

    ## Joining with `by = join_by(year, month)`
    ## Joining with `by = join_by(year, month)`

``` r
str(data_538)
```

    ## tibble [822 × 13] (S3: tbl_df/tbl/data.frame)
    ##  $ year        : num [1:822] 1947 1947 1947 1947 1947 ...
    ##  $ month       : chr [1:822] "January" "February" "March" "April" ...
    ##  $ month_num   : int [1:822] 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ gov_gop     : num [1:822] 23 23 23 23 23 23 23 23 23 23 ...
    ##  $ sen_gop     : num [1:822] 51 51 51 51 51 51 51 51 51 51 ...
    ##  $ rep_gop     : num [1:822] 253 253 253 253 253 253 253 253 253 253 ...
    ##  $ gov_dem     : num [1:822] 23 23 23 23 23 23 23 23 23 23 ...
    ##  $ sen_dem     : num [1:822] 45 45 45 45 45 45 45 45 45 45 ...
    ##  $ rep_dem     : num [1:822] 198 198 198 198 198 198 198 198 198 198 ...
    ##  $ president   : chr [1:822] "dem" "dem" "dem" "dem" ...
    ##  $ month_abb   : chr [1:822] "Jan" "Feb" "Mar" "Apr" ...
    ##  $ close       : num [1:822] NA NA NA NA NA NA NA NA NA NA ...
    ##  $ unemployment: num [1:822] NA NA NA NA NA NA NA NA NA NA ...

# Problem 2

``` r
mr_wheel_df =
  read_xlsx("hw2_data/202207 Trash Wheel Collection Data.xlsx",1, range = "A2:N549") |> 
  janitor::clean_names()|> 
  mutate(homes_powered = weight_tons*500/30
    )

prof_wheel_df =
  read_xlsx("hw2_data/202207 Trash Wheel Collection Data.xlsx",2 , range = "A2:M96") |> 
  janitor::clean_names()|> 
  mutate(homes_powered = weight_tons*500/30
    )

gwyn_wheel_df =
  read_xlsx("hw2_data/202207 Trash Wheel Collection Data.xlsx",4 , range = "A2:K108") |> 
  janitor::clean_names()|> 
  mutate(homes_powered = weight_tons*500/30
    )


merged_df =
  full_join(mr_wheel_df, 
            prof_wheel_df,
            gwyn_wheel_df, 
            by = "dumpster")
```

## Problem 3

``` r
mci_df = 
  read_csv("hw2_data/MCI_baseline.csv")
```

    ## New names:
    ## Rows: 484 Columns: 6
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (6): ...1, Age at the study baseline, 1 = Male, 0 = Female, Years of edu...
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## • `` -> `...1`
