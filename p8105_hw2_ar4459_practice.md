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
  read_csv("data/pols-month.csv") |>
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
  read_csv("data/snp.csv") |>
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
  read_csv("data/unemployment.csv") |>
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
  read_xlsx("data/202309 Trash Wheel Collection Data.xlsx", 1, range = "A2:N587") |> 
  janitor::clean_names()|> 
  drop_na(dumpster) |> 
  mutate(homes_powered = (weight_tons)*500/30,
         mr_trash = "Mr. Trash Wheel",
         year = as.numeric(year)
    ) |> 
  select(dumpster, month, year, date, weight = weight_tons, volume = volume_cubic_yards, everything()) 


prof_wheel_df =
  read_xlsx("data/202309 Trash Wheel Collection Data.xlsx",2 , range = "A2:M109") |> 
  janitor::clean_names()|> 
  drop_na(dumpster) |> 
  mutate(homes_powered = (weight_tons)*500/30,
         prof_trash = "Professor Trash Wheel"
    ) |> 
  select(dumpster, month, year, date, weight = weight_tons, volume = volume_cubic_yards, everything()) 


gwyn_wheel_df =
  read_xlsx("data/202309 Trash Wheel Collection Data.xlsx",4 , range = "A2:L159") |> 
  janitor::clean_names()|> 
  mutate(homes_powered = (weight_tons)*500/30,
         gwyn_trash = "Gwynnda Trash Wheel"
    ) |> 
  drop_na(dumpster) |> 
  select(dumpster, month, year, date, weight = weight_tons, volume = volume_cubic_yards, everything())

full_merge_df =
  bind_rows(mr_wheel_df, prof_wheel_df, gwyn_wheel_df, .id = "Dumpster")
```

The total number of observations in the resulting dataset is 845. Key
variables include the weight in tons in Mr Trash Wheel, Professor Trash
Wheel and Gwynnda.

The total weight collected by Professor Trash Wheel is 216.26 tons.

The total number of cigarette butts collected by Gwynnda in July of 2021
is 4.58^{4}.

## Problem 3

``` r
mci_df = 
  read_csv("data/MCI_baseline.csv", skip = 1) |> 
  janitor::clean_names() |>
  mutate(
    sex = case_match(
      sex, 
      1 ~ "male",
      0 ~"female"
    ),
    apoe4 = case_match(
      apoe4, 
      1 ~ "carrier",
      0 ~ "non-carrier"
    ), 
  ) |> 
   select(id, baseline_age = current_age, onset_age = age_at_onset, everything()) |> 
   filter(baseline_age < onset_age | is.na(onset_age))
```

    ## Rows: 483 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): Age at onset
    ## dbl (5): ID, Current Age, Sex, Education, apoe4
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

An important step in the import process include skipping the first row
in the csv file because it is a legend about the dataset and not data.
Additionally, I removed those who already had a diagnosis of MCI during
baseline. Lastly, I removed those who had no age of onset in the
dataset, meaning they have not had a diagnosis of MCI. Relevant features
of the dataset include the age at baseline, age at onset, and the
whether the participant is a carrier or non-carrier of the apoe4
variant. These are relevant because they provide information to
investigate whether participants with the apoe4 variant develop MCI at a
younger age.

``` r
# number of participants at baseline 
nrow(mci_df)
```

    ## [1] 93

``` r
# participants who developed MCI
developed_df = 
  filter(mci_df, onset_age != ".")

# average age at baseline
mean(pull(mci_df, baseline_age))
```

    ## [1] 65.54194

``` r
# proportion of women with apoe4 variant 
female_car_df = 
  select(mci_df, sex, apoe4) |> 
  filter(sex == "female", apoe4 == "carrier")
```

There were a total of 93 participants recruited at baseline.

Of the total participants at baseline, 93 was the number of participants
who developed MCI.

The average age at baseline is 65.5419355.

The proportion of women in the study who are carriers is 0.3225806.

``` r
amyloid_df = 
  read_csv("data/mci_amyloid.csv", skip = 1) |> 
  janitor::clean_names() |> 
  select(id = study_id, t0 = baseline, t1 = time_2, t2 = time_4, t3 = time_6, t4 = time_8) |> 
  pivot_longer(
    t0:t4,
    names_to = "time_points",
    values_to = "ratio")
```

    ## Rows: 487 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (5): Baseline, Time 2, Time 4, Time 6, Time 8
    ## dbl (1): Study ID
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

The steps of the import included loading the dataset using the read_csv
function and the relative pathname. I also used skip = 1 because the
first row in the csv file was a key or legend that described the time
points in the dataset. Since this row was not data, I did not include it
in my dataframe. I also changed the column name of baseline to t0 and
the remaining time points so it followed an increase time for each
column.

``` r
mci_only = anti_join(mci_df, amyloid_df, by = "id")
nrow(mci_only)
```

    ## [1] 3

``` r
amyloid_only = anti_join(amyloid_df, mci_df, by = "id")
nrow(amyloid_only)
```

    ## [1] 1985

There are a total of 3 participants who appear in only the baseline.
This includes participants with the following id: 14, 4, 268.

There are a total of 1985 participants who appear in only the amyloid.

``` r
included_both = inner_join(mci_df, amyloid_df, by = "id")
nrow(included_both)
```

    ## [1] 450

The total number of participants included in both datasets is 450. The
mean age at baseline is 65.6766667.

``` r
#export the result as  a CSV in data directory
write_csv(included_both, "data/demographic_biomarker_cleaned.csv")
```
