tidy data
================

``` r
library(tidyverse)
library(readxl)
library(dplyr)
```

## Tidy Data

``` r
#load data set
smoke_df_2020 = read_csv("data/adult20.csv")
```

    ## Warning: One or more parsing issues, see `problems()` for details

    ## Rows: 31568 Columns: 618

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (1): HHX
    ## dbl (606): URBRRL, RATCAT_A, INCGRP_A, INCTCFLG_A, FAMINCTC_A, IMPINCFLG_A, ...
    ## lgl  (11): OGFLG_A, OPFLG_A, CHFLG_A, PRPLCOV2_C_A, STOMAAGETC_A, RECTUAGETC...

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
smoke_df_2019 = read_csv("data/adult19.csv")
```

    ## Warning: One or more parsing issues, see `problems()` for details

    ## Rows: 31997 Columns: 534

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (1): HHX
    ## dbl (515): URBRRL, RATCAT_A, INCGRP_A, INCTCFLG_A, FAMINCTC_A, IMPINCFLG_A, ...
    ## lgl  (18): OGFLG_A, OPFLG_A, CHFLG_A, MAFLG_A, PRPLCOV2_C_A, RECTUAGETC_A, O...

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
clean_df = smoke_df_2020 %>% 
  #clean variable names
  janitor::clean_names()
```

#The smoking trend in the US over 5 years let’s clean the dataset for
observing the smoking trends in these five years lets select the
relavant questions first and the following explain the details of the
variables smoking_status: current smoking stastus.

``` r
cleaning_for_smoke_trend= function(dataset){
  dataset%>% 
  janitor::clean_names()%>% 
  select(cignow_a, smkcigst_a,smkage_a)%>% 
  rename(smoke_freq = cignow_a ,  smoking_status = smkcigst_a, smoking_age =smkage_a )}

smoke_trend_2020 = cleaning_for_smoke_trend(smoke_df_2020)
```

the number of people smoking

``` r
 finding_smoking_trend = function(dataframe){
   dataframe %>% 
  select(smoking_status)%>%
  mutate(smoking_status = recode(smoking_status,"1" = "current every day smoker", "2" ="current some day smoker",  "3"= "former smoker","4" = "never smoker"))%>% 
  count(smoking_status)%>%
  drop_na()}

finding_smoking_trend(smoke_trend_2020)
```

    ## Warning: Unreplaced values treated as NA as .x is not compatible. Please specify
    ## replacements exhaustively or supply .default

    ## # A tibble: 4 × 2
    ##   smoking_status               n
    ##   <chr>                    <int>
    ## 1 current every day smoker  2736
    ## 2 current some day smoker    877
    ## 3 former smoker             8142
    ## 4 never smoker             19224
