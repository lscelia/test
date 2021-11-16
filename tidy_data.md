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
smoke_df = read_csv("data/adult20.csv")
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
clean_df = smoke_df %>% 
  #clean variable names
  janitor::clean_names()
```
