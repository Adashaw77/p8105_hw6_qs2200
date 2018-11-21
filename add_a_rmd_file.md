P8105\_hw6\_qs2200
================
Qi Shao
11/20/2018

Problem 1
---------

Tidy the data.

``` r
homi_df = read_csv("./data/homicide-data.csv") 
```

    ## Parsed with column specification:
    ## cols(
    ##   uid = col_character(),
    ##   reported_date = col_integer(),
    ##   victim_last = col_character(),
    ##   victim_first = col_character(),
    ##   victim_race = col_character(),
    ##   victim_age = col_character(),
    ##   victim_sex = col_character(),
    ##   city = col_character(),
    ##   state = col_character(),
    ##   lat = col_double(),
    ##   lon = col_double(),
    ##   disposition = col_character()
    ## )

``` r
homi_df = 
  homi_df%>%
  mutate(city_state = str_c(city, ", ", state)) %>%
  janitor::clean_names() %>%
  mutate(homi_solved= ifelse(disposition == "Closed by arrest", 1, 0)) %>%
  filter(!(city_state %in%c("Dallas, TX", "Phoenix, AZ", "Kansas City, MO", "Tulsa, AL"))) %>%
  mutate(victim_race = ifelse(victim_race == "White", "white", "non-white"), victim_age = as.integer(victim_age), victim_race = fct_relevel(victim_race, "white", "non-white")) 
```

    ## Warning in evalq(as.integer(victim_age), <environment>): NAs introduced by
    ## coercion

``` r
balti_model = homi_df %>%
  filter(city_state == "Baltimore, MD")
```
