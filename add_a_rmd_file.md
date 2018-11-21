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

glm( homi_solved ~ victim_age + victim_sex + victim_race, data = balti_model, family = binomial()) %>%
broom::tidy() %>%
mutate(OR_estimate = exp(estimate), OR_conf_low = exp(estimate-qnorm(.975)*std.error), OR_conf_high = exp(estimate+qnorm(.975)*std.error)) %>%
filter(term == "victim_racenon-white") %>%
select(term,OR_conf_low, OR_estimate, OR_conf_high) %>% 
knitr::kable(digits = 3)
```

| term                  |  OR\_conf\_low|  OR\_estimate|  OR\_conf\_high|
|:----------------------|--------------:|-------------:|---------------:|
| victim\_racenon-white |          0.313|         0.441|            0.62|

The result of linear model shows that the adjusted odds ratio of solved homicides in which the victim is non-white estimated to be 0.441 times the odds ratio of homicides in which the victim is white.

``` r
each_city = homi_df %>%
  group_by(city_state) %>%
  nest()

odds_glm = function(df){
  glm( homi_solved ~ victim_age + victim_sex + victim_race, data = df, family =     binomial())  %>%
  broom::tidy() %>%
  mutate(OR_estimate = exp(estimate), OR_conf_low =        exp(estimate-qnorm(.975)*std.error), OR_conf_high = exp(estimate+qnorm(.975)*std.error)) %>%
  filter(term == "victim_racenon-white") %>%
  select(term,OR_conf_low, OR_estimate, OR_conf_high)
}

all_estimate = 
  mutate(each_city, or = map(data, odds_glm)) %>%
  select(city_state, or) %>%
  unnest() %>%
  select(-term)

all_estimate %>%
  mutate(city_state = fct_reorder(city_state, OR_estimate)) %>%
  ggplot(aes(x = city_state,y = OR_estimate)) + 
  geom_point() +
  geom_errorbar(aes(ymin = OR_conf_low,ymax = OR_conf_high))+
  geom_hline(yintercept = 1, alpha = 0.5, color = "red")+
  coord_flip()+
  theme_minimal()+
  labs(title = "Estimate and 95% CI of solving homicides odds ratio for each city", 
         x = "City and State", 
         y = "Solving homicides odds ratio comparing non-white victims to white victims") 
```

![](add_a_rmd_file_files/figure-markdown_github/problem%201.3-1.png)

From the plot, we can see in most cities in the study, the estimate odds ratio of solving homicides comparing non-white victims to white victims is less than 1. It indicate that in most of the cities, homicides in which the victim is non-white are substantially less likely to be resolved that those in which the victim is white.
