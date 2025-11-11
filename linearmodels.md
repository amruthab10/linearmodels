Linear Models
================
Amrutha Banda
2025-11-06

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.2     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.4     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(p8105.datasets)
```

Load the NYC airbnb data

``` r
data("nyc_airbnb")
```

Look at the data/ do some cleaning

``` r
nyc_airbnb = 
  nyc_airbnb |> 
  mutate(stars = review_scores_location / 2) |> 
  rename(
    borough = neighbourhood_group) |> 
  filter(borough != "Staten Island") |> 
  select(price, stars, borough, neighbourhood, room_type)
```

Do regression!!

``` r
fit= lm( price~ stars+ borough, data = nyc_airbnb)
```

Do some additional cleaning then refit

``` r
nyc_airbnb= 
  nyc_airbnb |> 
  mutate(
    borough = fct_infreq(borough), 
    room_type = fct_infreq(room_type) 
  )

fit= lm(price~ stars+ borough, data = nyc_airbnb)
```

Look at `lm` stuff

``` r
summary(fit)
names(summary(fit))
summary(fit)[["coefficients"]]
summary(fit)[["df"]]

fitted.values(fit)
```

``` r
fit |> 
  broom::tidy() |>  
  mutate(
    term= str_replace(term, "borough", "Borough: ")
  ) |>  
  select(term, estimate, p.value) |>  
  knitr:: kable(digits=3)
```

| term              | estimate | p.value |
|:------------------|---------:|--------:|
| (Intercept)       |   19.839 |   0.104 |
| stars             |   31.990 |   0.000 |
| Borough: Brooklyn |  -49.754 |   0.000 |
| Borough: Queens   |  -77.048 |   0.000 |
| Borough: Bronx    |  -90.254 |   0.000 |

``` r
fit |> 
  broom::glance()
```

    ## # A tibble: 1 × 12
    ##   r.squared adj.r.squared sigma statistic   p.value    df   logLik    AIC    BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>     <dbl> <dbl>    <dbl>  <dbl>  <dbl>
    ## 1    0.0342        0.0341  182.      271. 6.73e-229     4 -202113. 4.04e5 4.04e5
    ## # ℹ 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>

## Diagnostics

Look at residuals

``` r
nyc_airbnb |>  
  modelr::add_residuals(fit) |> 
  modelr::add_predictions(fit) |>  
  filter(resid<1000) |> 
  ggplot(aes(x= resid)) +
  geom_histogram()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](linearmodels_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
nyc_airbnb |>  
  modelr::add_residuals(fit) |> 
  modelr::add_predictions(fit) |>  
  filter(resid<1000) |> 
  ggplot(aes(x= borough, y=resid)) +
  geom_violin()
```

![](linearmodels_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

``` r
nyc_airbnb |>  
  modelr::add_residuals(fit) |> 
  modelr::add_predictions(fit) |>  
  filter(resid<1000) |> 
  ggplot(aes(x= stars, y=resid)) +
  geom_point()
```

![](linearmodels_files/figure-gfm/unnamed-chunk-8-3.png)<!-- -->

## Hypothesis Testing

``` r
fit |>  
  broom::tidy()
```

    ## # A tibble: 5 × 5
    ##   term            estimate std.error statistic   p.value
    ##   <chr>              <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)         19.8     12.2       1.63 1.04e-  1
    ## 2 stars               32.0      2.53     12.7  1.27e- 36
    ## 3 boroughBrooklyn    -49.8      2.23    -22.3  6.32e-109
    ## 4 boroughQueens      -77.0      3.73    -20.7  2.58e- 94
    ## 5 boroughBronx       -90.3      8.57    -10.5  6.64e- 26

What about a categorical variable?

``` r
fit_alt= lm(price ~ stars + borough + room_type, data=nyc_airbnb)
fit_null= lm(price ~ stars + borough, data= nyc_airbnb)

anova(fit_null, fit_alt) |> 
  broom::tidy()
```

    ## # A tibble: 2 × 7
    ##   term                        df.residual    rss    df   sumsq statistic p.value
    ##   <chr>                             <dbl>  <dbl> <dbl>   <dbl>     <dbl>   <dbl>
    ## 1 price ~ stars + borough           30525 1.01e9    NA NA            NA       NA
    ## 2 price ~ stars + borough + …       30523 9.21e8     2  8.42e7     1394.       0
