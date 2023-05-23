Exploring the BRFSS data
================
Mohammad Abooei Mehrizi

## Setup

### Load packages

``` r
library(ggplot2)
library(dplyr)
```

### Load data

``` r
load("brfss2013.RData")
```

------------------------------------------------------------------------

## Part 1: Data

I investigated these questions using BRFSS data set. This data was
collected from adult population of United States and its
territories.This data was collected using **Simple Random Sample**
method, hence **Random Sampling** is used for this project which allows
us to **Generalize** the outcome of this project to the population. Also
we didn’t make use of **Random Assignment**, so we cannot make
**Causal** conclusions.

------------------------------------------------------------------------

## Part 2: Research questions

**Research question 1:**

What is the **Checkup Routine** of people with Good **General Health**
state or better? This question is of interest to me, since I am willing
to know if someone is in **good** health state, do they skip their
**checkup** routine because of that, or do they do the checkup to be
sure.

**Research question 2:**

What is the average **Sleep Time** of people diagnosed with **Depressive
Disorder** and is it related to **Gender**? This question is of interest
to me, since I want to know do people with depressive disorder sleep
less or more than people without depressive disorder and if it is
related to gender.

**Research question 3:**

How many **Children** do people with different **Employment Status**
live with in their houses and is it affected by their **Education**?
This question is of interest to me, because I am interested to know how
many children do people with different employment status have and is
their education a **confounding variable** to this study.

------------------------------------------------------------------------

## Part 3: Exploratory data analysis

**Research question 1:**

What is the **Checkup Routine** of people with Good **General Health**
state or better? This question is of interest to me, since I am willing
to know if someone is in **good** health state, do they skip their
**checkup** routine because of that, or do they do the checkup to be
sure.

``` r
brfss2013$genhlth <- as.factor(brfss2013$genhlth)
brfss2013$checkup1 <- as.factor(brfss2013$checkup1)

brfss2013_1 <- brfss2013 %>%
  filter(!is.na(genhlth),!is.na(checkup1),genhlth=="Excellent" |genhlth=="Very good" |genhlth=="Good") 

ggplot(brfss2013_1, aes(x=genhlth, fill=checkup1))+
  geom_bar()
```

![](BRFSS_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
brfss2013_1 <- brfss2013_1 %>%
  mutate(check_length=as.numeric(checkup1))

brfss2013_1 %>% 
  group_by(checkup1,check_length)%>%
  summarise(count=n())
```

    ## `summarise()` has grouped output by 'checkup1'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 5 × 3
    ## # Groups:   checkup1 [5]
    ##   checkup1            check_length  count
    ##   <fct>                      <dbl>  <int>
    ## 1 Within past year               1 281931
    ## 2 Within past 2 years            2  49107
    ## 3 Within past 5 years            3  28605
    ## 4 5 or more years ago            4  27700
    ## 5 Never                          5   3401

``` r
brfss2013_1 %>%
  summarise(mean_check=mean(check_length) , sd_check=sd(check_length) , med_check=median(check_length) , n=n())
```

    ##   mean_check  sd_check med_check      n
    ## 1   1.519575 0.9621962         1 390744

As said in question 1, we are interested in adults with **good** health
state or higher, so we create a new data frame with people of applied
conditions. We also make a new numeric value using the last checkup time
data. As shown in the plot and summary statistics, even if people are in
a good health state, they tend to have done a checkup routine within the
past year or 2 years. The mean of last checkup (check_length) is around
1.5, which means most of the **healthy** people have visited a doctor
for a checkup within the past 3 years. (This question involves 3
variables: genhlth, checkup1, check_length)

**Research question 2:**

What is the average **Sleep Time** of people diagnosed with **Depressive
Disorder** and is it related to **Gender**? This question is of interest
to me, since I want to know do people with depressive disorder sleep
less or more than people without depressive disorder and if it is
related to gender.

``` r
brfss2013_dep <- brfss2013 %>%
  filter(!is.na(sleptim1), !is.na(sex), addepev2=="Yes")

brfss2013_not_dep <- brfss2013 %>%
  filter(!is.na(sleptim1), !is.na(sex), addepev2=="No")

brfss2013_dep %>%
  group_by(sex) %>%
  summarise(mean_sleep=mean(sleptim1),sd_sleep=sd(sleptim1),med_sleep=median(sleptim1)) 
```

    ## # A tibble: 2 × 4
    ##   sex    mean_sleep sd_sleep med_sleep
    ##   <fct>       <dbl>    <dbl>     <dbl>
    ## 1 Male         6.79     1.90         7
    ## 2 Female       6.92     1.82         7

``` r
brfss2013_not_dep %>%
  group_by(sex) %>%
  summarise(mean_sleep=mean(sleptim1),sd_sleep=sd(sleptim1),med_sleep=median(sleptim1))
```

    ## # A tibble: 2 × 4
    ##   sex    mean_sleep sd_sleep med_sleep
    ##   <fct>       <dbl>    <dbl>     <dbl>
    ## 1 Male         7.07     1.35         7
    ## 2 Female       7.11     1.36         7

``` r
ggplot(brfss2013_dep,aes(x=factor(sex),y=sleptim1)) + 
  geom_boxplot()
```

![](BRFSS_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

We want to know the relation between **Depressive Disorder**, **Sleep
Time**, and **Gender**. We create two new data frames based on having
depressive disorder. As shown in the summary statistics an the
side_by_side box plot, females tend to sleep more than males and also
people with depressive disorder tend to sleep **less** than other people
(around 0.3 hour for both females and males). Since this study uses
**random sampling**, we can generalize our outcome to the whole
population. (This question involves 3 variables: addepev2, sex,
sleptim1)

**Research question 3:**

How many **Children** do people with different **Employment Status**
live with in their houses and is it affected by their **Education**?
This question is of interest to me, because I am interested to know how
many children do people with different employment status have and is
their education a **confounding variable** to this study.

``` r
brfss2013_college <- brfss2013 %>%
  filter(!is.na(employ1), !is.na(children), educa=="College 4 years or more (College graduate)")

brfss2013_college <- brfss2013_college %>%
  group_by(employ1) %>%
  mutate(mean_child=mean(children), med_child=median(children))

brfss2013_college %>%
  group_by(employ1,med_child,mean_child) %>%
  summarise(count=n())
```

    ## `summarise()` has grouped output by 'employ1', 'med_child'. You can override
    ## using the `.groups` argument.

    ## # A tibble: 8 × 4
    ## # Groups:   employ1, med_child [8]
    ##   employ1                          med_child mean_child count
    ##   <fct>                                <dbl>      <dbl> <int>
    ## 1 Employed for wages                       0     0.750  86689
    ## 2 Self-employed                            0     0.606  16796
    ## 3 Out of work for 1 year or more           0     0.468   3046
    ## 4 Out of work for less than 1 year         0     0.567   3001
    ## 5 A homemaker                              1     1.43    7282
    ## 6 A student                                0     0.492   3173
    ## 7 Retired                                  0     0.0446 44608
    ## 8 Unable to work                           0     0.320   4276

``` r
ggplot(brfss2013_college,aes(x=factor(employ1),y=mean_child)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = -90, hjust = 0))
```

![](BRFSS_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

We want to study the number of children that people with different
employment status have and if it is affected by their education, so we
make a new data frame of people with college degree. As shown in the
plot and also in the summary statistics, there is no positive nor
negative relation between **employment status** and number of
**children** they live with. Although a **homemaker** seems to have more
kids compared to other people. so the **education** is most likely to be
a **confounding variable** in this study.(This question involves 3
variables: employ1, children, educa)
