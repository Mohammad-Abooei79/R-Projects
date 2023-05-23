Modeling and prediction for movies
================
Mohammad Abooei Mehrizi

## Setup

### Load packages

``` r
library(ggplot2)
library(dplyr)
library(statsr)
```

    ## Warning: package 'statsr' was built under R version 4.2.3

    ## Warning: package 'BayesFactor' was built under R version 4.2.3

    ## Warning: package 'coda' was built under R version 4.2.3

### Load data

``` r
load("movies.Rdata")
```

------------------------------------------------------------------------

## Part 1: Data

The data set is comprised of 651 randomly sampled movies produced and
released before 2016. This data was collected using **Simple Random
Sample** method of movies in IMDB and Rotten Tomatoes (population),
hence **Random Sampling** is used for this project which allows us to
**Generalize** the outcome of this project to the population. Also we
didn’t make use of **Random Assignment**, so we cannot make **Causal**
conclusions.

------------------------------------------------------------------------

## Part 2: Research question

How can we predict the **IMDB Rating** of a movie and what variables are
significant predictor for IMDB rating?

This question is of interest to me because every movie studio would like
make a movie that audience would like to pay for and watch it. A good
representation of this subject is IMDB rating of a movie, so if we
modeled this variable, we could tell a studio what characteristics a
movie should have to become popular.

------------------------------------------------------------------------

## Part 3: Exploratory data analysis

In this part we are going to use plots and summary statistics to test
whether or not some variables that we are not so sure, have a linear
relationship with IMDB_rating.

First we test the significance of the **runtime** of the movie.

``` r
m_run<- lm(imdb_rating~runtime ,data = movies)
summary(m_run)
```

    ## 
    ## Call:
    ## lm(formula = imdb_rating ~ runtime, data = movies)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.3099 -0.5791  0.0714  0.7572  2.2500 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 4.907873   0.227163  21.605  < 2e-16 ***
    ## runtime     0.014965   0.002111   7.088 3.56e-12 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.046 on 648 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.07195,    Adjusted R-squared:  0.07052 
    ## F-statistic: 50.24 on 1 and 648 DF,  p-value: 3.564e-12

``` r
ggplot(data = movies,aes(x=runtime , y=imdb_rating)) + 
  geom_jitter() + 
  stat_smooth(method = "lm" , se=TRUE)
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 1 rows containing non-finite values (`stat_smooth()`).

    ## Warning: Removed 1 rows containing missing values (`geom_point()`).

![](IMDB_reg_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

As we can see, the intercept has a pretty small p_value, meaning that
the runtime variable could be a significant predictor of the
IMDB_rating.

Now we check the conditions for linear regression.

``` r
ggplot(data = m_run ,aes(x=.resid)) + 
  geom_histogram() +
  xlab("Residuals")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](IMDB_reg_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
ggplot(data = m_run ,aes(sample=.resid)) +
  stat_qq()
```

![](IMDB_reg_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->

``` r
ggplot(data = m_run ,aes(x=.fitted , y=.resid)) +
  geom_point() + 
  geom_hline(yintercept = 0 , linetype ="dashed") +
  xlab("Fitted values") + 
  ylab("Residuals")
```

![](IMDB_reg_files/figure-gfm/unnamed-chunk-2-3.png)<!-- -->

As we can see from the plots, the first two conditions (linearity and
nearly normal residuals) are met. But the 3rd condition, constant
variability, is not met so we cannot use the runtime variable in the
model.

Now we test the significance of the **MPAA rating** of the movie.

``` r
m_mpaa<- lm(imdb_rating~mpaa_rating ,data = movies)
summary(m_mpaa)
```

    ## 
    ## Call:
    ## lm(formula = imdb_rating ~ mpaa_rating, data = movies)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.2812 -0.5551  0.1160  0.6778  2.4778 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         6.66316    0.24054  27.701   <2e-16 ***
    ## mpaa_ratingNC-17    0.03684    0.77943   0.047   0.9623    
    ## mpaa_ratingPG      -0.30807    0.25918  -1.189   0.2350    
    ## mpaa_ratingPG-13   -0.48195    0.25715  -1.874   0.0613 .  
    ## mpaa_ratingR       -0.14097    0.24739  -0.570   0.5690    
    ## mpaa_ratingUnrated  0.72084    0.28257   2.551   0.0110 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.048 on 645 degrees of freedom
    ## Multiple R-squared:  0.07294,    Adjusted R-squared:  0.06575 
    ## F-statistic: 10.15 on 5 and 645 DF,  p-value: 2.261e-09

``` r
ggplot(data = movies,aes(x=mpaa_rating , y=imdb_rating)) + 
  geom_jitter() + 
  stat_smooth(method = "lm" , se=TRUE)
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](IMDB_reg_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

As we can see from the plot and the summary statistics, the mpaa_rating
as a whole could not be a significant predictor of the IMDB_rating due
its large p_values and very small adjusted R_squared.

------------------------------------------------------------------------

## Part 4: Modeling

For our full model, we are going to **exclude** some variables since
they are only for **informational** purposes. These variables are :

title: Title of movie

studio: Studio that produced the movie

thtr_rel_year: Year the movie is released in theaters

thtr_rel_month: Month the movie is released in theaters

thtr_rel_day: Day of the month the movie is released in theaters

dvd_rel_year: Year the movie is released on DVD

dvd_rel_month: Month the movie is released on DVD

dvd_rel_day: Day of the month the movie is released on DVD

director: Director of the movie

actor1: First main actor/actress in the abridged cast of the movie

actor2: Second main actor/actress in the abridged cast of the movie

actor3: Third main actor/actress in the abridged cast of the movie

actor4: Fourth main actor/actress in the abridged cast of the movie

actor5: Fifth main actor/actress in the abridged cast of the movie

imdb_url: Link to IMDB page for the movie

rt_url: Link to Rotten Tomatoes page for the movie

We are also going to exclude some variables that are **collinear**.These
variables are:

critics_rating: Categorical variable for critics rating on Rotten
Tomatoes (Certified Fresh, Fresh, Rotten)

audience_rating: Categorical variable for audience rating on Rotten
Tomatoes (Spilled, Upright)

It is reasonable to use critics_score and audience_score as represntives
of these variables.

``` r
m_critics <- lm(critics_score~critics_rating, data = movies) 
summary(m_critics)
```

    ## 
    ## Call:
    ## lm(formula = critics_score ~ critics_rating, data = movies)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -30.4788 -10.3445  -0.4788  10.1407  27.5212 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            86.859      1.161   74.83  < 2e-16 ***
    ## critics_ratingFresh    -9.515      1.489   -6.39 3.18e-10 ***
    ## critics_ratingRotten  -55.380      1.393  -39.76  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 13.49 on 648 degrees of freedom
    ## Multiple R-squared:  0.7752, Adjusted R-squared:  0.7746 
    ## F-statistic:  1118 on 2 and 648 DF,  p-value: < 2.2e-16

``` r
m_audience <- lm(audience_score~audience_rating, data = movies) 
summary(m_audience)
```

    ## 
    ## Call:
    ## lm(formula = audience_score ~ audience_rating, data = movies)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -30.9345  -7.3032   0.6968   8.6968  19.6968 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)             41.9345     0.6133   68.38   <2e-16 ***
    ## audience_ratingUpright  35.3686     0.8070   43.83   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 10.17 on 649 degrees of freedom
    ## Multiple R-squared:  0.7475, Adjusted R-squared:  0.7471 
    ## F-statistic:  1921 on 1 and 649 DF,  p-value: < 2.2e-16

Also we are going to exclude some variables since they are not
**significant predictor** of the IMDB_rating as we discussed them in the
last section. these variables are:

runtime: Runtime of movie (in minutes)

mpaa_rating: MPAA rating of the movie (G, PG, PG-13, R, Unrated)

As a result, the variables we are going to consider for our **full
model** are:

title_type: Type of movie (Documentary, Feature Film, TV Movie)

genre: Genre of movie (Action & Adventure, Comedy, Documentary, Drama,
Horror, Mystery & Suspense, Other)

studio: Studio that produced the movie

imdb_num_votes: Number of votes on IMDB

critics_score: Critics score on Rotten Tomatoes

audience_score: Audience score on Rotten Tomatoes

best_pic_nom: Whether or not the movie was nominated for a best picture
Oscar (no, yes)

best_pic_win: Whether or not the movie won a best picture Oscar (no,
yes)

best_actor_win: Whether or not one of the main actors in the movie ever
won an Oscar (no, yes) – note that this is not necessarily whether the
actor won an Oscar for their role in the given movie

best_actress_win: Whether or not one of the main actresses in the movie
ever won an Oscar (no, yes) – not that this is not necessarily whether
the actresses won an Oscar for their role in the given movie

best_dir_win: Whether or not the director of the movie ever won an Oscar
(no, yes) – not that this is not necessarily whether the director won an
Oscar for the given movie

top200_box: Whether or not the movie is in the Top 200 Box Office list
on BoxOfficeMojo (no, yes)

**Model selection method**

Backwards selection:adjusted R_squared

We are going to start with the full model and drop one variable at a
time and pick the model with the highest increase in adjusted R_squared.

***Step 1: Drop one variable at a time.***

``` r
#full_model_1
full_model <- lm(imdb_rating ~ title_type+genre+imdb_num_votes+critics_score+audience_score+best_pic_nom+best_pic_win+best_actor_win+best_actress_win+best_dir_win+top200_box , data = movies) 
summary(full_model)
```

    ## 
    ## Call:
    ## lm(formula = imdb_rating ~ title_type + genre + imdb_num_votes + 
    ##     critics_score + audience_score + best_pic_nom + best_pic_win + 
    ##     best_actor_win + best_actress_win + best_dir_win + top200_box, 
    ##     data = movies)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.46385 -0.18842  0.03685  0.27089  1.11104 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                     3.807e+00  1.936e-01  19.668  < 2e-16 ***
    ## title_typeFeature Film         -1.010e-01  1.722e-01  -0.587  0.55775    
    ## title_typeTV Movie             -3.376e-01  2.714e-01  -1.244  0.21408    
    ## genreAnimation                 -4.326e-01  1.658e-01  -2.610  0.00928 ** 
    ## genreArt House & International  2.782e-01  1.385e-01   2.008  0.04508 *  
    ## genreComedy                    -1.569e-01  7.704e-02  -2.037  0.04205 *  
    ## genreDocumentary                2.549e-01  1.835e-01   1.389  0.16517    
    ## genreDrama                      1.114e-01  6.702e-02   1.662  0.09710 .  
    ## genreHorror                     7.830e-02  1.138e-01   0.688  0.49150    
    ## genreMusical & Performing Arts  1.218e-01  1.591e-01   0.766  0.44408    
    ## genreMystery & Suspense         2.651e-01  8.547e-02   3.101  0.00201 ** 
    ## genreOther                     -4.102e-02  1.320e-01  -0.311  0.75602    
    ## genreScience Fiction & Fantasy -2.078e-01  1.654e-01  -1.256  0.20956    
    ## imdb_num_votes                  1.052e-06  1.958e-07   5.373 1.09e-07 ***
    ## critics_score                   1.026e-02  9.503e-04  10.798  < 2e-16 ***
    ## audience_score                  3.257e-02  1.364e-03  23.875  < 2e-16 ***
    ## best_pic_nomyes                -5.556e-02  1.214e-01  -0.458  0.64739    
    ## best_pic_winyes                -9.668e-02  2.145e-01  -0.451  0.65237    
    ## best_actor_winyes               7.182e-02  5.416e-02   1.326  0.18528    
    ## best_actress_winyes             8.107e-02  6.081e-02   1.333  0.18294    
    ## best_dir_winyes                 9.083e-02  7.913e-02   1.148  0.25143    
    ## top200_boxyes                  -1.275e-01  1.283e-01  -0.993  0.32098    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4635 on 629 degrees of freedom
    ## Multiple R-squared:  0.8233, Adjusted R-squared:  0.8174 
    ## F-statistic: 139.6 on 21 and 629 DF,  p-value: < 2.2e-16

``` r
#-[title_type]
full_model <- lm(imdb_rating ~ genre+imdb_num_votes+critics_score+audience_score+best_pic_nom+best_pic_win+best_actor_win+best_actress_win+best_dir_win+top200_box , data = movies) 
summary(full_model)
```

    ## 
    ## Call:
    ## lm(formula = imdb_rating ~ genre + imdb_num_votes + critics_score + 
    ##     audience_score + best_pic_nom + best_pic_win + best_actor_win + 
    ##     best_actress_win + best_dir_win + top200_box, data = movies)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.46075 -0.18260  0.03507  0.26687  1.11610 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                     3.702e+00  8.042e-02  46.031  < 2e-16 ***
    ## genreAnimation                 -4.332e-01  1.657e-01  -2.614 0.009169 ** 
    ## genreArt House & International  2.778e-01  1.385e-01   2.006 0.045292 *  
    ## genreComedy                    -1.542e-01  7.693e-02  -2.005 0.045421 *  
    ## genreDocumentary                3.472e-01  9.700e-02   3.579 0.000372 ***
    ## genreDrama                      1.069e-01  6.690e-02   1.598 0.110478    
    ## genreHorror                     7.893e-02  1.137e-01   0.694 0.487826    
    ## genreMusical & Performing Arts  1.529e-01  1.501e-01   1.018 0.308843    
    ## genreMystery & Suspense         2.643e-01  8.543e-02   3.094 0.002062 ** 
    ## genreOther                     -5.769e-02  1.312e-01  -0.440 0.660294    
    ## genreScience Fiction & Fantasy -2.078e-01  1.654e-01  -1.257 0.209398    
    ## imdb_num_votes                  1.055e-06  1.954e-07   5.399  9.5e-08 ***
    ## critics_score                   1.030e-02  9.412e-04  10.947  < 2e-16 ***
    ## audience_score                  3.262e-02  1.363e-03  23.932  < 2e-16 ***
    ## best_pic_nomyes                -5.387e-02  1.213e-01  -0.444 0.657217    
    ## best_pic_winyes                -9.896e-02  2.144e-01  -0.462 0.644568    
    ## best_actor_winyes               7.393e-02  5.408e-02   1.367 0.172111    
    ## best_actress_winyes             7.919e-02  6.077e-02   1.303 0.192977    
    ## best_dir_winyes                 9.079e-02  7.902e-02   1.149 0.251010    
    ## top200_boxyes                  -1.276e-01  1.283e-01  -0.994 0.320379    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4633 on 631 degrees of freedom
    ## Multiple R-squared:  0.8229, Adjusted R-squared:  0.8176 
    ## F-statistic: 154.3 on 19 and 631 DF,  p-value: < 2.2e-16

``` r
#-[genre]
full_model <- lm(imdb_rating ~ title_type+imdb_num_votes+critics_score+audience_score+best_pic_nom+best_pic_win+best_actor_win+best_actress_win+best_dir_win+top200_box , data = movies) 
summary(full_model)
```

    ## 
    ## Call:
    ## lm(formula = imdb_rating ~ title_type + imdb_num_votes + critics_score + 
    ##     audience_score + best_pic_nom + best_pic_win + best_actor_win + 
    ##     best_actress_win + best_dir_win + top200_box, data = movies)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.51000 -0.18083  0.03193  0.28794  1.18812 
    ## 
    ## Coefficients:
    ##                          Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)             3.937e+00  1.100e-01  35.805  < 2e-16 ***
    ## title_typeFeature Film -2.352e-01  7.567e-02  -3.108  0.00197 ** 
    ## title_typeTV Movie     -4.578e-01  2.255e-01  -2.030  0.04277 *  
    ## imdb_num_votes          9.964e-07  1.986e-07   5.018 6.77e-07 ***
    ## critics_score           1.113e-02  9.571e-04  11.630  < 2e-16 ***
    ## audience_score          3.284e-02  1.363e-03  24.095  < 2e-16 ***
    ## best_pic_nomyes        -5.887e-02  1.247e-01  -0.472  0.63706    
    ## best_pic_winyes        -9.378e-02  2.206e-01  -0.425  0.67086    
    ## best_actor_winyes       1.038e-01  5.519e-02   1.881  0.06045 .  
    ## best_actress_winyes     1.023e-01  6.193e-02   1.652  0.09902 .  
    ## best_dir_winyes         9.659e-02  8.150e-02   1.185  0.23643    
    ## top200_boxyes          -1.730e-01  1.310e-01  -1.321  0.18699    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4783 on 639 degrees of freedom
    ## Multiple R-squared:  0.8088, Adjusted R-squared:  0.8056 
    ## F-statistic: 245.8 on 11 and 639 DF,  p-value: < 2.2e-16

``` r
#-[imdb_num_votes]
full_model <- lm(imdb_rating ~ title_type+genre+critics_score+audience_score+best_pic_nom+best_pic_win+best_actor_win+best_actress_win+best_dir_win+top200_box , data = movies) 
summary(full_model)
```

    ## 
    ## Call:
    ## lm(formula = imdb_rating ~ title_type + genre + critics_score + 
    ##     audience_score + best_pic_nom + best_pic_win + best_actor_win + 
    ##     best_actress_win + best_dir_win + top200_box, data = movies)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.39535 -0.22033  0.04105  0.28402  1.19597 
    ## 
    ## Coefficients:
    ##                                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                     3.731985   0.197305  18.915  < 2e-16 ***
    ## title_typeFeature Film         -0.053451   0.175776  -0.304  0.76116    
    ## title_typeTV Movie             -0.333504   0.277360  -1.202  0.22965    
    ## genreAnimation                 -0.459726   0.169343  -2.715  0.00681 ** 
    ## genreArt House & International  0.198168   0.140749   1.408  0.15964    
    ## genreComedy                    -0.183928   0.078554  -2.341  0.01952 *  
    ## genreDocumentary                0.181667   0.186970   0.972  0.33160    
    ## genreDrama                      0.071051   0.068055   1.044  0.29687    
    ## genreHorror                     0.049892   0.116115   0.430  0.66758    
    ## genreMusical & Performing Arts  0.043657   0.161881   0.270  0.78749    
    ## genreMystery & Suspense         0.262414   0.087334   3.005  0.00276 ** 
    ## genreOther                     -0.028140   0.134838  -0.209  0.83475    
    ## genreScience Fiction & Fantasy -0.203136   0.169026  -1.202  0.22989    
    ## critics_score                   0.010312   0.000971  10.619  < 2e-16 ***
    ## audience_score                  0.034352   0.001352  25.399  < 2e-16 ***
    ## best_pic_nomyes                 0.033996   0.122894   0.277  0.78215    
    ## best_pic_winyes                 0.102729   0.215914   0.476  0.63439    
    ## best_actor_winyes               0.072409   0.055341   1.308  0.19121    
    ## best_actress_winyes             0.091906   0.062101   1.480  0.13939    
    ## best_dir_winyes                 0.114647   0.080731   1.420  0.15607    
    ## top200_boxyes                   0.033542   0.127503   0.263  0.79258    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4736 on 630 degrees of freedom
    ## Multiple R-squared:  0.8152, Adjusted R-squared:  0.8094 
    ## F-statistic:   139 on 20 and 630 DF,  p-value: < 2.2e-16

``` r
#-[critics_score]
full_model <- lm(imdb_rating ~ title_type+genre+imdb_num_votes+audience_score+best_pic_nom+best_pic_win+best_actor_win+best_actress_win+best_dir_win+top200_box , data = movies) 
summary(full_model)
```

    ## 
    ## Call:
    ## lm(formula = imdb_rating ~ title_type + genre + imdb_num_votes + 
    ##     audience_score + best_pic_nom + best_pic_win + best_actor_win + 
    ##     best_actress_win + best_dir_win + top200_box, data = movies)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.77029 -0.17782  0.06381  0.28583  1.05645 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                     3.994e+00  2.098e-01  19.040  < 2e-16 ***
    ## title_typeFeature Film         -3.478e-01  1.857e-01  -1.873  0.06158 .  
    ## title_typeTV Movie             -5.164e-01  2.947e-01  -1.752  0.08021 .  
    ## genreAnimation                 -4.085e-01  1.803e-01  -2.265  0.02384 *  
    ## genreArt House & International  3.063e-01  1.507e-01   2.033  0.04250 *  
    ## genreComedy                    -1.516e-01  8.380e-02  -1.809  0.07086 .  
    ## genreDocumentary                2.429e-01  1.996e-01   1.217  0.22402    
    ## genreDrama                      2.191e-01  7.210e-02   3.038  0.00248 ** 
    ## genreHorror                     1.842e-01  1.233e-01   1.494  0.13558    
    ## genreMusical & Performing Arts  1.762e-01  1.730e-01   1.018  0.30887    
    ## genreMystery & Suspense         3.794e-01  9.226e-02   4.112 4.44e-05 ***
    ## genreOther                      7.244e-02  1.431e-01   0.506  0.61292    
    ## genreScience Fiction & Fantasy -1.002e-01  1.796e-01  -0.558  0.57725    
    ## imdb_num_votes                  1.073e-06  2.130e-07   5.038 6.15e-07 ***
    ## audience_score                  4.133e-02  1.193e-03  34.642  < 2e-16 ***
    ## best_pic_nomyes                -1.002e-03  1.320e-01  -0.008  0.99394    
    ## best_pic_winyes                -8.531e-02  2.334e-01  -0.366  0.71483    
    ## best_actor_winyes               8.358e-02  5.890e-02   1.419  0.15641    
    ## best_actress_winyes             9.946e-02  6.612e-02   1.504  0.13302    
    ## best_dir_winyes                 1.707e-01  8.570e-02   1.992  0.04684 *  
    ## top200_boxyes                  -4.316e-02  1.393e-01  -0.310  0.75685    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5042 on 630 degrees of freedom
    ## Multiple R-squared:  0.7906, Adjusted R-squared:  0.784 
    ## F-statistic: 118.9 on 20 and 630 DF,  p-value: < 2.2e-16

``` r
#-[audience_score]
full_model <- lm(imdb_rating ~ title_type+genre+imdb_num_votes+critics_score+best_pic_nom+best_pic_win+best_actor_win+best_actress_win+best_dir_win+top200_box , data = movies) 
summary(full_model)
```

    ## 
    ## Call:
    ## lm(formula = imdb_rating ~ title_type + genre + imdb_num_votes + 
    ##     critics_score + best_pic_nom + best_pic_win + best_actor_win + 
    ##     best_actress_win + best_dir_win + top200_box, data = movies)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.95225 -0.33818  0.06426  0.40669  1.92014 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                     4.902e+00  2.595e-01  18.894  < 2e-16 ***
    ## title_typeFeature Film         -8.733e-02  2.376e-01  -0.368  0.71335    
    ## title_typeTV Movie             -4.931e-01  3.743e-01  -1.317  0.18827    
    ## genreAnimation                 -2.378e-01  2.284e-01  -1.041  0.29827    
    ## genreArt House & International  5.523e-01  1.905e-01   2.900  0.00387 ** 
    ## genreComedy                    -1.466e-01  1.063e-01  -1.380  0.16819    
    ## genreDocumentary                6.861e-01  2.519e-01   2.724  0.00663 ** 
    ## genreDrama                      2.322e-01  9.219e-02   2.519  0.01201 *  
    ## genreHorror                    -1.612e-01  1.563e-01  -1.031  0.30287    
    ## genreMusical & Performing Arts  5.693e-01  2.179e-01   2.612  0.00921 ** 
    ## genreMystery & Suspense         1.642e-01  1.178e-01   1.395  0.16361    
    ## genreOther                      7.385e-03  1.820e-01   0.041  0.96766    
    ## genreScience Fiction & Fantasy -4.278e-01  2.278e-01  -1.878  0.06090 .  
    ## imdb_num_votes                  2.187e-06  2.621e-07   8.343 4.59e-16 ***
    ## critics_score                   2.375e-02  1.054e-03  22.538  < 2e-16 ***
    ## best_pic_nomyes                 1.767e-01  1.670e-01   1.058  0.29029    
    ## best_pic_winyes                -3.483e-01  2.956e-01  -1.178  0.23906    
    ## best_actor_winyes               3.505e-02  7.468e-02   0.469  0.63905    
    ## best_actress_winyes             1.393e-02  8.380e-02   0.166  0.86806    
    ## best_dir_winyes                 8.777e-02  1.092e-01   0.804  0.42166    
    ## top200_boxyes                  -1.505e-01  1.770e-01  -0.850  0.39558    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.6394 on 630 degrees of freedom
    ## Multiple R-squared:  0.6633, Adjusted R-squared:  0.6526 
    ## F-statistic: 62.04 on 20 and 630 DF,  p-value: < 2.2e-16

``` r
#-[best_pic_nom]
full_model <- lm(imdb_rating ~ title_type+genre+imdb_num_votes+critics_score+audience_score+best_pic_win+best_actor_win+best_actress_win+best_dir_win+top200_box , data = movies) 
summary(full_model)
```

    ## 
    ## Call:
    ## lm(formula = imdb_rating ~ title_type + genre + imdb_num_votes + 
    ##     critics_score + audience_score + best_pic_win + best_actor_win + 
    ##     best_actress_win + best_dir_win + top200_box, data = movies)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.46585 -0.18651  0.03401  0.27012  1.11174 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                     3.813e+00  1.931e-01  19.746  < 2e-16 ***
    ## title_typeFeature Film         -1.019e-01  1.721e-01  -0.592  0.55393    
    ## title_typeTV Movie             -3.367e-01  2.712e-01  -1.241  0.21495    
    ## genreAnimation                 -4.317e-01  1.657e-01  -2.606  0.00938 ** 
    ## genreArt House & International  2.781e-01  1.385e-01   2.009  0.04501 *  
    ## genreComedy                    -1.572e-01  7.698e-02  -2.043  0.04151 *  
    ## genreDocumentary                2.555e-01  1.834e-01   1.393  0.16401    
    ## genreDrama                      1.104e-01  6.695e-02   1.649  0.09955 .  
    ## genreHorror                     7.707e-02  1.136e-01   0.678  0.49794    
    ## genreMusical & Performing Arts  1.228e-01  1.590e-01   0.772  0.44016    
    ## genreMystery & Suspense         2.653e-01  8.541e-02   3.106  0.00198 ** 
    ## genreOther                     -4.579e-02  1.315e-01  -0.348  0.72773    
    ## genreScience Fiction & Fantasy -2.082e-01  1.653e-01  -1.259  0.20834    
    ## imdb_num_votes                  1.040e-06  1.939e-07   5.365 1.14e-07 ***
    ## critics_score                   1.024e-02  9.489e-04  10.795  < 2e-16 ***
    ## audience_score                  3.252e-02  1.359e-03  23.931  < 2e-16 ***
    ## best_pic_winyes                -1.364e-01  1.960e-01  -0.696  0.48677    
    ## best_actor_winyes               6.841e-02  5.361e-02   1.276  0.20238    
    ## best_actress_winyes             7.783e-02  6.036e-02   1.290  0.19766    
    ## best_dir_winyes                 9.276e-02  7.896e-02   1.175  0.24055    
    ## top200_boxyes                  -1.261e-01  1.282e-01  -0.984  0.32561    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4632 on 630 degrees of freedom
    ## Multiple R-squared:  0.8233, Adjusted R-squared:  0.8177 
    ## F-statistic: 146.8 on 20 and 630 DF,  p-value: < 2.2e-16

``` r
#-[best_pic_win]
full_model <- lm(imdb_rating ~ title_type+genre+imdb_num_votes+critics_score+audience_score+best_pic_nom+best_actor_win+best_actress_win+best_dir_win+top200_box , data = movies) 
summary(full_model)
```

    ## 
    ## Call:
    ## lm(formula = imdb_rating ~ title_type + genre + imdb_num_votes + 
    ##     critics_score + audience_score + best_pic_nom + best_actor_win + 
    ##     best_actress_win + best_dir_win + top200_box, data = movies)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.46351 -0.18784  0.03537  0.26986  1.11167 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                     3.807e+00  1.935e-01  19.677  < 2e-16 ***
    ## title_typeFeature Film         -1.000e-01  1.721e-01  -0.581  0.56125    
    ## title_typeTV Movie             -3.381e-01  2.712e-01  -1.246  0.21305    
    ## genreAnimation                 -4.337e-01  1.657e-01  -2.618  0.00906 ** 
    ## genreArt House & International  2.764e-01  1.384e-01   1.997  0.04620 *  
    ## genreComedy                    -1.583e-01  7.693e-02  -2.058  0.04005 *  
    ## genreDocumentary                2.534e-01  1.833e-01   1.382  0.16732    
    ## genreDrama                      1.107e-01  6.696e-02   1.654  0.09872 .  
    ## genreHorror                     7.773e-02  1.137e-01   0.684  0.49435    
    ## genreMusical & Performing Arts  1.208e-01  1.590e-01   0.760  0.44745    
    ## genreMystery & Suspense         2.645e-01  8.540e-02   3.097  0.00204 ** 
    ## genreOther                     -3.783e-02  1.317e-01  -0.287  0.77404    
    ## genreScience Fiction & Fantasy -2.069e-01  1.653e-01  -1.252  0.21121    
    ## imdb_num_votes                  1.037e-06  1.928e-07   5.380 1.05e-07 ***
    ## critics_score                   1.026e-02  9.497e-04  10.802  < 2e-16 ***
    ## audience_score                  3.260e-02  1.362e-03  23.942  < 2e-16 ***
    ## best_pic_nomyes                -7.771e-02  1.110e-01  -0.700  0.48395    
    ## best_actor_winyes               7.359e-02  5.398e-02   1.363  0.17329    
    ## best_actress_winyes             7.977e-02  6.070e-02   1.314  0.18924    
    ## best_dir_winyes                 8.077e-02  7.586e-02   1.065  0.28744    
    ## top200_boxyes                  -1.277e-01  1.282e-01  -0.996  0.31965    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4632 on 630 degrees of freedom
    ## Multiple R-squared:  0.8233, Adjusted R-squared:  0.8177 
    ## F-statistic: 146.8 on 20 and 630 DF,  p-value: < 2.2e-16

``` r
#-[best_actor_win]
full_model <- lm(imdb_rating ~ title_type+genre+imdb_num_votes+critics_score+audience_score+best_pic_nom+best_pic_win+best_actress_win+best_dir_win+top200_box , data = movies) 
summary(full_model)
```

    ## 
    ## Call:
    ## lm(formula = imdb_rating ~ title_type + genre + imdb_num_votes + 
    ##     critics_score + audience_score + best_pic_nom + best_pic_win + 
    ##     best_actress_win + best_dir_win + top200_box, data = movies)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.47085 -0.19232  0.03151  0.26756  1.10043 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                     3.812e+00  1.937e-01  19.685  < 2e-16 ***
    ## title_typeFeature Film         -9.758e-02  1.723e-01  -0.566  0.57140    
    ## title_typeTV Movie             -3.460e-01  2.715e-01  -1.274  0.20299    
    ## genreAnimation                 -4.319e-01  1.659e-01  -2.603  0.00945 ** 
    ## genreArt House & International  2.713e-01  1.385e-01   1.958  0.05062 .  
    ## genreComedy                    -1.581e-01  7.708e-02  -2.052  0.04060 *  
    ## genreDocumentary                2.545e-01  1.836e-01   1.386  0.16619    
    ## genreDrama                      1.151e-01  6.700e-02   1.718  0.08627 .  
    ## genreHorror                     7.076e-02  1.137e-01   0.622  0.53385    
    ## genreMusical & Performing Arts  1.220e-01  1.592e-01   0.767  0.44359    
    ## genreMystery & Suspense         2.751e-01  8.518e-02   3.230  0.00130 ** 
    ## genreOther                     -3.797e-02  1.320e-01  -0.288  0.77373    
    ## genreScience Fiction & Fantasy -2.165e-01  1.654e-01  -1.309  0.19097    
    ## imdb_num_votes                  1.053e-06  1.959e-07   5.373 1.09e-07 ***
    ## critics_score                   1.029e-02  9.507e-04  10.820  < 2e-16 ***
    ## audience_score                  3.252e-02  1.365e-03  23.833  < 2e-16 ***
    ## best_pic_nomyes                -3.342e-02  1.203e-01  -0.278  0.78128    
    ## best_pic_winyes                -1.173e-01  2.141e-01  -0.548  0.58390    
    ## best_actress_winyes             8.740e-02  6.065e-02   1.441  0.15007    
    ## best_dir_winyes                 9.914e-02  7.893e-02   1.256  0.20955    
    ## top200_boxyes                  -1.210e-01  1.283e-01  -0.943  0.34595    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4637 on 630 degrees of freedom
    ## Multiple R-squared:  0.8229, Adjusted R-squared:  0.8172 
    ## F-statistic: 146.3 on 20 and 630 DF,  p-value: < 2.2e-16

``` r
#-[best_actress_win]
full_model <- lm(imdb_rating ~ title_type+genre+imdb_num_votes+critics_score+audience_score+best_pic_nom+best_pic_win+best_actor_win++best_dir_win+top200_box , data = movies) 
summary(full_model)
```

    ## 
    ## Call:
    ## lm(formula = imdb_rating ~ title_type + genre + imdb_num_votes + 
    ##     critics_score + audience_score + best_pic_nom + best_pic_win + 
    ##     best_actor_win + +best_dir_win + top200_box, data = movies)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.46308 -0.18976  0.03594  0.26917  1.10025 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                     3.807e+00  1.937e-01  19.655  < 2e-16 ***
    ## title_typeFeature Film         -1.000e-01  1.723e-01  -0.580  0.56184    
    ## title_typeTV Movie             -3.295e-01  2.715e-01  -1.214  0.22535    
    ## genreAnimation                 -4.221e-01  1.657e-01  -2.547  0.01109 *  
    ## genreArt House & International  2.866e-01  1.385e-01   2.070  0.03890 *  
    ## genreComedy                    -1.479e-01  7.678e-02  -1.926  0.05453 .  
    ## genreDocumentary                2.602e-01  1.835e-01   1.418  0.15678    
    ## genreDrama                      1.233e-01  6.646e-02   1.855  0.06410 .  
    ## genreHorror                     7.943e-02  1.138e-01   0.698  0.48553    
    ## genreMusical & Performing Arts  1.245e-01  1.592e-01   0.782  0.43448    
    ## genreMystery & Suspense         2.772e-01  8.503e-02   3.260  0.00117 ** 
    ## genreOther                     -3.410e-02  1.320e-01  -0.258  0.79615    
    ## genreScience Fiction & Fantasy -2.082e-01  1.655e-01  -1.258  0.20888    
    ## imdb_num_votes                  1.061e-06  1.958e-07   5.417 8.63e-08 ***
    ## critics_score                   1.030e-02  9.505e-04  10.832  < 2e-16 ***
    ## audience_score                  3.249e-02  1.364e-03  23.825  < 2e-16 ***
    ## best_pic_nomyes                -3.676e-02  1.207e-01  -0.305  0.76076    
    ## best_pic_winyes                -8.319e-02  2.144e-01  -0.388  0.69817    
    ## best_actor_winyes               7.749e-02  5.402e-02   1.434  0.15194    
    ## best_dir_winyes                 9.278e-02  7.916e-02   1.172  0.24164    
    ## top200_boxyes                  -1.172e-01  1.282e-01  -0.915  0.36076    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4638 on 630 degrees of freedom
    ## Multiple R-squared:  0.8228, Adjusted R-squared:  0.8172 
    ## F-statistic: 146.3 on 20 and 630 DF,  p-value: < 2.2e-16

``` r
#-[best_dir_win]
full_model <- lm(imdb_rating ~ title_type+genre+imdb_num_votes+critics_score+audience_score+best_pic_nom+best_pic_win+best_actor_win+best_actress_win+top200_box , data = movies) 
summary(full_model)
```

    ## 
    ## Call:
    ## lm(formula = imdb_rating ~ title_type + genre + imdb_num_votes + 
    ##     critics_score + audience_score + best_pic_nom + best_pic_win + 
    ##     best_actor_win + best_actress_win + top200_box, data = movies)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.46534 -0.18941  0.03408  0.27080  1.10742 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                     3.800e+00  1.935e-01  19.635  < 2e-16 ***
    ## title_typeFeature Film         -9.310e-02  1.721e-01  -0.541  0.58882    
    ## title_typeTV Movie             -3.346e-01  2.715e-01  -1.233  0.21820    
    ## genreAnimation                 -4.393e-01  1.657e-01  -2.650  0.00824 ** 
    ## genreArt House & International  2.726e-01  1.385e-01   1.968  0.04946 *  
    ## genreComedy                    -1.587e-01  7.704e-02  -2.060  0.03981 *  
    ## genreDocumentary                2.533e-01  1.835e-01   1.380  0.16806    
    ## genreDrama                      1.099e-01  6.702e-02   1.640  0.10157    
    ## genreHorror                     7.728e-02  1.138e-01   0.679  0.49725    
    ## genreMusical & Performing Arts  1.234e-01  1.591e-01   0.776  0.43820    
    ## genreMystery & Suspense         2.651e-01  8.549e-02   3.101  0.00201 ** 
    ## genreOther                     -4.320e-02  1.320e-01  -0.327  0.74358    
    ## genreScience Fiction & Fantasy -2.036e-01  1.654e-01  -1.231  0.21892    
    ## imdb_num_votes                  1.065e-06  1.956e-07   5.445 7.44e-08 ***
    ## critics_score                   1.036e-02  9.464e-04  10.950  < 2e-16 ***
    ## audience_score                  3.257e-02  1.365e-03  23.868  < 2e-16 ***
    ## best_pic_nomyes                -6.299e-02  1.213e-01  -0.519  0.60368    
    ## best_pic_winyes                -2.716e-02  2.059e-01  -0.132  0.89506    
    ## best_actor_winyes               7.674e-02  5.400e-02   1.421  0.15579    
    ## best_actress_winyes             8.235e-02  6.081e-02   1.354  0.17613    
    ## top200_boxyes                  -1.313e-01  1.283e-01  -1.023  0.30672    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4636 on 630 degrees of freedom
    ## Multiple R-squared:  0.823,  Adjusted R-squared:  0.8174 
    ## F-statistic: 146.4 on 20 and 630 DF,  p-value: < 2.2e-16

``` r
#-[top200_box]
full_model <- lm(imdb_rating ~ title_type+genre+imdb_num_votes+critics_score+audience_score+best_pic_nom+best_pic_win+best_actor_win+best_actress_win+best_dir_win , data = movies) 
summary(full_model)
```

    ## 
    ## Call:
    ## lm(formula = imdb_rating ~ title_type + genre + imdb_num_votes + 
    ##     critics_score + audience_score + best_pic_nom + best_pic_win + 
    ##     best_actor_win + best_actress_win + best_dir_win, data = movies)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.45578 -0.18977  0.03517  0.27057  1.11330 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                     3.804e+00  1.936e-01  19.654  < 2e-16 ***
    ## title_typeFeature Film         -1.021e-01  1.722e-01  -0.593  0.55345    
    ## title_typeTV Movie             -3.381e-01  2.714e-01  -1.246  0.21332    
    ## genreAnimation                 -4.229e-01  1.655e-01  -2.555  0.01084 *  
    ## genreArt House & International  2.854e-01  1.383e-01   2.063  0.03952 *  
    ## genreComedy                    -1.499e-01  7.671e-02  -1.954  0.05115 .  
    ## genreDocumentary                2.627e-01  1.833e-01   1.433  0.15236    
    ## genreDrama                      1.195e-01  6.652e-02   1.796  0.07292 .  
    ## genreHorror                     8.576e-02  1.135e-01   0.756  0.45019    
    ## genreMusical & Performing Arts  1.305e-01  1.588e-01   0.821  0.41179    
    ## genreMystery & Suspense         2.743e-01  8.496e-02   3.229  0.00131 ** 
    ## genreOther                     -3.581e-02  1.319e-01  -0.272  0.78607    
    ## genreScience Fiction & Fantasy -2.116e-01  1.654e-01  -1.280  0.20107    
    ## imdb_num_votes                  1.007e-06  1.904e-07   5.288 1.71e-07 ***
    ## critics_score                   1.020e-02  9.485e-04  10.757  < 2e-16 ***
    ## audience_score                  3.258e-02  1.364e-03  23.884  < 2e-16 ***
    ## best_pic_nomyes                -5.284e-02  1.214e-01  -0.435  0.66349    
    ## best_pic_winyes                -9.767e-02  2.145e-01  -0.455  0.64905    
    ## best_actor_winyes               6.978e-02  5.412e-02   1.289  0.19770    
    ## best_actress_winyes             7.746e-02  6.070e-02   1.276  0.20238    
    ## best_dir_winyes                 9.286e-02  7.910e-02   1.174  0.24084    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4635 on 630 degrees of freedom
    ## Multiple R-squared:  0.8231, Adjusted R-squared:  0.8175 
    ## F-statistic: 146.5 on 20 and 630 DF,  p-value: < 2.2e-16

***Step 2 : Drop best_pic_nom and best_pic_win since they both increase
adjusted R_squared equally.***

``` r
#full_model_2
full_model <- lm(imdb_rating ~ title_type+genre+imdb_num_votes+critics_score+audience_score+best_actor_win+best_actress_win+best_dir_win+top200_box , data = movies) 
summary(full_model)
```

    ## 
    ## Call:
    ## lm(formula = imdb_rating ~ title_type + genre + imdb_num_votes + 
    ##     critics_score + audience_score + best_actor_win + best_actress_win + 
    ##     best_dir_win + top200_box, data = movies)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.46662 -0.18754  0.03623  0.26972  1.11328 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                     3.815e+00  1.930e-01  19.769  < 2e-16 ***
    ## title_typeFeature Film         -1.009e-01  1.720e-01  -0.587  0.55775    
    ## title_typeTV Movie             -3.370e-01  2.711e-01  -1.243  0.21430    
    ## genreAnimation                 -4.330e-01  1.656e-01  -2.615  0.00915 ** 
    ## genreArt House & International  2.751e-01  1.383e-01   1.989  0.04715 *  
    ## genreComedy                    -1.597e-01  7.687e-02  -2.078  0.03813 *  
    ## genreDocumentary                2.533e-01  1.833e-01   1.382  0.16735    
    ## genreDrama                      1.087e-01  6.687e-02   1.626  0.10447    
    ## genreHorror                     7.528e-02  1.136e-01   0.663  0.50766    
    ## genreMusical & Performing Arts  1.218e-01  1.589e-01   0.766  0.44376    
    ## genreMystery & Suspense         2.645e-01  8.537e-02   3.099  0.00203 ** 
    ## genreOther                     -4.361e-02  1.314e-01  -0.332  0.74006    
    ## genreScience Fiction & Fantasy -2.070e-01  1.652e-01  -1.253  0.21084    
    ## imdb_num_votes                  1.006e-06  1.875e-07   5.365 1.14e-07 ***
    ## critics_score                   1.023e-02  9.482e-04  10.786  < 2e-16 ***
    ## audience_score                  3.254e-02  1.358e-03  23.957  < 2e-16 ***
    ## best_actor_winyes               6.911e-02  5.358e-02   1.290  0.19758    
    ## best_actress_winyes             7.348e-02  6.001e-02   1.225  0.22121    
    ## best_dir_winyes                 7.707e-02  7.565e-02   1.019  0.30867    
    ## top200_boxyes                  -1.257e-01  1.282e-01  -0.981  0.32709    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.463 on 631 degrees of freedom
    ## Multiple R-squared:  0.8232, Adjusted R-squared:  0.8178 
    ## F-statistic: 154.6 on 19 and 631 DF,  p-value: < 2.2e-16

``` r
#-[title_type]
full_model <- lm(imdb_rating ~ genre+imdb_num_votes+critics_score+audience_score+best_actor_win+best_actress_win+best_dir_win+top200_box , data = movies) 
summary(full_model)
```

    ## 
    ## Call:
    ## lm(formula = imdb_rating ~ genre + imdb_num_votes + critics_score + 
    ##     audience_score + best_actor_win + best_actress_win + best_dir_win + 
    ##     top200_box, data = movies)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.46348 -0.18261  0.03802  0.26876  1.11833 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                     3.709e+00  7.957e-02  46.616  < 2e-16 ***
    ## genreAnimation                 -4.336e-01  1.655e-01  -2.619 0.009030 ** 
    ## genreArt House & International  2.747e-01  1.383e-01   1.987 0.047392 *  
    ## genreComedy                    -1.570e-01  7.676e-02  -2.046 0.041217 *  
    ## genreDocumentary                3.454e-01  9.675e-02   3.570 0.000385 ***
    ## genreDrama                      1.043e-01  6.675e-02   1.563 0.118611    
    ## genreHorror                     7.594e-02  1.135e-01   0.669 0.503799    
    ## genreMusical & Performing Arts  1.527e-01  1.499e-01   1.019 0.308666    
    ## genreMystery & Suspense         2.638e-01  8.533e-02   3.091 0.002081 ** 
    ## genreOther                     -6.009e-02  1.306e-01  -0.460 0.645665    
    ## genreScience Fiction & Fantasy -2.069e-01  1.652e-01  -1.253 0.210724    
    ## imdb_num_votes                  1.008e-06  1.870e-07   5.392 9.83e-08 ***
    ## critics_score                   1.027e-02  9.392e-04  10.935  < 2e-16 ***
    ## audience_score                  3.259e-02  1.357e-03  24.017  < 2e-16 ***
    ## best_actor_winyes               7.133e-02  5.350e-02   1.333 0.182913    
    ## best_actress_winyes             7.167e-02  5.997e-02   1.195 0.232488    
    ## best_dir_winyes                 7.685e-02  7.555e-02   1.017 0.309470    
    ## top200_boxyes                  -1.258e-01  1.281e-01  -0.982 0.326346    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4628 on 633 degrees of freedom
    ## Multiple R-squared:  0.8227, Adjusted R-squared:  0.8179 
    ## F-statistic: 172.8 on 17 and 633 DF,  p-value: < 2.2e-16

``` r
#-[genre]
full_model <- lm(imdb_rating ~ title_type+imdb_num_votes+critics_score+audience_score+best_actor_win+best_actress_win+best_dir_win+top200_box , data = movies) 
summary(full_model)
```

    ## 
    ## Call:
    ## lm(formula = imdb_rating ~ title_type + imdb_num_votes + critics_score + 
    ##     audience_score + best_actor_win + best_actress_win + best_dir_win + 
    ##     top200_box, data = movies)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.51229 -0.17744  0.02922  0.29241  1.18964 
    ## 
    ## Coefficients:
    ##                          Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)             3.944e+00  1.089e-01  36.222  < 2e-16 ***
    ## title_typeFeature Film -2.360e-01  7.539e-02  -3.130  0.00183 ** 
    ## title_typeTV Movie     -4.586e-01  2.253e-01  -2.036  0.04217 *  
    ## imdb_num_votes          9.505e-07  1.903e-07   4.996 7.57e-07 ***
    ## critics_score           1.109e-02  9.546e-04  11.620  < 2e-16 ***
    ## audience_score          3.280e-02  1.357e-03  24.177  < 2e-16 ***
    ## best_actor_winyes       1.010e-01  5.459e-02   1.849  0.06487 .  
    ## best_actress_winyes     9.421e-02  6.104e-02   1.543  0.12322    
    ## best_dir_winyes         8.312e-02  7.793e-02   1.067  0.28656    
    ## top200_boxyes          -1.706e-01  1.308e-01  -1.304  0.19273    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4778 on 641 degrees of freedom
    ## Multiple R-squared:  0.8086, Adjusted R-squared:  0.806 
    ## F-statistic:   301 on 9 and 641 DF,  p-value: < 2.2e-16

``` r
#-[imdb_num_votes]
full_model <- lm(imdb_rating ~ title_type+genre+critics_score+audience_score+best_actor_win+best_actress_win+best_dir_win+top200_box , data = movies) 
summary(full_model)
```

    ## 
    ## Call:
    ## lm(formula = imdb_rating ~ title_type + genre + critics_score + 
    ##     audience_score + best_actor_win + best_actress_win + best_dir_win + 
    ##     top200_box, data = movies)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.39044 -0.22031  0.04168  0.28472  1.19738 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                     3.7229303  0.1964045  18.955  < 2e-16 ***
    ## title_typeFeature Film         -0.0519386  0.1755365  -0.296  0.76742    
    ## title_typeTV Movie             -0.3335761  0.2770279  -1.204  0.22899    
    ## genreAnimation                 -0.4601910  0.1691210  -2.721  0.00669 ** 
    ## genreArt House & International  0.1979818  0.1405698   1.408  0.15950    
    ## genreComedy                    -0.1823139  0.0784243  -2.325  0.02040 *  
    ## genreDocumentary                0.1805017  0.1867234   0.967  0.33407    
    ## genreDrama                      0.0717555  0.0679643   1.056  0.29147    
    ## genreHorror                     0.0513755  0.1159528   0.443  0.65786    
    ## genreMusical & Performing Arts  0.0408620  0.1616173   0.253  0.80048    
    ## genreMystery & Suspense         0.2629395  0.0872222   3.015  0.00268 ** 
    ## genreOther                     -0.0265906  0.1342035  -0.198  0.84300    
    ## genreScience Fiction & Fantasy -0.2039083  0.1688171  -1.208  0.22755    
    ## critics_score                   0.0103418  0.0009686  10.677  < 2e-16 ***
    ## audience_score                  0.0344401  0.0013398  25.705  < 2e-16 ***
    ## best_actor_winyes               0.0740388  0.0547336   1.353  0.17663    
    ## best_actress_winyes             0.0989337  0.0611182   1.619  0.10600    
    ## best_dir_winyes                 0.1301483  0.0766268   1.698  0.08991 .  
    ## top200_boxyes                   0.0387340  0.1271393   0.305  0.76073    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4731 on 632 degrees of freedom
    ## Multiple R-squared:  0.8151, Adjusted R-squared:  0.8098 
    ## F-statistic: 154.8 on 18 and 632 DF,  p-value: < 2.2e-16

``` r
#-[critics_score]
full_model <- lm(imdb_rating ~ title_type+genre+imdb_num_votes+audience_score+best_actor_win+best_actress_win+best_dir_win+top200_box , data = movies) 
summary(full_model)
```

    ## 
    ## Call:
    ## lm(formula = imdb_rating ~ title_type + genre + imdb_num_votes + 
    ##     audience_score + best_actor_win + best_actress_win + best_dir_win + 
    ##     top200_box, data = movies)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.77089 -0.17783  0.06297  0.28576  1.05504 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                     3.995e+00  2.091e-01  19.108  < 2e-16 ***
    ## title_typeFeature Film         -3.469e-01  1.854e-01  -1.871  0.06183 .  
    ## title_typeTV Movie             -5.165e-01  2.943e-01  -1.755  0.07974 .  
    ## genreAnimation                 -4.093e-01  1.801e-01  -2.273  0.02336 *  
    ## genreArt House & International  3.044e-01  1.504e-01   2.024  0.04339 *  
    ## genreComedy                    -1.532e-01  8.359e-02  -1.833  0.06729 .  
    ## genreDocumentary                2.416e-01  1.993e-01   1.212  0.22582    
    ## genreDrama                      2.179e-01  7.188e-02   3.031  0.00254 ** 
    ## genreHorror                     1.830e-01  1.230e-01   1.487  0.13742    
    ## genreMusical & Performing Arts  1.755e-01  1.727e-01   1.016  0.30998    
    ## genreMystery & Suspense         3.787e-01  9.211e-02   4.112 4.45e-05 ***
    ## genreOther                      7.361e-02  1.424e-01   0.517  0.60533    
    ## genreScience Fiction & Fantasy -9.951e-02  1.793e-01  -0.555  0.57919    
    ## imdb_num_votes                  1.052e-06  2.038e-07   5.159 3.34e-07 ***
    ## audience_score                  4.134e-02  1.181e-03  34.994  < 2e-16 ***
    ## best_actor_winyes               8.394e-02  5.824e-02   1.441  0.15000    
    ## best_actress_winyes             9.664e-02  6.521e-02   1.482  0.13886    
    ## best_dir_winyes                 1.607e-01  8.183e-02   1.965  0.04991 *  
    ## top200_boxyes                  -4.294e-02  1.391e-01  -0.309  0.75765    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5035 on 632 degrees of freedom
    ## Multiple R-squared:  0.7905, Adjusted R-squared:  0.7846 
    ## F-statistic: 132.5 on 18 and 632 DF,  p-value: < 2.2e-16

``` r
#-[audience_score]
full_model <- lm(imdb_rating ~ title_type+genre+imdb_num_votes+critics_score+best_actor_win+best_actress_win+best_dir_win+top200_box , data = movies) 
summary(full_model)
```

    ## 
    ## Call:
    ## lm(formula = imdb_rating ~ title_type + genre + imdb_num_votes + 
    ##     critics_score + best_actor_win + best_actress_win + best_dir_win + 
    ##     top200_box, data = movies)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.9458 -0.3255  0.0505  0.4078  1.9256 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                     4.895e+00  2.591e-01  18.891  < 2e-16 ***
    ## title_typeFeature Film         -8.267e-02  2.376e-01  -0.348  0.72796    
    ## title_typeTV Movie             -4.973e-01  3.743e-01  -1.329  0.18443    
    ## genreAnimation                 -2.416e-01  2.284e-01  -1.058  0.29054    
    ## genreArt House & International  5.493e-01  1.903e-01   2.886  0.00404 ** 
    ## genreComedy                    -1.496e-01  1.061e-01  -1.410  0.15917    
    ## genreDocumentary                6.834e-01  2.518e-01   2.714  0.00683 ** 
    ## genreDrama                      2.332e-01  9.206e-02   2.533  0.01156 *  
    ## genreHorror                    -1.615e-01  1.562e-01  -1.034  0.30151    
    ## genreMusical & Performing Arts  5.672e-01  2.179e-01   2.603  0.00946 ** 
    ## genreMystery & Suspense         1.615e-01  1.177e-01   1.372  0.17064    
    ## genreOther                      2.652e-02  1.814e-01   0.146  0.88381    
    ## genreScience Fiction & Fantasy -4.257e-01  2.278e-01  -1.869  0.06212 .  
    ## imdb_num_votes                  2.177e-06  2.499e-07   8.710  < 2e-16 ***
    ## critics_score                   2.386e-02  1.047e-03  22.792  < 2e-16 ***
    ## best_actor_winyes               4.689e-02  7.397e-02   0.634  0.52640    
    ## best_actress_winyes             1.679e-02  8.279e-02   0.203  0.83935    
    ## best_dir_winyes                 5.599e-02  1.044e-01   0.536  0.59212    
    ## top200_boxyes                  -1.542e-01  1.769e-01  -0.871  0.38397    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.6393 on 632 degrees of freedom
    ## Multiple R-squared:  0.6623, Adjusted R-squared:  0.6527 
    ## F-statistic: 68.86 on 18 and 632 DF,  p-value: < 2.2e-16

``` r
#-[best_actor_win]
full_model <- lm(imdb_rating ~ title_type+genre+imdb_num_votes+critics_score+audience_score+best_actress_win+best_dir_win+top200_box , data = movies) 
summary(full_model)
```

    ## 
    ## Call:
    ## lm(formula = imdb_rating ~ title_type + genre + imdb_num_votes + 
    ##     critics_score + audience_score + best_actress_win + best_dir_win + 
    ##     top200_box, data = movies)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.47274 -0.18915  0.03484  0.26533  1.10266 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                     3.818e+00  1.931e-01  19.774  < 2e-16 ***
    ## title_typeFeature Film         -9.714e-02  1.721e-01  -0.564  0.57266    
    ## title_typeTV Movie             -3.457e-01  2.712e-01  -1.275  0.20289    
    ## genreAnimation                 -4.326e-01  1.657e-01  -2.611  0.00924 ** 
    ## genreArt House & International  2.683e-01  1.383e-01   1.940  0.05283 .  
    ## genreComedy                    -1.609e-01  7.691e-02  -2.092  0.03685 *  
    ## genreDocumentary                2.526e-01  1.833e-01   1.378  0.16881    
    ## genreDrama                      1.127e-01  6.684e-02   1.687  0.09219 .  
    ## genreHorror                     6.831e-02  1.135e-01   0.602  0.54751    
    ## genreMusical & Performing Arts  1.216e-01  1.590e-01   0.765  0.44479    
    ## genreMystery & Suspense         2.743e-01  8.508e-02   3.224  0.00133 ** 
    ## genreOther                     -3.868e-02  1.314e-01  -0.294  0.76857    
    ## genreScience Fiction & Fantasy -2.153e-01  1.652e-01  -1.303  0.19289    
    ## imdb_num_votes                  1.010e-06  1.876e-07   5.385 1.02e-07 ***
    ## critics_score                   1.026e-02  9.484e-04  10.817  < 2e-16 ***
    ## audience_score                  3.251e-02  1.359e-03  23.926  < 2e-16 ***
    ## best_actress_winyes             8.081e-02  5.977e-02   1.352  0.17684    
    ## best_dir_winyes                 8.392e-02  7.550e-02   1.112  0.26674    
    ## top200_boxyes                  -1.199e-01  1.281e-01  -0.935  0.34990    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4632 on 632 degrees of freedom
    ## Multiple R-squared:  0.8227, Adjusted R-squared:  0.8176 
    ## F-statistic: 162.9 on 18 and 632 DF,  p-value: < 2.2e-16

``` r
#-[best_actress_win]
full_model <- lm(imdb_rating ~ title_type+genre+imdb_num_votes+critics_score+audience_score+best_actor_win+best_dir_win+top200_box , data = movies) 
summary(full_model)
```

    ## 
    ## Call:
    ## lm(formula = imdb_rating ~ title_type + genre + imdb_num_votes + 
    ##     critics_score + audience_score + best_actor_win + best_dir_win + 
    ##     top200_box, data = movies)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.46514 -0.18627  0.03398  0.26851  1.10276 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                     3.813e+00  1.931e-01  19.750  < 2e-16 ***
    ## title_typeFeature Film         -9.989e-02  1.721e-01  -0.580  0.56186    
    ## title_typeTV Movie             -3.298e-01  2.712e-01  -1.216  0.22439    
    ## genreAnimation                 -4.233e-01  1.655e-01  -2.558  0.01076 *  
    ## genreArt House & International  2.835e-01  1.382e-01   2.051  0.04066 *  
    ## genreComedy                    -1.508e-01  7.655e-02  -1.970  0.04932 *  
    ## genreDocumentary                2.584e-01  1.833e-01   1.410  0.15900    
    ## genreDrama                      1.204e-01  6.622e-02   1.818  0.06950 .  
    ## genreHorror                     7.705e-02  1.136e-01   0.678  0.49792    
    ## genreMusical & Performing Arts  1.241e-01  1.589e-01   0.781  0.43518    
    ## genreMystery & Suspense         2.759e-01  8.489e-02   3.250  0.00122 ** 
    ## genreOther                     -3.608e-02  1.313e-01  -0.275  0.78354    
    ## genreScience Fiction & Fantasy -2.075e-01  1.653e-01  -1.255  0.20992    
    ## imdb_num_votes                  1.024e-06  1.870e-07   5.476 6.27e-08 ***
    ## critics_score                   1.027e-02  9.480e-04  10.832  < 2e-16 ***
    ## audience_score                  3.247e-02  1.358e-03  23.918  < 2e-16 ***
    ## best_actor_winyes               7.532e-02  5.336e-02   1.412  0.15856    
    ## best_dir_winyes                 8.115e-02  7.560e-02   1.073  0.28351    
    ## top200_boxyes                  -1.167e-01  1.280e-01  -0.912  0.36231    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4632 on 632 degrees of freedom
    ## Multiple R-squared:  0.8227, Adjusted R-squared:  0.8177 
    ## F-statistic:   163 on 18 and 632 DF,  p-value: < 2.2e-16

``` r
#-[best_dir_win]
full_model <- lm(imdb_rating ~ title_type+genre+imdb_num_votes+critics_score+audience_score+best_actor_win+best_actress_win+top200_box , data = movies) 
summary(full_model)
```

    ## 
    ## Call:
    ## lm(formula = imdb_rating ~ title_type + genre + imdb_num_votes + 
    ##     critics_score + audience_score + best_actor_win + best_actress_win + 
    ##     top200_box, data = movies)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.46792 -0.18766  0.03263  0.26779  1.10935 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                     3.808e+00  1.929e-01  19.744  < 2e-16 ***
    ## title_typeFeature Film         -9.413e-02  1.719e-01  -0.548  0.58423    
    ## title_typeTV Movie             -3.341e-01  2.711e-01  -1.232  0.21837    
    ## genreAnimation                 -4.385e-01  1.655e-01  -2.649  0.00827 ** 
    ## genreArt House & International  2.712e-01  1.383e-01   1.962  0.05026 .  
    ## genreComedy                    -1.603e-01  7.687e-02  -2.085  0.03743 *  
    ## genreDocumentary                2.528e-01  1.833e-01   1.379  0.16828    
    ## genreDrama                      1.080e-01  6.687e-02   1.615  0.10679    
    ## genreHorror                     7.497e-02  1.136e-01   0.660  0.50947    
    ## genreMusical & Performing Arts  1.238e-01  1.589e-01   0.779  0.43610    
    ## genreMystery & Suspense         2.650e-01  8.537e-02   3.104  0.00200 ** 
    ## genreOther                     -4.717e-02  1.313e-01  -0.359  0.71963    
    ## genreScience Fiction & Fantasy -2.036e-01  1.652e-01  -1.233  0.21813    
    ## imdb_num_votes                  1.031e-06  1.859e-07   5.545 4.31e-08 ***
    ## critics_score                   1.033e-02  9.432e-04  10.947  < 2e-16 ***
    ## audience_score                  3.252e-02  1.358e-03  23.946  < 2e-16 ***
    ## best_actor_winyes               7.294e-02  5.345e-02   1.365  0.17283    
    ## best_actress_winyes             7.617e-02  5.995e-02   1.271  0.20434    
    ## top200_boxyes                  -1.292e-01  1.281e-01  -1.009  0.31346    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.463 on 632 degrees of freedom
    ## Multiple R-squared:  0.8229, Adjusted R-squared:  0.8178 
    ## F-statistic: 163.1 on 18 and 632 DF,  p-value: < 2.2e-16

``` r
#=[top200_box]
full_model <- lm(imdb_rating ~ title_type+genre+imdb_num_votes+critics_score+audience_score+best_actor_win+best_actress_win+best_dir_win , data = movies) 
summary(full_model)
```

    ## 
    ## Call:
    ## lm(formula = imdb_rating ~ title_type + genre + imdb_num_votes + 
    ##     critics_score + audience_score + best_actor_win + best_actress_win + 
    ##     best_dir_win, data = movies)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.45856 -0.18854  0.03907  0.26948  1.11547 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                     3.812e+00  1.930e-01  19.755  < 2e-16 ***
    ## title_typeFeature Film         -1.020e-01  1.720e-01  -0.593  0.55364    
    ## title_typeTV Movie             -3.376e-01  2.711e-01  -1.245  0.21348    
    ## genreAnimation                 -4.234e-01  1.653e-01  -2.561  0.01066 *  
    ## genreArt House & International  2.823e-01  1.381e-01   2.044  0.04141 *  
    ## genreComedy                    -1.527e-01  7.654e-02  -1.995  0.04642 *  
    ## genreDocumentary                2.609e-01  1.831e-01   1.425  0.15457    
    ## genreDrama                      1.168e-01  6.636e-02   1.760  0.07887 .  
    ## genreHorror                     8.272e-02  1.133e-01   0.730  0.46569    
    ## genreMusical & Performing Arts  1.302e-01  1.587e-01   0.821  0.41200    
    ## genreMystery & Suspense         2.736e-01  8.486e-02   3.225  0.00133 ** 
    ## genreOther                     -3.825e-02  1.313e-01  -0.291  0.77086    
    ## genreScience Fiction & Fantasy -2.108e-01  1.652e-01  -1.276  0.20243    
    ## imdb_num_votes                  9.619e-07  1.821e-07   5.284 1.75e-07 ***
    ## critics_score                   1.017e-02  9.465e-04  10.746  < 2e-16 ***
    ## audience_score                  3.255e-02  1.358e-03  23.968  < 2e-16 ***
    ## best_actor_winyes               6.726e-02  5.354e-02   1.256  0.20952    
    ## best_actress_winyes             7.010e-02  5.990e-02   1.170  0.24236    
    ## best_dir_winyes                 7.909e-02  7.562e-02   1.046  0.29599    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.463 on 632 degrees of freedom
    ## Multiple R-squared:  0.8229, Adjusted R-squared:  0.8178 
    ## F-statistic: 163.1 on 18 and 632 DF,  p-value: < 2.2e-16

***Step 3: Drop title_type and continue.***

``` r
#full_model_3
full_model <- lm(imdb_rating ~ genre+imdb_num_votes+critics_score+audience_score+best_actor_win+best_actress_win+best_dir_win+top200_box , data = movies) 
summary(full_model)
```

    ## 
    ## Call:
    ## lm(formula = imdb_rating ~ genre + imdb_num_votes + critics_score + 
    ##     audience_score + best_actor_win + best_actress_win + best_dir_win + 
    ##     top200_box, data = movies)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.46348 -0.18261  0.03802  0.26876  1.11833 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                     3.709e+00  7.957e-02  46.616  < 2e-16 ***
    ## genreAnimation                 -4.336e-01  1.655e-01  -2.619 0.009030 ** 
    ## genreArt House & International  2.747e-01  1.383e-01   1.987 0.047392 *  
    ## genreComedy                    -1.570e-01  7.676e-02  -2.046 0.041217 *  
    ## genreDocumentary                3.454e-01  9.675e-02   3.570 0.000385 ***
    ## genreDrama                      1.043e-01  6.675e-02   1.563 0.118611    
    ## genreHorror                     7.594e-02  1.135e-01   0.669 0.503799    
    ## genreMusical & Performing Arts  1.527e-01  1.499e-01   1.019 0.308666    
    ## genreMystery & Suspense         2.638e-01  8.533e-02   3.091 0.002081 ** 
    ## genreOther                     -6.009e-02  1.306e-01  -0.460 0.645665    
    ## genreScience Fiction & Fantasy -2.069e-01  1.652e-01  -1.253 0.210724    
    ## imdb_num_votes                  1.008e-06  1.870e-07   5.392 9.83e-08 ***
    ## critics_score                   1.027e-02  9.392e-04  10.935  < 2e-16 ***
    ## audience_score                  3.259e-02  1.357e-03  24.017  < 2e-16 ***
    ## best_actor_winyes               7.133e-02  5.350e-02   1.333 0.182913    
    ## best_actress_winyes             7.167e-02  5.997e-02   1.195 0.232488    
    ## best_dir_winyes                 7.685e-02  7.555e-02   1.017 0.309470    
    ## top200_boxyes                  -1.258e-01  1.281e-01  -0.982 0.326346    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4628 on 633 degrees of freedom
    ## Multiple R-squared:  0.8227, Adjusted R-squared:  0.8179 
    ## F-statistic: 172.8 on 17 and 633 DF,  p-value: < 2.2e-16

``` r
#-[genre]
full_model <- lm(imdb_rating ~ imdb_num_votes+critics_score+audience_score+best_actor_win+best_actress_win+best_dir_win+top200_box , data = movies) 
summary(full_model)
```

    ## 
    ## Call:
    ## lm(formula = imdb_rating ~ imdb_num_votes + critics_score + audience_score + 
    ##     best_actor_win + best_actress_win + best_dir_win + top200_box, 
    ##     data = movies)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.47051 -0.19322  0.01939  0.28621  1.19361 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          3.661e+00  6.244e-02  58.628  < 2e-16 ***
    ## imdb_num_votes       8.252e-07  1.868e-07   4.419 1.17e-05 ***
    ## critics_score        1.162e-02  9.432e-04  12.319  < 2e-16 ***
    ## audience_score       3.356e-02  1.346e-03  24.923  < 2e-16 ***
    ## best_actor_winyes    8.966e-02  5.476e-02   1.637    0.102    
    ## best_actress_winyes  7.836e-02  6.129e-02   1.279    0.201    
    ## best_dir_winyes      6.390e-02  7.819e-02   0.817    0.414    
    ## top200_boxyes       -1.758e-01  1.317e-01  -1.335    0.182    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4812 on 643 degrees of freedom
    ## Multiple R-squared:  0.8053, Adjusted R-squared:  0.8032 
    ## F-statistic:   380 on 7 and 643 DF,  p-value: < 2.2e-16

``` r
#-[imdb_num_votes]
full_model <- lm(imdb_rating ~ genre+critics_score+audience_score+best_actor_win+best_actress_win+best_dir_win+top200_box , data = movies) 
summary(full_model)
```

    ## 
    ## Call:
    ## lm(formula = imdb_rating ~ genre + critics_score + audience_score + 
    ##     best_actor_win + best_actress_win + best_dir_win + top200_box, 
    ##     data = movies)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.38797 -0.21699  0.03621  0.28228  1.20276 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                     3.6663107  0.0809049  45.316  < 2e-16 ***
    ## genreAnimation                 -0.4604916  0.1690911  -2.723  0.00664 ** 
    ## genreArt House & International  0.1978248  0.1405405   1.408  0.15974    
    ## genreComedy                    -0.1806760  0.0783150  -2.307  0.02137 *  
    ## genreDocumentary                0.2275804  0.0963197   2.363  0.01844 *  
    ## genreDrama                      0.0672628  0.0678540   0.991  0.32192    
    ## genreHorror                     0.0524288  0.1159278   0.452  0.65124    
    ## genreMusical & Performing Arts  0.0562467  0.1521082   0.370  0.71167    
    ## genreMystery & Suspense         0.2625938  0.0871976   3.011  0.00270 ** 
    ## genreOther                     -0.0452119  0.1334670  -0.339  0.73491    
    ## genreScience Fiction & Fantasy -0.2034830  0.1687880  -1.206  0.22844    
    ## critics_score                   0.0103436  0.0009596  10.779  < 2e-16 ***
    ## audience_score                  0.0345163  0.0013379  25.800  < 2e-16 ***
    ## best_actor_winyes               0.0770862  0.0546576   1.410  0.15893    
    ## best_actress_winyes             0.0970724  0.0610928   1.589  0.11257    
    ## best_dir_winyes                 0.1314285  0.0765136   1.718  0.08634 .  
    ## top200_boxyes                   0.0400348  0.1270867   0.315  0.75285    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.473 on 634 degrees of freedom
    ## Multiple R-squared:  0.8146, Adjusted R-squared:  0.8099 
    ## F-statistic: 174.1 on 16 and 634 DF,  p-value: < 2.2e-16

``` r
#-[critics_score]
full_model <- lm(imdb_rating ~ genre+imdb_num_votes+audience_score+best_actor_win+best_actress_win+best_dir_win+top200_box , data = movies)
summary(full_model)
```

    ## 
    ## Call:
    ## lm(formula = imdb_rating ~ genre + imdb_num_votes + audience_score + 
    ##     best_actor_win + best_actress_win + best_dir_win + top200_box, 
    ##     data = movies)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.78291 -0.17897  0.06111  0.28489  1.00898 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                     3.637e+00  8.640e-02  42.098  < 2e-16 ***
    ## genreAnimation                 -4.116e-01  1.803e-01  -2.282  0.02281 *  
    ## genreArt House & International  3.011e-01  1.506e-01   1.999  0.04602 *  
    ## genreComedy                    -1.453e-01  8.362e-02  -1.737  0.08281 .  
    ## genreDocumentary                5.608e-01  1.032e-01   5.434 7.88e-08 ***
    ## genreDrama                      2.130e-01  7.192e-02   2.962  0.00317 ** 
    ## genreHorror                     1.841e-01  1.232e-01   1.494  0.13556    
    ## genreMusical & Performing Arts  2.845e-01  1.628e-01   1.747  0.08108 .  
    ## genreMystery & Suspense         3.786e-01  9.226e-02   4.103 4.61e-05 ***
    ## genreOther                      6.075e-02  1.418e-01   0.428  0.66853    
    ## genreScience Fiction & Fantasy -9.858e-02  1.796e-01  -0.549  0.58333    
    ## imdb_num_votes                  1.038e-06  2.037e-07   5.096 4.59e-07 ***
    ## audience_score                  4.156e-02  1.178e-03  35.299  < 2e-16 ***
    ## best_actor_winyes               8.440e-02  5.827e-02   1.448  0.14801    
    ## best_actress_winyes             9.540e-02  6.529e-02   1.461  0.14448    
    ## best_dir_winyes                 1.578e-01  8.192e-02   1.926  0.05459 .  
    ## top200_boxyes                  -4.244e-02  1.393e-01  -0.305  0.76076    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5043 on 634 degrees of freedom
    ## Multiple R-squared:  0.7892, Adjusted R-squared:  0.7839 
    ## F-statistic: 148.4 on 16 and 634 DF,  p-value: < 2.2e-16

``` r
#-[audience_score]
full_model <- lm(imdb_rating ~ genre+imdb_num_votes+critics_score+best_actor_win+best_actress_win+best_dir_win+top200_box , data = movies) 
summary(full_model)
```

    ## 
    ## Call:
    ## lm(formula = imdb_rating ~ genre + imdb_num_votes + critics_score + 
    ##     best_actor_win + best_actress_win + best_dir_win + top200_box, 
    ##     data = movies)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.94619 -0.32179  0.05411  0.41325  1.92753 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                     4.809e+00  8.992e-02  53.480  < 2e-16 ***
    ## genreAnimation                 -2.413e-01  2.284e-01  -1.056  0.29123    
    ## genreArt House & International  5.506e-01  1.903e-01   2.893  0.00395 ** 
    ## genreComedy                    -1.468e-01  1.060e-01  -1.384  0.16673    
    ## genreDocumentary                7.607e-01  1.315e-01   5.784 1.14e-08 ***
    ## genreDrama                      2.272e-01  9.194e-02   2.471  0.01374 *  
    ## genreHorror                    -1.605e-01  1.562e-01  -1.027  0.30480    
    ## genreMusical & Performing Arts  5.940e-01  2.055e-01   2.890  0.00399 ** 
    ## genreMystery & Suspense         1.606e-01  1.177e-01   1.365  0.17287    
    ## genreOther                     -9.685e-04  1.804e-01  -0.005  0.99572    
    ## genreScience Fiction & Fantasy -4.258e-01  2.278e-01  -1.869  0.06211 .  
    ## imdb_num_votes                  2.190e-06  2.493e-07   8.788  < 2e-16 ***
    ## critics_score                   2.391e-02  1.033e-03  23.140  < 2e-16 ***
    ## best_actor_winyes               5.122e-02  7.389e-02   0.693  0.48843    
    ## best_actress_winyes             1.361e-02  8.277e-02   0.164  0.86941    
    ## best_dir_winyes                 5.715e-02  1.044e-01   0.548  0.58419    
    ## top200_boxyes                  -1.541e-01  1.770e-01  -0.871  0.38430    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.6394 on 634 degrees of freedom
    ## Multiple R-squared:  0.6611, Adjusted R-squared:  0.6526 
    ## F-statistic: 77.31 on 16 and 634 DF,  p-value: < 2.2e-16

``` r
#-[best_actor_win]
full_model <- lm(imdb_rating ~ genre+imdb_num_votes+critics_score+audience_score+best_actress_win+best_dir_win+top200_box , data = movies) 
summary(full_model)
```

    ## 
    ## Call:
    ## lm(formula = imdb_rating ~ genre + imdb_num_votes + critics_score + 
    ##     audience_score + best_actress_win + best_dir_win + top200_box, 
    ##     data = movies)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.46989 -0.18664  0.03368  0.26469  1.10744 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                     3.716e+00  7.946e-02  46.767  < 2e-16 ***
    ## genreAnimation                 -4.331e-01  1.656e-01  -2.615 0.009141 ** 
    ## genreArt House & International  2.677e-01  1.383e-01   1.937 0.053242 .  
    ## genreComedy                    -1.583e-01  7.680e-02  -2.061 0.039756 *  
    ## genreDocumentary                3.411e-01  9.676e-02   3.526 0.000453 ***
    ## genreDrama                      1.083e-01  6.673e-02   1.624 0.104919    
    ## genreHorror                     6.882e-02  1.135e-01   0.606 0.544404    
    ## genreMusical & Performing Arts  1.513e-01  1.500e-01   1.009 0.313396    
    ## genreMystery & Suspense         2.739e-01  8.504e-02   3.221 0.001344 ** 
    ## genreOther                     -5.576e-02  1.307e-01  -0.427 0.669734    
    ## genreScience Fiction & Fantasy -2.155e-01  1.651e-01  -1.305 0.192330    
    ## imdb_num_votes                  1.013e-06  1.871e-07   5.417 8.63e-08 ***
    ## critics_score                   1.030e-02  9.395e-04  10.961  < 2e-16 ***
    ## audience_score                  3.256e-02  1.358e-03  23.985  < 2e-16 ***
    ## best_actress_winyes             7.917e-02  5.974e-02   1.325 0.185564    
    ## best_dir_winyes                 8.409e-02  7.541e-02   1.115 0.265221    
    ## top200_boxyes                  -1.198e-01  1.281e-01  -0.935 0.350116    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4631 on 634 degrees of freedom
    ## Multiple R-squared:  0.8222, Adjusted R-squared:  0.8177 
    ## F-statistic: 183.2 on 16 and 634 DF,  p-value: < 2.2e-16

``` r
#-[best_actress_win]
full_model <- lm(imdb_rating ~ genre+imdb_num_votes+critics_score+audience_score+best_actor_win+best_dir_win+top200_box , data = movies) 
summary(full_model)
```

    ## 
    ## Call:
    ## lm(formula = imdb_rating ~ genre + imdb_num_votes + critics_score + 
    ##     audience_score + best_actor_win + best_dir_win + top200_box, 
    ##     data = movies)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.46207 -0.18343  0.03362  0.26423  1.10796 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                     3.708e+00  7.959e-02  46.590  < 2e-16 ***
    ## genreAnimation                 -4.241e-01  1.654e-01  -2.564 0.010581 *  
    ## genreArt House & International  2.829e-01  1.381e-01   2.048 0.041007 *  
    ## genreComedy                    -1.483e-01  7.644e-02  -1.940 0.052786 .  
    ## genreDocumentary                3.495e-01  9.673e-02   3.613 0.000327 ***
    ## genreDrama                      1.158e-01  6.608e-02   1.752 0.080202 .  
    ## genreHorror                     7.763e-02  1.136e-01   0.684 0.494449    
    ## genreMusical & Performing Arts  1.547e-01  1.500e-01   1.032 0.302564    
    ## genreMystery & Suspense         2.749e-01  8.485e-02   3.239 0.001260 ** 
    ## genreOther                     -5.234e-02  1.305e-01  -0.401 0.688520    
    ## genreScience Fiction & Fantasy -2.074e-01  1.652e-01  -1.255 0.209783    
    ## imdb_num_votes                  1.026e-06  1.865e-07   5.501 5.47e-08 ***
    ## critics_score                   1.031e-02  9.389e-04  10.982  < 2e-16 ***
    ## audience_score                  3.253e-02  1.356e-03  23.980  < 2e-16 ***
    ## best_actor_winyes               7.733e-02  5.328e-02   1.451 0.147178    
    ## best_dir_winyes                 8.081e-02  7.551e-02   1.070 0.284927    
    ## top200_boxyes                  -1.171e-01  1.279e-01  -0.915 0.360599    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.463 on 634 degrees of freedom
    ## Multiple R-squared:  0.8223, Adjusted R-squared:  0.8178 
    ## F-statistic: 183.4 on 16 and 634 DF,  p-value: < 2.2e-16

``` r
#-[best_dir_win]
full_model <- lm(imdb_rating ~ genre+imdb_num_votes+critics_score+audience_score+best_actor_win+best_actress_win+top200_box , data = movies) 
summary(full_model)
```

    ## 
    ## Call:
    ## lm(formula = imdb_rating ~ genre + imdb_num_votes + critics_score + 
    ##     audience_score + best_actor_win + best_actress_win + top200_box, 
    ##     data = movies)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.46494 -0.18367  0.03451  0.26411  1.11433 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                     3.709e+00  7.957e-02  46.609  < 2e-16 ***
    ## genreAnimation                 -4.390e-01  1.655e-01  -2.653 0.008176 ** 
    ## genreArt House & International  2.709e-01  1.382e-01   1.960 0.050428 .  
    ## genreComedy                    -1.577e-01  7.676e-02  -2.055 0.040296 *  
    ## genreDocumentary                3.387e-01  9.653e-02   3.509 0.000482 ***
    ## genreDrama                      1.036e-01  6.675e-02   1.553 0.120989    
    ## genreHorror                     7.569e-02  1.135e-01   0.667 0.505231    
    ## genreMusical & Performing Arts  1.527e-01  1.499e-01   1.019 0.308727    
    ## genreMystery & Suspense         2.643e-01  8.533e-02   3.097 0.002040 ** 
    ## genreOther                     -6.380e-02  1.306e-01  -0.489 0.625341    
    ## genreScience Fiction & Fantasy -2.036e-01  1.651e-01  -1.233 0.218111    
    ## imdb_num_votes                  1.034e-06  1.853e-07   5.579 3.59e-08 ***
    ## critics_score                   1.036e-02  9.347e-04  11.088  < 2e-16 ***
    ## audience_score                  3.258e-02  1.357e-03  24.007  < 2e-16 ***
    ## best_actor_winyes               7.524e-02  5.336e-02   1.410 0.159040    
    ## best_actress_winyes             7.434e-02  5.991e-02   1.241 0.215109    
    ## top200_boxyes                  -1.293e-01  1.281e-01  -1.010 0.312881    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4629 on 634 degrees of freedom
    ## Multiple R-squared:  0.8224, Adjusted R-squared:  0.8179 
    ## F-statistic: 183.5 on 16 and 634 DF,  p-value: < 2.2e-16

``` r
#-[top200_box]
full_model <- lm(imdb_rating ~ genre+imdb_num_votes+critics_score+audience_score+best_actor_win+best_actress_win+best_dir_win , data = movies) 
summary(full_model)
```

    ## 
    ## Call:
    ## lm(formula = imdb_rating ~ genre + imdb_num_votes + critics_score + 
    ##     audience_score + best_actor_win + best_actress_win + best_dir_win, 
    ##     data = movies)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.45538 -0.18345  0.03933  0.26923  1.12053 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                     3.705e+00  7.944e-02  46.638  < 2e-16 ***
    ## genreAnimation                 -4.240e-01  1.653e-01  -2.566  0.01052 *  
    ## genreArt House & International  2.818e-01  1.381e-01   2.041  0.04163 *  
    ## genreComedy                    -1.500e-01  7.643e-02  -1.963  0.05013 .  
    ## genreDocumentary                3.540e-01  9.636e-02   3.673  0.00026 ***
    ## genreDrama                      1.124e-01  6.624e-02   1.697  0.09024 .  
    ## genreHorror                     8.337e-02  1.133e-01   0.736  0.46198    
    ## genreMusical & Performing Arts  1.616e-01  1.496e-01   1.080  0.28076    
    ## genreMystery & Suspense         2.729e-01  8.482e-02   3.217  0.00136 ** 
    ## genreOther                     -5.471e-02  1.305e-01  -0.419  0.67522    
    ## genreScience Fiction & Fantasy -2.108e-01  1.651e-01  -1.276  0.20229    
    ## imdb_num_votes                  9.644e-07  1.816e-07   5.312  1.5e-07 ***
    ## critics_score                   1.022e-02  9.375e-04  10.896  < 2e-16 ***
    ## audience_score                  3.260e-02  1.357e-03  24.028  < 2e-16 ***
    ## best_actor_winyes               6.947e-02  5.346e-02   1.299  0.19429    
    ## best_actress_winyes             6.829e-02  5.987e-02   1.141  0.25444    
    ## best_dir_winyes                 7.885e-02  7.553e-02   1.044  0.29688    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4628 on 634 degrees of freedom
    ## Multiple R-squared:  0.8224, Adjusted R-squared:  0.8179 
    ## F-statistic: 183.5 on 16 and 634 DF,  p-value: < 2.2e-16

***Step 4: Drop best_dir_win and top200_box since they increase the
adjusted R_squared equally.***

**Note**: From now on we won’t drop certain variables since we are sure
they are significant predictors.

``` r
#full_model_4
full_model <- lm(imdb_rating ~ genre+imdb_num_votes+critics_score+audience_score+best_actor_win+best_actress_win , data = movies) 
summary(full_model)
```

    ## 
    ## Call:
    ## lm(formula = imdb_rating ~ genre + imdb_num_votes + critics_score + 
    ##     audience_score + best_actor_win + best_actress_win, data = movies)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.45666 -0.18868  0.03509  0.26475  1.11649 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                     3.704e+00  7.944e-02  46.628  < 2e-16 ***
    ## genreAnimation                 -4.293e-01  1.652e-01  -2.599 0.009571 ** 
    ## genreArt House & International  2.782e-01  1.380e-01   2.015 0.044295 *  
    ## genreComedy                    -1.505e-01  7.643e-02  -1.970 0.049330 *  
    ## genreDocumentary                3.474e-01  9.616e-02   3.612 0.000327 ***
    ## genreDrama                      1.119e-01  6.625e-02   1.690 0.091573 .  
    ## genreHorror                     8.332e-02  1.133e-01   0.736 0.462264    
    ## genreMusical & Performing Arts  1.618e-01  1.497e-01   1.081 0.280087    
    ## genreMystery & Suspense         2.737e-01  8.482e-02   3.227 0.001318 ** 
    ## genreOther                     -5.836e-02  1.305e-01  -0.447 0.654839    
    ## genreScience Fiction & Fantasy -2.074e-01  1.651e-01  -1.256 0.209445    
    ## imdb_num_votes                  9.893e-07  1.800e-07   5.496 5.62e-08 ***
    ## critics_score                   1.031e-02  9.332e-04  11.048  < 2e-16 ***
    ## audience_score                  3.259e-02  1.357e-03  24.016  < 2e-16 ***
    ## best_actor_winyes               7.343e-02  5.333e-02   1.377 0.169058    
    ## best_actress_winyes             7.094e-02  5.982e-02   1.186 0.236104    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4629 on 635 degrees of freedom
    ## Multiple R-squared:  0.8221, Adjusted R-squared:  0.8179 
    ## F-statistic: 195.7 on 15 and 635 DF,  p-value: < 2.2e-16

``` r
#-[best_actor_win]
full_model <- lm(imdb_rating ~ genre+imdb_num_votes+critics_score+audience_score+best_actress_win , data = movies) 
summary(full_model)
```

    ## 
    ## Call:
    ## lm(formula = imdb_rating ~ genre + imdb_num_votes + critics_score + 
    ##     audience_score + best_actress_win, data = movies)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.46382 -0.18855  0.03553  0.26578  1.10472 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                     3.711e+00  7.933e-02  46.785  < 2e-16 ***
    ## genreAnimation                 -4.298e-01  1.653e-01  -2.600 0.009534 ** 
    ## genreArt House & International  2.703e-01  1.380e-01   1.958 0.050623 .  
    ## genreComedy                    -1.522e-01  7.647e-02  -1.990 0.046978 *  
    ## genreDocumentary                3.419e-01  9.614e-02   3.556 0.000404 ***
    ## genreDrama                      1.157e-01  6.624e-02   1.746 0.081252 .  
    ## genreHorror                     7.557e-02  1.132e-01   0.668 0.504688    
    ## genreMusical & Performing Arts  1.599e-01  1.498e-01   1.068 0.285980    
    ## genreMystery & Suspense         2.838e-01  8.456e-02   3.356 0.000837 ***
    ## genreOther                     -5.448e-02  1.305e-01  -0.417 0.676573    
    ## genreScience Fiction & Fantasy -2.158e-01  1.651e-01  -1.307 0.191598    
    ## imdb_num_votes                  9.990e-07  1.800e-07   5.551 4.18e-08 ***
    ## critics_score                   1.035e-02  9.333e-04  11.089  < 2e-16 ***
    ## audience_score                  3.256e-02  1.358e-03  23.980  < 2e-16 ***
    ## best_actress_winyes             7.913e-02  5.956e-02   1.328 0.184508    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4632 on 636 degrees of freedom
    ## Multiple R-squared:  0.8216, Adjusted R-squared:  0.8177 
    ## F-statistic: 209.2 on 14 and 636 DF,  p-value: < 2.2e-16

``` r
#-[best_actress_win]
full_model <- lm(imdb_rating ~ genre+imdb_num_votes+critics_score+audience_score+best_actor_win , data = movies) 
summary(full_model)
```

    ## 
    ## Call:
    ## lm(formula = imdb_rating ~ genre + imdb_num_votes + critics_score + 
    ##     audience_score + best_actor_win, data = movies)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.45588 -0.18932  0.03241  0.26583  1.10582 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                     3.703e+00  7.947e-02  46.604  < 2e-16 ***
    ## genreAnimation                 -4.208e-01  1.651e-01  -2.549 0.011038 *  
    ## genreArt House & International  2.856e-01  1.379e-01   2.071 0.038775 *  
    ## genreComedy                    -1.424e-01  7.614e-02  -1.870 0.061946 .  
    ## genreDocumentary                3.505e-01  9.615e-02   3.645 0.000289 ***
    ## genreDrama                      1.228e-01  6.564e-02   1.871 0.061866 .  
    ## genreHorror                     8.449e-02  1.133e-01   0.746 0.456151    
    ## genreMusical & Performing Arts  1.632e-01  1.497e-01   1.090 0.276167    
    ## genreMystery & Suspense         2.841e-01  8.439e-02   3.367 0.000806 ***
    ## genreOther                     -5.120e-02  1.304e-01  -0.393 0.694689    
    ## genreScience Fiction & Fantasy -2.075e-01  1.652e-01  -1.256 0.209439    
    ## imdb_num_votes                  1.011e-06  1.791e-07   5.645 2.49e-08 ***
    ## critics_score                   1.036e-02  9.325e-04  11.108  < 2e-16 ***
    ## audience_score                  3.252e-02  1.356e-03  23.980  < 2e-16 ***
    ## best_actor_winyes               7.971e-02  5.308e-02   1.502 0.133678    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.463 on 636 degrees of freedom
    ## Multiple R-squared:  0.8217, Adjusted R-squared:  0.8178 
    ## F-statistic: 209.4 on 14 and 636 DF,  p-value: < 2.2e-16

We cannot see any increase in the adjusted R_squared so we stop dropping
variables.Our final model would be:

``` r
final_model <- lm(imdb_rating ~ genre+imdb_num_votes+critics_score+audience_score+best_actor_win+best_actress_win , data = movies) 

summary(final_model)
```

    ## 
    ## Call:
    ## lm(formula = imdb_rating ~ genre + imdb_num_votes + critics_score + 
    ##     audience_score + best_actor_win + best_actress_win, data = movies)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.45666 -0.18868  0.03509  0.26475  1.11649 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                     3.704e+00  7.944e-02  46.628  < 2e-16 ***
    ## genreAnimation                 -4.293e-01  1.652e-01  -2.599 0.009571 ** 
    ## genreArt House & International  2.782e-01  1.380e-01   2.015 0.044295 *  
    ## genreComedy                    -1.505e-01  7.643e-02  -1.970 0.049330 *  
    ## genreDocumentary                3.474e-01  9.616e-02   3.612 0.000327 ***
    ## genreDrama                      1.119e-01  6.625e-02   1.690 0.091573 .  
    ## genreHorror                     8.332e-02  1.133e-01   0.736 0.462264    
    ## genreMusical & Performing Arts  1.618e-01  1.497e-01   1.081 0.280087    
    ## genreMystery & Suspense         2.737e-01  8.482e-02   3.227 0.001318 ** 
    ## genreOther                     -5.836e-02  1.305e-01  -0.447 0.654839    
    ## genreScience Fiction & Fantasy -2.074e-01  1.651e-01  -1.256 0.209445    
    ## imdb_num_votes                  9.893e-07  1.800e-07   5.496 5.62e-08 ***
    ## critics_score                   1.031e-02  9.332e-04  11.048  < 2e-16 ***
    ## audience_score                  3.259e-02  1.357e-03  24.016  < 2e-16 ***
    ## best_actor_winyes               7.343e-02  5.333e-02   1.377 0.169058    
    ## best_actress_winyes             7.094e-02  5.982e-02   1.186 0.236104    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4629 on 635 degrees of freedom
    ## Multiple R-squared:  0.8221, Adjusted R-squared:  0.8179 
    ## F-statistic: 195.7 on 15 and 635 DF,  p-value: < 2.2e-16

**Model diagnostics**

1\_ linear relationship between (numerical) x and y:

``` r
ggplot(data = final_model,aes(x=movies$imdb_num_votes , y=.resid)) + 
  geom_jitter() +
  geom_hline(yintercept = 0,linetype="dashed") +
  xlab("IMDB_num_votes") +
  ylab("Residuals")
```

![](IMDB_reg_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
ggplot(data = final_model,aes(x=movies$critics_score , y=.resid)) + 
  geom_jitter() +
  geom_hline(yintercept = 0,linetype="dashed") +
  xlab("Critics Score") +
  ylab("Residuals")
```

![](IMDB_reg_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->

``` r
ggplot(data = final_model,aes(x=movies$audience_score , y=.resid)) + 
  geom_jitter() +
  geom_hline(yintercept = 0,linetype="dashed") +
  xlab("Audience score") +
  ylab("Residuals")
```

![](IMDB_reg_files/figure-gfm/unnamed-chunk-10-3.png)<!-- -->

2\_ nearly normal residuals with mean 0.

``` r
ggplot(data = final_model,aes(x=.resid)) +
  geom_histogram() + 
  xlab("Residuals")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](IMDB_reg_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
ggplot(data = final_model ,aes(sample = .resid)) +
  stat_qq()
```

![](IMDB_reg_files/figure-gfm/unnamed-chunk-11-2.png)<!-- -->

3\_ constant variability of residuals

``` r
ggplot(data = final_model ,aes(x=.fitted , y=.resid)) +
  geom_jitter() +
  geom_hline(yintercept = 0 , linetype = "dashed") +
  xlab("Fitted") + 
  ylab("Residuals")
```

![](IMDB_reg_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

4\_ independent residuals:

This data was collected using **random sample** which means the
observations are **independent**.

``` r
plot(final_model$residuals)
```

![](IMDB_reg_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

All of the conditions are met so we can use our model for prediction.

------------------------------------------------------------------------

## Part 5: Prediction

The movie that we are going to predict its IMDB rating is **A Silent
Voice**.

``` r
a_silent_voive <- data.frame(genre = "Drama",imdb_num_votes = 62904,critics_score = 94,audience_score = 93, best_actor_win = "no" , best_actress_win = "no")

predict(final_model , a_silent_voive , interval = "prediction" , level = 0.95)
```

    ##        fit      lwr      upr
    ## 1 7.878235 6.965487 8.790983

The model predicts, with 95% confidence, the movie A silent voice (a
drama movie with 62904 votes on IMDB, 94 critics and 93 audience score
on Rotten Tomatoes, which neither of its actors nor actresses have not
won an Oscar) is expected to have an IMDB rating between **6.96** and
**8.79**.

Note: the IMDB rating of the movie is 8.2 which is in our interval :)

**References**: Google, IMDB, and Rotten Tomatoes

------------------------------------------------------------------------

## Part 6: Conclusion

In this project we modeled a multiple linear regression model to predict
the IMDB Rating of a movie. We learned that there are certain variables
that influence our response variable significantly:

1_Genre

2_Number of votes on IMDB

3,4_Critics and audience score on Rotten Tomatoes

5,6_Whether or not one the main cast of the movie has won an Oscar

There are also some shortcomings in this study. For example one of
significant predictors of the model is the audience score on Rotten
Tomatoes. A large number of people submit their scores in both IMDB and
Rotten Tomatoes which are very close to each other and many times the
same score. In future studies we could gather a different data set that
would cover this shortcoming.
