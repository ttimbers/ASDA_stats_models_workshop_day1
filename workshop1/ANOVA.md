Analysis of Variance (ANOVA) in R
================

This R Markdown document illustrates how to perform one-way and two-way ANOVA in R.

Table of Contents
-----------------

1.  [Load libraries](ANOVA.md#1-load-libraries)
2.  [1-way ANOVA with 2 groups](ANOVA.md#2-1-way-anova-with-2-groups)
3.  [Perform 1-way ANOVA with &gt; 2 groups](ANOVA.md#3-1-way-anova-with--2-groups)
4.  [Perform 2-way ANOVA with 2 groups](ANOVA.md#4-2-way-anova-with-2-groups)
5.  [Perform 2-way ANOVA with 2 groups and an interaction term](ANOVA.md#5-2-way-anova-with-2-groups-including-an-interaction-term)

------------------------------------------------------------------------

1. Load libraries
-----------------

At the beginning of our analysis, we will load all the libraries our code depends on, so that in the future it will be easy for us and our collaborators to re-use this code.

``` r
library(tidyverse) # meta-library that gives us access to dplyr, ggplot2 and many more
```

    ## Loading tidyverse: ggplot2
    ## Loading tidyverse: tibble
    ## Loading tidyverse: tidyr
    ## Loading tidyverse: readr
    ## Loading tidyverse: purrr
    ## Loading tidyverse: dplyr

    ## Warning: package 'ggplot2' was built under R version 3.3.2

    ## Conflicts with tidy packages ----------------------------------------------

    ## filter(): dplyr, stats
    ## lag():    dplyr, stats

``` r
library(broom) # puts output of statistical models in a nice data frame
```

2. 1-way ANOVA with 2 groups
----------------------------

### 2.1 Load and look at first dataset

The first "experiment"" we are going to analyze was done to address the question of whether sexual activity effects the longevity of male fruit flies. To assess this, we are going to use a modified version of the `fruitfly` data from the [`faraway` R package](https://cran.r-project.org/web/packages/faraway/faraway.pdf).

Our hypothesis for this experiment are as follows:

*Null Hypothesis: Sexual activity has no effect on the population mean longevity of male fruitflies.*

*Alternative Hypothesis: Sexual activity has an effect on population mean longevity of male fruitflies.*

Let's now read the dataset into R and take a look at the data frame to get familar with it:

``` r
# load data 
fruitfly_2_groups <- read_csv("data/fruitfly_2_groups.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   longevity = col_integer(),
    ##   sexually_active = col_character()
    ## )

``` r
# get number of rows and columns
dim(fruitfly_2_groups)
```

    ## [1] 50  2

``` r
# view first 6 records of data
head(fruitfly_2_groups)
```

    ## # A tibble: 6 x 2
    ##   longevity sexually_active
    ##       <int>           <chr>
    ## 1        40              No
    ## 2        37              No
    ## 3        44              No
    ## 4        47              No
    ## 5        47              No
    ## 6        47              No

``` r
# see summary of data
summary(fruitfly_2_groups)
```

    ##    longevity     sexually_active   
    ##  Min.   :16.00   Length:50         
    ##  1st Qu.:37.75   Class :character  
    ##  Median :47.00   Mode  :character  
    ##  Mean   :51.14                     
    ##  3rd Qu.:61.75                     
    ##  Max.   :96.00

We see that there are 2 columns, `longevity` and `sexually_active`. `longevity` is a quantitative variable, and is our response/dependent variable for this experiment. `sexually_active`, on the other hand, is a categorical variable, and is our single explanatory/independent variable for this experiment. There are 2 levels to the `sexually_active` variable, `Yes` and `No`.

### 2.2 Visualize data

Let's visualize this data, to get some intuition as to whether or not there is a difference in longevity between the male flies that are sexually active, and those that are not.

Given that our sample sizes are not too large, the most informative plot we can make are strip plots. We will also add the mean to these:

``` r
# plot raw data points for each group as a transparent grey/black point 
# overlay mean as a red diamond
ggplot(fruitfly_2_groups, aes(x = sexually_active, y = longevity)) + 
  geom_jitter(position = position_jitter(0.15),
              alpha = 0.7) +
  stat_summary(fun.y = mean, 
               geom = "point", 
               shape = 18, 
               size = 4, 
               color="red") +
  xlab("Sexually Active") +
  ylab("Longevity (days)")
```

![](ANOVA_files/figure-markdown_github/plot%20fruitfly_2_groups%20data-1.png)

We can see that there is a difference between the mean of these two samples, but what can we say/infer about the population means of these two groups? To say anything meaningful from a statistical stand point we need to perform a statistical test that will guide us in rejecting, or failing to reject, our null hypothesis (Sexual activity has no effect on the population mean longevity of male fruitflies).

### 2.3 Perform 1-way ANOVA with 2 groups

There are two perfectly acceptable statistical tests we could apply to this dataset, the first, you may be very familiar with - the t-test. The second, is the topic of our lesson today, Analysis of Variance (or ANOVA). Interstingly, the t-test is really a special case of ANOVA that can be used when only comparing 2 groups. ANOVA is a more generalizable test that we will later see can be used with &gt; 2 groups, and even more than one factor/category column.

#### 2.3.1 Formula notation in R

To perform an ANOVA in R, we need to understand R's formula notation, as this is the first argument we give to the ANOVA function (`aov`) in R. The formula notation starts with providing the response variable, then a special character, `~`, which can be read as "modelled as", and then the explanatory/independent variable(s). Thus, the formula notation for this experiment is:

    longevity ~ sexually_active

The formula notation can get more complex, to include additional explanatory/independent variables, as well as interaction terms. We wil introduce these are we attempt more complex analyses.

#### 2.3.2 ANOVA in R using `aov`

To do an ANOVA in R, we will use the `aov` function. As stated above, the first argument to this is the formula for the experiment/model, and the second argument we must provide is the name of the variable holding our data, here `fruitfly_2_groups`.

The `aov` function returns us a "model" object, and we need to use another function to see the results of the analysis. I suggest using the `tidy` function from the [`broom` R package](ftp://cran.r-project.org/pub/R/web/packages/broom/vignettes/broom.html) as it returns the results in a nice data frame, that is easy to do further work with. Another, more traditional function to access this data is the `summary` function, but again, I don't recommend this as accessing the individual numbers from the output of `aov` from this model is a bit trickier.

``` r
# create an ANOVA "model" object 
fruitfly_2_groups_model <- aov(longevity ~ sexually_active, 
                               data = fruitfly_2_groups)

# view output of aov() as a nice dataframe using tidy() from the broom package
(fruitfly_2_groups_results <- tidy(fruitfly_2_groups_model))
```

    ##              term df    sumsq    meansq statistic      p.value
    ## 1 sexually_active  1  7712.82 7712.8200  36.98012 1.884772e-07
    ## 2       Residuals 48 10011.20  208.5667        NA           NA

So what does this output mean? Jumping right to the most important result in regards for rejecting (or failing to reject) our null hypothesis is the p-value. In this simple one-way ANOVA we have a single p-value which has a very small value of ~ 1.88 x 10<sup>−07</sup>. Given that this is much much smaller than the commonly used threshold for rejecting the null hypothesis, p &lt; 0.05, we can reject our null hypothesis that sexual activity has no effect on the population mean longevity of male fruitflies, and accept the alternative hypothesis that sexual activity **does** has an effect on population mean longevity of male fruitflies.

3. 1-way ANOVA with &gt; 2 groups
---------------------------------

As mentioned above, an ANOVA can be used when there are more than 2 levels in your categorical explanatory/indpendent variable. For example, we will consider the following case:

We are still interested in whether sexual activity effects the longevity of male fruit flies, but want to understand this at a finer level (*e.g.,* does the amount of sexual activity matter?), thus in this "experiment" there are 4 categories for sexual `activity`. Specifically, males were kept:

1.  `none` - alone
2.  `low` - with a new virgin fruit fly every day
3.  `high` - with a new set of eight virgin fruit flies every day

So, for this case, our hypotheses are as follows:

*H*<sub>0</sub> : *μ*<sub>*i**s**o**l**a**t**e**d*</sub> = *μ*<sub>*l**o**w*</sub> = *μ*<sub>*h**i**g**h*</sub>

*H*<sub>*A*</sub> : at least one group's population mean differs from that of the other groups

Let's first load explore the data:

``` r
# load data
fruitfly_3_groups <- read_csv("data/fruitfly_3_groups.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   longevity = col_integer(),
    ##   activity = col_character()
    ## )

``` r
# get number of rows and columns
dim(fruitfly_3_groups)
```

    ## [1] 75  2

``` r
# view first 6 records of data
head(fruitfly_3_groups)
```

    ## # A tibble: 6 x 2
    ##   longevity activity
    ##       <int>    <chr>
    ## 1        40     none
    ## 2        37     none
    ## 3        44     none
    ## 4        47     none
    ## 5        47     none
    ## 6        47     none

``` r
# see summary of data
summary(fruitfly_3_groups)
```

    ##    longevity       activity        
    ##  Min.   :16.00   Length:75         
    ##  1st Qu.:41.00   Class :character  
    ##  Median :54.00   Mode  :character  
    ##  Mean   :53.01                     
    ##  3rd Qu.:63.50                     
    ##  Max.   :96.00

And, again as a good practice, let's visualize the data before we perform our statistical analysis:

``` r
# re-order factors to make them show up how we would like them on the plot
# instead of alphabetically (default R behaviour)
fruitfly_3_groups$activity <- factor(fruitfly_3_groups$activity, 
                                     levels = c("none", 
                                                "low",
                                                "high"))

# plot raw data points for each group as a transparent grey/black point 
# overlay mean as a red diamond
ggplot(fruitfly_3_groups, aes(x = activity, y = longevity)) + 
  geom_jitter(position = position_jitter(0.15),
              alpha = 0.7) +
  stat_summary(fun.y = mean, 
               geom = "point", 
               shape = 18, 
               size = 4, 
               color = "red") +
  xlab("Sexual Activity") +
  ylab("Longevity (days)")
```

![](ANOVA_files/figure-markdown_github/plot%20fruitfly_3_groups%20data-1.png)

So it looks the sample means of longevity for both low and high activity are lower than the sample means of the isolated male fruit fly. Are these differences in the sample means indicating that there are differences in the true population means between any of these groups? Again we turn to ANOVA to answer this:

``` r
# create an ANOVA "model" object 
fruitfly_3_groups_model <- aov(longevity ~ activity, 
                               data = fruitfly_3_groups)

# view output of aov() as a nice dataframe using tidy() from the broom package
(fruitfly_3_groups_results <- tidy(fruitfly_3_groups_model))
```

    ##        term df     sumsq   meansq statistic     p.value
    ## 1  activity  2  8239.227 4119.613  19.31099 1.93054e-07
    ## 2 Residuals 72 15359.760  213.330        NA          NA

So what does this output mean? Jumping right to the most important result in regards for rejecting (or failing to reject) our null hypothesis is the p-value. In this simple one-way ANOVA we have a single p-value which has a very small value of 1.884772 x 10<sup>−07</sup>. Given that this is much much smaller than the commonly used threshold for rejecting the null hypothesis, p &lt; 0.05, we can reject our null hypothesis that all the population mean for longevity of male fruit flys is equal between all groups, and accept the alternative hypothesis that **at least** one group's population mean differs from that of the other groups.

### 3.1 Assess which groups are different from eachother

But which groups population means are different? This is something ANOVA alone cannot tell us. To answer this we need to either perform pair-wise t-tests (followed by an adjustment or correction for multiple comparisons, such as a Bonferroni correction, or False Discovery Rate) OR follow the ANOVA with a contrast-test, such as Tukey's honestly significant difference (HSD) test. We'll do both here, and show that we get similar results:

``` r
# pairwise t-tests to observe group differences
(fruitfly_3_groups_pairwise <- tidy(pairwise.t.test(fruitfly_3_groups$longevity, 
                fruitfly_3_groups$activity,
                p.adjust.method = "bonferroni",
                pool.sd = TRUE,
                paired = FALSE)))
```

    ##   group1 group2      p.value
    ## 1    low   none 3.123483e-01
    ## 2   high   none 2.061573e-07
    ## 4   high    low 1.244791e-04

``` r
# Tukey's HSD test to observe group differences
(fruitfly_3_groups_HSD <- tidy(TukeyHSD(fruitfly_3_groups_model, "activity")))
```

    ##       term comparison estimate  conf.low  conf.high  adj.p.value
    ## 1 activity   low-none    -6.80 -16.68635   3.086354 2.332151e-01
    ## 2 activity  high-none   -24.84 -34.72635 -14.953646 2.051119e-07
    ## 3 activity   high-low   -18.04 -27.92635  -8.153646 1.218267e-04

From both of these multiple comparison tests, we see that there is no significant difference betweent the population mean longevity of male fruit flies who had no, or little sexual activity. But high sexual activity does appear to matter, as the population mean longevity of male fruit flies who had high sexual activity is significantly different from that of male flies who had either no, or low sexual activity.

4. 2-way ANOVA with 2 groups
----------------------------

In this experiment, we not only interested in how sexual activity might effect longevity, we are also interested in taking into account body size (assessed via thoraz length). We do this because the literature indicates this has been previously shown to affect longevity. Thus, now we have two categories/explanatory variables to look at, sexual activity (with levels `No` and `Yes`) and thorax length (with levels `short` and `long`).

So for this experiment, we have two sets of null and alternative hypotheses:

#### Hypotheses for sexual activity:

-   *H*<sub>0</sub> : *μ*<sub>*N**o*</sub> = *μ*<sub>*Y**e**s*</sub>
-   *H*<sub>*A*</sub> : *μ*<sub>*N**o*</sub> ≠ *μ*<sub>*Y**e**s*</sub>

#### Hypotheses for thorax length:

-   *H*<sub>0</sub> : *μ*<sub>*s**h**o**r**t*</sub> = *μ*<sub>*l**o**n**g*</sub>
-   *H*<sub>*A*</sub> : *μ*<sub>*s**h**o**r**t*</sub> ≠ *μ*<sub>*l**o**n**g*</sub>

Now that we have our case setup, let's load the data and explore it:

``` r
# load data
fruitfly_thorax_len <- read_csv("data/fruitfly_thorax_len.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   thorax = col_character(),
    ##   longevity = col_integer(),
    ##   sexually_active = col_character()
    ## )

``` r
# get number of rows and columns
dim(fruitfly_thorax_len)
```

    ## [1] 50  3

``` r
# view first 6 records of data
head(fruitfly_thorax_len)
```

    ## # A tibble: 6 x 3
    ##   thorax longevity sexually_active
    ##    <chr>     <int>           <chr>
    ## 1  short        40              No
    ## 2  short        37              No
    ## 3  short        44              No
    ## 4  short        47              No
    ## 5  short        47              No
    ## 6  short        47              No

``` r
# see summary of data
summary(fruitfly_thorax_len)
```

    ##     thorax            longevity     sexually_active   
    ##  Length:50          Min.   :16.00   Length:50         
    ##  Class :character   1st Qu.:37.75   Class :character  
    ##  Mode  :character   Median :47.00   Mode  :character  
    ##                     Mean   :51.14                     
    ##                     3rd Qu.:61.75                     
    ##                     Max.   :96.00

Next, let's plot the data:

``` r
# re-order factors to make them show up how we would like them on the plot
# instead of alphabetically (default R behaviour)
fruitfly_thorax_len$thorax <- factor(fruitfly_thorax_len$thorax, 
                                     levels = c("short", 
                                                "long"))
# plot strip charts of longevity, grouped by sexual activity
# and colored by thorax length
ggplot(fruitfly_thorax_len, 
       aes(x = sexually_active, y = longevity, color = thorax)) + 
  stat_summary(fun.y = mean, 
               geom = "point", 
               shape = 5, 
               size = 4,
               position = position_dodge(0.5)) +
  geom_jitter(position = position_dodge(0.5), alpha = 0.3) +
  scale_color_manual(values=c("black", "dodgerblue3")) +
  xlab("Sexual Activity") +
  ylab("Longevity (days)")
```

![](ANOVA_files/figure-markdown_github/plot%202-way%20ANOVA%20with%202%20groups-1.png)

This data visualization suggests that both sexual activity and body size/thorax length may effect longevity. Let's confirm (or disprove) this intuition by performing a 2-way (or 2-factor) ANOVA.

To perform a 2-way ANOVA, we modify the formula notation that we pass into to `aov` function by adding an additional factor/category/explanatory variable through the use of the `+` sign and the name of the factor/category/explanatory variable. Thus, our formula for this case is:

    longevity ~ activity + thorax

Everything else remains the same:

``` r
# create an ANOVA "model" object 
fruitfly_thorax_len_model <- aov(longevity ~ sexually_active + thorax, 
                               data = fruitfly_thorax_len)

# view output of aov() as a nice dataframe using tidy() from the broom package
(fruitfly_thorax_len_results <- tidy(fruitfly_thorax_len_model))
```

    ##              term df    sumsq   meansq statistic      p.value
    ## 1 sexually_active  1 7712.820 7712.820  72.01715 4.836511e-11
    ## 2          thorax  1 4977.642 4977.642  46.47789 1.546660e-08
    ## 3       Residuals 47 5033.558  107.097        NA           NA

Now we see that we get back an additional line in our results summary that corresponds to the hypotheses regarding the effect of body size/thoraz length on longevity. The p-value from this line, ~ 1.55 x 10<sup>−08</sup>, is very very small, and thus we can reject the null hypothesis (*μ*<sub>*s**h**o**r**t*</sub> = *μ*<sub>*l**o**n**g*</sub>) and infer that body size/thorax length has a statistically signifcant effect on longevity.

We also see, that in this case, we again observe a very small p-value, ~ 4.84 x 10<sup>−11</sup>, for the factor/category/explanatory variable of sexual activity, and thus we also reject the null hypothesis (*μ*<sub>*N**o*</sub> = *μ*<sub>*Y**e**s*</sub>) and infer that sexual activity also has a statistically signifcant effect on longevity.

5. 2-way ANOVA with 2 groups including an interaction term
----------------------------------------------------------

Often times when we are dealing with experiments/cases where we have 2 or more factor/category/explanatory variables we will want to first ask if there is an interaction effect between them on their influence/effect on the respoonse variable. What do we mean by interaction effect? Essentially an interaction effect is observed when the effect of two explanatory variables on the respoonse variable is not additive (for example, their effect could instead be synergistic).

Our hypotheses for whether or not there is an interaction are:

-   *H*<sub>0</sub> : There is **no** interaction effect between sexual activity and thorax length on the mean longevity of the population.
-   *H*<sub>*A*</sub> : There is an interaction effect between sexual activity and thorax length on the mean longevity of the population.

In a simple case, as presented in this experiment, we first assess the hypotheses in regards to the presence or absence of interaction. If we reject the interaction effect null hypothesis, then we interpret the data only in regards to this null hypothesis. If we fail to reject the interaction effect null hypothesis, then we can proceed and investigate/test the hypotheses for each individual factor/category/explanatory variable (often referred to as "main effects").

Can we get an intuitive sense for this via visualization? Yes we can by making an interaction plot (see example below). Essentially, we are looking at the slope of the lines that connects the means. If the slopes of the interaction lines are parrallel, then the ANOVA results will very likely tell us that we will fail to reject the interaction effect null hypothesis. On the other hand, if they are not parrallel, the ANOVA results will very likely tell us to reject the interaction effect null hypothesis, and we can infer that there is an interaction effect on the response variable between the two factor/category/explanatory variables.

Let's look at the interaction plot for our case:

``` r
# plot to investigate possible interaction effect of sexual 
# activity and thorax length on longevity
ggplot(fruitfly_thorax_len, 
       aes(x = sexually_active, y = longevity, color = thorax)) + 
  stat_summary(fun.y = mean, 
               geom = "point", 
               shape = 18, 
               size = 3) +
  stat_summary(fun.y = mean, 
               geom = "line", 
               aes(group = thorax)) +
  scale_color_manual(values=c("black", "dodgerblue3")) +
  xlab("Sexual Activity") +
  ylab("Longevity (days)")
```

![](ANOVA_files/figure-markdown_github/plot%20ANOVA%20with%20interaction-1.png)

Although not perfectly parrallel, the lines on the interaction plot are pretty close to parrallel, and thus the ANOVA results will very likely tell us that we will fail to reject the interaction effect null hypothesis. Let's proceed with the analysis to be sure.

One way to include an interaction term in your ANOVA model is to use the `*` symbol between two factor/category/explanatory variables. This causes R to test the null hypotheses for the effect of each individual factor/category/explanatory variables, as well as the combined effect of these two explanatory variables. Thus for us, our formula notation is now:

    longevity ~ activity * thorax

Everything else about our input to the functions remains the same:

``` r
# create an ANOVA "model" object 
fruitfly_thorax_len_model <- aov(longevity ~ sexually_active * thorax, 
                               data = fruitfly_thorax_len)

# view output of aov() as a nice dataframe using tidy() from the broom package
(fruitfly_thorax_len_results <- tidy(fruitfly_thorax_len_model))
```

    ##                     term df    sumsq    meansq statistic      p.value
    ## 1        sexually_active  1 7712.820 7712.8200 72.302578 5.421254e-11
    ## 2                 thorax  1 4977.642 4977.6416 46.662093 1.634302e-08
    ## 3 sexually_active:thorax  1  126.545  126.5450  1.186275 2.817554e-01
    ## 4              Residuals 46 4907.013  106.6742        NA           NA

As stated above, as a rule of thumb for cases such as these, the first hypotheses we should attend to are those regarding the interaction effect (or lack thereof). We can see our output from ANOVA now has an additional line that refers to the testing of the interaction effect hypothesis:

    sexually_active:thorax  1  126.545  126.5450  1.186275 2.817554e-01

We observe that the p-value from this line is not very small, ~ 0.3, and not less than the standard p-value threshold for rejecting null hypotheses (0.05). Thus, as our interaction plot suggested, we fail to reject the null hypotheses (there is **no** interaction effect between sexual activity and thorax length on the mean longevity of the population) and we would then proceed to investigate the hypotheses of each main effect independently. This could be done by either interpreting the relevant p-values from our current ANOVA results table, or re-running the analysis without the interaction term (as done in the previous case).
