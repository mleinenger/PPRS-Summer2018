PPRS
================
Mallorie Leinenger
7/16/2018

Phonological and Predictive Reading Strategies
==============================================

Overview
--------

These data come from an eye tracking experiment investigating phonological coding and predictability effects during silent reading. To measure the use of phonological coding, participants read 180 neutral sentences that contained a contextually appropriate (but not predictable) target word. We used the gaze-contingent boundary paradigm (Rayner, 1975) to manipulate the preview information that readers had prior to directly fixating the target word. Readers either had an *identical* preview of the target (*beach*-*beach*), a *phonologically related* preview (*beech*-*beach*), or a control preview that shared the same number of letters with the correct target as the phonologically related preview, but did not fully overlap on phonology (*bench*-*beach*). We recorded readers eye movements as they read the sentences and compared reading time on the target word as a function of the type of preview to investigate the use of phonological codes. Following Leinenger (2018), individual participant survival analyses were computed on the first fixation duration data for the phonologically related and control conditions to determine the divergence point estimate for each participant. This estimate was taken to represent the earliest observable influence of phonology on the eye movement record and therefore as an index of how early participants were generating and using phonological codes, relative to one another.

We also included 62 sentences with a manipulation of contextual constraint to investigate reliance on predictive strategies. Participants read sentences which either rendered a target word highly predictable or not predictable, and we compared reading time as a function of the constraint condition. Larger differences between the predictable and unpredictable conditions (whereby the predictable targets were read faster than the unpredictable) were taken as reflecting a greater reliance on context for generating predictions about upcoming words. Example stimuli appear below:

-   She moved from the country to the large **city** to find a better job. (Cloze = .9, predictable)
-   We took a walk in the quiet **city** before we drove back home. (Cloze = 0, unpredictable)

Ultimately, participants scores on a battery of offline language assessments (verbal fluency, ) were related to their use of phonological coding and predictive strategies, to determine what specific language skills (or language skill profiles) were associated with different online reading strategies.

Examining the use of Phonological Codes
---------------------------------------

``` r
# read in the data
d <- read.csv("PPRS_phon.csv")

# create new "Preview" variable and recode variables into factors
d$Preview <- as.factor(ifelse(d$cond==1,"Identical", ifelse(d$cond==2, "Phonologically Related", "Control")))
d$previewtype<-as.factor(ifelse(d$cond == 1, 2, ifelse(d$cond == 2, 1, 3)))
d$skp<-ifelse(d$skp==100,0,1)
d$subj<-as.factor(d$subj)
d$cond<-as.factor(d$cond)
d$item<-as.factor(d$item)
d$seq<-as.factor(d$seq)

# ffd = first fixation duration, sfd = single fixation duration, gzd = gazd duration, gpt = go-past time, tvt = total time, skp = skipping probability, rgo = regression out probability, rgi = regression in probability
str(d)
```

    ## 'data.frame':    5223 obs. of  14 variables:
    ##  $ seq        : Factor w/ 180 levels "0","1","2","3",..: 106 68 143 25 115 157 11 108 97 132 ...
    ##  $ subj       : Factor w/ 31 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ item       : Factor w/ 180 levels "1","2","3","4",..: 2 3 4 5 6 7 8 9 10 11 ...
    ##  $ cond       : Factor w/ 3 levels "1","2","3": 2 3 1 2 3 1 2 3 1 2 ...
    ##  $ ffd        : int  200 164 156 287 170 263 274 302 177 300 ...
    ##  $ sfd        : int  200 164 156 287 170 263 274 302 177 300 ...
    ##  $ gzd        : int  200 164 156 287 170 263 274 302 177 300 ...
    ##  $ gpt        : int  200 164 156 287 170 263 274 302 177 300 ...
    ##  $ tvt        : int  200 164 156 287 170 263 274 302 177 300 ...
    ##  $ skp        : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ rgi        : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ rgo        : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Preview    : Factor w/ 3 levels "Control","Identical",..: 3 1 2 3 1 2 3 1 2 3 ...
    ##  $ previewtype: Factor w/ 3 levels "1","2","3": 1 3 2 1 3 2 1 3 2 1 ...

``` r
head(d)
```

    ##   seq subj item cond ffd sfd gzd gpt tvt skp rgi rgo
    ## 1 105    1    2    2 200 200 200 200 200   0   0   0
    ## 2  67    1    3    3 164 164 164 164 164   0   0   0
    ## 3 142    1    4    1 156 156 156 156 156   0   0   0
    ## 4  24    1    5    2 287 287 287 287 287   0   0   0
    ## 5 114    1    6    3 170 170 170 170 170   0   0   0
    ## 6 156    1    7    1 263 263 263 263 263   0   0   0
    ##                  Preview previewtype
    ## 1 Phonologically Related           1
    ## 2                Control           3
    ## 3              Identical           2
    ## 4 Phonologically Related           1
    ## 5                Control           3
    ## 6              Identical           2

``` r
# compute basic summary statistics
byCond <- group_by(d, Preview)
stats.m<-summarize_if(byCond,is.numeric,funs(mean), na.rm=TRUE)
stats.se<-summarize_if(byCond,is.numeric,funs(std.error), na.rm=TRUE)
stats.m
```

    ## # A tibble: 3 x 9
    ##   Preview                  ffd   sfd   gzd   gpt   tvt   skp   rgi   rgo
    ##   <fct>                  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1 Control                 247.  254.  269.  337.  321. 0.235 0.210 0.180
    ## 2 Identical               222.  224.  235.  284.  278. 0.272 0.168 0.125
    ## 3 Phonologically Related  242.  247.  263.  317.  302. 0.236 0.186 0.144

``` r
stats.se
```

    ## # A tibble: 3 x 9
    ##   Preview                ffd   sfd   gzd   gpt   tvt    skp    rgi     rgo
    ##   <fct>                <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>  <dbl>   <dbl>
    ## 1 Control               2.35  2.67  2.74  5.62  4.32 0.0102 0.0105 0.0105 
    ## 2 Identical             2.04  2.11  2.42  5.77  4.13 0.0107 0.0101 0.00928
    ## 3 Phonologically Rela…  2.17  2.39  2.75  4.99  3.89 0.0102 0.0101 0.00963

``` r
# create a figure that plots duration measures

d$Preview<- factor(d$Preview,levels(d$Preview)[c(2,3,1)])
dat.SCA<-melt(data=d, id = c("subj","item","Preview"), measure = c("ffd","sfd","gzd","gpt","tvt"))
p.meanse.SCA<-ggplot(data = subset(dat.SCA, variable %in% c("ffd","sfd","gzd","gpt","tvt")), aes(y = value, x= Preview, shape=Preview, color=Preview)) + 
  stat_summary(fun.data = "mean_se") + stat_summary(fun.y = mean, geom = "point") + facet_grid(.~variable) +
  labs(y = "Reading Time (ms)", x = "", shape="Preview Condition", color="Preview Condition") + theme_grey(base_size=16) + scale_y_continuous(breaks=seq(0,500,20)) + 
  theme(axis.text.x = element_text(colour="grey4", size=0), axis.text.y = element_text(colour = "grey4", size=20))
p.meanse.SCA + scale_colour_manual(values=c("aquamarine4","aquamarine3","coral3")) + scale_shape_manual(values=c(1,0,2)) 
```

![](PPRS_markdown_files/figure-markdown_github/read%20phonological%20data-1.png)

``` r
# create a figure that plots probability measures

dat.SCA<-melt(data=d, id = c("subj","item","Preview"), measure = c("skp","rgo","rgi"))
p.meanse.SCA<-ggplot(data = subset(dat.SCA, variable %in% c("skp","rgo","rgi")), aes(y = value, x= Preview, shape=Preview, color=Preview)) + 
  stat_summary(fun.data = "mean_se") + stat_summary(fun.y = mean, geom = "point") + facet_grid(.~variable) +
  labs(y = "Probability (%)", x = "", shape="Preview Condition", color="Preview Condition") + theme_grey(base_size=16) + scale_y_continuous(breaks=seq(0,1,.02)) + 
  theme(axis.text.x = element_text(colour="grey4", size=0), axis.text.y = element_text(colour = "grey4", size=20))
p.meanse.SCA + scale_colour_manual(values=c("aquamarine4","aquamarine3","coral3")) + scale_shape_manual(values=c(1,0,2)) 
```

![](PPRS_markdown_files/figure-markdown_github/read%20phonological%20data-2.png)

Running linear mixed effect regression models to determine significant mean differences
---------------------------------------------------------------------------------------

For each model, the phonologically related condition is represented by the intercept and treatment coded contrasts test for significant differences between the phonologically

``` r
# first fixation duration

#lm.ffd<-lmer(ffd ~ previewtype + (previewtype|subj) + (previewtype|item), data=d, control=lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000))) failed to converge

#lm.ffd<-lmer(ffd ~ previewtype + (1|subj) + (0 + previewtype|subj) + (previewtype|item), data=d, control=lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000))) failed to converge

#lm.ffd<-lmer(ffd ~ previewtype + (1|subj) + (0 + previewtype|subj) + (1|item) + (0 + previewtype|item), data=d, control=lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000))) failed to converge

lm.ffd<-lmer(ffd ~ previewtype + (previewtype|subj) + (1|item), data=d, control=lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
summary(lm.ffd)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: ffd ~ previewtype + (previewtype | subj) + (1 | item)
    ##    Data: d
    ## Control: lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e+05))
    ## 
    ## REML criterion at convergence: 45227.1
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.8725 -0.6471 -0.1299  0.4626  6.9487 
    ## 
    ## Random effects:
    ##  Groups   Name         Variance Std.Dev. Corr       
    ##  item     (Intercept)   108.18  10.401              
    ##  subj     (Intercept)   633.94  25.178              
    ##           previewtype2   62.01   7.875   -0.99      
    ##           previewtype3   57.76   7.600   -0.10 -0.01
    ##  Residual              5650.72  75.171              
    ## Number of obs: 3929, groups:  item, 180; subj, 31
    ## 
    ## Fixed effects:
    ##              Estimate Std. Error t value
    ## (Intercept)   240.781      5.043  47.750
    ## previewtype2  -19.569      3.282  -5.962
    ## previewtype3    4.499      3.238   1.389
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) prvwt2
    ## previewtyp2 -0.645       
    ## previewtyp3 -0.302  0.402

``` r
# single fixation duration

lm.sfd<-lmer(sfd ~ previewtype + (previewtype|subj) + (previewtype|item), data=d, control=lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
summary(lm.sfd)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: sfd ~ previewtype + (previewtype | subj) + (previewtype | item)
    ##    Data: d
    ## Control: lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e+05))
    ## 
    ## REML criterion at convergence: 38178.4
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.6513 -0.6421 -0.1087  0.4602  6.8436 
    ## 
    ## Random effects:
    ##  Groups   Name         Variance Std.Dev. Corr       
    ##  item     (Intercept)   221.20  14.873              
    ##           previewtype2   62.51   7.906   -0.46      
    ##           previewtype3  222.01  14.900   -0.28  0.39
    ##  subj     (Intercept)   803.64  28.349              
    ##           previewtype2  101.99  10.099   -0.96      
    ##           previewtype3  103.52  10.174    0.00 -0.27
    ##  Residual              5392.25  73.432              
    ## Number of obs: 3323, groups:  item, 180; subj, 31
    ## 
    ## Fixed effects:
    ##              Estimate Std. Error t value
    ## (Intercept)   246.658      5.674  43.468
    ## previewtype2  -22.872      3.667  -6.237
    ## previewtype3    6.301      3.833   1.644
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) prvwt2
    ## previewtyp2 -0.681       
    ## previewtyp3 -0.246  0.306

``` r
# gaze duration

#lm.gzd<-lmer(gzd ~ previewtype + (previewtype|subj) + (previewtype|item), data=d, control=lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000))) failed to converge

#lm.gzd<-lmer(gzd ~ previewtype + (1|subj) + (0 + previewtype|subj) + (previewtype|item), data=d, control=lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000))) failed to converge

#lm.gzd<-lmer(gzd ~ previewtype + (1|subj) + (0 + previewtype|subj) + (1|item) + (0 + previewtype|item), data=d, control=lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000))) failed to converge

lm.gzd<-lmer(gzd ~ previewtype + (previewtype|subj) + (1|item), data=d, control=lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
summary(lm.gzd)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: gzd ~ previewtype + (previewtype | subj) + (1 | item)
    ##    Data: d
    ## Control: lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e+05))
    ## 
    ## REML criterion at convergence: 46717.8
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.4324 -0.6405 -0.1485  0.4552 11.1638 
    ## 
    ## Random effects:
    ##  Groups   Name         Variance Std.Dev. Corr       
    ##  item     (Intercept)   171.6   13.10               
    ##  subj     (Intercept)   961.5   31.01               
    ##           previewtype2  196.9   14.03    -0.82      
    ##           previewtype3  124.8   11.17    -0.14  0.01
    ##  Residual              8231.1   90.73               
    ## Number of obs: 3929, groups:  item, 180; subj, 31
    ## 
    ## Fixed effects:
    ##              Estimate Std. Error t value
    ## (Intercept)   260.724      6.194  42.094
    ## previewtype2  -26.681      4.383  -6.088
    ## previewtype3    5.740      4.077   1.408
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) prvwt2
    ## previewtyp2 -0.657       
    ## previewtyp3 -0.312  0.352

``` r
# go-past time

lm.gpt<-lmer(gpt ~ previewtype + (previewtype|subj) + (previewtype|item), data=d, control=lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
summary(lm.gpt)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: gpt ~ previewtype + (previewtype | subj) + (previewtype | item)
    ##    Data: d
    ## Control: lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e+05))
    ## 
    ## REML criterion at convergence: 52305.6
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.0108 -0.5054 -0.2031  0.2099 16.1043 
    ## 
    ## Random effects:
    ##  Groups   Name         Variance Std.Dev. Corr       
    ##  item     (Intercept)   1807.9   42.52              
    ##           previewtype2   517.9   22.76   -0.31      
    ##           previewtype3   162.9   12.76   -0.96  0.58
    ##  subj     (Intercept)   3433.0   58.59              
    ##           previewtype2   512.8   22.65   -0.19      
    ##           previewtype3   141.2   11.88    1.00 -0.23
    ##  Residual              33784.3  183.81              
    ## Number of obs: 3928, groups:  item, 180; subj, 31
    ## 
    ## Fixed effects:
    ##              Estimate Std. Error t value
    ## (Intercept)   314.735     12.123  25.961
    ## previewtype2  -30.681      8.516  -3.603
    ## previewtype3   19.352      7.540   2.567
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) prvwt2
    ## previewtyp2 -0.345       
    ## previewtyp3 -0.067  0.383

``` r
# total time

#lm.tvt<-lmer(tvt ~ previewtype + (previewtype|subj) + (previewtype|item), data=d, control=lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000))) failed to converge

#lm.tvt<-lmer(tvt ~ previewtype + (1|subj) + (0 + previewtype|subj) + (previewtype|item), data=d, control=lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000))) failed to converge

#lm.tvt<-lmer(tvt ~ previewtype + (1|subj) + (0 + previewtype|subj) + (1|item) + (0 + previewtype|item), data=d, control=lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000))) failed to converge

lm.tvt<-lmer(tvt ~ previewtype + (previewtype|subj) + (1|item), data=d, control=lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
summary(lm.tvt)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: tvt ~ previewtype + (previewtype | subj) + (1 | item)
    ##    Data: d
    ## Control: lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e+05))
    ## 
    ## REML criterion at convergence: 56267.1
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.2315 -0.6038 -0.1885  0.3517  9.7702 
    ## 
    ## Random effects:
    ##  Groups   Name         Variance Std.Dev. Corr       
    ##  item     (Intercept)   1072.0   32.74              
    ##  subj     (Intercept)   2332.5   48.30              
    ##           previewtype2   393.8   19.84   -0.17      
    ##           previewtype3   137.8   11.74    0.32  0.32
    ##  Residual              21217.2  145.66              
    ## Number of obs: 4378, groups:  item, 180; subj, 31
    ## 
    ## Fixed effects:
    ##              Estimate Std. Error t value
    ## (Intercept)   298.484      9.780  30.520
    ## previewtype2  -23.383      6.533  -3.579
    ## previewtype3   19.653      5.750   3.418
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) prvwt2
    ## previewtyp2 -0.308       
    ## previewtyp3 -0.151  0.448

``` r
# skipping

glm.skp <- glmer(skp ~ previewtype + (previewtype|subj) + (previewtype|item), data=d, family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
summary(glm.skp)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: skp ~ previewtype + (previewtype | subj) + (previewtype | item)
    ##    Data: d
    ## Control: 
    ## glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e+05))
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   5331.7   5430.1  -2650.8   5301.7     5208 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -1.7365 -0.5483 -0.4095 -0.1479  5.9521 
    ## 
    ## Random effects:
    ##  Groups Name         Variance  Std.Dev. Corr       
    ##  item   (Intercept)  0.2905249 0.53900             
    ##         previewtype2 0.0167137 0.12928  -1.00      
    ##         previewtype3 0.0001719 0.01311   1.00 -1.00
    ##  subj   (Intercept)  0.8110131 0.90056             
    ##         previewtype2 0.0514416 0.22681  -0.98      
    ##         previewtype3 0.0056533 0.07519   0.34 -0.53
    ## Number of obs: 5223, groups:  item, 180; subj, 31
    ## 
    ## Fixed effects:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  -1.42920    0.18073  -7.908 2.62e-15 ***
    ## previewtype2  0.30728    0.10122   3.036   0.0024 ** 
    ## previewtype3 -0.02685    0.10019  -0.268   0.7887    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) prvwt2
    ## previewtyp2 -0.638       
    ## previewtyp3 -0.224  0.447

``` r
# regression-out

glm.rgo <- glmer(rgo ~ previewtype + (previewtype|subj) + (previewtype|item), data=d, family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
summary(glm.rgo)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: rgo ~ previewtype + (previewtype | subj) + (previewtype | item)
    ##    Data: d
    ## Control: 
    ## glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e+05))
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   3097.2   3191.3  -1533.6   3067.2     3912 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -1.1872 -0.4262 -0.3077 -0.2151  5.3470 
    ## 
    ## Random effects:
    ##  Groups Name         Variance Std.Dev. Corr       
    ##  item   (Intercept)  0.65062  0.8066              
    ##         previewtype2 0.45898  0.6775   -0.72      
    ##         previewtype3 0.36964  0.6080   -0.84  0.98
    ##  subj   (Intercept)  0.78418  0.8855              
    ##         previewtype2 0.23284  0.4825   -0.14      
    ##         previewtype3 0.03413  0.1847   -0.61 -0.01
    ## Number of obs: 3927, groups:  item, 180; subj, 31
    ## 
    ## Fixed effects:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)   -2.2170     0.2064 -10.741  < 2e-16 ***
    ## previewtype2  -0.1011     0.1937  -0.522  0.60161    
    ## previewtype3   0.4733     0.1560   3.034  0.00241 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) prvwt2
    ## previewtyp2 -0.443       
    ## previewtyp3 -0.590  0.513

``` r
# regression-in

#glm.rgi <- glmer(rgi ~ previewtype + (previewtype|subj) + (previewtype|item), data=d, family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000))) failed to converge

glm.rgi <- glmer(rgi ~ previewtype + (1|subj) + (0 + previewtype|subj) + (previewtype|item), data=d, family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
summary(glm.rgi)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: 
    ## rgi ~ previewtype + (1 | subj) + (0 + previewtype | subj) + (previewtype |  
    ##     item)
    ##    Data: d
    ## Control: 
    ## glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e+05))
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   4032.4   4134.6  -2000.2   4000.4     4362 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -1.3278 -0.4946 -0.3864 -0.2504  4.4782 
    ## 
    ## Random effects:
    ##  Groups Name         Variance  Std.Dev.  Corr       
    ##  item   (Intercept)  1.948e-01 0.4414150            
    ##         previewtype2 1.046e-01 0.3234319 -0.12      
    ##         previewtype3 6.479e-02 0.2545307  0.24 -0.99
    ##  subj   previewtype1 4.799e-01 0.6927757            
    ##         previewtype2 5.921e-01 0.7695080 1.00       
    ##         previewtype3 4.736e-01 0.6881740 0.89  0.90 
    ##  subj.1 (Intercept)  2.687e-07 0.0005183            
    ## Number of obs: 4378, groups:  item, 180; subj, 31
    ## 
    ## Fixed effects:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)   -1.6512     0.1509 -10.943   <2e-16 ***
    ## previewtype2  -0.1704     0.1242  -1.372    0.170    
    ## previewtype3   0.1466     0.1254   1.169    0.242    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) prvwt2
    ## previewtyp2 -0.243       
    ## previewtyp3 -0.416  0.367

Computing Divergence Point Estimates using IP-DPA Survival Analysis Technique (Reingold & Sheridan, 2014)
---------------------------------------------------------------------------------------------------------

``` r
tmp<-select(d,subj,ffd,cond) %>% arrange(cond) %>%
  rename(duration=ffd) %>% rename(subject=subj) %>% rename(condition=cond)
tmp$condition <- as.numeric(tmp$condition)
survdata<- as.data.frame(tmp %>% as_tibble() %>% mutate(condition = condition-1)) %>%
  filter(condition != 0, !is.na(duration))
survdata$condition<- as.factor(survdata$condition)
str(survdata)
```

    ## 'data.frame':    2659 obs. of  3 variables:
    ##  $ subject  : Factor w/ 31 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ duration : int  200 287 274 300 240 168 143 199 178 207 ...
    ##  $ condition: Factor w/ 2 levels "1","2": 1 1 1 1 1 1 1 1 1 1 ...

We have 31 participants who each have between 36 and 105 data points:

``` r
n.per.sbj <- table(survdata$subject)
length(n.per.sbj)
```

    ## [1] 31

``` r
range(n.per.sbj)
```

    ## [1]  36 105

We can now use these data to generate divergence point estimates (DPE) for each participant:

``` r
ip.dpa <- DPA.ip(survdata$subject, survdata$duration, survdata$condition, quiet = TRUE)
dpe <- as.data.frame(ip.dpa$dp_matrix)
# critical columns in output
# 'dpcount' = the number of iterations (out of 1000) on which a DPE was obtained
#' median_dp_duration' = median of the DPEs obtained on each iteration
str(dpe)
```

    ## 'data.frame':    31 obs. of  6 variables:
    ##  $ subject        : Factor w/ 31 levels "1","2","3","4",..: 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ dpcount        : num  18 1000 1000 1000 1000 612 1000 848 1000 974 ...
    ##  $ median_dp_point: num  1097.5 91.5 1 492 105 ...
    ##  $ median_duration: num  312.5 145.5 95.5 228.5 145.5 ...
    ##  $ ci.lower       : num  312.5 145.5 95.5 228.5 113.5 ...
    ##  $ ci.upper       : num  313.4 150 95.5 250.5 283 ...

``` r
dpe$subject[dpe$dpcount<500]
```

    ## [1] 1  15 16 17 19 31
    ## 31 Levels: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 ... 31

Doing so reveals that the DPE for 6 participants were unreliable (i.e., a DP was found on fewer than half of the iterations). Removing those participants reveals a mean DPE of ~192 across the remaining participants (the value moves around ever so slightly each time the bootstrap re-sampling procedure runs). This tells us that on average, phonological coding was influencing behavior by as early as 192 ms after fixation on the target word began.

``` r
dpe.rel <- filter(dpe, dpcount >= 500)
summarize(dpe.rel, mean.dpe = mean(median_duration, na.rm=TRUE))
```

    ##   mean.dpe
    ## 1   191.12

``` r
DP<-mean(dpe.rel$median_duration)
ci.lower<-mean(dpe.rel$ci.lower)
ci.upper<-mean(dpe.rel$ci.upper)
```

Finally, we can represent this visually by examining the survival curves created using the ggsurv function.

``` r
d$survdat<-as.integer(ifelse(d$Preview=="Identical",NA,d$ffd))
tmp2 <- filter(d, !subj %in% c(1, 15, 16, 17, 19, 31))
ffd.surv <- survfit(Surv(survdat) ~ Preview, data=tmp2)
pl2<-ggsurv(s=ffd.surv)

#my.label1 = bquote("Divergence Point " ~ .(format(DP, digits=3)) ~ "ms")
#my.label2 = bquote("95% CI: " ~ .(format(ci.lower, digits=3)) ~ "-" ~ .(format(ci.upper, digits=3)) ~ "ms")
  
pl2 + geom_vline(xintercept = DP, linetype = "dotted") + 
  annotate("rect", xmin=ci.lower, xmax=ci.upper, ymin=0, ymax=1, alpha = .2) + 
  theme(axis.text.x = element_text(colour="grey4", size=16), axis.text.y = element_text(colour = "grey4", size=16)) + 
  labs(y = "Survival", x = "Time") + theme_grey(base_size=16)
```

![](PPRS_markdown_files/figure-markdown_github/survival%20figure%20Exp%201-1.png)

Examining the use of Context and Predictive Strategies
------------------------------------------------------

``` r
# read in the data
d <- read.csv("PPRS_pred.csv")

# create new "Constraint" variable and recode variables into factors
d$Constraint <- as.factor(ifelse(d$cond==4,"High", "Low"))
d$skp<-ifelse(d$skp==100,0,1)
d$subj<-as.factor(d$subj)
d$cond<-as.factor(d$cond)
d$item<-as.factor(d$item)
d$seq<-as.factor(d$seq)

str(d)
```

    ## 'data.frame':    1820 obs. of  13 variables:
    ##  $ seq       : Factor w/ 63 levels "0","1","2","3",..: 6 38 45 12 8 57 51 16 54 56 ...
    ##  $ subj      : Factor w/ 31 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ item      : Factor w/ 63 levels "1","2","3","4",..: 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ cond      : Factor w/ 2 levels "4","5": 1 2 1 2 1 2 1 2 1 2 ...
    ##  $ ffd       : int  209 176 142 172 195 266 179 265 305 282 ...
    ##  $ sfd       : int  209 176 142 172 195 266 179 265 305 282 ...
    ##  $ gzd       : int  209 176 142 172 195 266 179 265 305 282 ...
    ##  $ gpt       : int  209 176 142 172 268 266 179 265 305 282 ...
    ##  $ tvt       : int  209 176 142 172 195 266 348 265 305 282 ...
    ##  $ skp       : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ rgo       : int  0 0 0 0 1 0 0 0 0 0 ...
    ##  $ rgi       : int  0 0 0 0 0 0 1 0 0 0 ...
    ##  $ Constraint: Factor w/ 2 levels "High","Low": 1 2 1 2 1 2 1 2 1 2 ...

``` r
head(d)
```

    ##   seq subj item cond ffd sfd gzd gpt tvt skp rgo rgi Constraint
    ## 1   5    1    1    4 209 209 209 209 209   0   0   0       High
    ## 2  37    1    2    5 176 176 176 176 176   0   0   0        Low
    ## 3  44    1    3    4 142 142 142 142 142   0   0   0       High
    ## 4  11    1    4    5 172 172 172 172 172   0   0   0        Low
    ## 5   7    1    5    4 195 195 195 268 195   0   1   0       High
    ## 6  56    1    6    5 266 266 266 266 266   0   0   0        Low

``` r
# compute basic summary statistics
byCond <- group_by(d, Constraint)
stats.m<-summarize_if(byCond,is.numeric,funs(mean), na.rm=TRUE)
stats.se<-summarize_if(byCond,is.numeric,funs(std.error), na.rm=TRUE)
stats.m
```

    ## # A tibble: 2 x 9
    ##   Constraint   ffd   sfd   gzd   gpt   tvt   skp    rgo   rgi
    ##   <fct>      <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl>
    ## 1 High        211.  209.  230.  263.  267. 0.233 0.0978 0.145
    ## 2 Low         224.  225.  252.  302.  311. 0.201 0.114  0.210

``` r
stats.se
```

    ## # A tibble: 2 x 9
    ##   Constraint   ffd   sfd   gzd   gpt   tvt    skp    rgo    rgi
    ##   <fct>      <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>  <dbl>  <dbl>
    ## 1 High        2.77  2.88  3.87  6.21  5.57 0.0140 0.0113 0.0129
    ## 2 Low         2.81  3.13  4.03  7.65  6.30 0.0133 0.0118 0.0146

``` r
# create a figure that plots duration measures

dat.SCA<-melt(data=d, id = c("subj","item","Constraint"), measure = c("ffd","sfd","gzd","gpt","tvt"))
p.meanse.SCA<-ggplot(data = subset(dat.SCA, variable %in% c("ffd","sfd","gzd","gpt","tvt")), aes(y = value, x= Constraint, shape=Constraint, color=Constraint)) + 
  stat_summary(fun.data = "mean_se") + stat_summary(fun.y = mean, geom = "point") + facet_grid(.~variable) +
  labs(y = "Reading Time (ms)", x = "", shape="Constraint", color="Constraint") + theme_grey(base_size=16) + scale_y_continuous(breaks=seq(0,500,20)) + 
  theme(axis.text.x = element_text(colour="grey4", size=0), axis.text.y = element_text(colour = "grey4", size=20))
p.meanse.SCA + scale_colour_manual(values=c("dodgerblue1","grey0")) + scale_shape_manual(values=c(1,0)) 
```

![](PPRS_markdown_files/figure-markdown_github/read%20predictability%20data-1.png)

``` r
# create a figure that plots probability measures

dat.SCA<-melt(data=d, id = c("subj","item","Constraint"), measure = c("skp", "rgo", "rgi"))
p.meanse.SCA<-ggplot(data = subset(dat.SCA, variable %in% c("skp", "rgo", "rgi")), aes(y = value, x= Constraint, shape=Constraint, color=Constraint)) + 
  stat_summary(fun.data = "mean_se") + stat_summary(fun.y = mean, geom = "point") + facet_grid(.~variable) +
  labs(y = "Reading Time (ms)", x = "", shape="Constraint", color="Constraint") + theme_grey(base_size=16) + scale_y_continuous(breaks=seq(0,1,.02)) + 
  theme(axis.text.x = element_text(colour="grey4", size=0), axis.text.y = element_text(colour = "grey4", size=20))
p.meanse.SCA + scale_colour_manual(values=c("dodgerblue1","grey0")) + scale_shape_manual(values=c(1,0)) 
```

![](PPRS_markdown_files/figure-markdown_github/read%20predictability%20data-2.png)

Running linear mixed effect regression models to determine significant mean differences
---------------------------------------------------------------------------------------

``` r
# setting up contrasts (deviation coding)

contrasts(d$Constraint) <- rbind(-.5,.5)

# first fixation duration

lm.ffd<-lmer(ffd ~ Constraint + (Constraint|subj) + (Constraint|item), data=d, control=lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
summary(lm.ffd)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: ffd ~ Constraint + (Constraint | subj) + (Constraint | item)
    ##    Data: d
    ## Control: lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e+05))
    ## 
    ## REML criterion at convergence: 16195.5
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.6912 -0.5679 -0.1905  0.3753  6.3104 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev. Corr 
    ##  item     (Intercept)  179.9   13.41         
    ##           Constraint1  185.2   13.61    0.53 
    ##  subj     (Intercept)  568.2   23.84         
    ##           Constraint1  209.7   14.48    -0.34
    ##  Residual             4685.6   68.45         
    ## Number of obs: 1425, groups:  item, 63; subj, 31
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error t value
    ## (Intercept)  216.995      4.966  43.696
    ## Constraint1   13.271      4.816   2.756
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## Constraint1 -0.098

``` r
# single fixation duration

lm.sfd<-lmer(sfd ~ Constraint + (Constraint|subj) + (Constraint|item), data=d, control=lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
summary(lm.sfd)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: sfd ~ Constraint + (Constraint | subj) + (Constraint | item)
    ##    Data: d
    ## Control: lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e+05))
    ## 
    ## REML criterion at convergence: 13657.2
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.4321 -0.5662 -0.1855  0.3431  6.3091 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev. Corr
    ##  item     (Intercept)  253.5   15.92        
    ##           Constraint1  222.7   14.92    0.70
    ##  subj     (Intercept)  681.9   26.11        
    ##           Constraint1  350.0   18.71    0.05
    ##  Residual             4343.7   65.91        
    ## Number of obs: 1207, groups:  item, 63; subj, 31
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error t value
    ## (Intercept)  217.486      5.462  39.819
    ## Constraint1   17.656      5.457   3.235
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## Constraint1 0.118

``` r
# gaze duration

lm.gzd<-lmer(gzd ~ Constraint + (Constraint|subj) + (Constraint|item), data=d, control=lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
summary(lm.gzd)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: gzd ~ Constraint + (Constraint | subj) + (Constraint | item)
    ##    Data: d
    ## Control: lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e+05))
    ## 
    ## REML criterion at convergence: 17169.6
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.6599 -0.5601 -0.2328  0.3190  7.3489 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance  Std.Dev. Corr
    ##  item     (Intercept)  423.4446 20.578       
    ##           Constraint1  380.3883 19.504   0.32
    ##  subj     (Intercept) 1339.9648 36.606       
    ##           Constraint1    0.6226  0.789   1.00
    ##  Residual             9293.4209 96.402       
    ## Number of obs: 1425, groups:  item, 63; subj, 31
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error t value
    ## (Intercept)   238.27       7.54    31.6
    ## Constraint1    21.66       5.70     3.8
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## Constraint1 0.063

``` r
# go-past time

lm.gpt<-lmer(gpt ~ Constraint + (Constraint|subj) + (Constraint|item), data=d, control=lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
summary(lm.gpt)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: gpt ~ Constraint + (Constraint | subj) + (Constraint | item)
    ##    Data: d
    ## Control: lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e+05))
    ## 
    ## REML criterion at convergence: 18811.8
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -1.9092 -0.5011 -0.2499  0.1888  7.3485 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev. Corr
    ##  item     (Intercept)  2214.9   47.06       
    ##           Constraint1  2750.2   52.44   0.90
    ##  subj     (Intercept)  2423.4   49.23       
    ##           Constraint1   941.3   30.68   0.48
    ##  Residual             29170.1  170.79       
    ## Number of obs: 1425, groups:  item, 63; subj, 31
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error t value
    ## (Intercept)   279.51      11.62  24.061
    ## Constraint1    36.99      12.56   2.945
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## Constraint1 0.399

``` r
# total time

lm.tvt<-lmer(tvt ~ Constraint + (Constraint|subj) + (Constraint|item), data=d, control=lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
summary(lm.tvt)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: tvt ~ Constraint + (Constraint | subj) + (Constraint | item)
    ##    Data: d
    ## Control: lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e+05))
    ## 
    ## REML criterion at convergence: 19710.8
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.3507 -0.5807 -0.2233  0.3237  5.6175 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev. Corr
    ##  item     (Intercept)  1440     37.94       
    ##           Constraint1  3152     56.15   0.54
    ##  subj     (Intercept)  2929     54.12       
    ##           Constraint1  1273     35.67   0.46
    ##  Residual             21530    146.73       
    ## Number of obs: 1526, groups:  item, 63; subj, 31
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error t value
    ## (Intercept)   285.64      11.49  24.862
    ## Constraint1    40.70      12.20   3.334
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## Constraint1 0.333

``` r
# skipping

glm.skp <- glmer(skp ~ Constraint + (Constraint|subj) + (Constraint|item), data=d, family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
summary(glm.skp)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: skp ~ Constraint + (Constraint | subj) + (Constraint | item)
    ##    Data: d
    ## Control: 
    ## glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e+05))
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   1723.7   1767.8   -853.9   1707.7     1812 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -1.6117 -0.5096 -0.3451 -0.1737  5.3177 
    ## 
    ## Random effects:
    ##  Groups Name        Variance  Std.Dev. Corr 
    ##  item   (Intercept) 0.4513670 0.67184       
    ##         Constraint1 0.0432703 0.20802  -1.00
    ##  subj   (Intercept) 0.9951173 0.99756       
    ##         Constraint1 0.0001873 0.01369  1.00 
    ## Number of obs: 1820, groups:  item, 63; subj, 31
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  -1.6407     0.2124  -7.724 1.13e-14 ***
    ## Constraint1  -0.1647     0.1530  -1.076    0.282    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## Constraint1 -0.045

``` r
# regression-out

glm.rgo <- glmer(rgo ~ Constraint + (Constraint|subj) + (Constraint|item), data=d, family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
summary(glm.rgo)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: rgo ~ Constraint + (Constraint | subj) + (Constraint | item)
    ##    Data: d
    ## Control: 
    ## glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e+05))
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##    942.1    984.2   -463.1    926.1     1415 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -0.8001 -0.3699 -0.2771 -0.2026  4.9064 
    ## 
    ## Random effects:
    ##  Groups Name        Variance Std.Dev. Corr
    ##  item   (Intercept) 0.2106   0.4589       
    ##         Constraint1 0.3491   0.5908   0.18
    ##  subj   (Intercept) 0.6326   0.7954       
    ##         Constraint1 0.1126   0.3356   0.82
    ## Number of obs: 1423, groups:  item, 63; subj, 31
    ## 
    ## Fixed effects:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -2.466954   0.198238 -12.444   <2e-16 ***
    ## Constraint1 -0.005754   0.265294  -0.022    0.983    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## Constraint1 0.160

``` r
# regression-in

glm.rgi <- glmer(rgi ~ Constraint + (Constraint|subj) + (Constraint|item), data=d, family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
summary(glm.rgi)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: rgi ~ Constraint + (Constraint | subj) + (Constraint | item)
    ##    Data: d
    ## Control: 
    ## glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e+05))
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   1356.6   1399.2   -670.3   1340.6     1518 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -1.4161 -0.4570 -0.3571 -0.2504  3.9569 
    ## 
    ## Random effects:
    ##  Groups Name        Variance Std.Dev. Corr 
    ##  item   (Intercept) 0.04055  0.2014        
    ##         Constraint1 0.88796  0.9423   -0.55
    ##  subj   (Intercept) 0.59863  0.7737        
    ##         Constraint1 0.05012  0.2239   1.00 
    ## Number of obs: 1526, groups:  item, 63; subj, 31
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  -1.7716     0.1647 -10.757   <2e-16 ***
    ## Constraint1   0.4399     0.2098   2.097    0.036 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## Constraint1 0.054

Computing the effect of predictability for each participant using Survival Analyses
-----------------------------------------------------------------------------------

``` r
tmp<-select(d,subj,ffd,cond) %>% arrange(cond) %>%
  rename(duration=ffd) %>% rename(subject=subj) %>% rename(condition=cond)
tmp$condition <- as.numeric(tmp$condition)
survdata<- as.data.frame(tmp %>% as_tibble() %>% filter(!is.na(duration)))
survdata$condition<- as.factor(survdata$condition)
str(survdata)
```

    ## 'data.frame':    1425 obs. of  3 variables:
    ##  $ subject  : Factor w/ 31 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ duration : int  209 142 195 179 305 164 168 181 211 185 ...
    ##  $ condition: Factor w/ 2 levels "1","2": 1 1 1 1 1 1 1 1 1 1 ...

We have 31 participants who each have between 24 and 62 data points:

``` r
n.per.sbj <- table(survdata$subject)
length(n.per.sbj)
```

    ## [1] 31

``` r
range(n.per.sbj)
```

    ## [1] 24 62

We can now use these data to generate divergence point estimates (DPE) for each participant:

``` r
ip.dpa <- DPA.ip(survdata$subject, survdata$duration, survdata$condition, quiet = TRUE)
dpe <- as.data.frame(ip.dpa$dp_matrix)
# critical columns in output
# 'dpcount' = the number of iterations (out of 1000) on which a DPE was obtained
#' median_dp_duration' = median of the DPEs obtained on each iteration
str(dpe)
```

    ## 'data.frame':    31 obs. of  6 variables:
    ##  $ subject        : Factor w/ 31 levels "1","2","3","4",..: 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ dpcount        : num  1000 1000 1000 9 1000 1000 1000 1000 1000 1000 ...
    ##  $ median_dp_point: num  1 1 1 879 241 125 266 43 1 1 ...
    ##  $ median_duration: num  134 88 124 264 146 ...
    ##  $ ci.lower       : num  134 88 124 263 146 ...
    ##  $ ci.upper       : num  134 177 124 264 170 ...

``` r
dpe$subject[dpe$dpcount<500]
```

    ## [1] 4
    ## 31 Levels: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 ... 31

Doing so reveals that the DPE for 1 participant was unreliable (i.e., a DP was found on fewer than half of the iterations). Removing that participant reveals a mean DPE of ~151 across the remaining participants (the value moves around ever so slightly each time the bootstrap re-sampling procedure runs). This tells us that on average, the constraint manipulation was influencing behavior by as early as 151 ms after fixation on the target word began.

``` r
dpe.rel <- filter(dpe, dpcount >= 500)
summarize(dpe.rel, mean.dpe = mean(median_duration, na.rm=TRUE))
```

    ##   mean.dpe
    ## 1 150.5583

``` r
DP<-mean(dpe.rel$median_duration)
ci.lower<-mean(dpe.rel$ci.lower)
ci.upper<-mean(dpe.rel$ci.upper)
```

And we can plot the survival figure for the entire group of participants.

``` r
d$survdat<-as.integer(d$ffd)
tmp2 <- filter(d, !subj %in% c(4))
ffd.surv <- survfit(Surv(survdat) ~ Constraint, data=tmp2)
pl2<-ggsurv(s=ffd.surv)

#my.label1 = bquote("Divergence Point " ~ .(format(DP, digits=3)) ~ "ms")
#my.label2 = bquote("95% CI: " ~ .(format(ci.lower, digits=3)) ~ "-" ~ .(format(ci.upper, digits=3)) ~ "ms")
  
pl2 + geom_vline(xintercept = DP, linetype = "dotted") + 
  annotate("rect", xmin=ci.lower, xmax=ci.upper, ymin=0, ymax=1, alpha = .2) + 
  theme(axis.text.x = element_text(colour="grey4", size=16), axis.text.y = element_text(colour = "grey4", size=16)) + 
  labs(y = "Survival", x = "Time") + theme_grey(base_size=16)
```

![](PPRS_markdown_files/figure-markdown_github/survival%20figure%20pred-1.png) \#\# Determining hte size of the predictability effect for each participant.

We can compute each participant's mean gzd for the high and low constraint items and then subtract their gzd in the high constraint condition from their gzd in the low constraint condition to determine the size of each individual participant's predictability effect.

``` r
tmp <- setNames(aggregate(d$gzd, by=list(d$subj,d$cond), FUN=mean, na.rm=TRUE), c("subject","condition","gzd"))
pred <- setNames(spread(tmp,condition,gzd), c("subject","HC","LC"))
pred$diff <- pred$LC-pred$HC
head(pred,31)
```

    ##    subject       HC       LC        diff
    ## 1        1 208.2800 222.4828  14.2027586
    ## 2        2 214.8750 277.4800  62.6050000
    ## 3        3 213.5833 240.1500  26.5666667
    ## 4        4 260.3810 254.6800  -5.7009524
    ## 5        5 178.7692 198.6667  19.8974359
    ## 6        6 226.0000 220.6552  -5.3448276
    ## 7        7 318.4348 308.2273 -10.2075099
    ## 8        8 240.7692 299.1724  58.4031830
    ## 9        9 148.8000 193.4706  44.6705882
    ## 10      10 235.1923 293.4333  58.2410256
    ## 11      11 193.6667 210.7143  17.0476190
    ## 12      12 269.0500 322.3810  53.3309524
    ## 13      13 209.0435 219.2609  10.2173913
    ## 14      14 189.8000 254.2778  64.4777778
    ## 15      15 255.3333 227.8696 -27.4637681
    ## 16      16 263.0000 295.5357  32.5357143
    ## 17      17 194.0000 223.6111  29.6111111
    ## 18      18 215.2800 224.6000   9.3200000
    ## 19      19 147.6250 183.0690  35.4439655
    ## 20      20 232.0357 231.0690  -0.9667488
    ## 21      21 248.3889 249.9286   1.5396825
    ## 22      22 206.3636 241.9231  35.5594406
    ## 23      23 270.0800 232.4400 -37.6400000
    ## 24      24 199.3333 240.2727  40.9393939
    ## 25      25 241.0952 261.5000  20.4047619
    ## 26      26 189.1304 188.5500  -0.5804348
    ## 27      27 213.3333 240.5862  27.2528736
    ## 28      28 260.4583 299.0345  38.5761494
    ## 29      29 290.5000 284.5333  -5.9666667
    ## 30      30 295.7407 360.9630  65.2222222
    ## 31      31 232.4286 224.1579  -8.2706767
