> overall <- bottom.pitches %>% 
+   group_by(game.state) %>% 
+   summarise(strike.rate = mean(called.type == "strike"), 
+             strikes = sum(called.type == "strike"), 
+             balls = sum(called.type == "ball"),  n())
> o1 <- odds.ratio(overall$strike.rate[1], 
+                  overall$strike.rate[2], 
+                  overall$strikes[1], 
+                  overall$strikes[2], 
+                  overall$balls[1], 
+                  overall$balls[2])
> o2 <- odds.ratio(overall$strike.rate[3], 
+                  overall$strike.rate[2], 
+                  overall$strikes[1], 
+                  overall$strikes[2], 
+                  overall$balls[1], 
+                  overall$balls[2])
> o1
[1] 1.152730 1.089765 1.219334
> o2
[1] 0.8370889 0.7913645 0.8854552
> overall.true <- bottom.pitches %>% 
+   group_by(true.type, game.state) %>% 
+   summarise(strike.rate = mean(called.type == "strike"),  
+             strikes = sum(called.type == "strike"), 
+             balls = sum(called.type == "ball"),  n())
> ot1 <- odds.ratio(overall.true$strike.rate[1], 
+                   overall.true$strike.rate[2], 
+                   overall.true$strikes[1], 
+                   overall.true$strikes[2], 
+                   overall.true$balls[1], 
+                   overall.true$balls[2])
> ot2 <- odds.ratio(overall.true$strike.rate[3], 
+                   overall.true$strike.rate[2], 
+                   overall.true$strikes[1], 
+                   overall.true$strikes[2], 
+                   overall.true$balls[1], 
+                   overall.true$balls[2])
> ot3 <- odds.ratio(overall.true$strike.rate[4], 
+                   overall.true$strike.rate[5], 
+                   overall.true$strikes[4], 
+                   overall.true$strikes[5], 
+                   overall.true$balls[4], 
+                   overall.true$balls[5])
> ot4 <- odds.ratio(overall.true$strike.rate[6], 
+                   overall.true$strike.rate[5], 
+                   overall.true$strikes[6], 
+                   overall.true$strikes[5], 
+                   overall.true$balls[6], 
+                   overall.true$balls[5])
> ot1
[1] 1.297795 1.165698 1.444862
> ot2
[1] 0.8679775 0.7796296 0.9663370
> ot3
[1] 1.139778 1.012479 1.283081
> ot4
[1] 0.7638031 0.6824580 0.8548441
> table(bottom.pitches$inning)

   10    11    12    13    14    15    16    17    18    19    20    21    22 
14743  8114  4720  2509  1365   570   311   180   164    73    20     5    11 
> bottom.pitches %>% 
+   group_by(inning, game.state) %>% 
+   summarise(strike.rate = mean(called.type == "strike"), n()) %>% 
+   print.data.frame()
   inning    game.state strike.rate  n()
1      10 Loss Imminent   0.3482120 3831
2      10       Neutral   0.3115107 6507
3      10  Win Imminent   0.2694665 4405
4      11 Loss Imminent   0.3405051 2138
5      11       Neutral   0.3122960 3676
6      11  Win Imminent   0.2708696 2300
7      12 Loss Imminent   0.3203343 1436
8      12       Neutral   0.3095472 2032
9      12  Win Imminent   0.2827476 1252
10     13 Loss Imminent   0.3480742  701
11     13       Neutral   0.3047252 1037
12     13  Win Imminent   0.2931258  771
13     14 Loss Imminent   0.3528265  513
14     14       Neutral   0.2837022  497
15     14  Win Imminent   0.2563380  355
16     15 Loss Imminent   0.3333333  144
17     15       Neutral   0.3217054  258
18     15  Win Imminent   0.2500000  168
19     16 Loss Imminent   0.3186813   91
20     16       Neutral   0.2516556  151
21     16  Win Imminent   0.3043478   69
22     17 Loss Imminent   0.3636364   22
23     17       Neutral   0.2600000  100
24     17  Win Imminent   0.3275862   58
25     18 Loss Imminent   0.3200000   50
26     18       Neutral   0.4117647   68
27     18  Win Imminent   0.1956522   46
28     19 Loss Imminent   0.2592593   27
29     19       Neutral   0.2903226   31
30     19  Win Imminent   0.3333333   15
31     20 Loss Imminent   0.1538462   13
32     20       Neutral   0.4285714    7
33     21       Neutral   0.4000000    5
34     22 Loss Imminent   0.2727273   11
> table(bottom.pitches$game_year)

2008 2009 2010 2011 2012 2013 2014 2015 2016 
3476 3419 3354 3716 3484 4318 4256 3486 3276 
> bottom.pitches %>% 
+   group_by(game_year, game.state) %>% 
+   summarise(strike.rate = mean(called.type == "strike"), n()) %>% print.data.frame()
   game_year    game.state strike.rate  n()
1       2008 Loss Imminent   0.3352999 1017
2       2008       Neutral   0.3036466 1426
3       2008  Win Imminent   0.2826718 1033
4       2009 Loss Imminent   0.3461951  933
5       2009       Neutral   0.3098220 1517
6       2009  Win Imminent   0.2559340  969
7       2010 Loss Imminent   0.3471933  962
8       2010       Neutral   0.3147766 1455
9       2010  Win Imminent   0.2689434  937
10      2011 Loss Imminent   0.3407881  939
11      2011       Neutral   0.3148911 1699
12      2011  Win Imminent   0.2884972 1078
13      2012 Loss Imminent   0.3374643 1049
14      2012       Neutral   0.3095910 1418
15      2012  Win Imminent   0.2605703 1017
16      2013 Loss Imminent   0.3485663 1116
17      2013       Neutral   0.2989431 1987
18      2013  Win Imminent   0.2823045 1215
19      2014 Loss Imminent   0.3486726 1130
20      2014       Neutral   0.3244397 1874
21      2014  Win Imminent   0.2859425 1252
22      2015 Loss Imminent   0.3407173  948
23      2015       Neutral   0.3086817 1555
24      2015  Win Imminent   0.2573754  983
25      2016 Loss Imminent   0.3193658  883
26      2016       Neutral   0.3011127 1438
27      2016  Win Imminent   0.2670157  955
> set.seed(0)
> bottom.pitches.fit <- bottom.pitches
> bottom.pitches %>% 
+   filter(!is.na(sz_bot)) %>% 
+   group_by(stand) %>% 
+   summarise(ave_bot = mean(sz_bot), 
+             ave_top = mean(sz_top), n = n())
# A tibble: 2 x 4
  stand  ave_bot  ave_top     n
  <chr>    <dbl>    <dbl> <int>
1     L 1.603585 3.395832 13893
2     R 1.582352 3.412287 17046
> bottom.pitches.fit <- bottom.pitches.fit %>% 
+   mutate(hdiff = ifelse(stand == "L", .5*(sz_bot - 1.603585) + .5*(sz_top - 3.395832), 
+                         .5*(sz_bot - 1.582352) + .5*(sz_top - 3.412287)), pz = pz - hdiff)
> m1 <- bam(strike ~ s(px, pz, by = factor(stand), k = 50) + 
+             s(px, pz, by = factor(game.state), k = 50) + 
+           factor(game.state) + factor(stand) + 
+           factor(balls)*factor(strikes),
+           data = bottom.pitches.fit, method = "fREML", 
+           discrete = TRUE, family = binomial(link='logit'))
> summary(m1)

Family: binomial 
Link function: logit 

Formula:
strike ~ s(px, pz, by = factor(stand), k = 50) + s(px, pz, by = factor(game.state), 
    k = 50) + factor(game.state) + factor(stand) + factor(balls) * 
    factor(strikes)

Parametric coefficients:
                                Estimate Std. Error z value Pr(>|z|)    
(Intercept)                     -5.78799    0.81081  -7.138 9.44e-13 ***
factor(game.state)Neutral       -0.49515    0.27520  -1.799 0.071986 .  
factor(game.state)Win Imminent  -0.48030    0.22618  -2.124 0.033710 *  
factor(stand)R                  -2.50429    1.34742  -1.859 0.063087 .  
factor(balls)1                   0.23678    0.06847   3.458 0.000544 ***
factor(balls)2                   0.09268    0.10060   0.921 0.356867    
factor(balls)3                   0.44079    0.13858   3.181 0.001469 ** 
factor(strikes)1                -0.63266    0.07759  -8.154 3.52e-16 ***
factor(strikes)2                -1.02281    0.15624  -6.546 5.89e-11 ***
factor(balls)1:factor(strikes)1  0.21112    0.12183   1.733 0.083114 .  
factor(balls)2:factor(strikes)1  0.42727    0.16158   2.644 0.008183 ** 
factor(balls)3:factor(strikes)1  0.31722    0.20321   1.561 0.118509    
factor(balls)1:factor(strikes)2  0.07014    0.20345   0.345 0.730286    
factor(balls)2:factor(strikes)2  0.44710    0.21691   2.061 0.039281 *  
factor(balls)3:factor(strikes)2  0.26458    0.25738   1.028 0.303966    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Approximate significance of smooth terms:
                                            edf Ref.df   Chi.sq p-value    
s(px,pz):factor(stand)L                  24.147 28.494 1881.358  <2e-16 ***
s(px,pz):factor(stand)R                  19.866 22.981 2149.796  <2e-16 ***
s(px,pz):factor(game.state)Loss Imminent  5.691  7.646    4.461   0.778    
s(px,pz):factor(game.state)Neutral        5.591  7.499    4.360   0.790    
s(px,pz):factor(game.state)Win Imminent   2.004  2.006    0.073   0.964    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Rank: 258/260
R-sq.(adj) =   0.68   Deviance explained = 64.9%
fREML =  35238  Scale est. = 1         n = 30939
> m2 <- bam(strike ~ s(px, pz, by = factor(stand), k = 50)+ 
+             factor(balls)*factor(strikes), 
+           data = bottom.pitches.fit, method = "fREML", 
+           discrete = TRUE, family = binomial(link='logit'))
> summary(m2)

Family: binomial 
Link function: logit 

Formula:
strike ~ s(px, pz, by = factor(stand), k = 50) + factor(balls) * 
    factor(strikes)

Parametric coefficients:
                                Estimate Std. Error z value Pr(>|z|)    
(Intercept)                     -7.28042    0.73078  -9.962  < 2e-16 ***
factor(balls)1                   0.23054    0.06815   3.383 0.000718 ***
factor(balls)2                   0.08005    0.10010   0.800 0.423860    
factor(balls)3                   0.40462    0.13746   2.944 0.003244 ** 
factor(strikes)1                -0.61625    0.07735  -7.968 1.62e-15 ***
factor(strikes)2                -1.00835    0.15628  -6.452 1.10e-10 ***
factor(balls)1:factor(strikes)1  0.19748    0.12147   1.626 0.103989    
factor(balls)2:factor(strikes)1  0.43165    0.16076   2.685 0.007251 ** 
factor(balls)3:factor(strikes)1  0.31906    0.20180   1.581 0.113870    
factor(balls)1:factor(strikes)2  0.08774    0.20333   0.432 0.666082    
factor(balls)2:factor(strikes)2  0.44621    0.21685   2.058 0.039623 *  
factor(balls)3:factor(strikes)2  0.29586    0.25633   1.154 0.248417    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Approximate significance of smooth terms:
                          edf Ref.df Chi.sq p-value    
s(px,pz):factor(stand)L 24.87  28.80   2741  <2e-16 ***
s(px,pz):factor(stand)R 21.54  24.83   3277  <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

R-sq.(adj) =  0.678   Deviance explained = 64.6%
fREML =  35281  Scale est. = 1         n = 30939
> m3 <- bam(strike ~ s(px, pz, by = interaction(factor(stand), factor(game.state)), k = 50) + 
+             factor(game.state) + factor(stand)+ 
+             factor(balls)*factor(strikes),
+           data = bottom.pitches.fit, method = "fREML", 
+           discrete = TRUE, family = binomial(link='logit'))
> summary(m3)

Family: binomial 
Link function: logit 

Formula:
strike ~ s(px, pz, by = interaction(factor(stand), factor(game.state)), 
    k = 50) + factor(game.state) + factor(stand) + factor(balls) * 
    factor(strikes)

Parametric coefficients:
                                Estimate Std. Error z value Pr(>|z|)    
(Intercept)                     -6.30375    0.91925  -6.857 7.01e-12 ***
factor(game.state)Neutral       -1.58509    1.15905  -1.368 0.171446    
factor(game.state)Win Imminent  -1.11199    1.22711  -0.906 0.364838    
factor(stand)R                  -0.86133    0.98673  -0.873 0.382711    
factor(balls)1                   0.23900    0.06848   3.490 0.000482 ***
factor(balls)2                   0.10398    0.10085   1.031 0.302554    
factor(balls)3                   0.44147    0.13789   3.202 0.001366 ** 
factor(strikes)1                -0.63218    0.07771  -8.135 4.11e-16 ***
factor(strikes)2                -1.03276    0.15655  -6.597 4.20e-11 ***
factor(balls)1:factor(strikes)1  0.20583    0.12193   1.688 0.091405 .  
factor(balls)2:factor(strikes)1  0.41270    0.16174   2.552 0.010723 *  
factor(balls)3:factor(strikes)1  0.33189    0.20285   1.636 0.101821    
factor(balls)1:factor(strikes)2  0.07403    0.20379   0.363 0.716400    
factor(balls)2:factor(strikes)2  0.43839    0.21754   2.015 0.043879 *  
factor(balls)3:factor(strikes)2  0.28489    0.25719   1.108 0.267975    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Approximate significance of smooth terms:
                                                                         edf Ref.df Chi.sq p-value    
s(px,pz):interaction(factor(stand), factor(game.state))L.Loss Imminent 18.85  22.48  807.9  <2e-16 ***
s(px,pz):interaction(factor(stand), factor(game.state))R.Loss Imminent 18.20  21.50  870.9  <2e-16 ***
s(px,pz):interaction(factor(stand), factor(game.state))L.Neutral       19.19  22.56 1191.2  <2e-16 ***
s(px,pz):interaction(factor(stand), factor(game.state))R.Neutral       18.73  21.80 1424.9  <2e-16 ***
s(px,pz):interaction(factor(stand), factor(game.state))L.Win Imminent  20.78  24.94  763.9  <2e-16 ***
s(px,pz):interaction(factor(stand), factor(game.state))R.Win Imminent  17.80  20.96  999.4  <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

R-sq.(adj) =  0.681   Deviance explained = 65.1%
fREML =  35278  Scale est. = 1         n = 30939
> m4 <- bam(strike ~ s(px, pz, by = factor(stand), k = 50) + 
+             factor(game.state) + factor(stand)+ 
+             factor(balls)*factor(strikes),
+           data = bottom.pitches.fit, method = "fREML", 
+           discrete = TRUE, family = binomial(link='logit'))
> summary(m4)

Family: binomial 
Link function: logit 

Formula:
strike ~ s(px, pz, by = factor(stand), k = 50) + factor(game.state) + 
    factor(stand) + factor(balls) * factor(strikes)

Parametric coefficients:
                                Estimate Std. Error z value Pr(>|z|)    
(Intercept)                     -5.84410    0.79749  -7.328 2.33e-13 ***
factor(game.state)Neutral       -0.30419    0.05337  -5.700 1.20e-08 ***
factor(game.state)Win Imminent  -0.51854    0.05919  -8.761  < 2e-16 ***
factor(stand)R                  -2.51650    1.35445  -1.858 0.063176 .  
factor(balls)1                   0.23545    0.06839   3.443 0.000576 ***
factor(balls)2                   0.09264    0.10055   0.921 0.356843    
factor(balls)3                   0.43608    0.13835   3.152 0.001622 ** 
factor(strikes)1                -0.63100    0.07753  -8.139 4.00e-16 ***
factor(strikes)2                -1.01739    0.15625  -6.511 7.45e-11 ***
factor(balls)1:factor(strikes)1  0.21222    0.12168   1.744 0.081140 .  
factor(balls)2:factor(strikes)1  0.42798    0.16149   2.650 0.008043 ** 
factor(balls)3:factor(strikes)1  0.32504    0.20280   1.603 0.108986    
factor(balls)1:factor(strikes)2  0.07297    0.20346   0.359 0.719877    
factor(balls)2:factor(strikes)2  0.43902    0.21688   2.024 0.042944 *  
factor(balls)3:factor(strikes)2  0.24837    0.25719   0.966 0.334186    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Approximate significance of smooth terms:
                          edf Ref.df Chi.sq p-value    
s(px,pz):factor(stand)L 25.29  29.65   2721  <2e-16 ***
s(px,pz):factor(stand)R 20.96  24.08   3172  <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

R-sq.(adj) =   0.68   Deviance explained = 64.8%
fREML =  35244  Scale est. = 1         n = 30939
> AIC(m1)  ## final model
[1] 13546.14
> AIC(m2)  ## naive model
[1] 13623.19
> AIC(m3)  ## alternative model
[1] 13573.68
> AIC(m4)  ## alternative model
[1] 13548.38
> anova(m1, m2, test="LRT")  ## Reject naive model in factor of a term for score state
Analysis of Deviance Table

Model 1: strike ~ s(px, pz, by = factor(stand), k = 50) + s(px, pz, by = factor(game.state), 
    k = 50) + factor(game.state) + factor(stand) + factor(balls) * 
    factor(strikes)
Model 2: strike ~ s(px, pz, by = factor(stand), k = 50) + factor(balls) * 
    factor(strikes)
  Resid. Df Resid. Dev      Df Deviance  Pr(>Chi)    
1     30853      13397                               
2     30873      13506 -20.043  -108.91 3.225e-14 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
