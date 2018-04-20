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
[1] 1.155590 1.092070 1.222805
> o2
[1] 0.8355873 0.7896572 0.8841890
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
[1] 1.304174 1.170565 1.453032
> ot2
[1] 0.8732140 0.7837561 0.9728825
> ot3
[1] 1.135390 1.008157 1.278680
> ot4
[1] 0.7579362 0.6767256 0.8488924
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
1     L 1.603626 3.398830 14400
2     R 1.581850 3.416771 17891
> bottom.pitches.fit <- bottom.pitches.fit %>% 
+   mutate(hdiff = ifelse(stand == "L", .5*(sz_bot - 1.603585) + .5*(sz_top - 3.395832), 
+                         .5*(sz_bot - 1.582352) + .5*(sz_top - 3.412287)), pz = pz - hdiff)
> m1 <- bam(strike ~ s(px, pz, by = factor(stand), k = 50) + 
+             s(px, pz, by = factor(game.state), k = 50) + 
+           factor(game.state) + factor(stand) + 
+           factor(balls)*factor(strikes),
+           data = bottom.pitches.fit, method = "fREML", 
+           discrete = TRUE, family = binomial(link='logit'))
Warning message:
In bgam.fitd(G, mf, gp, scale, nobs.extra = 0, rho = rho, coef = coef,  :
  fitted probabilities numerically 0 or 1 occurred
> summary(m1)

Family: binomial 
Link function: logit 

Formula:
strike ~ s(px, pz, by = factor(stand), k = 50) + s(px, pz, by = factor(game.state), 
    k = 50) + factor(game.state) + factor(stand) + factor(balls) * 
    factor(strikes)

Parametric coefficients:
                                Estimate Std. Error z value Pr(>|z|)    
(Intercept)                     -5.77157    0.81331  -7.096 1.28e-12 ***
factor(game.state)Neutral       -0.50134    0.26325  -1.904  0.05686 .  
factor(game.state)Win Imminent  -0.51627    0.22534  -2.291  0.02196 *  
factor(stand)R                  -2.92321    1.38174  -2.116  0.03438 *  
factor(balls)1                   0.20193    0.06667   3.029  0.00246 ** 
factor(balls)2                   0.07541    0.09812   0.769  0.44216    
factor(balls)3                   0.43944    0.13594   3.233  0.00123 ** 
factor(strikes)1                -0.64735    0.07583  -8.537  < 2e-16 ***
factor(strikes)2                -0.99496    0.15173  -6.557 5.48e-11 ***
factor(balls)1:factor(strikes)1  0.24836    0.11883   2.090  0.03662 *  
factor(balls)2:factor(strikes)1  0.46446    0.15773   2.945  0.00323 ** 
factor(balls)3:factor(strikes)1  0.25034    0.19984   1.253  0.21032    
factor(balls)1:factor(strikes)2  0.06189    0.19818   0.312  0.75483    
factor(balls)2:factor(strikes)2  0.46033    0.21076   2.184  0.02895 *  
factor(balls)3:factor(strikes)2  0.21553    0.25156   0.857  0.39156    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Approximate significance of smooth terms:
                                            edf Ref.df   Chi.sq p-value    
s(px,pz):factor(stand)L                  25.221 29.625 2013.365  <2e-16 ***
s(px,pz):factor(stand)R                  19.354 22.493 2304.017  <2e-16 ***
s(px,pz):factor(game.state)Loss Imminent  5.910  7.940    4.539   0.798    
s(px,pz):factor(game.state)Neutral        5.123  6.891    3.405   0.837    
s(px,pz):factor(game.state)Win Imminent   2.001  2.002    0.117   0.943    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Rank: 258/260
R-sq.(adj) =  0.679   Deviance explained = 64.7%
fREML =  36809  Scale est. = 1         n = 32291
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
(Intercept)                     -7.41324    0.73433 -10.095  < 2e-16 ***
factor(balls)1                   0.19758    0.06635   2.978  0.00290 ** 
factor(balls)2                   0.06287    0.09756   0.644  0.51929    
factor(balls)3                   0.39894    0.13485   2.958  0.00309 ** 
factor(strikes)1                -0.63055    0.07559  -8.342  < 2e-16 ***
factor(strikes)2                -0.98303    0.15166  -6.482 9.06e-11 ***
factor(balls)1:factor(strikes)1  0.23430    0.11847   1.978  0.04796 *  
factor(balls)2:factor(strikes)1  0.46886    0.15688   2.989  0.00280 ** 
factor(balls)3:factor(strikes)1  0.25947    0.19846   1.307  0.19107    
factor(balls)1:factor(strikes)2  0.07483    0.19804   0.378  0.70555    
factor(balls)2:factor(strikes)2  0.46637    0.21059   2.215  0.02679 *  
factor(balls)3:factor(strikes)2  0.25440    0.25041   1.016  0.30965    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Approximate significance of smooth terms:
                          edf Ref.df Chi.sq p-value    
s(px,pz):factor(stand)L 24.93  28.93   2856  <2e-16 ***
s(px,pz):factor(stand)R 21.96  25.26   3441  <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

R-sq.(adj) =  0.676   Deviance explained = 64.4%
fREML =  36856  Scale est. = 1         n = 32291
> m3 <- bam(strike ~ s(px, pz, by = interaction(factor(stand), factor(game.state)), k = 50) + 
+             factor(game.state) + factor(stand)+ 
+             factor(balls)*factor(strikes),
+           data = bottom.pitches.fit, method = "fREML", 
+           discrete = TRUE, family = binomial(link='logit'))
Warning message:
In bgam.fitd(G, mf, gp, scale, nobs.extra = 0, rho = rho, coef = coef,  :
  fitted probabilities numerically 0 or 1 occurred
> summary(m3)

Family: binomial 
Link function: logit 

Formula:
strike ~ s(px, pz, by = interaction(factor(stand), factor(game.state)), 
    k = 50) + factor(game.state) + factor(stand) + factor(balls) * 
    factor(strikes)

Parametric coefficients:
                                Estimate Std. Error z value Pr(>|z|)    
(Intercept)                     -6.31120    0.91269  -6.915 4.68e-12 ***
factor(game.state)Neutral       -1.53095    1.15644  -1.324  0.18555    
factor(game.state)Win Imminent  -0.84294    1.20003  -0.702  0.48241    
factor(stand)R                  -1.10989    0.97649  -1.137  0.25570    
factor(balls)1                   0.20428    0.06669   3.063  0.00219 ** 
factor(balls)2                   0.08577    0.09828   0.873  0.38281    
factor(balls)3                   0.44419    0.13544   3.280  0.00104 ** 
factor(strikes)1                -0.64715    0.07591  -8.526  < 2e-16 ***
factor(strikes)2                -1.00295    0.15192  -6.602 4.06e-11 ***
factor(balls)1:factor(strikes)1  0.24201    0.11889   2.036  0.04179 *  
factor(balls)2:factor(strikes)1  0.45050    0.15780   2.855  0.00431 ** 
factor(balls)3:factor(strikes)1  0.25550    0.19952   1.281  0.20036    
factor(balls)1:factor(strikes)2  0.06242    0.19833   0.315  0.75297    
factor(balls)2:factor(strikes)2  0.45224    0.21110   2.142  0.03217 *  
factor(balls)3:factor(strikes)2  0.22688    0.25127   0.903  0.36656    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Approximate significance of smooth terms:
                                                                         edf Ref.df Chi.sq p-value    
s(px,pz):interaction(factor(stand), factor(game.state))L.Loss Imminent 19.10  22.77  849.7  <2e-16 ***
s(px,pz):interaction(factor(stand), factor(game.state))R.Loss Imminent 18.44  21.75  935.3  <2e-16 ***
s(px,pz):interaction(factor(stand), factor(game.state))L.Neutral       19.29  22.68 1231.0  <2e-16 ***
s(px,pz):interaction(factor(stand), factor(game.state))R.Neutral       19.01  22.09 1477.7  <2e-16 ***
s(px,pz):interaction(factor(stand), factor(game.state))L.Win Imminent  20.82  25.04  797.8  <2e-16 ***
s(px,pz):interaction(factor(stand), factor(game.state))R.Win Imminent  18.09  21.30 1052.2  <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

R-sq.(adj) =   0.68   Deviance explained = 64.9%
fREML =  36856  Scale est. = 1         n = 32291
> m4 <- bam(strike ~ s(px, pz, by = factor(stand), k = 50) + 
+             factor(game.state) + factor(stand)+ 
+             factor(balls)*factor(strikes),
+           data = bottom.pitches.fit, method = "fREML", 
+           discrete = TRUE, family = binomial(link='logit'))
Warning message:
In bgam.fitd(G, mf, gp, scale, nobs.extra = 0, rho = rho, coef = coef,  :
  fitted probabilities numerically 0 or 1 occurred
> summary(m4)

Family: binomial 
Link function: logit 

Formula:
strike ~ s(px, pz, by = factor(stand), k = 50) + factor(game.state) + 
    factor(stand) + factor(balls) * factor(strikes)

Parametric coefficients:
                                Estimate Std. Error z value Pr(>|z|)    
(Intercept)                     -5.83880    0.79972  -7.301 2.86e-13 ***
factor(game.state)Neutral       -0.30223    0.05196  -5.817 6.01e-09 ***
factor(game.state)Win Imminent  -0.52130    0.05757  -9.054  < 2e-16 ***
factor(stand)R                  -2.93740    1.38714  -2.118  0.03421 *  
factor(balls)1                   0.20142    0.06658   3.025  0.00249 ** 
factor(balls)2                   0.07410    0.09805   0.756  0.44978    
factor(balls)3                   0.43122    0.13577   3.176  0.00149 ** 
factor(strikes)1                -0.64538    0.07578  -8.516  < 2e-16 ***
factor(strikes)2                -0.98965    0.15165  -6.526 6.76e-11 ***
factor(balls)1:factor(strikes)1  0.24872    0.11871   2.095  0.03616 *  
factor(balls)2:factor(strikes)1  0.46720    0.15762   2.964  0.00304 ** 
factor(balls)3:factor(strikes)1  0.26346    0.19946   1.321  0.18655    
factor(balls)1:factor(strikes)2  0.06314    0.19815   0.319  0.74998    
factor(balls)2:factor(strikes)2  0.45721    0.21067   2.170  0.02999 *  
factor(balls)3:factor(strikes)2  0.20273    0.25135   0.807  0.41991    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Approximate significance of smooth terms:
                          edf Ref.df Chi.sq p-value    
s(px,pz):factor(stand)L 25.35  29.77   2844  <2e-16 ***
s(px,pz):factor(stand)R 21.43  24.57   3319  <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

R-sq.(adj) =  0.678   Deviance explained = 64.7%
fREML =  36816  Scale est. = 1         n = 32291
> AIC(m1)  ## final model
[1] 14201.67
> AIC(m2)  ## naive model
[1] 14286.1
> AIC(m3)  ## alternative model
[1] 14242.98
> AIC(m4)  ## alternative model
[1] 14204.64
> anova(m1, m2, test="LRT")  ## Reject naive model in factor of a term for score state
Analysis of Deviance Table

Model 1: strike ~ s(px, pz, by = factor(stand), k = 50) + s(px, pz, by = factor(game.state), 
    k = 50) + factor(game.state) + factor(stand) + factor(balls) * 
    factor(strikes)
Model 2: strike ~ s(px, pz, by = factor(stand), k = 50) + factor(balls) * 
    factor(strikes)
  Resid. Df Resid. Dev      Df Deviance  Pr(>Chi)    
1     32204      14051                               
2     32224      14168 -19.966  -116.27 1.351e-15 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
