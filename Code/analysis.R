########################################################################################
#### Code for MLB analyis: do umpires call balls and strikes as to end the game early? 
#### This .R code contains code for analysis
########################################################################################

### Required packages & function for odds ratio
library(mgcv)
library(parallel)
library(stringr)
library(tidyverse)

odds.ratio <- function(p1, p2, n1, n2, n3, n4){
  or <- (p1/(1-p1))/(p2/(1-p2))
  se <- sqrt(1/n1 + 1/n2 + 1/n3 + 1/n4)
  low <- exp(log(or) - 1.96*se)
  upp <- exp(log(or) + 1.96*se)
  return(c(or, low, upp))}

### Read in data
bottom.pitches <- read_csv("~/bottom_tenth_pitches.csv")


################################################################################
#### Aggregate analysis -- without consideration of pitch location
################################################################################

## Overall averages and odds ratios
overall <- bottom.pitches %>% 
  group_by(game.state) %>% 
  summarise(strike.rate = mean(called.type == "strike"), 
            strikes = sum(called.type == "strike"), 
            balls = sum(called.type == "ball"),  n())

o1 <- odds.ratio(overall$strike.rate[1], 
                 overall$strike.rate[2], 
                 overall$strikes[1], 
                 overall$strikes[2], 
                 overall$balls[1], 
                 overall$balls[2])
o2 <- odds.ratio(overall$strike.rate[3], 
                 overall$strike.rate[2], 
                 overall$strikes[1], 
                 overall$strikes[2], 
                 overall$balls[1], 
                 overall$balls[2])
o1
o2


## Overall given true state
overall.true <- bottom.pitches %>% 
  group_by(true.type, game.state) %>% 
  summarise(strike.rate = mean(called.type == "strike"),  
            strikes = sum(called.type == "strike"), 
            balls = sum(called.type == "ball"),  n())

ot1 <- odds.ratio(overall.true$strike.rate[1], 
                  overall.true$strike.rate[2], 
                  overall.true$strikes[1], 
                  overall.true$strikes[2], 
                  overall.true$balls[1], 
                  overall.true$balls[2])

ot2 <- odds.ratio(overall.true$strike.rate[3], 
                  overall.true$strike.rate[2], 
                  overall.true$strikes[1], 
                  overall.true$strikes[2], 
                  overall.true$balls[1], 
                  overall.true$balls[2])

ot3 <- odds.ratio(overall.true$strike.rate[4], 
                  overall.true$strike.rate[5], 
                  overall.true$strikes[4], 
                  overall.true$strikes[5], 
                  overall.true$balls[4], 
                  overall.true$balls[5])

ot4 <- odds.ratio(overall.true$strike.rate[6], 
                  overall.true$strike.rate[5], 
                  overall.true$strikes[6], 
                  overall.true$strikes[5], 
                  overall.true$balls[6], 
                  overall.true$balls[5])
ot1
ot2
ot3
ot4



### Overall given inning
table(bottom.pitches$inning)
bottom.pitches %>% 
  group_by(inning, game.state) %>% 
  summarise(strike.rate = mean(called.type == "strike"), n()) %>% 
  print.data.frame()

### Overall given season
table(bottom.pitches$game_year)
bottom.pitches %>% 
  group_by(game_year, game.state) %>% 
  summarise(strike.rate = mean(called.type == "strike"), n()) %>% print.data.frame()




################################################################################
#### Generalized additive models: 10th inning onwards
################################################################################

## Data set for GAM's
set.seed(0)
bottom.pitches.fit <- bottom.pitches 


## Adjust for batter height using lefty/righty average strike zone heights (results similar without this)

bottom.pitches %>% 
  filter(!is.na(sz_bot)) %>% 
  group_by(stand) %>% 
  summarise(ave_bot = mean(sz_bot), 
            ave_top = mean(sz_top), n = n())

bottom.pitches.fit <- bottom.pitches.fit %>% 
  mutate(hdiff = ifelse(stand == "L", .5*(sz_bot - 1.603585) + .5*(sz_top - 3.395832), 
                        .5*(sz_bot - 1.582352) + .5*(sz_top - 3.412287)), pz = pz - hdiff)


####### ####### ####### ####### ####### ####### ####### ####### ####### 
####### Final model - score state effect by strike zone location
####### ####### ####### ####### ####### ####### ####### ####### ####### 

m1 <- bam(strike ~ s(px, pz, by = factor(stand), k = 50) + 
            s(px, pz, by = factor(game.state), k = 50) + 
          factor(game.state) + factor(stand) + 
          factor(balls)*factor(strikes),
          data = bottom.pitches.fit, method = "fREML", 
          discrete = TRUE, family = binomial(link='logit'))
summary(m1)


## Alternative model: no effect of score state (naive model)
m2 <- bam(strike ~ s(px, pz, by = factor(stand), k = 50)+ 
            factor(balls)*factor(strikes), 
          data = bottom.pitches.fit, method = "fREML", 
          discrete = TRUE, family = binomial(link='logit'))
summary(m2)


## Alternative model: effect of score state on strike zone differs by handedness
m3 <- bam(strike ~ s(px, pz, by = interaction(factor(stand), factor(game.state)), k = 50) + 
            factor(game.state) + factor(stand)+ 
            factor(balls)*factor(strikes),
          data = bottom.pitches.fit, method = "fREML", 
          discrete = TRUE, family = binomial(link='logit'))
summary(m3)

## Alternative model: effect of score state that is constant across strike zone
m4 <- bam(strike ~ s(px, pz, by = factor(stand), k = 50) + 
            factor(game.state) + factor(stand)+ 
            factor(balls)*factor(strikes),
          data = bottom.pitches.fit, method = "fREML", 
          discrete = TRUE, family = binomial(link='logit'))
summary(m4)





AIC(m1)  ## final model
AIC(m2)  ## naive model
AIC(m3)  ## alternative model
AIC(m4)  ## alternative model

anova(m1, m2, test="LRT")  ## Reject naive model in factor of a term for score state



################################################################################
#### Visualizing changes in strike zone
################################################################################

seq <- 0.05 ## Change to 0.01 for exact numbers as in the manuscript
pre <- expand.grid(px = seq(-2, 2, seq), 
                   pz = seq(0.5, 4.5, seq), 
                   strikes = 0, 
                   balls = 0, 
                   stand = c("R", "L"), 
                   game.state = c("Loss Imminent", "Win Imminent", "Neutral"))
pre$predict <- predict.gam(m1, pre, type = "response")

pre.10 <- pre %>% 
  spread(game.state, predict) %>% 
  rename(loss.prob = `Loss Imminent`, 
         win.prob = `Win Imminent`, 
         tied.prob = `Neutral`) %>% 
  mutate(diff1 = loss.prob - tied.prob, 
         diff2 = win.prob - tied.prob)

pre.all.both <- pre.10 %>% 
  gather("type", "diff", diff1:diff2) %>%
  mutate(type = ifelse(type == "diff1", 
                       "Loss Imminent vs. Neutral", 
                       "Win Imminent vs. Neutral"))

min(pre.all.both$diff)
max(pre.all.both$diff)

d1 <- pre.all.both %>% filter(type == "Loss Imminent vs. Neutral") %>% select(diff) 
d2 <- pre.all.both %>% filter(type == "Win Imminent vs. Neutral") %>% select(diff) 

max(d1 - d2)



p <- ggplot(filter(pre.all.both, stand == "R"), aes(x=px, y=pz, z = diff)) + 
  geom_tile(aes(fill = diff)) + 
  scale_fill_gradient2("Change in strike rate", 
                       low = "#af8dc3", mid = "white", high = "#7fbf7b", 
                       labels = c("-12%", "-6%", "0%", "+6%", "+12%"), 
                       breaks = c(-0.12, -0.06, 0, 0.06, 0.12), 
                       lim = c(-0.13, 0.13)) + 
  xlab("Horizontal pitch location") + 
  ylab("Vertical pitch location") + 
  facet_wrap(~ type, nrow = 2) + 
  theme_bw() 
  #labs(title = "Change in strike zone (absolute percentages)", 
  #     subtitle = "2008-2016 games, extra innings") + facet_wrap(~ type + stand, nrow = 2) + theme_bw()
p


ggsave(p, file = "~/Dropbox/mlb-shirking/Figures/Fig1.pdf", height = 6, width = 5)



## Recent anecdoate
bottom.pitches %>% 
  filter(game_year > 2015, 
         true.type == "strike", 
         called.type == "ball", 
         game.state == "Win Imminent", runner.on) %>% 
  group_by(gid)  %>% 
  count() %>% arrange(-n)

ex <- bottom.pitches %>% filter(gid == "CHC STL 2016-08-11", game.state == "Win Imminent")
ggplot(ex, aes(px, pz, colour = called.type)) + geom_point()
#http://www.cbssports.com/mlb/news/watch-cubs-win-10th-straight-game-on-a-controversial-ball-four-call/

