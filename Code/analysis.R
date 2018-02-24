### Required packages
library(mgcv)
library(parallel)
library(stringr)
library(tidyverse)

################################################################################
#### Initial data wrangling: extra innings only
################################################################################
 
df.pitches <- read_csv("~/Dropbox/mlb-shirking/Data/first_tenth_pitches.csv")

data.all <- df.pitches %>% 
  mutate(runner.on = !is.na(on_1b) | !is.na(on_2b) | !is.na(on_3b))

data.all1 <- data.all %>% 
          rename(px = plate_x, pz = plate_z) %>% 
  mutate(gid = paste(home_team, away_team, game_date), 
         px = as.numeric(as.character(px)), 
         pz = as.numeric(as.character(pz)), 
         zone = as.numeric(as.character(zone)))

### Number of runs scored on each pitch 
data.all1$runs <- ifelse(is.na(data.all1$des), 0, str_count(data.all1$des, "scores") + 
                        str_count(data.all1$des, "homers") + str_count(data.all1$des, "grand slam"))

### Number of runs scored in top half of inning
top.score <- data.all1 %>% 
  group_by(gid, inning, inning_topbot) %>% 
  summarise(runs.scored.away.all = sum(runs)) %>% 
  filter(inning_topbot == "Top") %>% 
  select(-inning_topbot)

## Rejoin top.score with original data
data.all2 <- data.all1 %>% 
  ungroup() %>% 
  left_join(top.score) %>%
  arrange(at_bat_number, pitch_number)

## Identify pitches in bottom half of inning where home team is trailing
data.all3 <- data.all2 %>%
  filter(inning_topbot == "Bot") %>% 
  group_by(gid, inning) %>%
  mutate(runs.scored.home.cum = cumsum(runs), 
         game.state = ifelse(runs.scored.home.cum < runs.scored.away.all, "Loss Imminent", 
                              ifelse(runner.on, "Win Imminent", "Neutral")))

## Filter for taken pitches
types <- c("ball", "blocked_ball", "called_strike")
bottom.pitches.all <- data.all3  %>% filter(description %in% types) %>% 
  mutate(true.type = ifelse(zone < 10, "strike", "ball"), 
         called.type = ifelse(description == "called_strike", "strike", "ball"), 
         strike = as.numeric(called.type == "strike"))%>% 
  filter(!is.na(true.type))

bottom.pitches <- bottom.pitches.all %>% filter(inning > 9)

################################################################################
#### Aggregate analysis
################################################################################

## Anecdote
bottom.pitches %>% 
  filter(game_year > 2015, true.type == "strike", called.type == "ball", game.state == "Win Imminent", runner.on) %>% 
  group_by(gid)  %>% 
  count() %>% arrange(-n)

ex <- bottom.pitches %>% filter(gid == "CHC STL 2016-08-11", game.state == "Win Imminent")
ggplot(ex, aes(px, pz, colour = called.type)) + geom_point()
#http://www.cbssports.com/mlb/news/watch-cubs-win-10th-straight-game-on-a-controversial-ball-four-call/


## Overall averages
odds.ratio <- function(p1, p2, n1, n2, n3, n4){
  or <- (p1/(1-p1))/(p2/(1-p2))
  se <- sqrt(1/n1 + 1/n2 + 1/n3 + 1/n4)
  low <- exp(log(or) - 1.96*se)
  upp <- exp(log(or) + 1.96*se)
  return(c(or, low, upp))}


overall <- bottom.pitches %>% 
  group_by(game.state) %>% 
  summarise(strike.rate = mean(called.type == "strike"), strikes = sum(called.type == "strike"), balls = sum(called.type == "ball"),  n())

o1 <- odds.ratio(overall$strike.rate[1], overall$strike.rate[2], overall$strikes[1], overall$strikes[2], overall$balls[1], overall$balls[2])
o2 <- odds.ratio(overall$strike.rate[3], overall$strike.rate[2], overall$strikes[1], overall$strikes[2], overall$balls[1], overall$balls[2])
o1
o2


## Overall given true state
overall.true <- bottom.pitches %>% 
  group_by(true.type, game.state) %>% 
  summarise(strike.rate = mean(called.type == "strike"),  strikes = sum(called.type == "strike"), balls = sum(called.type == "ball"),  n())

ot1 <- odds.ratio(overall.true$strike.rate[1], overall.true$strike.rate[2], overall.true$strikes[1], overall.true$strikes[2], overall.true$balls[1], overall.true$balls[2])
ot2 <- odds.ratio(overall.true$strike.rate[3], overall.true$strike.rate[2], overall.true$strikes[1], overall.true$strikes[2], overall.true$balls[1], overall.true$balls[2])
ot3 <- odds.ratio(overall.true$strike.rate[4], overall.true$strike.rate[5], overall.true$strikes[4], overall.true$strikes[5], overall.true$balls[4], overall.true$balls[5])
ot4 <- odds.ratio(overall.true$strike.rate[6], overall.true$strike.rate[5], overall.true$strikes[6], overall.true$strikes[5], overall.true$balls[6], overall.true$balls[5])
ot1
ot2
ot3
ot4



### Overall given inning
table(bottom.pitches$inning)
bottom.pitches %>% 
  group_by(inning, game.state) %>% 
  summarise(strike.rate = mean(called.type == "strike"), n()) %>% print.data.frame()

### Overall given season
table(bottom.pitches$game_year)
bottom.pitches %>% 
  group_by(game_year, game.state) %>% 
  summarise(strike.rate = mean(called.type == "strike"), n()) %>% print.data.frame()

################################################################################
#### GAM's: 10th inning only
################################################################################

## Data set for GAM's
set.seed(0)
bottom.pitches.fit <- bottom.pitches 


## Adjust for batter height using lefty/righty average strike zone heights (results similar without this)

bottom.pitches %>% filter(!is.na(sz_bot)) %>% 
  group_by(stand) %>% 
  summarise(ave_bot = mean(sz_bot), ave_top = mean(sz_top), n = n())

bottom.pitches.fit <- bottom.pitches.fit %>% 
  mutate(hdiff = ifelse(stand == "L", .5*(sz_bot - 1.603585) + .5*(sz_top - 3.395832), 
                        .5*(sz_bot - 1.582352) + .5*(sz_top - 3.412287)), pz = pz - hdiff)


## Full model - score state effect by strike zone location
m1 <- bam(strike ~ s(px, pz, by = factor(stand), k = 50) + 
            s(px, pz, by = factor(game.state), k = 50) + 
          factor(game.state) + factor(stand) + 
            factor(),
          data = bottom.pitches.fit, method = "fREML", 
          discrete = TRUE, family = binomial(link='logit'))
summary(m1)

## Naive model - no score state effect
m2 <- bam(strike ~ s(px, pz, by = factor(stand), k = 50), 
          data = bottom.pitches.fit, method = "fREML", 
          discrete = TRUE, family = binomial(link='logit'))
summary(m2)

AIC(m1)
AIC(m2)

anova(m1, m2, test="LRT")



################################################################################
#### Visualizing changes in strike zone
################################################################################

seq <- 0.05 ## Change to 0.01 for better figure as in the manuscript
pre <- expand.grid(px = seq(-2, 2, seq), pz = seq(0.5, 4.5, seq), stand = c("R", "L"), 
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
  mutate(type = ifelse(type == "diff1", "Loss Imminent vs. Neutral", "Win Imminent vs. Neutral"))

min(pre.all.both$diff)
max(pre.all.both$diff)

d1 <- pre.all.both %>% filter(type == "Loss Imminent vs. Neutral") %>% select(diff) 
d2 <- pre.all.both %>% filter(type == "Win Imminent vs. Neutral") %>% select(diff) 

max(d1 - d2)



p <- ggplot(pre.all.both, aes(x=px, y=pz, z = diff)) + 
  geom_tile(aes(fill = diff)) + 
  scale_fill_gradient2("Change in strike %", 
                       low = "red", mid = "white", high = "darkblue") + 
  xlab("Horizontal pitch location") + ylab("Vertical pitch location") + facet_wrap(~ type + stand, nrow = 2) + theme_bw(16)
  #labs(title = "Change in strike zone (absolute percentages)", 
  #     subtitle = "2008-2016 games, extra innings") + facet_wrap(~ type + stand, nrow = 2) + theme_bw()
p


ggsave(p, "~/Dropbox/mlb-shirking/Figures/Fig1.pdf")



########################################################################################################################
##### Compare to the first inning (sort of a null model)
########################################################################################################################

bottom.pitches1 <- bottom.pitches.all %>% filter(inning == 1)

bottom.pitches.2 <- bottom.pitches1 %>% 
  mutate(hdiff = ifelse(stand == "L", .5*(sz_bot - 1.6) + .5*(sz_top - 3.4), .5*(sz_bot - 1.58) + .5*(sz_top - 3.41)), 
         pz = pz - hdiff)


m1.first <- bam(strike ~ s(px, pz, by = factor(stand), k = 50) +  factor(game.state), 
            data = bottom.pitches.2, method = "fREML", 
            discrete = TRUE, family = binomial(link='logit'))



### Visualizing the first inning (in comparison to the tenth)

pre <- expand.grid(px = seq(-2, 2, 0.05), pz = seq(0.5, 4.5, 0.05), 
                   stand = c("R", "L"),  
                   game.state = c("Loss Imminent", "Win Imminent", "Neutral"))
pre$predict <- predict.gam(m1.first, pre, type = "response")

pre.1 <- pre %>% 
  spread(game.state, predict) %>% 
  rename(loss.prob = `Loss Imminent`, 
         win.prob = `Win Imminent`, 
         tied.prob = `Neutral`) %>% 
  mutate(diff1 = loss.prob - tied.prob, 
         diff2 = win.prob - tied.prob)


pre.all.both <- pre.1 %>% 
  gather("type", "diff", diff1:diff2) %>%
  mutate(type = ifelse(type == "diff1", "Loss Imminent vs. Neutral", "Win Imminent vs. Neutral"))


pre.1$inning <- "1st inning"
pre.10$inning <- "Extra innings"

pre.all.both <- bind_rows(pre.1, pre.10) %>% gather("type", "diff", diff1:diff2) %>%
  mutate(type = ifelse(type == "diff1", "Loss Imminent (with runners) vs. Neutral", "Win Imminent vs. Neutral"))


p <- ggplot(pre.all.both, aes(x=px, y=pz, z = diff)) + 
  geom_tile(aes(fill = diff)) + 
  scale_fill_gradient2("Increased Strike %", 
                       low = "red", mid = "white", high = "darkblue") + 
  xlab("Horizontal pitch location") + ylab("Vertical pitch location") + 
  labs(title = "Change in strike zone, absolute percentages", 
       subtitle = "2008-2016 games, 1st and 10th innings") + facet_wrap(~inning + type + stand, nrow = 2) + theme_bw()
p

