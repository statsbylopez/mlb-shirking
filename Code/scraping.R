########################################################################################
#### Code for MLB analyis: do umpires call balls and strikes as to end the game early? 
#### This .R code contains code for scraping balls/strikes calls and initial data cleaning
########################################################################################

### Required packages
library(baseballr)
library(mgcv)
library(parallel)
library(readr)
library(stringr)  
library(ggplot2)
library(lubridate)
library(parallel)
library(splancs)
library(PBSmapping)



################################################################################
#### Download data via baseballwithr package: 
#### Weekly downloads, 2008 to 2016
#### Weekly downloads required given scraping constraints of baseball savant website
################################################################################


start.dates <- c(seq(ymd('2008-04-01'),ymd('2008-09-30'), by = 'weeks'), 
                 seq(ymd('2009-04-01'),ymd('2009-09-30'), by = 'weeks'), 
                 seq(ymd('2010-04-01'),ymd('2010-09-30'), by = 'weeks'), 
                 seq(ymd('2011-04-01'),ymd('2011-09-30'), by = 'weeks'), 
                 seq(ymd('2012-04-01'),ymd('2012-09-30'), by = 'weeks'), 
                 seq(ymd('2013-04-01'),ymd('2013-09-30'), by = 'weeks'), 
                 seq(ymd('2014-04-01'),ymd('2014-09-30'), by = 'weeks'), 
                 seq(ymd('2015-04-01'),ymd('2015-09-30'), by = 'weeks'), 
                 seq(ymd('2016-04-01'),ymd('2016-09-30'), by = 'weeks'))

end.dates  <- c(seq(ymd('2008-04-07'),ymd('2008-10-06'), by = 'weeks'), 
                seq(ymd('2009-04-07'),ymd('2009-10-06'), by = 'weeks'), 
                seq(ymd('2010-04-07'),ymd('2010-10-06'), by = 'weeks'), 
                seq(ymd('2011-04-07'),ymd('2011-10-06'), by = 'weeks'), 
                seq(ymd('2012-04-07'),ymd('2012-10-06'), by = 'weeks'),
                seq(ymd('2013-04-07'),ymd('2013-10-06'), by = 'weeks'), 
                seq(ymd('2014-04-07'),ymd('2014-10-06'), by = 'weeks'), 
                seq(ymd('2015-04-07'),ymd('2015-10-06'), by = 'weeks'), 
                seq(ymd('2016-04-07'),ymd('2016-10-06'), by = 'weeks'))



df.dates <- data.frame(start.dates, end.dates)
df.pitches <- NULL
rows <- c(1:nrow(df.dates)) 
var.list <- c("game_date", "game_year", "game_pk", "pitcher", "batter", "player_name", "home_team", "away_team", 
              "description", "pitch_type", "zone", "plate_x", "plate_z",  "inning", "inning_topbot", "pitch_number", 
              "des", "stand",  "at_bat_number", "pitch_number", "runner.on", "sz_bot", "sz_top", 
              "on_1b", "on_2b", "on_3b", "balls", "strikes")


for (i in 1:nrow(df.dates)){
  temp.df <- scrape_statcast_savant_batter_all(as.character(df.dates[i,1]), as.character(df.dates[i, 2]))
  temp.df <- filter(temp.df, inning >= 10)  ## pitches in 10th inning onwards
  temp.df <- temp.df[, colnames(temp.df) %in% var.list] ## only important variables are kept
  df.pitches <- rbind(df.pitches, temp.df)
  print(paste(i, "th row of ", nrow(df.dates)))
  Sys.sleep(5)    ## pauses scrape to avoid errors in scraper
}


## Download raw data from from tenth inning onwards
write.csv(df.pitches, "~/tenth_pitches.csv", row.names = FALSE)



################################################################################
#### Initial data wrangling: extra innings only
################################################################################

df.pitches <- read_csv("~/tenth_pitches.csv")

df.pitches <- df.pitches %>%
  mutate(runner.on = !is.na(on_1b) | !is.na(on_2b) | !is.na(on_3b)) %>% 
  rename(px = plate_x, pz = plate_z) %>% 
  mutate(gid = paste(home_team, away_team, game_date), 
         px = as.numeric(as.character(px)), 
         pz = as.numeric(as.character(pz)), 
         zone = as.numeric(as.character(zone)))

### Number of runs scored on each pitch 
df.pitches$runs <- ifelse(is.na(df.pitches$des), 0, str_count(df.pitches$des, "scores") + 
                           str_count(df.pitches$des, "homers") + str_count(df.pitches$des, "grand slam"))

### Number of runs scored in top half of inning
top.score <- df.pitches %>% 
  group_by(gid, inning, inning_topbot) %>% 
  summarise(runs.scored.away.all = sum(runs)) %>% 
  filter(inning_topbot == "Top") %>% 
  select(-inning_topbot)

## Rejoin top.score with original data
df.pitches <- df.pitches %>% 
  ungroup() %>% 
  left_join(top.score) %>%
  arrange(at_bat_number, pitch_number)

## Identify pitches in bottom half of inning, define game state
df.pitches.bottom <- df.pitches %>%
  filter(inning_topbot == "Bot") %>% 
  group_by(gid, inning) %>%
  mutate(runs.scored.home.cum = cumsum(runs), 
         game.state = ifelse(runs.scored.home.cum < runs.scored.away.all, "Loss Imminent", 
                             ifelse(runner.on, "Win Imminent", "Neutral")))

## Filter for taken pitches
types <- c("ball", "blocked_ball", "called_strike")
df.pitches.bottom <- df.pitches.bottom  %>% 
  filter(description %in% types) %>% 
  mutate(true.type = ifelse(zone < 10, "strike", "ball"), 
         called.type = ifelse(description == "called_strike", "strike", "ball"), 
         strike = as.numeric(called.type == "strike"))%>% 
  filter(!is.na(true.type))

## overall number of pitches
nrow(df.pitches.bottom)  

## number of true strikes and balls
table(df.pitches.bottom$true.type)

write.csv(df.pitches.bottom, "~/bottom_tenth_pitches.csv", row.names = FALSE)

