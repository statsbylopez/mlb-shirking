################################################################################
#### Initial data wrangling: read in all of the .csv's
################################################################################

### Read in all csv's from the week-level data

setwd("~/Dropbox/mlb-shirking/Data/Scrape/")
files <- (Sys.glob("*.csv"))

pitches.all <- NULL
for (i in 1:(length(files))){ 
  pitches.i <- read.csv(files[i], header = TRUE) 
  pitches.all <- bind_rows(pitches.all, pitches.i)
  print(i)
} 

dim(pitches.all)

extra.innings <- pitches.all %>% filter(inning >= 10) 

df.pitches <- extra.innings %>%
  mutate(runner.on = !is.na(on_1b) | !is.na(on_2b) | !is.na(on_3b)) %>% 
  rename(px = plate_x, pz = plate_z) %>% 
  mutate(gid = paste(home_team, away_team, game_date), 
         px = as.numeric(as.character(px)), 
         pz = as.numeric(as.character(pz)), 
         zone = as.numeric(as.character(zone)))


## Identify pitches in bottom half of inning, define game state
df.pitches.bottom <- df.pitches %>%
  filter(inning_topbot == "Bot") %>% 
  mutate(game.state = ifelse(home_score < away_score, "Loss Imminent", 
                             ifelse(runner.on, "Win Imminent", "Neutral")))

df.pitches.bottom %>% mutate(tied = home_score == away_score) %>% group_by(game.state, tied, runner.on) %>% count()

## Filter for taken pitches
types <- c("ball", "blocked_ball", "called_strike")
df.pitches.bottom <- df.pitches.bottom  %>% 
  filter(description %in% types) %>% 
  mutate(true.type = ifelse(zone < 10, "strike", "ball"), 
         called.type = ifelse(description == "called_strike", "strike", "ball"), 
         strike = as.numeric(called.type == "strike")) %>% 
  filter(!is.na(true.type))

df.pitches.bottom <- df.pitches.bottom %>% mutate(month = month(game_date)) %>% filter(month < 10, game_year < 2017) %>% select(-month)

## overall number of pitches 
nrow(df.pitches.bottom) 

## number of true strikes and balls
table(df.pitches.bottom$true.type)

write.csv(df.pitches.bottom, "~/Dropbox/mlb-shirking/Data/bottom_tenth_pitches.csv", row.names = FALSE)



