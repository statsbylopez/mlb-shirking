################################################################################
#### Code for MLB analyis: do umpires leave early? 
################################################################################
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
rows <- c(1:nrow(df.dates)) ## note: no extra inning games in cycle 118
var.list <- c("game_date", "game_year", "game_pk", "pitcher", "batter", "player_name", "home_team", "away_team", 
              "description", "pitch_type", "zone", "plate_x", "plate_z", 
          "inning", "inning_topbot", "pitch_number", "des", "stand", "at_bat_number", "pitch_number", "runner.on", "sz_bot", "sz_top", "on_1b", "on_2b", "on_3b")


for (i in 1:nrow(df.dates)){
  temp.df <- scrape_statcast_savant_batter_all(as.character(df.dates[i,1]), as.character(df.dates[i, 2]))
  temp.df <- filter(temp.df, inning >= 10 | inning == 1)
  temp.df <- temp.df[, colnames(temp.df) %in% var.list]
  df.pitches <- rbind(df.pitches, temp.df)
  print(paste(i, "th row of ", nrow(df.dates)))
  Sys.sleep(5)    ## pauses scrape to avoid errors
}


## Download pitches from first and tenth innings
write.csv(filter(df.pitches, inning ==1 | inning > 9), "~/first_tenth_pitches.csv", row.names = FALSE)



