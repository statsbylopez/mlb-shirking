########################################################################################
#### Code for MLB analyis: do umpires call balls and strikes as to end the game early? 
#### This .R code contains code for scraping balls/strikes calls and initial data cleaning
########################################################################################

### Required packages
library(lubridate)
library(baseballr)
library(mgcv)
library(parallel)
library(readr)
library(stringr)  
library(ggplot2)
library(parallel)
library(splancs)
library(PBSmapping)


################################################################################
#### Download data via baseballwithr package: 
#### Weekly downloads, 2008 to 2016
#### Weekly downloads required given scraping constraints of baseball savant website
################################################################################


start.dates <- c(seq(ymd('2008-03-31'),ymd('2008-10-30'), by = 'weeks'), 
                 seq(ymd('2009-04-05'),ymd('2009-10-30'), by = 'weeks'), 
                 seq(ymd('2010-04-04'),ymd('2010-10-30'), by = 'weeks'), 
                 seq(ymd('2011-03-31'),ymd('2011-10-30'), by = 'weeks'), 
                 seq(ymd('2012-04-01'),ymd('2012-10-30'), by = 'weeks'), 
                 seq(ymd('2013-04-01'),ymd('2013-10-30'), by = 'weeks'), 
                 seq(ymd('2014-03-30'),ymd('2014-10-30'), by = 'weeks'), 
                 seq(ymd('2015-04-05'),ymd('2015-10-30'), by = 'weeks'), 
                 seq(ymd('2016-04-03'),ymd('2016-10-30'), by = 'weeks'), 
                 seq(ymd('2017-04-02'),ymd('2017-10-30'), by = 'weeks'))

end.dates  <- c(seq(ymd('2008-04-06'),ymd('2008-11-06'), by = 'weeks'), 
                seq(ymd('2009-04-11'),ymd('2009-11-06'), by = 'weeks'), 
                seq(ymd('2010-04-10'),ymd('2010-11-03'), by = 'weeks'), 
                seq(ymd('2011-04-06'),ymd('2011-11-06'), by = 'weeks'), 
                seq(ymd('2012-04-07'),ymd('2012-11-06'), by = 'weeks'),
                seq(ymd('2013-04-07'),ymd('2013-11-06'), by = 'weeks'), 
                seq(ymd('2014-04-05'),ymd('2014-11-06'), by = 'weeks'), 
                seq(ymd('2015-04-11'),ymd('2015-11-06'), by = 'weeks'), 
                seq(ymd('2016-04-09'),ymd('2016-11-06'), by = 'weeks'), 
                seq(ymd('2017-04-08'),ymd('2017-11-06'), by = 'weeks'))


start.dates[92]
end.dates[92]


df.dates <- data.frame(start.dates, end.dates)
df.pitches <- NULL
rows <- c(1:nrow(df.dates)) 
var.list <- c("game_date", "game_year", "game_pk", "pitcher", "batter", "player_name", "home_team", "away_team", 
              "description", "pitch_type", "zone", "plate_x", "plate_z",  "inning", "inning_topbot", "pitch_number", 
              "des", "stand",  "at_bat_number", "pitch_number", "runner.on", "sz_bot", "sz_top", 
              "on_1b", "on_2b", "on_3b", "balls", "strikes", "home_score", "away_score", "bat_score", "fld_score", 
              "post_away_score", "post_home_score", 
              "post_bat_score", "post_fld_score")


for (i in 1:nrow(df.dates)){
  temp.df <- scrape_statcast_savant_batter_all(as.character(df.dates[i,1]), as.character(df.dates[i, 2]))
  #temp.df <- filter(temp.df, inning >= 10)  ## pitches in 10th inning onwards
  temp.df <- temp.df[, colnames(temp.df) %in% var.list] ## only important variables are kept
  print(paste(i, "th row of ", nrow(df.dates)))
  if(nrow(temp.df) > 5) {
    file.name <- paste0("~/Dropbox/mlb-shirking/Data/Scrape/week", i, ".csv")
    write.csv(temp.df, file.name, row.names = FALSE)
  }
  Sys.sleep(5)    ## pauses scrape to avoid errors in scraper
}

