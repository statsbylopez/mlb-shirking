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

for (i in 1:nrow(df.dates)){
  temp.df <- scrape_statcast_savant_batter_all(as.character(df.dates[i,1]), as.character(df.dates[i, 2]))
  #temp.df <- subset(temp.df, inning >= 10)
  df.pitches <- rbind(df.pitches, temp.df)
  print(paste(i, "th row of ", nrow(df.dates)))
}


## Download pitches from first and tenth innings
write.csv(filter(df.pitches, inning ==1 | inning > 9), "~/Dropbox/MLB_tenth/first_tenth_pitches.csv", row.names = FALSE)


## Alternative code -- to download all pitches in all games
#write.csv(df.pitches, "~/Dropbox/mlb-shirking/Data/allpitches.csv", row.names = FALSE)


