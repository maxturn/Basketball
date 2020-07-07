setwd("~/desktop/R Projects/basketball")
rm(list=ls())

# https://cran.r-project.org/web/packages/ballr/vignettes/use-ballr.html


library(ballr)
library(dplyr)

## Current Standings
standings = NBAStandingsByDate("2019")
standings

players = NBAPerGameStatistics(season = 2019)
GSW = players %>% 
    filter(tm == "GSW")
str(GSW)

GSW$player

Green = GSW %>% 
    filter(player == "Draymond Green")
str(Green)

