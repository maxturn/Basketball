setwd("~/desktop/R Projects/basketball")
rm(list=ls())

# http://blog.schochastics.net/post/analyzing-nba-player-data-i-getting-data/

# Used packages
library(tidyverse)  # for data wrangling
library(janitor)  # for data cleaning
library(rvest)  # for web scraping
library(corrplot)  # correlation plots


# we will develp a function which automaticallys scrapes data and puts it in 
# a nice format following this structure:
scrape_stats = function(season){
    #scrape
    #clean
    return(player_stats)
}

# Get the total statistics per player to get the html table shown on the page
url = "https://www.basketball-reference.com/leagues/NBA_2017_totals.html"
ostats <- url %>% 
    read_html() %>% 
    html_table() %>% 
    .[[1]]
str(ostats)

# Use the janitor package to do some basic cleaning by turning the stats to 
# numeric variables and NA to 0
stats <- ostats %>% 
    remove_empty("cols") %>%  # if any exists
    clean_names() %>%  # all comlumn names to lower case and removes "%"
    filter(player != "Player") %>%  # deletes the header line in the data frame
    mutate_at(vars(-c(player, tm, pos)), as.numeric) %>%   # this turns all the stat cols numbers
    mutate_at(vars(-c(player, tm, pos)), funs(replace(., is.na(.), 0))) %>%  # turn NA's to 0
    as_tibble()

str(stats)  # now we have a clean stats table

# Some players occur several times, namely those that switch Franchises. We
# Will only keep their total statistics so we can slice the data
stats <- stats %>% 
    group_by(player) %>% 
    slice(1) %>% 
    ungroup()

# Now we can wrap all these steps into a function
scrape_stats <- function(season = 2020) {
    # scrape
    url = paste("https://www.basketball-reference.com/leagues/NBA_",season,"_totals.html", sep="")
    stats_tot <- url %>% 
        read_html() %>% 
        html_table() %>% 
        .[[1]]
    # clean
    player_stats <- stats_tot %>% 
        remove_empty("cols") %>%  # if any exists
        clean_names() %>%  # all comlumn names to lower case and removes "%"
        filter(player != "Player") %>%  # deletes the header line in the data frame
        mutate_at(vars(-c(player, tm, pos)), as.numeric) %>%   # this turns all the stat cols numbers
        mutate_at(vars(-c(player, tm, pos)), funs(replace(., is.na(.), 0))) %>%  # turn NA's to 0
        as_tibble() %>% 
        group_by(player) %>% 
        slice(1) %>% 
        ungroup() %>% 
        select(-rk)  # deleted the rk column
    return(player_stats)
}

scrape_stats(season = 2012)


data <- scrape_stats(season = 2020)

myFavs <- data %>% 
    filter(player %in% c("Trae Young", "Ja Morant", 
                         "James Harden", "Joel Embiid",
                         "Luka Dončić", "LeBron James", "Stephen Curry")) %>% 
    select("player", "pos") %>% 
    print()



names(data)




           