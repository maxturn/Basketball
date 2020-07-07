setwd("~/desktop/R Projects/basketball")
rm(list = ls())

# https://toddwschneider.com/posts/ballr-interactive-nba-shot-charts-with-r-and-shiny/

packages = c("shiny", "ggplot2", "hexbin", "dplyr", "httr", "jsonlite")
install.packages(packages)
library(shiny)
runGitHub("ballr", "toddwschneider")
