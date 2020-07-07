setwd("~/desktop/R Projects/basketball")
rm(list=ls())

# http://blog.schochastics.net/post/analyzing-nba-player-data-i-getting-data/

# Used packages
library(tidyverse)  # for data wrangling
library(janitor)  # for data cleaning
library(rvest)  # for web scraping
library(corrplot)  # correlation plots
library(factoextra) # for pca and cluster


# Function to Scrape the Data
scrape_stats <- function(season = 2017) {
    # total stats
    # scrape
    url = paste("https://www.basketball-reference.com/leagues/NBA_",season,"_totals.html", sep="")
    stats_tot <- url %>% 
        read_html() %>% 
        html_table() %>% 
        .[[1]]
    # clean
    player_stats_tot <- stats_tot %>% 
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
    
    # per minute
    url = paste("https://www.basketball-reference.com/leagues/NBA_",season,"_per_minute.html", sep="")
    stats_pm = url %>% 
        read_html() %>% 
        html_table() %>% 
        .[[1]]
    
    player_stats_pm = stats_pm %>% 
        remove_empty("cols") %>% 
        clean_names() %>% 
        filter(!player == "Player") %>% 
        mutate_at(vars(-c(player, tm, pos)), as.numeric) %>% 
        mutate_at(vars(-c(player, tm, pos)), funs(replace(., is.na(.), 0))) %>% 
        as_tibble() %>% 
        group_by(player) %>% 
        slice(1) %>% 
        ungroup() %>% 
        rename_at(vars(9:29), funs(paste(., "-pm"))) %>% 
        select(-rk)
    
    # advanced
    url = paste("https://www.basketball-reference.com/leagues/NBA_",season,"_advanced.html", sep="")
    stats_adv = url %>% 
        read_html() %>% 
        html_table () %>% 
        .[[1]]
    
    player_stats_adv = stats_adv %>% 
        remove_empty("cols") %>% 
        clean_names() %>% 
        filter(!player == "Player") %>% 
        mutate_at(vars(-c(player, tm, pos)), as.numeric) %>% 
        mutate_at(vars(-c(player, tm, pos)), funs(replace(., is.na(.), 0))) %>% 
        as_tibble() %>% 
        group_by(player) %>%  
        slice(1) %>% 
        ungroup() %>% 
        select(-rk)
    
    player_stats <- full_join(player_stats_tot, player_stats_pm,
                              by = c("player", "pos", "age", "tm", "g", "gs", "mp")) %>% 
        full_join(player_stats_adv, 
                  by = c("player", "pos", "age", "tm", "g", "mp"))
    
    return(player_stats)
}

# Scrape some data
scrape_stats(2016)


# Start by building a data frame of player stats from the last two seasons to 
# have a big enough sample for our analysis. We can use the map_dfr() function 
# from the purrr package for this purpose. The function applies the given 
# function to all arguments and binds the result rowwise in a data frame.
player_stats <- map_dfr(2016:2017, scrape_stats)

# reduce the noise of the data by considering only players that played more than
# 500 minutes in a season
player_stats <- player_stats %>% 
    filter(mp >= 500)

# Reduce Dimensionality
# we will use principal component analysis (pca)
pca_nba <- player_stats %>% 
    select(fg:vorp) %>% 
    as.matrix() %>% 
    prcomp(center = TRUE, scale = TRUE, retx = TRUE)

# plot the explain variane per PC
fviz_eig(pca_nba, ncp = 15)

# Now we need to decide on how many components to keep. This is a very 
# subjective matter and there does not seem to be a golden standard rule. I 
# decided on keeping 10 components which together account for roughly 90% of 
# the variance in the data.
player_stats_ld <- pca_nba$x[, 1:10]

# Using the get_pca_var() function from factoextra, we can compute the stats 
# that contribute the most to our chosen components.
nba_var <- get_pca_var(pca_nba)
pcs <- nba_var$contrib[, 1:10]
colnames(pcs) <- paste("PC", 1:10, sep="")

as_tibble(pcs, rownames = "stat") %>% 
    gather(pc, contrib, PC1:PC10) %>% 
    mutate(pc = factor(pc, levels=paste("PC", 1:10, sep=""))) %>% 
    group_by(pc) %>% 
    top_n(5, contrib) %>% 
    ggplot(aes(x=stat, y=contrib)) +
        geom_col() +
        coord_flip() +
        facet_wrap(~pc, scales = "free", ncol = 5) +
        labs(x="", y="")
# The first two components seem to be related to the scoring skills of players 
# (2P and 3P). Very clearly determined are also the last two components, 
# relating to free throws and blocks respectively.

# The silhouette value describes how similar a data point is to its own cluster
# compared to other clusters. It ranges from −1 to +1, where high values 
# indicate that data points are well matched to their own cluster and poorly to 
# other clusters.
fviz_nbclust(player_stats_ld, kmeans, method="silhouette")
# So the silhouette value suggest that there are only 3 clusters of players


# Clustering using kmeans
# - with the "optimal" number of clusters determined, we can call the kmeans()
# function to compute the clustering
player_clus <- kmeans(player_stats_ld, centers = 3)

# visulaization of the cluster centers
as_tibble(player_clus$centers) %>% 
    gather(component, value, PC1:PC10) %>% 
    mutate(clust = rep(1:3, 10)) %>% 
    ggplot(aes(x=factor(component, levels = paste("PC", 10:1, sep="")), 
               y=value)) +
    geom_col() +
    coord_flip() +
    facet_wrap(~clust) +
    labs(x="", y="")
# the first two components already seem to determine the clusters quite well.
# So what do we make out of these clusters? Hard to say, since I am also not 
# that big of an NBA expert. We certainly did not uncover some revolutionary 
# new set of positions.




# Similarity networks of players
# used libraries
library(tidyverse)  # for data wrangling
library(rvest)      # for web scraping
library(janitor)    # for data cleaning
library(igraph)     # for network data structures and tools
library(ggraph)     # for network visulaizations
library(Rtsne)


# scrape the player data
player_stats <- scrape_stats(season = 2020) %>% 
    filter(mp > 150)

head(player_stats)

Labels <- player_stats$pos
player_stats$Labels <-as.factor(player_stats$pos)

colors = rainbow(length(unique(player_stats$Labels)))
names(colors) = unique(player_stats$Labels)

tsne <- Rtsne(player_stats[,-c(1:7)], dims = 2, perplexity=30, verbose = TRUE, 
              max_iter = 500)

plot(tsne$Y, t='n', main="tsne")
text(tsne$Y, labels = player_stats$Labels, col=colors[player_stats$Labels])


tsne_players <- player_stats %>% 
    select(fg:vorp) %>% 
    as.matrix() %>% 
    scale()


tsne_players <- player_stats %>% 
    select(fg:vorp)
# Compute t-SNE without doing the PCA step
tsne_output <- Rtsne(tsne_players, PCA = FALSE, dims = 2, perplexity = 30)

# Show the obtained embedding coordinates
head(tsne_output$Y)

# Store the first two coordinates and plot them 
tsne_plot <- data.frame(tsne_x = tsne_output$Y[, 1], tsne_y = tsne_output$Y[, 2], 
                        digit = as.factor(player_stats$pos))

# Plot the coordinates
ggplot(tsne_plot, aes(x = tsne_x, y = tsne_y, color = digit)) + 
    ggtitle("t-SNE of NBA Data") + 
    geom_text(aes(label = digit)) + 
    theme(legend.position = "none")
