################################
# STEP 01 - LOAD INPUT DATA ####
################################


# Libraries ####

library(dplyr)
library(ggplot2)
library(nflplotR)
library(nflreadr)
library(gganimate)


# Data ####

# non-tracking data
games <- readr::read_csv('./input/games.csv')
players <- readr::read_csv('./input/players.csv')
plays <- readr::read_csv('./input/plays.csv')
player_play <- readr::read_csv('./input/player_play.csv')


# tracking data

# check to see if processed version already exists
# if yes, then read in the file. If not, then run loop to create it

if (file.exists('./data/tracking.rds')) {
   
   tracking <- readRDS('./data/tracking.rds')
   
} else {
   
   # tracking files' names and paths
   tracking_files <- list.files(path = './input/', 
                                full.names = TRUE)[grep(pattern = 'tracking', 
                                                        x = list.files('./input/'))]
   
   # initialize list to store results
   tracking_list <- list()
   
   # loop over files
   for (i in seq_along(tracking_files)) {
      
      tracking_list[[i]] <- 
         readr::read_csv(file = tracking_files[i]) %>% 
         mutate(x = ifelse(playDirection == 'left', 120 - x, x),
                y = ifelse(playDirection == 'left', (160 / 3) - y, y),
                dir = ifelse(playDirection == 'left', dir + 180, dir),
                o = ifelse(playDirection == 'left', o + 180, o)) %>% 
         mutate(dir = ifelse(dir > 360, dir - 360, dir),
                o = ifelse(o > 360, o - 360, o))
      
   }
   
   # bind together results into single df
   tracking <- data.table::rbindlist(tracking_list) %>% tibble()
   
   # save results for future loading
   saveRDS(tracking, './data/tracking.rds')
   #readr::write_csv(tracking, file = './data/tracking.csv')
   
   # if successful, remove the loop's temp objects from environment
   if (file.exists('./data/tracking.rds') & nrow(tracking == 59327373)) {
      
      rm(list = c('tracking_files', 'tracking_list', 'i'))
      
   }
   
}
